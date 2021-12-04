#Disable Warning IDE1006
Imports System.Net.Sockets

Public Class formMain

#Region "Load | 初始化"

    Public Const LoadTitle As String = "你说我猜"
    Private Sub LoadForm() Handles Me.Loaded
        FrmMain = Me
        AniStartRun()
        Title = LoadTitle
    End Sub

#End Region

#Region "Client | 客户端交流"

    '常量
    Public ClientSocket As Socket
    Public Const ClientLength As Integer = 1024
    Public ClientEncoding As Encoding = Encoding.UTF8
    Public Const ClientPort As Integer = 2333
    Public Const ClientVersion As Integer = 14
#If DEBUG Then
    Public Const ClientIP As String = "192.168.1.109"
    Public ClientHeartbeatTimeout As Integer = 10000
#Else
    Public Const ClientIP As String = "119.91.71.4"
    Public ClientHeartbeatTimeout As Integer = 5
#End If

    '发送
    ''' <summary>
    ''' 向服务端发送信息。
    ''' </summary>
    ''' <param name="data">要发送的信息的完整内容。</param>
    ''' <param name="showError">在发送失败时是否抛出异常。</param>
    Public Sub ClientSend(data As String, Optional showError As Boolean = True)
        If Not data.StartsWith("Beat") Then Log("Send: " & data)
        Dim th As New Thread(Sub()
                                 Try
                                     ClientSocket.Send(ClientEncoding.GetBytes(data & "¨"))
                                 Catch ex As Exception
                                     If showError Then Throw ex
                                 End Try
                             End Sub)
        th.Start()
    End Sub

    '心跳包
    Public ClientHeartbeatCount As Integer = 0
    ''' <summary>
    ''' 计量心跳包延迟，并在未收到数据时断开链接。
    ''' </summary>
    Public Sub ClientHeartbeat()
        Dim th As New Thread(Sub()
                                 Do Until UserState = UserStates.Offline
                                     ClientHeartbeatCount += 1
                                     If ClientHeartbeatCount >= ClientHeartbeatTimeout Then ClientExecute("Exit|连接超时")
                                     Thread.Sleep(1000)
                                 Loop
                             End Sub)
        th.Start()
    End Sub

    '执行
    ''' <summary>
    ''' 接受服务端数据。
    ''' </summary>
    Public Sub ClientReceiver()
        Dim th As New Thread(Sub()
                                 Try
                                     Dim bytes(ClientLength) As Byte
                                     Do Until UserState = UserStates.Offline
                                         Dim bytesRec As Integer = ClientSocket.Receive(bytes)
                                         ClientReceive(ClientEncoding.GetString(bytes, 0, bytesRec))
                                     Loop
                                 Catch ex As Exception
                                     Try
                                         ClientReceive("Exit|" & ex.Message)
                                     Catch
                                     End Try
                                 End Try
                             End Sub)
        th.Start()
    End Sub
    ''' <summary>
    ''' 处理含有换行符的服务器原始信息，将分拆后的实际数据移交处理。
    ''' </summary>
    Public Sub ClientReceive(data As String)
        Dim datas() As String = data.Split("¨")
        For Each str As String In datas
            If Not str = "" Then ClientExecute(str)
        Next
    End Sub

#End Region 'Send, Execute, Version

#Region "User | 用户数据"

    ''' <summary>
    ''' 用户名。
    ''' </summary>
    Public UserName As String
    ''' <summary>
    ''' 用户是否为房主。
    ''' </summary>
    Public UserMaster As Boolean = False

    ''' <summary>
    ''' 用户目前的页面。
    ''' </summary>
    Public Property UserState As UserStates
        Get
            Return _UserState
        End Get
        Set(value As UserStates)
            If value = _UserState Then Exit Property
            _UserState = value
            Dispatcher.Invoke(Sub() UserStateChange(value))
        End Set
    End Property
    Private _UserState As UserStates = UserStates.Offline
    Public Enum UserStates As Integer
        ''' <summary>
        ''' 尚未登录。
        ''' </summary>
        Offline
        ''' <summary>
        ''' 正在大厅中。
        ''' </summary>
        Center
        ''' <summary>
        ''' 正在房间中。
        ''' </summary>
        Room
        ''' <summary>
        ''' 正在游戏中。
        ''' </summary>
        Game
    End Enum

    ''' <summary>
    ''' 更新用户页面状态，由 UserState_Set 在 UI 线程中自动触发。
    ''' </summary>
    ''' <param name="newState"></param>
    Private Sub UserStateChange(newState As UserStates)
        Dispatcher.Invoke(Sub()
                              Try
                                  Select Case UserState
                                      Case UserStates.Offline
                                          panLogin.Visibility = Visibility.Visible
                                          panCenter.Visibility = Visibility.Collapsed
                                          panRoom.Visibility = Visibility.Collapsed
                                          panChat.Visibility = Visibility.Collapsed
                                          panList.Visibility = Visibility.Collapsed
                                          panGame.Visibility = Visibility.Collapsed
                                      Case UserStates.Center
                                          panLogin.Visibility = Visibility.Collapsed
                                          panCenter.Visibility = Visibility.Visible
                                          panRoom.Visibility = Visibility.Collapsed
                                          panChat.Visibility = Visibility.Collapsed
                                          panList.Visibility = Visibility.Collapsed
                                          panGame.Visibility = Visibility.Collapsed
                                      Case UserStates.Room
                                          panLogin.Visibility = Visibility.Collapsed
                                          panCenter.Visibility = Visibility.Collapsed
                                          panRoom.Visibility = Visibility.Visible
                                          panChat.Visibility = Visibility.Visible
                                          panList.Visibility = Visibility.Visible
                                          panGame.Visibility = Visibility.Collapsed
                                          panChat.Margin = New Thickness(250, 90, 25, 25)
                                      Case UserStates.Game
                                          panLogin.Visibility = Visibility.Collapsed
                                          panCenter.Visibility = Visibility.Collapsed
                                          panRoom.Visibility = Visibility.Collapsed
                                          panChat.Visibility = Visibility.Visible
                                          panList.Visibility = Visibility.Visible
                                          panGame.Visibility = Visibility.Visible
                                  End Select
                              Catch
                              End Try
                          End Sub)
    End Sub

#End Region 'Name, State

#Region "Login | 登录"

    ''' <summary>
    ''' 开始登录。
    ''' </summary>
    Public Sub LoginOn(name As String)
        If btnLogin.IsEnabled = False Then Exit Sub
        If Not UserState = UserStates.Offline Then Exit Sub
        btnLogin.IsEnabled = False
        labLogin.Content = ""
        UserName = name
        Dim th As New Thread(Sub()
                                 Try

#Region "发送登录请求并重新构建 ClientSocket"

                                     '构建 Socket
                                     If Not IsNothing(ClientSocket) Then ClientSocket.Dispose()
                                     Dim remoteEP As New IPEndPoint(IPAddress.Parse(ClientIP), ClientPort)
                                     ClientSocket = New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp) With {
                                         .ReceiveTimeout = ClientHeartbeatTimeout * 1000 + 5000,
                                         .SendTimeout = ClientHeartbeatTimeout * 1000 + 5000
                                     }
                                     '连接并发送登录请求
                                     ClientSocket.Connect(remoteEP)
                                     ClientSend("Login|" & ClientVersion & "|" & UserName.Replace("¨", "-").Replace("|", "/") & "|" & NetworkInformation.NetworkInterface.GetAllNetworkInterfaces(0).GetPhysicalAddress.ToString)
                                     '接取信息
                                     Dim bytes(ClientLength) As Byte
                                     Dim bytesRec As Integer = ClientSocket.Receive(bytes)
                                     Dim ret As String = ClientEncoding.GetString(bytes, 0, bytesRec)

#End Region

#Region "登录成功的处理"

                                     If ret.Contains("Center") Then
                                         '转交接受的所有信息
                                         ClientReceive(ret)
                                         '重置心跳包
                                         ClientHeartbeat()
                                         ClientHeartbeatCount = 0
                                         '开始接受数据
                                         ClientReceiver()
                                         '更改标题
                                         Dispatcher.Invoke(Sub() Title = LoadTitle & " - " & UserName)
                                         Exit Sub
                                     End If

#End Region

#Region "登录失败的处理（递交 Exit 请求）"

                                     '由非“Center|”的返回值触发
                                     If ret.StartsWith("Exit|") Then
                                         ClientReceive(ret)
                                     Else
                                         ClientReceive("Exit|未知信息：" & ret.Replace("Error|", ""))
                                     End If
                                 Catch ex As Exception
                                     '由一个 Exception 触发
                                     Try
                                         If ex.GetType.Equals(GetType(SocketException)) Then
                                             If CType(ex, SocketException).ErrorCode = 10061 Then
                                                 ClientReceive("Exit|服务器已关闭")
                                                 Exit Sub
                                             End If
                                         End If
                                         ClientReceive("Exit|" & ex.Message)
                                     Catch
                                     End Try

#End Region

                                 Finally
                                     Try
                                         Dispatcher.Invoke(Sub() btnLogin.IsEnabled = True)
                                     Catch
                                     End Try
                                 End Try
                             End Sub)
        th.Start()
    End Sub
    ''' <summary>
    ''' 退出登录并返回到登录页面。
    ''' </summary>
    Public Sub LoginOff() Handles Me.Closing
        If UserState = UserStates.Offline Then Exit Sub
        Dim th As New Thread(AddressOf LoginOffCode)
        th.Start()
    End Sub
    Private Sub LoginOffCode()
        '由于 Lamuda 语句不支持 OERN，故独立写作一个 Sub
        On Error Resume Next
        '更改标题
        Dispatcher.Invoke(Sub() Title = LoadTitle)
        '向服务器发出退出请求
        ClientSend("Exit", False)
        '更新 UI
        UserState = UserStates.Offline
    End Sub

    '触发登录
    Private Sub btnLogin_Click() Handles btnLogin.Click
        LoginOn(textLogin.Text.Trim)
    End Sub
    Private Sub textLogin_KeyUp(sender As Object, e As KeyEventArgs) Handles textLogin.KeyUp
        If e.Key = Key.Enter And btnLogin.IsEnabled Then btnLogin_Click()
    End Sub

#End Region 'On, Off

#Region "Chat | 聊天框"

    ''' <summary>
    ''' 清空聊天消息。
    ''' </summary>
    Public Sub ChatClear()
        On Error Resume Next
        Dispatcher.Invoke(Sub() listChat.Items.Clear())
    End Sub

    ''' <summary>
    ''' 输出聊天消息。
    ''' </summary>
    Public Sub ChatShow(text As String, isbold As Boolean)
        On Error Resume Next
        Dispatcher.Invoke(Sub()
                              Try
                                  Dim tb As New TextBlock With {
                                    .Text = text,
                                    .FontSize = 14,
                                    .FontWeight = If(isbold, FontWeights.Bold, FontWeights.Normal),
                                    .Tag = GetUUID()
                                  }
                                  listChat.Items.Add(tb)
                                  If listChat.Items.Count >= 100 Then listChat.Items.RemoveAt(0)
                                  listChat.ScrollIntoView(tb)
                              Catch
                              End Try
                          End Sub)
    End Sub

    ''' <summary>
    ''' 用户主动发送文本框内的聊天消息。
    ''' </summary>
    Private Sub ChatSend() Handles btnChat.Click
        If textChat.Text.Trim = "" Then Exit Sub
        If Keyboard.IsKeyDown(Key.LeftCtrl) AndAlso CheckSure.Visibility = Visibility.Visible Then CheckSure.IsChecked = True
        ClientSend(New RegularExpressions.Regex("[\u0000-\u001F\u007F-\u00A0]").Replace("Chat|" & CheckSure.IsChecked & "|" & textChat.Text.Replace("¨", "-").Replace("|", "/"), ""))
        textChat.Text = ""
        CheckSure.IsChecked = False
    End Sub
    Private Sub textChat_KeyUp(sender As Object, e As KeyEventArgs) Handles textChat.KeyUp
        If e.Key = Key.Enter Then ChatSend()
    End Sub

#End Region 'Show, Clear

#Region "Center | 大厅"

    ''' <summary>
    ''' 选择房间。
    ''' </summary>
    Private Sub listCenter_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles listCenter.SelectionChanged
        btnCenterJoin.IsEnabled = False
        If listCenter.SelectedIndex >= 0 Then btnCenterJoin.IsEnabled = True
    End Sub

    ''' <summary>
    ''' 创建房间。
    ''' </summary>
    Private Sub btnCenterCreate_Click(sender As Object, e As RoutedEventArgs) Handles btnCenterCreate.Click
        UserMaster = True
        ClientSend("Create")
        btnRoomExit.IsEnabled = True
    End Sub

    ''' <summary>
    ''' 加入房间。
    ''' </summary>
    Private Sub btnCenterJoin_Click(sender As Object, e As RoutedEventArgs) Handles btnCenterJoin.Click
        UserMaster = False
        ClientSend("Join|" & listCenter.SelectedIndex)
        btnRoomExit.IsEnabled = True
    End Sub

#End Region

#Region "Room | 房间"

    ''' <summary>
    ''' 退出房间。
    ''' </summary>
    Private Sub btnRoomExit_Click(sender As Object, e As RoutedEventArgs) Handles btnRoomExit.Click, btnGameExit.Click
        If UserMaster And panList.Items.Count > 1 Then
            If MsgBox("你是该房间的房主，退出房间会导致房间被解散，是否确认？", MsgBoxStyle.Exclamation + MsgBoxStyle.OkCancel, "提示") = MsgBoxResult.Cancel Then Exit Sub
        ElseIf UserState = UserStates.Game And Not labGameTitle.Content.ToString.Contains("旁观") Then
            If MsgBox("当前正在游戏中，突然下线是很没人品的行为哦，是否确认？", MsgBoxStyle.Exclamation + MsgBoxStyle.OkCancel, "提示") = MsgBoxResult.Cancel Then Exit Sub
        End If
        ClientSend("Leave")
    End Sub

    ''' <summary>
    ''' 准备 / 取消准备 / 开始游戏。
    ''' </summary>
    Private Sub btnRoomPrepare_Click(sender As Object, e As RoutedEventArgs) Handles btnRoomPrepare.Click
        Select Case btnRoomPrepare.Text
            Case "准备"
                ClientSend("Prepare|True")
                btnRoomPrepare.Text = "取消准备"
                btnRoomExit.IsEnabled = False
                btnRoomPrepare.IsEnabled = False
            Case "取消准备"
                ClientSend("Prepare|False")
                btnRoomPrepare.Text = "准备"
                btnRoomExit.IsEnabled = True
            Case "开始游戏"
                ClientSend("Start")
        End Select
        btnRoomPrepare.IsEnabled = False
        Dim th As New Thread(Sub()
                                 Thread.Sleep(1000)
                                 Dispatcher.Invoke(Sub() btnRoomPrepare.IsEnabled = True)
                             End Sub)
        th.Start()
    End Sub

#End Region

#Region "Game | 游戏"

    Private Sub timerGame_Tick() Handles timerGame.Tick
        If UserState = UserStates.Game And Val(labGameTimer.Content) > 0 Then labGameTimer.Content = Val(labGameTimer.Content) - 1
    End Sub
    Private Sub btnGameSelect1_Click(sender As Button, e As EventArgs) Handles btnGameSelect1.Click, btnGameSelect2.Click
        Dim SendContent As String = sender.Tag
        RunInThread(Sub() ClientSend(SendContent))
        sender.IsEnabled = False
    End Sub

#End Region

#Region "List | 列表"

    Private Sub panList_SelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles panList.SelectionChanged
        panList.SelectedIndex = -1
    End Sub

#End Region

    ''' <summary>
    ''' 执行客户端指令。
    ''' </summary>
    Public Sub ClientExecute(Command As String)
        If Not Command.StartsWith("Beat") Then Log("Execute: " & Command)
        '预处理信息，获取其类型与参数
        If Command = "" Then Exit Sub
        Dim CommandType As String = Command.Split("|")(0)
        Dim Parm As String = ""
        If Command.StartsWith(CommandType & "|") Then Parm = Command.Substring(CommandType.Length + 1)
        Dim Parms() As String = Parm.Split("|")
        '根据类型执行
        Select Case CommandType

            Case "Exit"
                'Exit(String 退出原因...)：退出登录并返回登录页面，在登录页面显示退出原因
                Dispatcher.Invoke(Sub() If labLogin.Content = "" Then labLogin.Content = Parm)
                LoginOff()

            Case "Chat"
                'Chat(String 文本, Boolean 是否加粗)：在聊天栏增加一行文本，由于是在 List 显示故可以换行
                ChatShow(Parms(0), If(Parms.Length = 1, False, Parms(1)))

            Case "Sureable"
                'Sureable(Boolean 是否可以启用超勇)：启用或关闭超勇
                Dispatcher.Invoke(Sub()
                                      CheckSure.Visibility = If(Parm, Visibility.Visible, Visibility.Collapsed)
                                  End Sub)

            Case "Chatable"
                'Chatable(Boolean 是否可以聊天)：启用或关闭聊天
                Dispatcher.Invoke(Sub()
                                      textChat.IsEnabled = Parm
                                      btnChat.IsEnabled = Parm
                                      If Parm Then textChat.Focus()
                                  End Sub)

            Case "Clear"
                'Clear()：清空聊天栏
                ChatClear()

            Case "Beat"
                'Beat(String 辨识码...)：回应心跳包并重置掉线计时
                ClientSend("Beat|" & Parm, False)
                ClientHeartbeatCount = 0

            Case "Center"
                'Center(String[] 名称...)：进入大厅，并给出房间列表
                UserState = UserStates.Center
                Dispatcher.Invoke(Sub()
                                      listCenter.Items.Clear()
                                      listCenter.SelectedIndex = -1
                                      For Each Room As String In Parms
                                          If Room.Trim.Length > 0 Then listCenter.Items.Add(Room.Trim)
                                      Next
                                  End Sub)

            Case "Room"
                'Room(String 名称)：进入房间，并给出房间名
                UserState = UserStates.Room
                Dispatcher.Invoke(Sub()
                                      btnRoomPrepare.IsEnabled = Not UserMaster
                                      btnRoomPrepare.Text = If(UserMaster, "开始游戏", "准备")
                                      btnRoomExit.IsEnabled = True
                                      labRoomName.Content = Parms(0)
                                  End Sub)

            Case "Game"
                'Game(String 标题)：进入游戏页面，并给出标题
                UserState = UserStates.Game
                Dispatcher.Invoke(Sub()
                                      labGameTitle.Content = Parms(0)
                                      Beep()
                                  End Sub)

            Case "Msgbox"
                'Msgbox(String 内容...)：弹窗
                Dim th As New Thread(Sub() MsgBox(Parm, MsgBoxStyle.Information, "提示"))
                th.Start()

            Case "List"
                'List(String[] 颜色/右/左/角标)：刷新用户列表
                Dispatcher.Invoke(Sub()
                                      panList.Items.Clear()
                                      For Each u As String In Parms
                                          If Not u = "" Then
                                              Dim token As New UserToken(u) With {.Padding = New Thickness(2, 4, 0, 4), .Width = 197}
                                              panList.Items.Add(token)
                                          End If
                                      Next
                                  End Sub)

            Case "Start"
                'Start(Boolean 是否可以开始)：向房主切换是否可以开始游戏
                If Not (UserMaster And UserState = UserStates.Room) Then Exit Sub
                Dispatcher.Invoke(Sub() btnRoomPrepare.IsEnabled = Parms(0))

            Case "Content"
                'Content(String 内容...)：改变游戏显示区内容
                Dispatcher.Invoke(Sub() labGameContent.Content = Parm)

            Case "Timer"
                'Timer(Integer 时间, String 颜色)：设置倒计时
                Dispatcher.Invoke(Sub()
                                      timerGame.Reset()
                                      labGameTimer.Content = Parms(0)
                                      Select Case Parms(1)
                                          Case "Orange"
                                              labGameTimer.Foreground = New MyColor(255, 128, 0)
                                          Case "Red"
                                              labGameTimer.Foreground = New MyColor(255, 0, 0)
                                          Case "Blue"
                                              labGameTimer.Foreground = New MyColor(0, 128, 255)
                                      End Select
                                  End Sub)

            Case "Observe"
                'Observe(Integer 人数)：显示旁观者人数
                Dispatcher.Invoke(Sub()
                                      If Val(Parms(0)) = 0 Then
                                          labGameObserve.Visibility = Visibility.Collapsed
                                      Else
                                          labGameObserve.Visibility = Visibility.Visible
                                          labGameObserve.Content = "旁观者：" & Parms(0) & " 人"
                                      End If
                                  End Sub)

            Case "Select"
                'Select(String 选择1, String 选择2)：改变选题区显示
                RunInUi(Sub()
                            '显示
                            panChat.Margin = New Thickness(250, 200, 25, 25)
                            btnGameSelect1.Text = Parms(0)
                            btnGameSelect1.Tag = "Select|0"
                            btnGameSelect1.IsEnabled = True
                            btnGameSelect2.Text = Parms(1)
                            btnGameSelect2.Tag = "Select|1"
                            btnGameSelect2.IsEnabled = True
                        End Sub)

            Case "Hint"
                'Hint(String 提示1, String 提示2)：让选题区显示提示选项
                RunInUi(Sub()
                            '显示
                            panChat.Margin = New Thickness(250, 200, 25, 25)
                            btnGameSelect1.Text = "发送长度提示：" & Parms(0)
                            btnGameSelect1.Tag = "Hint|" & Parms(0)
                            btnGameSelect1.IsEnabled = True
                            btnGameSelect2.Text = "发送类别提示：" & Parms(1)
                            btnGameSelect2.Tag = "Hint|" & Parms(1)
                            btnGameSelect2.IsEnabled = True
                        End Sub)

            Case "SelectClear"
                'SelectClear：隐藏选题区
                RunInUi(Sub() panChat.Margin = New Thickness(250, 130, 25, 25))

        End Select
    End Sub

End Class
