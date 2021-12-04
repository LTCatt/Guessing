#Disable Warning IDE1006
Imports System.ComponentModel
Imports System.Net.Sockets

Public Class formMain

#Region "Load | 初始化"

    Private Sub LoadForm() Handles Me.Loaded
        frmMain = Me
        SGLoad()
    End Sub

#End Region

#Region "Log | 日志框"

    ''' <summary>
    ''' 输出日志消息。
    ''' </summary>
    Public Sub LogShow(text As String)
        On Error Resume Next
        Dispatcher.Invoke(Sub()
                              Try
                                  If text.Contains("WSACancelBlockingCall") Then Exit Sub
                                  Dim log As String = "[" & Date.Now.ToLongTimeString & "." & FillLength(Date.Now.Millisecond, "0", 3) & "] " & text
                                  listLog.Items.Insert(0, log)
                                  If listLog.Items.Count >= 200 Then listLog.Items.RemoveAt(199)
                              Catch
                              End Try
                          End Sub)
    End Sub

    ''' <summary>
    ''' 手动发包。
    ''' </summary>
    Private Sub LogSend() Handles btnLog.Click
        If textLogUser.Text = "" Then
            LogShow("[Server] 已手动发包：" & textLog.Text)
            BoardcastAll(textLog.Text)
            textLog.Text = ""
        Else
            For Each u As UserData In UserList
                If u.Name.Contains(textLogUser.Text) Then
                    u.Send(textLog.Text)
                    LogShow("[Server] 已对用户 " & u.Name & " 手动发包：" & textLog.Text)
                End If
            Next
            textLog.Text = ""
            textLogUser.Text = ""
        End If
    End Sub
    Private Sub textLog_KeyUp(sender As Object, e As KeyEventArgs) Handles textLog.KeyUp, textLogUser.KeyUp
        If e.Key = Key.Enter Then LogSend()
    End Sub

#End Region

#Region "Server | 服务端交流"

    '常量
    Public ServerThread As Thread
    Public ServerSocket As Socket
    Public Const ServerLength As Integer = 1024
    Public ServerEncoding As Encoding = Encoding.UTF8
    Public Const ServerPort As Integer = 2333
    Public Const ServerVersion As Integer = 14
#If DEBUG Then
    Public Const ServerIP As String = "192.168.1.109"
    Public Const ServerHeartbeatFail As Integer = 10000
#Else
    Public Const ServerIP As String = "10.0.8.7"
    Public Const ServerHeartbeatFail As Integer = 2
#End If

    '心跳包
    Public ServerHeartbeatCode As Integer = 1
    Public Const ServerHeartbeatInterval As Integer = 2000
    ''' <summary>
    ''' 发送心跳包。
    ''' </summary>
    Public Sub ServerHeartbeat()
        Dim th As New Thread(Sub()
                                 Do Until ServerState = ServerStates.Closed
                                     Try
                                         '检查已有用户是否有未回应心跳包的
                                         For i = 0 To UserList.Count - 1
                                             If CType(UserList(i), UserData).HeartbeatFail <= ServerHeartbeatFail Then
                                                 CType(UserList(i), UserData).HeartbeatFail += 1
                                             Else
                                                 LogShow("[Server] 用户 " & UserList(i).Name & " 未回应心跳包")
                                                 CType(UserList(i), UserData).Delete(True, "连接超时")
                                             End If
                                         Next
                                         '发送新的心跳包
                                         ServerHeartbeatCode += 1
                                         If ServerHeartbeatCode > 10000 Then ServerHeartbeatCode = 1
                                         BoardcastAll("Beat|" & ServerHeartbeatCode)
                                         '等待
                                         Thread.Sleep(ServerHeartbeatInterval)
                                     Catch ex As Exception
                                         LogShow("[Server] 心跳包发送失败：" & ex.ToString)
                                     End Try
                                 Loop
                             End Sub)
        th.Start()
    End Sub

    '执行
    ''' <summary>
    ''' 服务端的主代码。
    ''' </summary>
    Public Sub ServerRun()
        Try
            ServerSocket = New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp) With {
                                         .ReceiveTimeout = ServerHeartbeatInterval * 3 + 5000,
                                         .SendTimeout = ServerHeartbeatInterval * 3 + 5000
                                     }
            ServerSocket.Bind(New IPEndPoint(IPAddress.Parse(ServerIP), ServerPort))
            ServerSocket.Listen(100)
            While True
                Try
#Region "接取新连接请求的文本，保存为 quest；接取新连接请求的 socket，保存为 newUser"

                    Dim bytes(ServerLength) As Byte
                    Dim newUser As Socket = ServerSocket.Accept()
                    Dim length As Integer = newUser.Receive(bytes)
                    Dim quest As String = ServerEncoding.GetString(bytes, 0, length).Replace("¨", "")
                    LogShow("[User] 接到新用户数据：" & quest)

#End Region
#Region "检验登录请求并返回"

                    '确认为登录请求
                    If Not quest.StartsWith("Login|") Then
                        newUser.Send(ServerEncoding.GetBytes("Exit|请重新登录¨"))
                        LogShow("[User] 该请求并非登录请求，已拒绝。")
                        Continue While
                    End If

                    '检验客户端版本
                    If quest.Replace("Login|", "").Contains("|") Then
                        Dim ClientVersion As Integer = Val(quest.Split("|")(1))
                        If ClientVersion < ServerVersion Then
                            newUser.Send(ServerEncoding.GetBytes("Exit|客户端版本过老，最新版下载：http://t.im/gamecenter¨"))
                            LogShow("[User] 客户端版本过老（" & ClientVersion & "），已拒绝。")
                            Continue While
                        ElseIf ClientVersion > ServerVersion Then
                            newUser.Send(ServerEncoding.GetBytes("Exit|客户端版本过新¨"))
                            LogShow("[User] 客户端版本过新（" & ClientVersion & "），已拒绝。")
                            Continue While
                        End If
                    Else
                        newUser.Send(ServerEncoding.GetBytes("Exit|客户端版本过老，最新版下载：http://t.im/gamecenter¨"))
                        LogShow("[User] 客户端版本过老，已拒绝。")
                        Continue While
                    End If

                    '获取并检查用户名
                    Dim UserName As String = quest.Split("|")(2)
                    If RegexSearch(UserName, "^[\u4e00-\u9fa5_a-zA-Z0-9 ]+$").Count = 0 Then
                        newUser.Send(ServerEncoding.GetBytes("Exit|用户名包含异常字符¨"))
                        LogShow("[User] 用户名包含异常字符（" & UserName & "），已拒绝。")
                        Continue While
                    End If
                    If Len(UserName) < 2 Then
                        newUser.Send(ServerEncoding.GetBytes("Exit|你的用户名过短¨"))
                        LogShow("[User] 用户名过短（" & UserName & "），已拒绝。")
                        Continue While
                    End If
                    If UserName = "Client" Or UserName = "Server" Or UserName = "User" Or UserName = "系统" Or UserName.Contains("|") Or UserName.Contains("¨") Then
                        newUser.Send(ServerEncoding.GetBytes("Exit|用户名非法¨"))
                        LogShow("[User] 用户名非法（" & UserName & "），已拒绝。")
                        Continue While
                    End If

                    '检查用户名或 MAC 重复
                    Dim MAC As String = quest.Split("|")(3)
                    For Each us As UserData In UserList
                        If us.Name = UserName Then
                            newUser.Send(ServerEncoding.GetBytes("Exit|该用户已登录¨"))
                            LogShow("[User] 用户名与现有用户重复（" & UserName & "），已拒绝。")
                            Continue While
                        End If
#If Not DEBUG Then
                        If us.MAC = MAC Then
                            newUser.Send(ServerEncoding.GetBytes("Exit|该电脑上已有其它用户登录¨"))
                            LogShow("[User] MAC 与现有用户重复（" & UserName & " 与 " & us.Name & "），已拒绝。")
                            Continue While
                        End If
#End If
                    Next

                    '添加进用户列表
                    Dim u As New UserData With {
                        .Client = newUser,
                        .Name = UserName,
                        .MAC = MAC
                    }
                    UserList.Add(u)
                    UserJoin(u)

#End Region
                Catch ex As Exception
                    If ServerState = ServerStates.Running Then LogShow("[Server] 服务端运行时出错：" & vbCrLf & ex.ToString)
                End Try
            End While
        Catch ex As Exception
            If ServerState = ServerStates.Running Then LogShow("[Server] 服务端启动时出错：" & vbCrLf & ex.ToString)
            ServerOff()
        End Try
    End Sub
    ''' <summary>
    ''' 处理含有换行符的客户端原始信息，将分拆后的实际数据移交处理。
    ''' </summary>
    Public Sub ServerReceive(data As String, u As UserData)
        Dim datas() As String = data.Split("¨")
        For Each str As String In datas
            If Not str = "" Then ServerExecute(str, u)
        Next
    End Sub

#End Region

#Region "Server | 服务器开关"

    ''' <summary>
    ''' 服务器目前的开启状态。
    ''' </summary>
    Public ServerState As ServerStates = ServerStates.Closed
    Public Enum ServerStates As Integer
        ''' <summary>
        ''' 已关闭。
        ''' </summary>
        Closed
        ''' <summary>
        ''' 已开启。
        ''' </summary>
        Running
        ''' <summary>
        ''' 正在开启。
        ''' </summary>
        Starting
        ''' <summary>
        ''' 正在关闭。
        ''' </summary>
        Closing
    End Enum

    ''' <summary>
    ''' 将服务端状态同步到 UI 按钮。
    ''' </summary>
    Private Sub ServerStateUpdate()
        Dispatcher.Invoke(Sub()
                              Select Case ServerState
                                  Case ServerStates.Closed
                                      btnState.Content = "启动服务端"
                                      btnState.IsEnabled = True
                                  Case ServerStates.Running
                                      btnState.Content = "关闭服务端"
                                      btnState.IsEnabled = True
                                  Case ServerStates.Starting
                                      btnState.Content = "启动中"
                                      btnState.IsEnabled = False
                                  Case ServerStates.Closing
                                      btnState.Content = "关闭中"
                                      btnState.IsEnabled = False
                              End Select
                          End Sub)
    End Sub

    ''' <summary>
    ''' 启动服务端。自带异线程。
    ''' </summary>
    Private Sub ServerOn()
        Dim th As New Thread(Sub()
                                 Try
                                     '状态检测与 Log 处理
                                     ServerStateUpdate()
                                     If Not ServerState = ServerStates.Closed Then Exit Sub
                                     ServerState = ServerStates.Starting
                                     ServerStateUpdate()
                                     LogShow("[Server] 服务端启动中")

                                     '启动服务端的代码
                                     ServerHeartbeat()
                                     ServerThread = New Thread(AddressOf ServerRun)
                                     ServerThread.Start()

                                     '后期处理
                                     ServerState = ServerStates.Running
                                     ServerStateUpdate()
                                     LogShow("[Server] 服务端已启动")
                                 Catch ex As Exception
                                     ServerState = ServerStates.Closed
                                     ServerStateUpdate()
                                     LogShow("[Server] 服务端启动失败：" & ex.ToString())
                                 End Try
                             End Sub)
        th.Start()
    End Sub
    ''' <summary>
    ''' 关闭服务端。自带异线程。
    ''' </summary>
    Private Sub ServerOff(Optional endProgram As Boolean = False)
        Dim th As New Thread(Sub()
                                 Try
                                     '状态检测与 Log 处理
                                     ServerStateUpdate()
                                     If Not ServerState = ServerStates.Running Then Exit Sub
                                     ServerState = ServerStates.Closing
                                     ServerStateUpdate()
                                     LogShow("[Server] 服务端关闭中")

                                     '关闭服务端的代码
                                     Do Until UserList.Count = 0
                                         CType(UserList(0), UserData).Delete(False, "服务器已关闭")
                                     Loop
                                     ServerSocket.Close()
                                     ServerThread.Abort()
                                     RoomList = New ArrayList

                                     '后期处理
                                     LogShow("[Server] 服务端已关闭")
                                 Catch ex As Exception
                                     LogShow("[Server] 服务端关闭时出错：" & ex.ToString())
                                 Finally
                                     ServerState = ServerStates.Closed
                                     ServerStateUpdate()
                                     If endProgram Then End
                                 End Try
                             End Sub)
        th.Start()
    End Sub

    'UI 引发处理
    Private Sub btnState_Click() Handles btnState.Click
        Select Case btnState.Content
            Case "启动服务端"
                ServerOn()
            Case "关闭服务端"
                ServerOff()
        End Select
    End Sub
    Private Sub formClosing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        If ServerState = ServerStates.Running Then
            ServerOff(True)
            e.Cancel = True
        End If
    End Sub

#End Region

#Region "User | 用户"

    ''' <summary>
    ''' 用户实例。
    ''' </summary>
    Public Class UserData

        ''' <summary>
        ''' 用户名。
        ''' </summary>
        Public Name As String
        ''' <summary>
        ''' 用户的 MAC 地址。
        ''' </summary>
        Public MAC As String
        ''' <summary>
        ''' 用户的 Socket。
        ''' </summary>
        Public Client As Socket
        ''' <summary>
        ''' 监听用户的线程。
        ''' </summary>
        Public Thread As Thread
        ''' <summary>
        ''' 连续未回应心跳包数据的次数。
        ''' </summary>
        Public HeartbeatFail As Integer = 0
        ''' <summary>
        ''' 玩家上次发送消息的时间。
        ''' </summary>
        Public LastSend As Integer = My.Computer.Clock.TickCount

        ''' <summary>
        ''' 玩家在游戏房间的状态。
        ''' </summary>
        Public RoomState As UserRoomStates = UserRoomStates.Away
        ''' <summary>
        ''' 玩家所在的游戏房间。
        ''' </summary>
        Public Room As RoomData
        ''' <summary>
        ''' 玩家在游戏中的状态。
        ''' </summary>
        Public GameState As UserGameStates = UserGameStates.NotReady

        ''' <summary>
        ''' 你说我猜的玩家信息。
        ''' </summary>
        Public SG As SGUserData

        ''' <summary>
        ''' 激活这个用户的监听。
        ''' </summary>
        Public Sub Active()
            Thread = New Thread(Sub()
                                    Try
                                        Dim bytes(ServerLength) As Byte
                                        While frmMain.UserList.Contains(Me)
                                            '处理从客户端接收到的 data 字符串
                                            Dim bytesRec As Integer = Client.Receive(bytes)
                                            Dim data As String = frmMain.ServerEncoding.GetString(bytes, 0, bytesRec)
                                            frmMain.ServerReceive(data, Me)
                                        End While
                                    Catch ex As Exception
                                        If frmMain.UserList.Contains(Me) Then
                                            If ex.GetType.Equals(GetType(SocketException)) Then
                                                If CType(ex, SocketException).SocketErrorCode = 10053 Then
                                                    frmMain.LogShow("[" & Name & "] 客户端已关闭")
                                                    Delete(True, ex.Message)
                                                    Exit Sub
                                                End If
                                            End If
                                            frmMain.LogShow("[" & Name & "] 用户监听出错：" & vbCrLf & ex.ToString)
                                            Delete(True, ex.Message)
                                        End If
                                    End Try
                                End Sub)
            Thread.Start()
        End Sub

        ''' <summary>
        ''' 发送信息。
        ''' </summary>
        Public Sub Send(str As String, Optional throwEx As Boolean = True)
            Try
                If Not str.StartsWith("Beat") Then Log("Send: " & str & " (User: " & Name & ")")
                Client.Send(frmMain.ServerEncoding.GetBytes(str & "¨"))
            Catch ex As Exception
                If throwEx Then Throw ex
            End Try
        End Sub

        ''' <summary>
        ''' 从当前用户列表移除本用户。
        ''' </summary>
        Public Sub Delete(boardcast As Boolean, reason As String)
            On Error Resume Next
            If frmMain.UserList.Contains(Me) Then
                frmMain.UserList.Remove(Me)
                Send("Exit|" & reason, False)
                If Not IsNothing(Client) Then
                    Client.Dispose()
                    Client = Nothing
                End If
                frmMain.UserLeave(Me)
            End If
        End Sub

    End Class

    ''' <summary>
    ''' 用户在房间中的状态。
    ''' </summary>
    Public Enum UserRoomStates
        ''' <summary>
        ''' 不在任何房间内。
        ''' </summary>
        Away
        ''' <summary>
        ''' 是房主。
        ''' </summary>
        Master
        ''' <summary>
        ''' 是参与者。
        ''' </summary>
        User
    End Enum
    ''' <summary>
    ''' 用户在游戏中的状态。
    ''' </summary>
    Public Enum UserGameStates
        ''' <summary>
        ''' 未准备。
        ''' </summary>
        NotReady
        ''' <summary>
        ''' 已准备（不是房主）。
        ''' </summary>
        Ready
        ''' <summary>
        ''' 是房主。
        ''' </summary>
        Master
        ''' <summary>
        ''' 旁观。
        ''' </summary>
        Observe
    End Enum

    ''' <summary>
    ''' 当前的用户列表。
    ''' </summary>
    Public UserList As New ArrayList

#End Region

#Region "Room | 房间"

    ''' <summary>
    ''' 房间实例。
    ''' </summary>
    Public Class RoomData

        ''' <summary>
        ''' 房间的名字。
        ''' </summary>
        Public Name As String
        ''' <summary>
        ''' 房主。
        ''' </summary>
        Public Master As UserData
        ''' <summary>
        ''' 其中的用户列表。
        ''' </summary>
        Public Users As ArrayList
        ''' <summary>
        ''' 游戏中状态。
        ''' </summary>
        Public Gaming As Boolean = False

        ''' <summary>
        ''' 你说我猜的房间信息。
        ''' </summary>
        Public SG As New SGRoomData

        ''' <summary>
        ''' 创建房间。
        ''' </summary>
        Public Sub New(m As UserData)
            Name = m.Name & " 的房间"
            Master = m
            Users = New ArrayList
            frmMain.RoomList.Add(Me)
            frmMain.LogShow("[" & m.Name & "] 创建了新的房间")
            Join(m)
            frmMain.Dispatcher.Invoke(Sub() frmMain.labRoom.Content = "房间：" & frmMain.RoomList.Count)
        End Sub
        ''' <summary>
        ''' 加入房间。
        ''' </summary>
        Public Sub Join(user As UserData)
            Users.Add(user)
            user.Room = Me
            user.Send("Room|" & Name)
            user.RoomState = If(user.Equals(Master), UserRoomStates.Master, UserRoomStates.User)
            If Gaming Then
                user.GameState = UserGameStates.Observe
                SGObserve(user)
            Else
                user.GameState = If(user.Equals(Master), UserGameStates.Master, UserGameStates.NotReady)
                frmMain.BoardcastInRoom("Chat|系统：" & user.Name & " 加入了房间。|False¨Chatable|True¨Sureable|False", Me)
            End If
            frmMain.BoardcastInRoom(frmMain.BoardcastList(Me), Me)
            RefreshStartable()
            frmMain.LogShow("[" & user.Name & "] 加入了房间：" & Name)
            frmMain.BoardcastInCenter(frmMain.BoardcastCenter())
        End Sub
        ''' <summary>
        ''' 离开房间。
        ''' </summary>
        Public Sub Leave(user As UserData, Optional boardcast As Boolean = True)
            On Error Resume Next
            If Not Users.Contains(user) Then Exit Sub
            If Master.Equals(user) Then
                '如果是房主则解散房间
                Break()
            Else
                Users.Remove(user)
                user.Room = Nothing
                user.RoomState = UserRoomStates.Away
                user.Send(frmMain.BoardcastCenter())
                RefreshStartable()
                frmMain.LogShow("[" & user.Name & "] 离开了房间：" & Name)
                If boardcast Then frmMain.BoardcastInRoom("Chat|系统：" & user.Name & " 离开了房间。|False", Me)
                '游戏结束判断
                If GetGameUsers().Count < 3 And Gaming Then
                    frmMain.BoardcastInRoom("Chat|系统：由于人数不足，游戏被强制结束！|True", Me)
                    EndGame()
                Else
                    frmMain.BoardcastInRoom(frmMain.BoardcastList(Me), Me)
                    frmMain.BoardcastInCenter(frmMain.BoardcastCenter())
                    SGLeave(user, Me) '游戏内离开判断
                End If
                user.GameState = UserGameStates.NotReady
            End If
        End Sub
        ''' <summary>
        ''' 解散房间。
        ''' </summary>
        Public Sub Break()
            frmMain.RoomList.Remove(Me)
            For Each u As UserData In Users
                u.Room = Nothing
                u.RoomState = UserRoomStates.Away
                u.GameState = UserGameStates.NotReady
                If Not u.Equals(Master) Then u.Send("Msgbox|房主 " & Master.Name & " 解散了房间！")
            Next
            Gaming = False
            Users.Clear()
            frmMain.LogShow("[" & Master.Name & "] 解散了房间：" & Name)
            frmMain.Dispatcher.Invoke(Sub() frmMain.labRoom.Content = "房间：" & frmMain.RoomList.Count)
            frmMain.BoardcastInCenter(frmMain.BoardcastCenter())
        End Sub

        ''' <summary>
        ''' 刷新是否可以开始游戏。
        ''' </summary>
        Public Function RefreshStartable() As Boolean
            RefreshStartable = False
            If Users.Count > 2 Then
                RefreshStartable = True
                For Each user As UserData In Users
                    If user.GameState = UserGameStates.NotReady Then RefreshStartable = False
                Next
            End If
            Master.Send("Start|" & Str(RefreshStartable))
        End Function
        ''' <summary>
        ''' 开始游戏。
        ''' </summary>
        Public Sub StartGame()
            Gaming = True
            frmMain.BoardcastInCenter(frmMain.BoardcastCenter)
            SGStart(Me)
        End Sub
        ''' <summary>
        ''' 结束游戏。
        ''' </summary>
        Public Sub EndGame()
            Gaming = False
            SGEnd(Me)
            For Each u As UserData In Users
                u.GameState = UserGameStates.NotReady
            Next
            Master.GameState = UserGameStates.Master
            frmMain.BoardcastInRoom("Room|" & Name, Me)
            frmMain.BoardcastInRoom(frmMain.BoardcastList(Me), Me)
            frmMain.BoardcastInCenter(frmMain.BoardcastCenter)
        End Sub

        ''' <summary>
        ''' 获取非旁观用户列表。
        ''' </summary>
        ''' <returns></returns>
        Public Function GetGameUsers() As ArrayList
            GetGameUsers = New ArrayList
            For Each u As UserData In Users
                If Not u.GameState = UserGameStates.Observe Then GetGameUsers.Add(u)
            Next
        End Function

    End Class

    ''' <summary>
    ''' 当前的房间列表。
    ''' </summary>
    Public RoomList As New ArrayList

#End Region

#Region "Boardcast | 通告"

    ''' <summary>
    ''' 向所有用户发包。
    ''' </summary>
    Public Sub BoardcastAll(str As String)
        For Each us As UserData In UserList
            us.Send(str)
        Next
    End Sub
    ''' <summary>
    ''' 向所有在大厅的用户发包。
    ''' </summary>
    Public Sub BoardcastInCenter(str As String)
        For Each us As UserData In UserList
            If us.RoomState = UserRoomStates.Away Then us.Send(str)
        Next
    End Sub
    ''' <summary>
    ''' 向所有在某房间的用户发包。
    ''' </summary>
    Public Sub BoardcastInRoom(str As String, room As RoomData)
        For Each us As UserData In room.Users
            us.Send(str)
        Next
    End Sub

    ''' <summary>
    ''' 获取更新房间消息的命令包。
    ''' </summary>
    Public Function BoardcastCenter() As String
        BoardcastCenter = "Center|"
        For Each r As RoomData In RoomList
            If r.Users.Count > 0 Then BoardcastCenter += If(r.Gaming, "游戏中", "等待中") & " / " & r.Name & " / " & r.Users.Count & " 人|"
        Next
    End Function
    ''' <summary>
    ''' 获取更新用户消息的命令包。
    ''' </summary>
    Public Function BoardcastList(room As RoomData) As String
        'List
        BoardcastList = "List|"
        If room.Gaming Then
            BoardcastList += SGList(room)
        Else
            For Each u As UserData In room.Users
                Select Case u.GameState
                    Case UserGameStates.NotReady
                        BoardcastList += "Red/" & u.Name & "/"
                    Case UserGameStates.Ready
                        BoardcastList += "Green/" & u.Name & "/准备"
                    Case UserGameStates.Master
                        BoardcastList += "Blue/" & u.Name & "/房主"
                End Select
                BoardcastList += "/False|"
            Next
        End If
        'Observe
        Dim obCount As Integer = 0
        For Each u As UserData In room.Users
            If u.GameState = UserGameStates.Observe Then obCount += 1
        Next
        BoardcastList += "¨Observe|" & obCount
    End Function

#End Region

    ''' <summary>
    ''' 执行服务端指令。
    ''' </summary>
    Public Sub ServerExecute(Command As String, u As UserData)
        If Not Command.StartsWith("Beat") Then Log("Execute: " & Command & " (User: " & u.Name & ")")
        '预处理信息，获取其类型与参数
        If Command = "" Then Exit Sub
        Dim CommandType As String = Command.Split("|")(0)
        Dim Parm As String = ""
        If Command.StartsWith(CommandType & "|") Then Parm = Command.Substring(CommandType.Length + 1)
        Dim Parms() As String = Parm.Split("|")
        '根据类型执行
        Select Case CommandType

            Case "Exit"
                'Exit()：客户端退出
                u.Delete(True, "")

            Case "Beat"
                'Beat(String 辨识码...)：重置掉线计时
                If Parm = ServerHeartbeatCode Then u.HeartbeatFail = 0

            Case "Create"
                'Create()：创建房间
                If u.RoomState = UserRoomStates.Away Then
                    u.Send("Clear")
                    Dim r As New RoomData(u)
                End If

            Case "Join"
                'Join(Integer 房间编号)：加入游戏房间
                If u.RoomState = UserRoomStates.Away And Val(Parms(0)) < RoomList.Count Then
                    Dim r As RoomData = RoomList(Val(Parms(0)))
                    u.Send("Clear")
                    r.Join(u)
                End If

            Case "Leave"
                'Leave()：离开游戏房间
                If Not u.RoomState = UserRoomStates.Away Then u.Room.Leave(u)

            Case "Chat"
                'Chat(Boolean 赌狗, String 文本)：提交聊天信息
                If My.Computer.Clock.TickCount - u.LastSend < 600 Then
                    u.Send("Chat|系统：你说话太快了，缓缓吧……|False")
                    Exit Sub
                End If
                u.LastSend = My.Computer.Clock.TickCount
                If u.Room.Gaming Then
                    SGChat(Parms(1), Parms(0), u)
                Else
                    If Not u.RoomState = UserRoomStates.Away Then BoardcastInRoom("Chat|" & u.Name & "：" & Parms(1) & "|False", u.Room)
                End If

            Case "Prepare"
                'Prepare(Boolean 是否准备)：切换玩家准备状态
                If u.RoomState = UserRoomStates.User Then
                    Dim newState As Boolean = Parms(0)
                    Dim oldState As Boolean = u.GameState = UserGameStates.Ready
                    If newState Xor oldState Then
                        u.GameState = If(newState, UserGameStates.Ready, UserGameStates.NotReady)
                        u.Room.RefreshStartable()
                        BoardcastInRoom("Chat|系统：" & u.Name & If(newState, " 已准备。", " 取消了准备。") & "|False", u.Room)
                        BoardcastInRoom(BoardcastList(u.Room), u.Room)
                    End If
                End If

            Case "Start"
                'Start()：开始游戏
                If Not u.Room.RefreshStartable Then
                    u.Send(BoardcastList(u.Room))
                    Exit Sub
                End If
                u.Room.StartGame()

            Case "Kick"
                'Kick(String 玩家名)：踢出玩家
                If u.RoomState = UserRoomStates.Master And Not u.Room.Gaming Then
                    For i = 0 To u.Room.Users.Count - 1
                        Dim user As UserData = u.Room.Users(i)
                        If user.Name = Parm Then
                            u.Room.Leave(user, False)
                            BoardcastInRoom("Chat|系统：房主 " & u.Name & " 将 " & user.Name & " 踢出了房间。|False", u.Room)
                            user.Send("Msgbox|房主 " & u.Name & " 将你踢出了房间。")
                            Exit Sub
                        End If
                    Next
                End If

            Case "Select"
                'Select(Integer 选择编号)：选择某个题目编号
                If Not u.SG.State = SGStates.Turning Then Exit Sub
                Dim Selection As Integer = Parm
                '设置题目
                u.Room.SG.Answer = u.Room.SG.Selections(Selection)(0)
                u.Room.SG.Hint = u.Room.SG.Selections(Selection)(1)
                u.Room.SG.Hinted = New List(Of String)
                '发送公告
                For Each us As UserData In u.Room.Users
                    If us.SG.State = SGStates.Turning Then
                        us.Send("Chat|系统：你选择的题目为【" & u.Room.SG.Answer & "】。|True¨" &
                                "Content|请描述：" & u.Room.SG.Answer & "¨" &
                                "Hint|" & u.Room.SG.Answer.Length & " 个字|" & u.Room.SG.Hint & "¨" &
                                "Chatable|True¨Sureable|False")
                    Else
                        us.Send("Content|正在描述")
                    End If
                Next

            Case "Hint"
                'Hint(String 提示内容)：对其余玩家发送提示
                If Not u.SG.State = SGStates.Turning Then Exit Sub
                u.Room.SG.Hinted.Add(Parm)
                For Each us As UserData In u.Room.Users
                    If Parm.EndsWith("个字") Then
                        us.Send("Chat|提示：答案长度为【" & Parm & "】。|True")
                    Else
                        us.Send("Chat|提示：题目类别为【" & Parm & "】。|True")
                    End If
                    If Not us.SG.State = SGStates.Turning Then us.Send("Content|提示：" & Join(u.Room.SG.Hinted.ToArray, "，"))
                Next

        End Select
    End Sub

    ''' <summary>
    ''' 用户加入。
    ''' </summary>
    Public Sub UserJoin(u As UserData)
        '初始化处理
        u.Active()
        Dispatcher.Invoke(Sub()
                              labUser.Content = "用户：" & UserList.Count
                              LogShow("[User] 已接受新用户：" & u.Name)
                          End Sub)
        '对新用户处理
        u.Send(BoardcastCenter())
    End Sub
    ''' <summary>
    ''' 用户离开。
    ''' </summary>
    Public Sub UserLeave(u As UserData)
        '初始化处理
        Dispatcher.Invoke(Sub()
                              frmMain.labUser.Content = "用户：" & frmMain.UserList.Count
                              LogShow("[User] 已移除用户：" & u.Name)
                          End Sub)
        '对用户处理
        If Not IsNothing(u.Room) Then u.Room.Leave(u)
    End Sub

End Class
