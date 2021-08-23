Public Class UserToken

    Public Enum Colors As Integer
        Green
        Red
        Blue
    End Enum
    Private _Color As Colors = Colors.Blue
    Public Property Color As Colors
        Get
            Return _Color
        End Get
        Set(value As Colors)
            _Color = value
            Select Case _Color
                Case Colors.Red
                    rectLeft.Fill = New MyColor(255, 130, 130)
                Case Colors.Green
                    rectLeft.Fill = New MyColor(130, 255, 130)
                Case Colors.Blue
                    rectLeft.Fill = New MyColor(130, 130, 255)
            End Select
        End Set
    End Property

    Public Property LeftText As String
        Get
            Return labLeft.Content
        End Get
        Set(value As String)
            labLeft.Content = value
        End Set
    End Property

    Public Property RightText As String
        Get
            Return labRight.Text
        End Get
        Set(value As String)
            labRight.Text = value
        End Set
    End Property

    Public Sub New()
        InitializeComponent()
    End Sub
    Public Sub New(data As String)
        InitializeComponent()
        On Error Resume Next
        Dim parms() As String = data.Split("/")
        Color = GetStringFromEnum(CType(parms(0), Colors))
        RightText = parms(1)
        LeftText = parms(2)
    End Sub

    Private Sub UserToken_MouseRightButtonUp(sender As Object, e As MouseButtonEventArgs) Handles Me.MouseRightButtonUp
        If frmMain.UserMaster And (Not Color = Colors.Blue) And frmMain.panRoom.Visibility = Visibility.Visible AndAlso MsgBox("你是否要踢出该玩家？", MsgBoxStyle.OkCancel + MsgBoxStyle.Information, "提示") = MsgBoxResult.Ok Then
            Dim Name As String = RightText
            Dim th As New Thread(Sub() frmMain.ClientSend("Kick|" & Name))
            th.Start()
        End If
    End Sub

End Class
