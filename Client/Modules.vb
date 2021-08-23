Imports System.Security.Cryptography
Imports System.Windows.Threading

Public Module Modules

#Region "声明"

    Public Const MAINFORM_NAME As String = "你说我猜"
    Public Const APPLICATION_NAME As String = "GC"
    Public frmMain As formMain
    ''' <summary>
    ''' 程序的启动路径，以“\”结尾。
    ''' </summary>
    ''' <remarks></remarks>
    Public PATH As String = GetPathFromFullPath(System.Windows.Forms.Application.ExecutablePath)
    Public PATH_IMAGE As String = "pack://application:,,,/images/"
    Public Rnd As New Random
    Public BitmapCache As New Dictionary(Of String, MyBitmap)

#End Region

#Region "API"

    ''' <summary>
    ''' 获取指定窗口的句柄。返回查找到的第一个窗口的句柄。
    ''' </summary>
    ''' <param name="lClientassName">窗口的类名，使用Spy++可以查看。</param>
    ''' <param name="lpWindowName">窗口的标题。</param>
    ''' <returns>查找到的第一个窗口的句柄。</returns>
    ''' <remarks></remarks>
    Public Declare Function FindWindow Lib "user32" Alias "FindWindowA" (ByVal lClientassName As String, ByVal lpWindowName As String) As Integer
    ''' <summary>
    ''' 彻底释放某个句柄对应项目的资源。
    ''' </summary>
    ''' <param name="hObject">需要释放资源的句柄。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Declare Function DeleteObject Lib "gdi32" Alias "DeleteObject" (ByVal hObject As IntPtr) As Boolean
    ''' <summary>
    ''' 根据句柄设置对应窗口的标题。
    ''' </summary>
    ''' <param name="hwnd">窗口句柄。</param>
    ''' <param name="lpString">要设置的标题。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Declare Function SetWindowText Lib "user32" Alias "SetWindowTextA" (ByVal hwnd As Integer, ByVal lpString As String) As Integer
    ''' <summary>
    ''' 获取某个句柄对应的窗口的所属进程ID。
    ''' </summary>
    ''' <param name="hwnd">窗口句柄。</param>
    ''' <param name="lpdwProcessId">获取到的ID。需要变量承载。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Declare Function GetWindowThreadProcessId Lib "user32" Alias "GetWindowThreadProcessId" (ByVal hwnd As Integer, ByVal lpdwProcessId As Integer) As Integer
    ''' <summary>
    ''' 设置某个句柄对应的窗口的属性。
    ''' </summary>
    ''' <param name="hwnd">窗口句柄。</param>
    ''' <param name="hWndInsertAfter">置顶标记。-1 为置顶，-2 为不置顶。</param>
    ''' <param name="x">窗口的 X 坐标。</param>
    ''' <param name="y">窗口的 Y 坐标。</param>
    ''' <param name="cx">窗口的宽度。</param>
    ''' <param name="cy">窗口的高度。</param>
    ''' <param name="wFlags">利用 Or 连接的参数。
    ''' SWP_HIDEWINDOW：隐藏窗口。
    ''' SWP_SHOWWINDOW：显示窗口。
    ''' SWP_NOMOVE：维持当前位置（忽略 X 和 Y 参数）。
    ''' SWP_NOSIZE：维持当前尺寸（忽略 cx 和 cy 参数）。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Declare Function SetWindowPos Lib "user32" Alias "SetWindowPos" (ByVal hwnd As Integer, ByVal hWndInsertAfter As Integer, ByVal x As Integer, ByVal y As Integer, ByVal cx As Integer, ByVal cy As Integer, ByVal wFlags As Integer) As Integer

    Public Const SWP_HIDEWINDOW As Integer = 8 * 16 '0x0080
    Public Const SWP_SHOWWINDOW As Integer = 4 * 16 '0x0040
    Public Const SWP_NOMOVE As Integer = 2 '0x0002
    Public Const SWP_NOSIZE As Integer = 1 '0x0001

#End Region

#Region "类"

    ''' <summary>
    ''' 支持浮点数与常见类型隐式转换的颜色。
    ''' </summary>
    ''' <remarks></remarks>
    Public Class MyColor

        '属性
        Public A As Double = 0
        Public R As Double = 0
        Public G As Double = 0
        Public B As Double = 0

        '类型转换
        Public Shared Widening Operator CType(ByVal col As Color) As MyColor
            Return New MyColor(col)
        End Operator
        Public Shared Widening Operator CType(ByVal conv As MyColor) As Color
            Return Color.FromArgb(MathByte(conv.A), MathByte(conv.R), MathByte(conv.G), MathByte(conv.B))
        End Operator
        Public Shared Widening Operator CType(ByVal conv As MyColor) As System.Drawing.Color
            Return System.Drawing.Color.FromArgb(MathByte(conv.A), MathByte(conv.R), MathByte(conv.G), MathByte(conv.B))
        End Operator
        Public Shared Widening Operator CType(ByVal bru As SolidColorBrush) As MyColor
            Return New MyColor(bru.Color)
        End Operator
        Public Shared Widening Operator CType(ByVal conv As MyColor) As SolidColorBrush
            Return New SolidColorBrush(Color.FromArgb(MathByte(conv.A), MathByte(conv.R), MathByte(conv.G), MathByte(conv.B)))
        End Operator
        Public Shared Widening Operator CType(ByVal bru As Brush) As MyColor
            Return New MyColor(bru)
        End Operator
        Public Shared Widening Operator CType(ByVal conv As MyColor) As Brush
            Return New SolidColorBrush(Color.FromArgb(MathByte(conv.A), MathByte(conv.R), MathByte(conv.G), MathByte(conv.B)))
        End Operator

        '颜色运算
        Shared Operator +(ByVal a As MyColor, ByVal b As MyColor) As MyColor
            Return New MyColor With {.A = a.A + b.A, .B = a.B + b.B, .G = a.G + b.G, .R = a.R + b.R}
        End Operator
        Shared Operator -(ByVal a As MyColor, ByVal b As MyColor) As MyColor
            Return New MyColor With {.A = a.A - b.A, .B = a.B - b.B, .G = a.G - b.G, .R = a.R - b.R}
        End Operator
        Shared Operator *(ByVal a As MyColor, ByVal b As Double) As MyColor
            Return New MyColor With {.A = a.A * b, .B = a.B * b, .G = a.G * b, .R = a.R * b}
        End Operator
        Shared Operator /(ByVal a As MyColor, ByVal b As Double) As MyColor
            Return New MyColor With {.A = a.A / b, .B = a.B / b, .G = a.G / b, .R = a.R / b}
        End Operator

        '构造函数
        Public Sub New()
        End Sub
        Public Sub New(ByVal col As Color)
            Me.A = col.A
            Me.R = col.R
            Me.G = col.G
            Me.B = col.B
        End Sub
        Public Sub New(ByVal newA As Double, ByVal col As Color)
            Me.A = newA
            Me.R = col.R
            Me.G = col.G
            Me.B = col.B
        End Sub
        Public Sub New(ByVal newR As Double, ByVal newG As Double, ByVal newB As Double)
            Me.A = 255
            Me.R = newR
            Me.G = newG
            Me.B = newB
        End Sub
        Public Sub New(ByVal newA As Double, ByVal newR As Double, ByVal newG As Double, ByVal newB As Double)
            Me.A = newA
            Me.R = newR
            Me.G = newG
            Me.B = newB
        End Sub
        Public Sub New(brush As Brush)
            Dim col As New MyColor(CType(brush, SolidColorBrush).Color)
            A = col.A
            R = col.R
            G = col.G
            B = col.B
        End Sub
        Public Sub New(obj As Object)
            A = obj.A
            R = obj.R
            G = obj.G
            B = obj.B
        End Sub

        'HSL
        Public Function Hue(ByVal v1 As Double, ByVal v2 As Double, ByVal vH As Double) As Double
            If vH < 0 Then vH += 1
            If vH > 1 Then vH -= 1
            If vH < 0.16667 Then Return v1 + (v2 - v1) * 6 * vH
            If vH < 0.5 Then Return v2
            If vH < 0.66667 Then Return v1 + (v2 - v1) * (4 - vH * 6)
            Return v1
        End Function
        Public Function FromHSL(ByVal sH As Double, ByVal sS As Double, ByVal sL As Double) As MyColor
            If sS = 0 Then
                R = sL * 2.55
                G = R
                B = R
            Else
                Dim H = sH / 360
                Dim S = sS / 100
                Dim L = sL / 100
                S = If(L < 0.5, S * L + L, S * (1.0 - L) + L)
                L = 2 * L - S
                R = 255 * Hue(L, S, H + 1 / 3)
                G = 255 * Hue(L, S, H)
                B = 255 * Hue(L, S, H - 1 / 3)
            End If
            A = 255
            Return Me
        End Function
        Public Function FromHSL2(ByVal sH As Double, ByVal sS As Double, ByVal sL As Double) As MyColor
            If sS = 0 Then
                R = sL * 2.55 : G = R : B = R
            Else
                Dim cent As Double() = {0.00, +0.1, +0.25, +0.25, +0.25, +0.25, +0.3, +0.07, -0.1, -0.05, +0.05, +0.02, 0.00, 0.00}
                Dim center As Double = sH / 30.0
                Dim intCenter As Integer = center
                center = 50 + (cent(intCenter) + (center - intCenter) * (cent(intCenter + 1) - cent(intCenter))) * sS
                sL = (If(sL < center, sL / center, 1 + (sL - center) / (100 - center))) * 50
                FromHSL(sH, sS, sL)
            End If
            A = 255
            Return Me
        End Function

        '复写
        Public Shadows Function ToString() As String
            Return "(" & A & "," & R & "," & G & "," & B & ")"
        End Function

    End Class

    Public Class AdvancedRect

        '属性
        Public Property Width As Double = 0
        Public Property Height As Double = 0
        Public Property Left As Double = 0
        Public Property Top As Double = 0

        '构造函数
        Public Sub New()
        End Sub
        Public Sub New(ByVal left As Double, ByVal top As Double, ByVal width As Double, ByVal height As Double)
            Me.Left = left
            Me.Top = top
            Me.Width = width
            Me.Height = height
        End Sub

    End Class '浮点矩形
    Public Class FileSize

        Public Value As Integer = 0

        '类型转换
        Public Shared Widening Operator CType(ByVal value As Long) As FileSize
            Return New FileSize(value)
        End Operator
        Public Shared Widening Operator CType(ByVal value As FileSize) As Long
            Return value.Value
        End Operator
        Public Shared Widening Operator CType(ByVal value As Integer) As FileSize
            Return New FileSize(value)
        End Operator
        Public Shared Widening Operator CType(ByVal value As FileSize) As Integer
            Return value.Value
        End Operator

        '运算
        Shared Operator +(ByVal a As FileSize, ByVal b As FileSize) As FileSize
            Return a.Value + b.Value
        End Operator
        Shared Operator -(ByVal a As FileSize, ByVal b As FileSize) As FileSize
            Return a.Value - b.Value
        End Operator
        Shared Operator *(ByVal a As FileSize, ByVal b As FileSize) As Integer
            Return a.Value * b.Value
        End Operator
        Shared Operator /(ByVal a As FileSize, ByVal b As FileSize) As Double
            Return MathRange(a.Value / b.Value, 0)
        End Operator
        Shared Operator =(ByVal a As FileSize, ByVal b As FileSize) As Boolean
            Return a.Value = b.Value
        End Operator
        Shared Operator <>(ByVal a As FileSize, ByVal b As FileSize) As Boolean
            Return Not a.Value = b.Value
        End Operator
        Shared Operator >(ByVal a As FileSize, ByVal b As FileSize) As Boolean
            Return a.Value > b.Value
        End Operator
        Shared Operator <(ByVal a As FileSize, ByVal b As FileSize) As Boolean
            Return a.Value < b.Value
        End Operator
        Shared Operator &(ByVal a As FileSize, ByVal b As String) As String
            Return a.ToString & b
        End Operator
        Shared Operator &(ByVal a As String, ByVal b As FileSize) As String
            Return a & b.ToString
        End Operator

        '构造函数
        Public Sub New()
        End Sub
        Public Sub New(ByVal value As Integer)
            Me.Value = value
        End Sub

        '复写
        Public Shadows Function ToString() As String
            Select Case Me.Value
                Case Is < 0
                    Return "未知"
                Case Is < 1024
                    Return Me.Value & "B"
                Case Is < 1024 ^ 2
                    Return Math.Round(Me.Value / 1024, 1) & "K"
                Case Is < 1024 ^ 3
                    Return Math.Round(Me.Value / 1024 ^ 2, 2) & "M"
                Case Else
                    Return Math.Round(Me.Value / 1024 ^ 3, 2) & "G"
            End Select
        End Function

    End Class '文件大小

    Public Class Time

        Private Property Value As Integer = 0

        '类型转换
        Public Shared Widening Operator CType(ByVal value As Integer) As Time
            Return New Time(value)
        End Operator
        Public Shared Widening Operator CType(ByVal value As Time) As Integer
            Return value.Value
        End Operator
        Public Shared Widening Operator CType(ByVal value As Int64) As Time
            Return New Time(value)
        End Operator
        Public Shared Widening Operator CType(ByVal value As Time) As Int64
            Return value.Value
        End Operator

        '运算
        Shared Operator +(ByVal a As Time, ByVal b As Time) As Time
            Return a.Value + b.Value
        End Operator
        Shared Operator -(ByVal a As Time, ByVal b As Time) As Time
            Return a.Value - b.Value
        End Operator
        Shared Operator *(ByVal a As Time, ByVal b As Time) As Integer
            Return a.Value * b.Value
        End Operator
        Shared Operator /(ByVal a As Time, ByVal b As Time) As Double
            Return MathRange(a.Value / b.Value, 0)
        End Operator
        Shared Operator =(ByVal a As Time, ByVal b As Time) As Boolean
            Return a.Value = b.Value
        End Operator
        Shared Operator <>(ByVal a As Time, ByVal b As Time) As Boolean
            Return Not a.Value = b.Value
        End Operator
        Shared Operator >(ByVal a As Time, ByVal b As Time) As Boolean
            Return a.Value > b.Value
        End Operator
        Shared Operator <(ByVal a As Time, ByVal b As Time) As Boolean
            Return a.Value < b.Value
        End Operator
        Shared Operator &(ByVal a As Time, ByVal b As String) As String
            Return a.ToString & b
        End Operator
        Shared Operator &(ByVal a As String, ByVal b As Time) As String
            Return a & b.ToString
        End Operator

        '构造函数
        Public Sub New()
        End Sub
        Public Sub New(ByVal value As Integer)
            Me.Value = value
        End Sub

        '复写
        Public Shadows Function ToString() As String
            Dim left As Integer = Me.Value
            Dim a As Integer
            a = Math.Floor(left / 3600)
            ToString = FillLength(a.ToString, "0", 2)
            left = left - a * 3600
            a = Math.Floor(left / 60)
            ToString = ToString & ":" & FillLength(a.ToString, "0", 2) & ":" & FillLength((left - a * 60), "0", 2)
        End Function

    End Class '时间

#End Region

#Region "控件"

    ''' <summary>
    ''' 获取控件或窗体的左边距。无视Alignment。
    ''' </summary>
    ''' <param name="control"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetLeft(ByVal control) As Double
        GetLeft = 0
        If control.GetType.BaseType.Name.Equals("Window") Then
            Return control.Left
        Else
            Select Case control.HorizontalAlignment
                Case HorizontalAlignment.Left, HorizontalAlignment.Stretch, HorizontalAlignment.Center
                    Return control.Margin.Left
                Case HorizontalAlignment.Right
                    Return CType(control.Parent, Object).ActualWidth - control.ActualWidth - control.Margin.Right
                    'Case HorizontalAlignment.Stretch
                    '    Dim MaxWidth As Double = CType(control.Parent, Object).ActualWidth - control.Margin.Left - control.Margin.Right
                    '    Return If(control.ActualWidth >= MaxWidth, control.Margin.Left, control.Margin.Left + (MaxWidth - control.ActualWidth) / 2)
                    'Case HorizontalAlignment.Center
                    '    Dim CenterPos As Double = control.Margin.Left + (CType(control.Parent, Object).ActualWidth - control.Margin.Left - control.Margin.Right) / 2
                    '    Return CenterPos - control.ActualWidth / 2
            End Select
        End If
    End Function
    ''' <summary>
    ''' 获取控件或窗体的顶边距。无视Alignment。
    ''' </summary>
    ''' <param name="control"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetTop(ByVal control) As Double
        GetTop = 0
        If control.GetType.BaseType.Name.Equals("Window") Then
            Return control.Top
        Else
            Select Case control.VerticalAlignment
                Case VerticalAlignment.Top, VerticalAlignment.Stretch, VerticalAlignment.Center
                    Return control.Margin.Top
                Case VerticalAlignment.Bottom
                    Return -control.Margin.Bottom
                    'Return CType(control.Parent, Object).ActualHeight - control.ActualHeight - control.Margin.Bottom
                    'Case VerticalAlignment.Stretch
                    '    Dim MaxHeight As Double = CType(control.Parent, Object).ActualHeight - control.Margin.Top - control.Margin.Bottom
                    '    Return If(control.ActualHeight >= MaxHeight, control.Margin.Top, control.Margin.Top + (MaxHeight - control.ActualHeight) / 2)
                    'Case VerticalAlignment.Center
                    '    Dim CenterPos As Double = control.Margin.Top + (CType(control.Parent, Object).ActualHeight - control.Margin.Top - control.Margin.Bottom) / 2
                    '    Return CenterPos - control.ActualHeight / 2
            End Select
        End If
    End Function

    ''' <summary>
    ''' 将一个IList中的内容添加到另外一个IList。
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub IListCopy(ByVal list As IList, ByVal arr As IList)
        For Each item In arr
            list.Add(item)
        Next
    End Sub

    Public Sub SetLeft(ByVal control As Object, ByVal newValue As Double)
        If Double.IsNaN(newValue) Or Double.IsInfinity(newValue) Then Exit Sub '安全性检查

        If control.GetType.BaseType.Name.Equals("Window") Then
            control.Left = newValue
        Else
            Select Case control.HorizontalAlignment
                Case HorizontalAlignment.Left, HorizontalAlignment.Stretch, HorizontalAlignment.Center
                    control.Margin = New Thickness(newValue, control.Margin.Top, control.Margin.Right, control.Margin.Bottom)
                Case HorizontalAlignment.Right
                    control.Margin = New Thickness(control.Margin.Left, control.Margin.Top, CType(control.Parent, Object).ActualWidth - control.ActualWidth - newValue, control.Margin.Bottom)
            End Select
        End If
    End Sub '设置Left
    Public Sub SetTop(ByVal control As Object, ByVal newValue As Double)
        If Double.IsNaN(newValue) Or Double.IsInfinity(newValue) Then Exit Sub '安全性检查

        If control.GetType.BaseType.Name.Equals("Window") Then
            control.Top = newValue
        Else
            Select Case control.VerticalAlignment
                Case VerticalAlignment.Top, VerticalAlignment.Stretch, VerticalAlignment.Center
                    control.Margin = New Thickness(control.Margin.Left, newValue, control.Margin.Right, control.Margin.Bottom)
                Case VerticalAlignment.Bottom
                    control.Margin = New Thickness(control.Margin.Left, control.Margin.Top, control.Margin.Right, -newValue)
                    'control.Margin = New Thickness(control.Margin.Left, control.Margin.Top, control.Margin.Right, CType(control.Parent, Object).ActualHeight - control.ActualHeight - newValue)
            End Select
        End If
    End Sub '设置Top

#End Region

#Region "数学"

    ''' <summary>
    ''' 获取两数间的百分比。小数点精确到6位。
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function MathPercent(ByVal ValueA As Double, ByVal ValueB As Double, ByVal Percent As Object) As Double
        Return Math.Round(ValueA * (1 - Percent) + ValueB * Percent, 6) '解决Double计算错误
    End Function
    ''' <summary>
    ''' 获取两颜色间的百分比，根据RGB计算。小数点精确到6位。
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function MathPercent(ByVal ValueA As MyColor, ByVal ValueB As MyColor, ByVal Percent As Object) As MyColor
        Return MathRound(ValueA * (1 - Percent) + ValueB * Percent, 6) '解决Double计算错误
    End Function

    ''' <summary>
    ''' 检查一个数字是否正确（如检查无限、NaN等）。
    ''' </summary>
    ''' <param name="Num">需要检查的数字。</param>
    ''' <returns>是否正确。</returns>
    ''' <remarks></remarks>
    Public Function MathCheck(ByVal Num As Double) As Boolean
        Return Not (Double.IsInfinity(Num) Or Double.IsNaN(Num))
    End Function

    '贝塞尔曲线计算
    Public Function MathBezier(ByVal t As Double, ByVal x1 As Double, ByVal y1 As Double, ByVal x2 As Double, ByVal y2 As Double) As Double
        Dim a, b
        a = t
        Do
            b = 3 * a * ((1 / 3 + x1 - x2) * a * a + (x2 - 2 * x1) * a + x1)
            a = a + (t - b) * 0.5
        Loop Until Math.Abs(b - t) < 0.01 '精度1%
        Return 3 * a * ((1 / 3 + y1 - y2) * a * a + (y2 - 2 * y1) * a + y1)
    End Function
    'Byte合理化
    Public Function MathByte(ByVal d As Double) As Byte
        If d < 0 Then d = 0
        If d > 255 Then d = 255
        Return Math.Round(d)
    End Function
    '提供扩展类支持的Round
    Public Function MathRound(ByVal col As MyColor, Optional ByVal w As Integer = 0) As MyColor
        Return New MyColor With {.A = Math.Round(col.A, w), .R = Math.Round(col.R, w), .G = Math.Round(col.G, w), .B = Math.Round(col.B, w)}
    End Function
    '把数值限定范围
    Public Function MathRange(ByVal value As Double, ByVal min As Double, Optional ByVal max As Double = Double.MaxValue) As Double
        Return Math.Max(min, Math.Min(max, value))
    End Function
    ''' <summary>
    ''' 检查数值是否在范围中
    ''' </summary>
    Public Function MathInRange(ByVal value As Double, ByVal min As Double, Optional ByVal max As Double = Double.MaxValue) As Double
        Return value <= max And value >= min
    End Function
    'Byte类溢出处理
    Public Function ByteOverflow(ByVal int As Integer) As Byte
        If int < 0 Then
            Do Until int > 0
                int = int + 256
            Loop
            Return int
        Else
            Return int Mod 256
        End If
    End Function

#End Region

#Region "文本"

    '填充长度
    Public Function FillLength(ByVal str As String, ByVal code As String, ByVal length As Byte)
        If (Len(str) > length) Then Return Mid(str, 1, length)
        Return Mid(str.Replace(" ", "-").PadRight(length), str.Length + 1).Replace(" ", code) & str
    End Function
    '正则搜索
    Public Function RegexSearch(ByVal str As String, ByVal regex As String) As ArrayList
        Try
            RegexSearch = New ArrayList
            Dim RegexSearchRes = (New RegularExpressions.Regex(regex)).Matches(str)
            If IsNothing(RegexSearchRes) Then Return RegexSearch
            For Each item As RegularExpressions.Match In RegexSearchRes
                RegexSearch.Add(item.Value)
            Next
        Catch
            Return New ArrayList
        End Try
    End Function
    ''' <summary>
    ''' 从用户输入的字符串中提取数字
    ''' </summary>
    ''' <param name="str">用户输入的字符串</param>
    ''' <returns>字符串中的数字，没有任何数字返回0</returns>
    ''' <remarks></remarks>
    Public Function GetDoubleFromString(ByVal str As String) As Double
        Dim regResult As ArrayList = RegexSearch(str, "(-?[0-9]\d*)?\.?[0-9]\d*")
        If regResult.Count = 0 Then
            Return 0
        Else
            Return Val(regResult(0))
        End If
    End Function
    '把Exception转为字符串
    Public Function GetStringFromException(ByVal ex As Exception, Optional ByVal isLong As Boolean = False) As String
        '构造基本信息与InnerException信息
        GetStringFromException = ex.Message.Replace(vbCrLf, "")
        If Not IsNothing(ex.InnerException) Then
            GetStringFromException = GetStringFromException & vbCrLf & "InnerException: " & ex.InnerException.Message.Replace(vbCrLf, "")
            If Not IsNothing(ex.InnerException.InnerException) Then
                GetStringFromException = GetStringFromException & vbCrLf & "InnerException: " & ex.InnerException.InnerException.Message.Replace(vbCrLf, "")
            End If
        End If
        '添加堆栈信息
        If isLong Then
            GetStringFromException = GetStringFromException & vbCrLf
            For Each Stack As String In ex.StackTrace.Split(vbCrLf)
                If Stack.Contains("Client.") Then GetStringFromException = GetStringFromException & vbCrLf & Stack
            Next
        End If
        '清除双回车
        GetStringFromException = GetStringFromException.Replace(vbCrLf & vbCrLf, vbCrLf).Replace(vbCrLf & vbCrLf, vbCrLf)
    End Function

#End Region

#Region "图片"

    ''' <summary>
    ''' 裁剪一个BitmapSource。
    ''' </summary>
    ''' <param name="img">源图片。</param>
    ''' <param name="x"></param>
    ''' <param name="y"></param>
    ''' <param name="width"></param>
    ''' <param name="height"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function CutBitmap(ByVal img As BitmapSource, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer) As BitmapSource
        Try
            Return New CroppedBitmap(img, New Int32Rect(x, y, width, height))
        Catch ex As Exception
            ExShow(ex, "裁剪图片失败")
            Return Nothing
        End Try
    End Function

#End Region

#Region "文件"

    ''' <summary>
    ''' 从应用资源释放文件。
    ''' </summary>
    ''' <param name="FileName">需要释放的文件在应用资源中的名称。</param>
    ''' <param name="OutputPath">释放的路径，包含文件名。</param>
    Public Sub OutputFileInResource(ByVal FileName As String, ByVal OutputPath As String)
        Try
            Dim Res = My.Resources.ResourceManager.GetObject(FileName)
            If IsNothing(Res) Then Throw New FileNotFoundException("没有找到名为" & FileName & "的资源")
            Directory.CreateDirectory(Mid(OutputPath, 1, OutputPath.LastIndexOf("\")))
            Using OutputStream As New FileStream(OutputPath, FileMode.OpenOrCreate, FileAccess.Write)
                OutputStream.Write(Res, 0, Res.Length)
                OutputStream.Flush()
            End Using
            Log("[System] 释放文件：" & FileName)
        Catch ex As Exception
            ExShow(ex, "释放文件失败")
            Throw ex
        End Try
    End Sub

    ''' <summary>
    ''' 从文件路径或者URL获取不包含文件名的路径。不包含路径将会抛出异常。
    ''' </summary>
    ''' <param name="FilePath">文件路径，以标识符结尾。如果不包含文件名则必须以标识符结尾。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetPathFromFullPath(ByVal FilePath As String) As String
        If Not (FilePath.Contains("\") Or FilePath.Contains("/")) Then Throw New FileFormatException("不包含路径：" & FilePath)
        If FilePath.EndsWith("\") Or FilePath.EndsWith("/") Then Return FilePath
        GetPathFromFullPath = Left(FilePath, FilePath.LastIndexOfAny({"\", "/"}) + 1)
        If GetPathFromFullPath = "" Then Throw New FileFormatException("不包含路径：" & FilePath)
    End Function

    ''' <summary>
    ''' 从文件路径或者URL获取不包含路径的文件名。不包含文件名将会抛出异常。
    ''' </summary>
    ''' <param name="FilePath">文件路径。可以不完整。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetFileNameFromPath(ByVal FilePath As String) As String
        If FilePath.EndsWith("\") Or FilePath.EndsWith("/") Then Throw New FileFormatException("不包含文件名：" & FilePath)
        If Not (FilePath.Contains("\") Or FilePath.Contains("/")) Then Return FilePath
        GetFileNameFromPath = Mid(FilePath, FilePath.LastIndexOfAny({"\", "/"}) + 2)
        If GetFileNameFromPath.Contains("\\") And GetFileNameFromPath.Contains("?") Then
            '包含网络参数
            GetFileNameFromPath = Left(GetFileNameFromPath, FilePath.LastIndexOf("?"))
        End If
        If GetFileNameFromPath = "" Then Throw New FileFormatException("不包含文件名：" & FilePath)
    End Function

    ''' <summary>
    ''' 获取文件的版本信息。
    ''' </summary>
    ''' <param name="Path">文件目录。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetFileVersion(ByVal Path As String) As String
        Try
            Return Diagnostics.FileVersionInfo.GetVersionInfo(Path).FileVersion
        Catch ex As Exception
            ExShow(ex, "获取文件版本失败：" & Path)
            Return ""
        End Try
    End Function

    ''' <summary>
    ''' 获取一个文件夹下的所有文件的完整路径。
    ''' </summary>
    ''' <param name="SourcePath">源文件夹，不以“\”结尾。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetFilesFromPath(ByVal SourcePath As String) As ArrayList
        GetFilesFromPath = New ArrayList
        GetFilesFromPath(SourcePath, GetFilesFromPath)
    End Function
    Private Function GetFilesFromPath(ByVal SourcePath As String, ByRef CurrentList As ArrayList) As ArrayList
        Try
            If Not Directory.Exists(SourcePath) Then Return New ArrayList
            '列举文件
            For Each File As FileInfo In (New DirectoryInfo(SourcePath)).GetFiles
                CurrentList.Add(File.FullName)
            Next
            '列举目录
            For Each Folder As DirectoryInfo In (New DirectoryInfo(SourcePath)).GetDirectories
                GetFilesFromPath(Folder.FullName, CurrentList)
            Next
        Catch ex As Exception
            ExShow(ex, "列举文件失败：" & SourcePath)
        End Try
        '返回
        Return CurrentList
    End Function

    '获取MD5
    Public Function GetFileMD5(ByVal filepath As String) As String
        Try
            '网上抄的啊，自己改了改而已XD
            Dim result As String = ""
            Using fstream As New FileStream(filepath, FileMode.Open, FileAccess.Read)
                Dim dataToHash(fstream.Length - 1) As Byte
                fstream.Read(dataToHash, 0, fstream.Length)
                Dim hashvalue As Byte() = CType(CryptoConfig.CreateFromName("MD5"), HashAlgorithm).ComputeHash(dataToHash)
                Dim i As Double
                For i = 0 To hashvalue.Length - 1
                    result += FillLength(Hex(hashvalue(i)).ToLower, "0", 2)
                Next
            End Using
            Return result
        Catch ex As Exception
            ExShow(ex, "获取文件MD5失败：" & filepath)
            Return ""
        End Try
    End Function
    '读取文件
    Public Function ReadFileToEnd(ByVal filePath As String, Optional ByVal encode As Encoding = Nothing, Optional ByVal isFullPath As Boolean = True) As String
        Try
            Dim actualPath As String = If(isFullPath, "", PATH) & filePath
            If File.Exists(actualPath) Then
                encode = If(IsNothing(encode), New UTF8Encoding(False), encode)
                Using reader As New StreamReader(actualPath, encode)
                    ReadFileToEnd = reader.ReadToEnd
                End Using
            Else
                Return ""
            End If
        Catch ex As Exception
            ExShow(ex, "读取文件出错：" & filePath)
            Return ""
        End Try
    End Function
    '写入文件
    Public Sub WriteFile(ByVal filePath As String, ByVal text As String, Optional ByVal add As Boolean = False, Optional ByVal encode As Encoding = Nothing, Optional ByVal isFullPath As Boolean = True)
        Try
            Dim actualpath As String = If(isFullPath, "", PATH) & filePath
            If Not File.Exists(actualpath) Then
                Directory.CreateDirectory(GetPathFromFullPath(actualpath))
                File.Create(actualpath).Dispose()
            End If
            encode = If(IsNothing(encode), New UTF8Encoding(False), encode)
            Using writer As New StreamWriter(actualpath, add, encode)
                writer.Write(text)
                writer.Flush()
            End Using
        Catch ex As Exception
            ExShow(ex, "写入文件时出错：" & filePath)
        End Try
    End Sub

    Private Class IniCache
        Public content As String = ""
        Public cache As New Dictionary(Of String, String)
    End Class
    Private CacheIni As New Dictionary(Of String, IniCache)
    ''' <summary>
    ''' 获取Ini文件内容，这可能会使用到缓存。
    ''' </summary>
    ''' <param name="FileName">文件路径或简写。简写将会使用“Center\文件名.ini”作为路径。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Function GetIni(ByVal FileName As String) As IniCache
        If Not FileName.Contains(":\") Then FileName = PATH & "Center\" & FileName & ".ini"
        If CacheIni.ContainsKey(FileName) Then
            '返回缓存中的信息
            Dim Cache As New IniCache
            CacheIni.TryGetValue(FileName, Cache)
            Return Cache '防止ByRef导致缓存变更
        Else
            If File.Exists(FileName) Then
                '返回文件信息并且记入缓存
                Dim Cache As String = (vbCrLf & ReadFileToEnd(FileName) & vbCrLf).Replace(vbCrLf, vbCr).Replace(vbLf, vbCr).Replace(vbCr, vbCrLf).Replace(vbCrLf & vbCrLf, vbCrLf)
                Dim Ini As New IniCache With {.content = Cache}
                CacheIni.Add(FileName, Ini)
                Return Ini
            Else
                '返回空信息
                Dim Ini As New IniCache With {.content = ""}
                CacheIni.Add(FileName, Ini)
                Return Ini
            End If
        End If
    End Function
    ''' <summary>
    ''' 读取Ini文件，这可能会使用到缓存。
    ''' </summary>
    ''' <param name="FileName">文件路径或简写。简写将会使用“Center\文件名.ini”作为路径。</param>
    ''' <param name="Key">键。</param>
    ''' <param name="DefaultValue">没有找到键时返回的默认值。</param>
    Public Function ReadIni(ByVal FileName As String, ByVal Key As String, Optional ByVal DefaultValue As String = "") As String
        Try
            '获取目前文件
            Dim NowIni As IniCache = GetIni(FileName)
            If IsNothing(NowIni) Then Return DefaultValue
            '使用缓存
            If NowIni.cache.ContainsKey(Key) Then Return If(NowIni.cache(Key), DefaultValue)
            '新读取文件
            If NowIni.content.Contains(vbCrLf & Key & ":") Then
                Dim Ret As String = Mid(NowIni.content, NowIni.content.IndexOf(vbCrLf & Key & ":") + 3)
                Ret = If(Ret.Contains(vbCrLf), Mid(Ret, 1, Ret.IndexOf(vbCrLf)), Ret).Replace(Key & ":", "")
                NowIni.cache.Add(Key, If(Ret = vbLf, "", Ret))
                Return If(Ret = vbLf, "", Ret)
            Else
                Return DefaultValue
            End If
        Catch ex As Exception
            '读取失败
            Return DefaultValue
        End Try
    End Function
    ''' <summary>
    ''' 写入ini文件，这会更新缓存。
    ''' </summary>
    ''' <param name="FileName">文件路径或简写。简写将会使用“Center\文件名.ini”作为路径。</param>
    ''' <param name="Key">键。</param>
    ''' <param name="Value">值。</param>
    ''' <remarks></remarks>
    Public Sub WriteIni(ByVal FileName As String, ByVal Key As String, ByVal Value As String)
        On Error Resume Next
        FileName = If(FileName.Contains(":\"), FileName, PATH & "Center\" & FileName & ".ini")
        If IsNothing(Value) Then Value = ""
        Value = Value.Replace(vbCrLf, "")
        '创建文件夹
        If Not Directory.Exists(GetPathFromFullPath(FileName)) Then Directory.CreateDirectory(GetPathFromFullPath(FileName))
        '获取目前文件
        Dim NowFile As String = GetIni(FileName).content
        '如果值一样就不处理
        If NowFile.Contains(vbCrLf & Key & ":" & Value & vbCrLf) Then Exit Sub
        '处理文件
        Dim FindResult As String = ReadIni(FileName, Key)
        If FindResult = "" And Not NowFile.Contains(vbCrLf & Key & ":") Then
            '不存在这个键
            NowFile = NowFile & vbCrLf & Key & ":" & Value
        Else
            '存在这个键
            NowFile = NowFile.Replace(vbCrLf & Key & ":" & FindResult & vbCrLf, vbCrLf & Key & ":" & Value & vbCrLf)
        End If
        WriteFile(FileName, NowFile)
        '刷新目前缓存
        CacheIni(FileName).content = NowFile
        CacheIni(FileName).cache.Remove(Key)
        CacheIni(FileName).cache.Add(Key, Value)
    End Sub
#End Region

#Region "系统"

    ''' <summary>
    ''' 测试某段代码用时，单位为毫秒/次。
    ''' </summary>
    ''' <param name="run">需要测试的代码。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function TimeTest(ByVal run As ThreadStart) As Double
        Dim StartTime As Long = My.Computer.Clock.TickCount
        Dim RunCount As Integer = 0
        Do While True
            run.Invoke()
            RunCount = RunCount + 1
            If My.Computer.Clock.TickCount - StartTime > 1000 Or RunCount > 100000 Then Exit Do
        Loop
        Return (My.Computer.Clock.TickCount - StartTime) / RunCount
    End Function

    ''' <summary>
    ''' 打开文件（等同于使用“运行”窗口）。
    ''' </summary>
    ''' <param name="FileName">文件名。可以为“notepad”等缩写。</param>
    ''' <param name="Arguments">运行参数。</param>
    ''' <param name="WaitForExit">是否等待该程序结束。默认为False。</param>
    ''' <remarks></remarks>
    Public Sub Shell(ByVal FileName As String, Optional ByVal Arguments As String = "", Optional ByVal WaitForExit As Boolean = False)
        Try
            Dim Program As New Process
            Program.StartInfo.Arguments = Arguments
            Program.StartInfo.FileName = FileName
            Program.Start()
            If WaitForExit Then Program.WaitForExit()
        Catch ex As Exception
            ExShow(ex, "执行命令失败：" & FileName)
        End Try
    End Sub

    ''' <summary>
    ''' 返回一个枚举对应的字符串。
    ''' </summary>
    ''' <param name="EnumData">一个已经实例化的枚举类型。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetStringFromEnum(ByVal EnumData As Object) As String
        Return [Enum].GetName(EnumData.GetType, EnumData)
    End Function

    ''' <summary>
    ''' 从指定的枚举中查找某字符串对应的枚举项。
    ''' </summary>
    ''' <param name="EnumData">源枚举。</param>
    ''' <param name="Value">对应枚举一项的字符串。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetEnumFromString(ByVal EnumData As Type, ByVal Value As String)
        Return [Enum].Parse(EnumData, Value)
    End Function

    ''' <summary>
    ''' 获取格式类似于“11:08:52.037”的当前时间的字符串。
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetTime() As String
        Return Date.Now.ToLongTimeString & "." & FillLength(Date.Now.Millisecond, "0", 3)
    End Function

    ''' <summary>
    ''' 数组去重。
    ''' </summary>
    ''' <param name="array"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function ArrayNoDouble(ByVal array As Array) As ArrayList
        Dim ResultArray As New ArrayList

        For i = 0 To UBound(array)
            For ii = i + 1 To UBound(array)
                If array(i).Equals(array(ii)) Then GoTo NextElement
            Next
            ResultArray.Add(array(i))
NextElement:
        Next i

        Return ResultArray
    End Function

    ''' <summary>
    ''' 将数组从大到小排序。
    ''' </summary>
    ''' <param name="Array">纯数字的ArrayList。</param>
    ''' <remarks></remarks>
    Public Sub ArrayBigToSmall(ByRef Array As ArrayList)
        If Array.Count < 2 Then Exit Sub
        For i = 1 To Array.Count - 1
            If Array(i) > Array(i - 1) Then
                Dim c = Array(i - 1)
                Array(i - 1) = Array(i)
                Array(i) = c
                If i >= 2 Then i = i - 2
            End If
        Next i
    End Sub

    ''' <summary>
    ''' 强制将一个支持IList接口的对象转换为任意格式的一维数组。
    ''' </summary>
    ''' <typeparam name="T">转换后的数组类型。</typeparam>
    ''' <param name="Source"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function ArrayConventer(Of T, B As IList)(ByVal Source As B) As T()
        Dim Re(Source.Count - 1) As T
        For i = 0 To Source.Count - 1
            Re(i) = Source(i)
        Next
        Return Re
    End Function

    ''' <summary>
    ''' 将数组与对象的混合数组拆分为对象的数组。
    ''' </summary>
    ''' <param name="array">需要拆分的数组。</param>
    ''' <param name="requiredType">最终要求的类型。</param>
    Public Sub ArrayUnpack(ByRef array As Object, ByVal requiredType As Type)
        '如果 array 本身是单个对象，返回它自身的数组
        If requiredType.IsAssignableFrom(array.GetType) Then array = New ArrayList({array})
        '如果 array 不是 ArrayList，则先转化为 ArrayList
        If Not array.GetType.Equals(GetType(ArrayList)) Then array = New ArrayList(CType(array, ICollection))
        '拆分 array
        For i As Integer = 0 To array.Count - 1
            If Not requiredType.IsAssignableFrom(array(i).GetType) Then
                array.AddRange(array(i))
                array.RemoveAt(i)
                i -= 1
            End If
        Next
    End Sub

    ''' <summary>
    ''' 检查是否拥有某一文件夹的读取权限。如果文件夹不存在，会返回 False。
    ''' </summary>
    ''' <param name="Path">检查目录。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function CheckDirectoryPermission(ByVal Path As String) As Boolean
        Try
            If Path = "" Then Return False
            If Not Directory.Exists(Path) Then Return False
            Dim folderCheck As New DirectoryInfo(Path)
            folderCheck.EnumerateFiles()
            Return True
        Catch
            Return False
        End Try
    End Function

    ''' <summary>
    ''' 获取程序启动参数。
    ''' </summary>
    ''' <param name="Name">参数名。</param>
    ''' <param name="DefaultValue">默认值。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetProgramArgument(ByVal Name As String, Optional ByVal DefaultValue As Object = "")
        Dim AllArguments() As String = Microsoft.VisualBasic.Command.Split(" ")
        For i = 0 To AllArguments.Length - 1
            If AllArguments(i) = "-" & Name Then
                If AllArguments.Length = i + 1 Then Return True
                If AllArguments(i + 1).StartsWith("-") Then Return True
                Return AllArguments(i + 1)
            End If
        Next
        Return DefaultValue
    End Function

    '注册表IO
    Public Function ReadReg(ByVal Key As String, Optional ByVal DefaultValue As String = "") As String
        Try
            Dim parentKey As Microsoft.Win32.RegistryKey, softKey As Microsoft.Win32.RegistryKey
            parentKey = My.Computer.Registry.CurrentUser
            softKey = parentKey.OpenSubKey("Software\" & APPLICATION_NAME, True)
            If softKey Is Nothing Then
                ReadReg = DefaultValue '不存在则返回默认值
            Else
                Dim readValue As New System.Text.StringBuilder
                readValue.AppendLine(softKey.GetValue(Key))
                Dim value = readValue.ToString.Replace(vbCrLf, "") '去除莫名的回车
                Return If(value = "", DefaultValue, value) '错误则返回默认值
            End If
        Catch ex As Exception
            ExShow(ex, "读取注册表出错：" & Key)
            Return DefaultValue
        End Try
    End Function
    Public Sub WriteReg(ByVal Key As String, ByVal Value As String, Optional ByVal ShowException As Boolean = False)
        Try
            Dim parentKey As Microsoft.Win32.RegistryKey, softKey As Microsoft.Win32.RegistryKey
            parentKey = My.Computer.Registry.CurrentUser
            softKey = parentKey.OpenSubKey("Software\" & APPLICATION_NAME, True)
            If softKey Is Nothing Then softKey = parentKey.CreateSubKey("Software\" & APPLICATION_NAME) '如果不存在就创建  
            softKey.SetValue(Key, Value)
        Catch ex As Exception
            ExShow(ex, "写入注册表出错：" & Key)
            If ShowException Then Throw '如果显示错误就丢一个
        End Try
    End Sub

    Private UUID As Integer = 0
    ''' <summary>
    ''' 获取一个全程序内不会重复的数字（伪UUID）。
    ''' </summary>
    ''' <returns>UUID，一个大于0的有序整数。</returns>
    ''' <remarks></remarks>
    Public Function GetUUID() As Integer
        UUID = UUID + 1
        Return UUID
    End Function

#End Region

#Region "随机"

    ''' <summary>
    ''' 随机选择其一。
    ''' </summary>
    Public Function RandomOne(ByVal objects As Array)
        Return objects.GetValue(RandomInteger(0, objects.Length - 1))
    End Function
    ''' <summary>
    ''' 随机选择其一。
    ''' </summary>
    Public Function RandomOne(ByVal objects As ArrayList)
        Return objects(RandomInteger(0, objects.Count - 1))
    End Function

    ''' <summary>
    ''' 将 ArrayList 随机打乱。
    ''' </summary>
    Public Function RandomChaos(ByVal array As ArrayList) As ArrayList
        RandomChaos = New ArrayList
        Do While array.Count > 0
            Dim i As Integer = RandomInteger(0, array.Count - 1)
            RandomChaos.Add(array(i))
            array.RemoveAt(i)
        Loop
    End Function

    ''' <summary>
    ''' 取随机整数。
    ''' </summary>
    Public Function RandomInteger(ByVal min As Integer, ByVal max As Integer) As Integer
        Return Math.Round((max - min) * Rnd.NextDouble) + min
    End Function

    ''' <summary>
    ''' 取随机小数。
    ''' </summary>
    Public Function RandomDouble(min As Double, max As Double) As Double
        Return Rnd.NextDouble * (max - min) + min
    End Function

    ''' <summary>
    ''' 取随机布尔值。
    ''' </summary>
    Public Function RandomBoolean() As Boolean
        Return Rnd.NextDouble >= 0.5
    End Function

    ''' <summary>
    ''' 进行加权随机。若发生错误则返回 Nothing。
    ''' </summary>
    ''' <param name="elements">进行随机的元素。</param>
    ''' <param name="weights">各个元素对应的权重。</param>
    ''' <returns>elements 中的一项。</returns>
    Public Function RandomWeight(elements As ArrayList, weights As ArrayList)
        '若发生错误，返回 Nothing
        If Not elements.Count = weights.Count Then Return Nothing
        If elements.Count = 0 Then Return Nothing
        '计算总权重
        Dim weight As Double = 0
        For Each w As Double In weights
            If w > 0 Then weight += w
        Next
        '随机应取得的权重
        weight = RandomDouble(0, weight)
        '加权随机并实时返回
        For i As Integer = 0 To elements.Count - 1
            If weights(i) > 0 Then
                weight -= weights(i)
                If weight < 0 Then Return elements(i)
            End If
        Next
        '到最后也没有返回，则返回随机项
        Return elements(RandomDouble(0, elements.Count - 1))
    End Function

#End Region

#Region "错误"

    ''' <summary>
    ''' 处理错误信息。
    ''' </summary>
    ''' <param name="ex"></param>
    ''' <param name="description">描述文本，如“下载文件时出错”。</param>
    ''' <remarks></remarks>
    Public Sub ExShow(ByVal ex As Exception, ByVal description As String)
        On Error Resume Next
        Log("[Error] " & description & "：" & GetStringFromException(ex, True),, True)
    End Sub

    ''' <summary>
    ''' 输出Log。
    ''' </summary>
    ''' <param name="LogText">Log文本。</param>
    ''' <remarks></remarks>
    Public Sub Log(ByVal LogText As String, Optional ByVal Notice As Boolean = False, Optional DeveloperNotice As Boolean = False)
        Console.WriteLine("[" & GetTime() & "] " & LogText)
    End Sub

#End Region

End Module '基础函数

Public Module MyAnimationer

#Region "声明"
    ''' <summary>
    ''' 动画组列表。（组名→ArrayList）
    ''' </summary>
    ''' <remarks></remarks>
    Private AniGroups As New Dictionary(Of String, ArrayList)
    ''' <summary>
    ''' 上一次记刻的时间。
    ''' </summary>
    ''' <remarks></remarks>
    Public AniLastTick As Integer
    ''' <summary>
    ''' 动画执行是否开启。
    ''' </summary>
    ''' <remarks></remarks>
    Public AniRunning As Boolean = False
#End Region

#Region "类与枚举"

    ''' <summary>
    ''' 单个动画对象。
    ''' </summary>
    ''' <remarks></remarks>
    Public Structure Animation

        ''' <summary>
        ''' 动画种类。
        ''' </summary>
        ''' <remarks></remarks>
        Public AniType As AniType
        ''' <summary>
        ''' 动画副种类。
        ''' </summary>
        ''' <remarks></remarks>
        Public AniSubType As AniSubType

        ''' <summary>
        ''' 动画总长度。
        ''' </summary>
        ''' <remarks></remarks>
        Public TotalLength As Integer
        ''' <summary>
        ''' 已经执行的动画长度。如果为负数则为延迟。
        ''' </summary>
        ''' <remarks></remarks>
        Public FinishLength As Integer
        ''' <summary>
        ''' 已经完成的百分比。
        ''' </summary>
        ''' <remarks></remarks>
        Public Percent As Double
        ''' <summary>
        ''' 是否为“以后”。
        ''' </summary>
        ''' <remarks></remarks>
        Public After As Boolean

        ''' <summary>
        ''' 插值器类型。
        ''' </summary>
        ''' <remarks></remarks>
        Public Ease As AniEase
        ''' <summary>
        ''' 动画对象。
        ''' </summary>
        ''' <remarks></remarks>
        Public Obj As Object
        ''' <summary>
        ''' 动画值。
        ''' </summary>
        ''' <remarks></remarks>
        Public Value As Object
        ''' <summary>
        ''' 上次执行时的动画值。
        ''' </summary>
        ''' <remarks></remarks>
        Public Last As Object

        Public Overrides Function ToString() As String
            Return GetStringFromEnum(AniType) & " | " & FinishLength & "/" & TotalLength & "(" & Math.Round(Percent * 100) & "%) | " & Obj.name & "(" & Obj.GetType.Name & ")"
        End Function

    End Structure
    ''' <summary>
    ''' 动画基础种类。
    ''' </summary>
    ''' <remarks></remarks>
    Public Enum AniType As Byte
        ''' <summary>
        ''' 单个Double的动画，包括位置、长宽、透明度等。这需要附属类型。
        ''' </summary>
        ''' <remarks></remarks>
        DoubleAnimation = 0
        ''' <summary>
        ''' 颜色属性的动画。这需要附属类型。
        ''' </summary>
        ''' <remarks></remarks>
        ColorAnimation = 1
        ''' <summary>
        ''' 缩放控件大小。比起4个DoubleAnimation来说效率更高。
        ''' </summary>
        ''' <remarks></remarks>
        Scale = 2
        ''' <summary>
        ''' 文字一个个出现。
        ''' </summary>
        ''' <remarks></remarks>
        TextAppear = 3
        ''' <summary>
        ''' 执行代码。
        ''' </summary>
        ''' <remarks></remarks>
        Code = 4
        ''' <summary>
        ''' 执行按照规定格式书写的伪代码。比起Code来说这有很大的局限性，但效率更高。
        ''' </summary>
        ''' <remarks></remarks>
        CodeEvent = 5
        ''' <summary>
        ''' 以 WPF 方式缩放控件。
        ''' </summary>
        ScaleTransform = 6
        ''' <summary>
        ''' 以 WPF 方式旋转控件。
        ''' </summary>
        RotateTransform = 7
    End Enum
    ''' <summary>
    ''' 动画扩展种类。
    ''' </summary>
    ''' <remarks></remarks>
    Public Enum AniSubType As Byte

        X = 0
        Y = 1
        Width = 2
        Height = 3
        Opacity = 4
        Value = 5
        Radius = 10
        BorderThickness = 11
        StrokeThickness = 13

        Background = 6
        BorderBrush = 7
        Foreground = 8
        Stroke = 9
        Fill = 12

    End Enum

#End Region

#Region "种类"

    'DoubleAnimation

    ''' <summary>
    ''' 移动X轴的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">进行移动的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaX(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.X,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 移动Y轴的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">进行移动的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaY(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Y,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变宽度的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">宽度改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaWidth(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Width,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变高度的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">高度改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaHeight(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Height,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变透明度的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">透明度改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaOpacity(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Opacity,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变对象的Value属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">Value属性改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaValue(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Value,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变对象的Radius属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">Radius属性改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaRadius(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.Radius,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变对象的BorderThickness属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">BorderThickness属性改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaBorderThickness(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.BorderThickness,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function
    ''' <summary>
    ''' 改变对象的StrokeThickness属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">StrokeThickness属性改变的值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaStrokeThickness(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.DoubleAnimation, .AniSubType = AniSubType.StrokeThickness,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function

    'ColorAnimation

    ''' <summary>
    ''' 改变Background颜色属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">颜色改变的值。以RGB加减法进行计算。不用担心超额。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaBackGround(ByVal Obj As Object, ByVal Value As MyColor, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ColorAnimation, .AniSubType = AniSubType.Background,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay, .Last = New MyColor}
    End Function
    ''' <summary>
    ''' 改变BorderBrush颜色属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">颜色改变的值。以RGB加减法进行计算。不用担心超额。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaBorderBrush(ByVal Obj As Object, ByVal Value As MyColor, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ColorAnimation, .AniSubType = AniSubType.BorderBrush,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay, .Last = New MyColor}
    End Function
    ''' <summary>
    ''' 改变Foreground颜色属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">颜色改变的值。以RGB加减法进行计算。不用担心超额。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaForeGround(ByVal Obj As Object, ByVal Value As MyColor, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ColorAnimation, .AniSubType = AniSubType.Foreground,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay, .Last = New MyColor}
    End Function
    ''' <summary>
    ''' 改变Stroke颜色属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">颜色改变的值。以RGB加减法进行计算。不用担心超额。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaStroke(ByVal Obj As Object, ByVal Value As MyColor, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ColorAnimation, .AniSubType = AniSubType.Stroke,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay, .Last = New MyColor}
    End Function
    ''' <summary>
    ''' 改变Fill颜色属性的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">颜色改变的值。以RGB加减法进行计算。不用担心超额。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaFill(ByVal Obj As Object, ByVal Value As MyColor, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ColorAnimation, .AniSubType = AniSubType.Fill,
                                   .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay, .Last = New MyColor}
    End Function

    'Scale

    ''' <summary>
    ''' 缩放控件的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。</param>
    ''' <param name="Value">大小改变的百分比（如-0.6）或值。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <param name="Absolute">大小改变是否为绝对值。若为False则会按照控件当前大小计算相对缩放值。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaScale(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False, Optional ByVal Absolute As Boolean = False) As Animation
        Dim ChangeRect As New AdvancedRect
        If Absolute Then
            ChangeRect = New AdvancedRect(-0.5 * Value, -0.5 * Value, Value, Value)
        Else
            ChangeRect = New AdvancedRect(-0.5 * Obj.ActualWidth * Value, -0.5 * Obj.ActualHeight * Value, Obj.ActualWidth * Value, Obj.ActualHeight * Value)
        End If
        Return New Animation With {.AniType = AniType.Scale, .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = ChangeRect, .After = After, .FinishLength = -Delay}
    End Function

    'TextAppear

    ''' <summary>
    ''' 让一段文字一个个字出现或消失的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。必须是Label或TextBlock。</param>
    ''' <param name="Hide">是否为一个个字隐藏。默认为False（一个个字出现）。这些字必须已经存在了。</param>
    ''' <param name="TimePerText">是否采用根据文本长度决定时间的方式。</param>
    ''' <param name="Time">动画长度（毫秒）。若TimePerText为True，这代表每个字所占据的时间。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaTextAppear(ByVal Obj As Object, Optional ByVal Hide As Boolean = False, Optional ByVal TimePerText As Boolean = True, Optional ByVal Time As Integer = 70, Optional ByVal Delay As Integer = 0, Optional ByVal After As Boolean = False) As Animation
        'Are we cool yet？
        Return New Animation With {.AniType = AniType.TextAppear, .TotalLength = If(TimePerText, Time * Len(If(Obj.GetType.Name = "TextBlock", Obj.Text, Obj.Context.ToString)), Time), .Obj = Obj, .Value = {If(Obj.GetType.Name = "TextBlock", Obj.Text, Obj.Context.ToString), Hide}, .After = After, .FinishLength = -Delay}
    End Function

    'Code

    ''' <summary>
    ''' 执行代码。
    ''' </summary>
    ''' <param name="Code">一个ThreadStart。这将会在执行时在主线程调用。</param>
    ''' <param name="Delay">代码延迟执行的时间（毫秒）。</param>
    ''' <param name="After">是否等到以前的动画完成后才执行。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaCode(ByVal Code As ThreadStart, Optional ByVal Delay As Integer = 0, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.Code,
                                   .TotalLength = 1, .Value = Code, .After = After, .FinishLength = -Delay}
    End Function

    'CodeEvent

    ''' <summary>
    ''' 执行按照规定格式书写的伪代码。这有很大的局限性，但不会建立线程，性能也更好。
    ''' </summary>
    ''' <param name="Code">伪代码元素数组。</param>
    ''' <param name="Delay">代码延迟执行的时间（毫秒）。</param>
    ''' <param name="After">是否等到以前的动画完成后才执行。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaCode(ByVal Code As Array, Optional ByVal Delay As Integer = 0, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.CodeEvent,
                                   .TotalLength = 1, .Value = Code, .After = After, .FinishLength = -Delay}
    End Function

    'ScaleTransform

    ''' <summary>
    ''' 按照 WPF 方式缩放控件的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。它必须已经拥有了单一的 ScaleTransform 值。</param>
    ''' <param name="Value">大小改变的百分比（如-0.6）。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaScaleTransform(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.ScaleTransform, .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function

    'RotateTransform

    ''' <summary>
    ''' 按照 WPF 方式旋转控件的动画。
    ''' </summary>
    ''' <param name="Obj">动画的对象。它必须已经拥有了单一的 ScaleTransform 值。</param>
    ''' <param name="Value">大小改变的百分比（如-0.6）。</param>
    ''' <param name="Time">动画长度（毫秒）。</param>
    ''' <param name="Delay">动画延迟执行的时间（毫秒）。</param>
    ''' <param name="Ease">插值器类型。</param>
    ''' <param name="After">是否等到以前的动画完成后才继续本动画。</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function AaRotateTransform(ByVal Obj As Object, ByVal Value As Double, Optional ByVal Time As Integer = 400, Optional ByVal Delay As Integer = 0, Optional ByVal Ease As AniEase = Nothing, Optional ByVal After As Boolean = False) As Animation
        Return New Animation With {.AniType = AniType.RotateTransform, .TotalLength = Time, .Ease = If(Ease, New AniEaseNone), .Obj = Obj, .Value = Value, .After = After, .FinishLength = -Delay}
    End Function

    '特殊

    ''' <summary>
    ''' 将一个StackPanel中的各个项目依次显示。
    ''' </summary>
    ''' <remarks></remarks>
    Public Function AaStack(ByVal Stack As StackPanel) As ArrayList
        AaStack = New ArrayList
        Dim AniDelay As Integer = 0
        For Each Item In Stack.Children
            Item.Opacity = 0
            AaStack.Add(AaOpacity(Item, 1, 100, AniDelay))
            AniDelay = AniDelay + 25
        Next
    End Function

#End Region

#Region "插值器"

    '基类
    Public MustInherit Class AniEase
        ''' <summary>
        ''' 获取增加值。
        ''' </summary>
        ''' <param name="t1">较大的时间百分比。</param>
        ''' <param name="t0">较小的时间百分比。</param>
        ''' <returns></returns>
        Public MustOverride Function GetDelta(t1 As Double, t0 As Double) As Double
    End Class

    ''' <summary>
    ''' 线性，无缓动。
    ''' </summary>
    Public Class AniEaseNone
        Inherits AniEase
        Public Overrides Function GetDelta(t1 As Double, t0 As Double) As Double
            t1 = MathRange(t1, 0, 1)
            t0 = MathRange(t0, 0, 1)
            Return t1 - t0
        End Function
    End Class

    ''' <summary>
    ''' 平滑开始。
    ''' </summary>
    Public Class AniEaseStart
        Inherits AniEase
        Public Overrides Function GetDelta(t1 As Double, t0 As Double) As Double
            t1 = MathRange(t1, 0, 1)
            t0 = MathRange(t0, 0, 1)
            Return (t1 - t0) * (t1 + t0)
        End Function
    End Class

    ''' <summary>
    ''' 平滑结束。
    ''' </summary>
    Public Class AniEaseEnd
        Inherits AniEase
        Public Overrides Function GetDelta(t1 As Double, t0 As Double) As Double
            t1 = MathRange(t1, 0, 1)
            t0 = MathRange(t0, 0, 1)
            Return (t1 - t0) * (2 - t1 - t0)
        End Function
    End Class

    ''' <summary>
    ''' 跳跃开始。
    ''' </summary>
    Public Class AniEaseJumpStart
        Inherits AniEase

        ''' <summary>
        ''' </summary>
        ''' <param name="JumpEndPercent">跳跃结束时占整个动画的百分比。</param>
        Public Sub New(JumpEndPercent As Double)
            o = JumpEndPercent
        End Sub
        Private ReadOnly o As Double
        Public Overrides Function GetDelta(t1 As Double, t0 As Double) As Double
            t1 = MathRange(t1, 0, 1)
            t0 = MathRange(t0, 0, 1)
            Return (t1 - t0) * (t1 + t0 - o) / (1 - o)
        End Function
    End Class

    ''' <summary>
    ''' 跳跃结束。
    ''' </summary>
    Public Class AniEaseJumpEnd
        Inherits AniEase

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="JumpStartPercent">跳跃开始时占整个动画的百分比。</param>
        Public Sub New(JumpStartPercent As Double)
            o = JumpStartPercent
        End Sub
        Private ReadOnly o As Double
        Public Overrides Function GetDelta(t1 As Double, t0 As Double) As Double
            t1 = MathRange(t1, 0, 1)
            t0 = MathRange(t0, 0, 1)
            Return (t1 - t0) * (o + 1 - t1 - t0) / o
        End Function
    End Class

#End Region

    '等待处理的动画组列表
    Private ReadOnly AniWaitingList As New ArrayList
    Private ReadOnly AniWaitingListName As New ArrayList

    ''' <summary>
    ''' 开始一个动画组。
    ''' </summary>
    ''' <param name="AniGroup">由Aa开头的函数初始化的Animation对象集合。</param>
    ''' <param name="Name">动画组的名称。如果重复会直接停止同名动画组。</param>
    ''' <param name="RefreshTime">是否重新开始这一帧的计时。如果该动画组连续执行请设置为False。</param>
    ''' <remarks></remarks>
    Public Sub AniStart(ByVal AniGroup As ArrayList, ByVal Name As String, Optional ByVal refreshTime As Boolean = True)

        '把组动画（如 AniStack）分解
        Dim AniGroupSet As New ArrayList
        For i = 0 To AniGroup.Count - 1
            If AniGroup(i).GetType.ToString.Contains("ArrayList") Then
                AniGroupSet.AddRange(AniGroup(i))
            Else
                AniGroupSet.Add(AniGroup(i))
            End If
        Next i

        '添加到正在执行的动画组
        If refreshTime Then AniLastTick = My.Computer.Clock.TickCount '避免处理动画时已经造成了极大的延迟，导致动画突然结束
        AniStop(Name)
        AniGroups.Add(Name, AniGroupSet)

    End Sub
    ''' <summary>
    ''' 开始一个动画组。
    ''' </summary>
    ''' <param name="AniGroup">由Aa开头的函数初始化的Animation对象集合。</param>
    ''' <param name="Name">动画组的名称。如果重复会直接停止同名动画组。</param>
    ''' <param name="RefreshTime">是否重新开始这一帧的计时。如果该动画组连续执行请设置为False。</param>
    ''' <remarks></remarks>
    Public Sub AniStart(ByVal aniGroup As Array, ByVal name As String, Optional ByVal refreshTime As Boolean = True)
        '让Array和ArrayList都可以添加
        AniStart(New ArrayList(aniGroup), name, refreshTime)
    End Sub
    ''' <summary>
    ''' 停止一个动画组。
    ''' </summary>
    ''' <param name="name">需要停止的动画组的名称。</param>
    ''' <remarks></remarks>
    Public Sub AniStop(ByVal Name As String)
        If AniGroups.ContainsKey(Name) Then AniGroups.Remove(Name)
    End Sub

    Private ReadOnly AniMissions As New ArrayList

    Private AniFPSCounter As Integer = 0
    Private AniFPSTimer As Long = 0
    ''' <summary>
    ''' 当前的动画 FPS。
    ''' </summary>
    Public AniFPS As Integer = 0

    ''' <summary>
    ''' 开始动画执行。
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub AniStartRun()
        '初始化计时器
        AniLastTick = My.Computer.Clock.TickCount
        AniFPSTimer = My.Computer.Clock.TickCount
        AniRunning = True '标记动画执行开始

        Dim t = New Thread(Sub()
                               Do While True
                                   '两帧之间的间隔时间
                                   Dim DeltaTime As Integer = My.Computer.Clock.TickCount - AniLastTick
                                   If DeltaTime < 2 Then GoTo Sleeper
                                   AniLastTick = My.Computer.Clock.TickCount
                                   '执行动画
                                   frmMain.Dispatcher.Invoke(Sub() AniTimer(DeltaTime))
                                   '记录 FPS
                                   If AniLastTick - AniFPSTimer >= 1000 Then
                                       AniFPS = AniFPSCounter
                                       AniFPSCounter = 0
                                       AniFPSTimer = AniLastTick
                                   End If
                                   AniFPSCounter = AniFPSCounter + 1
Sleeper:
                                   '控制 FPS
                                   Thread.Sleep(1)
                               Loop
                           End Sub)
        t.Start()
    End Sub

    Private AniCodeList As New ArrayList
    ''' <summary>
    ''' 动画定时器事件。
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub AniTimer(DeltaTime As Integer)
        Rnd.NextDouble() '重置随机数发生器

        For Each Code In AniCodeList
            frmMain.Dispatcher.Invoke(Code)
        Next
        AniCodeList.Clear()

        Try

            Dim i As Integer = 0
            '循环每个动画组
            Do While i < AniGroups.Count
                '初始化
                Dim aniGroup = AniGroups.Values(i)
                Dim CanRemoveAfter = True '是否应该去除“之后”标记
                Dim ii = 0

                '循环每个动画
                Do While ii < aniGroup.Count
                    Dim Anim As Animation = aniGroup(ii)
                    '执行种类
                    If Anim.After = False Then '之前
                        CanRemoveAfter = False '取消“之后”标记 
                        '增加执行时间
                        Anim.FinishLength = Anim.FinishLength + DeltaTime
                        '执行动画
                        If Anim.FinishLength > 0 Then Anim = AniRun(Anim) '执行动画
                        '如果当前动画已执行完毕
                        If Anim.FinishLength >= Anim.TotalLength Then
                            '删除
                            aniGroup.RemoveAt(ii)
                            GoTo NextAni
                        End If
                        aniGroup(ii) = Anim
                    Else '之后
                        If CanRemoveAfter Then
                            '之后改为之前
                            CanRemoveAfter = False
                            Anim.After = False
                            aniGroup(ii) = Anim
                            '重新循环该动画
                            GoTo NextAni
                        Else
                            '不能去除该“之后”标记，结束该动画组
                            Exit Do
                        End If
                    End If
                    ii = ii + 1
NextAni:
                Loop

                '如果当前动画组都执行完毕则删除
                If aniGroup.Count = 0 Then
                    AniGroups.Remove(AniGroups.Keys(i))
                    i = i - 1
                End If
                '继续循环
                i = i + 1
            Loop

        Catch ex As Exception
            ExShow(ex, "动画刻执行失败")
        End Try
    End Sub

    ''' <summary>
    ''' 执行一个动画。
    ''' </summary>
    ''' <param name="Ani">执行的动画对象。</param>
    ''' <remarks></remarks>
    Private Function AniRun(ByVal Ani As Animation) As Animation
        Try
            Select Case Ani.AniType

                Case AniType.DoubleAnimation
                    Dim Delta As Double = MathPercent(0, Ani.Value, Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, Ani.Percent))
                    Select Case Ani.AniSubType
                        Case AniSubType.X
                            SetLeft(Ani.Obj, GetLeft(Ani.Obj) + Delta)
                        Case AniSubType.Y
                            SetTop(Ani.Obj, GetTop(Ani.Obj) + Delta)
                        Case AniSubType.Opacity
                            Ani.Obj.Opacity = MathRange(Ani.Obj.Opacity + Delta, 0, 1)
                        Case AniSubType.Width
                            Ani.Obj.Width = MathRange(Ani.Obj.Width + Delta, 0)
                        Case AniSubType.Height
                            Ani.Obj.Height = MathRange(Ani.Obj.Height + Delta, 0)
                        Case AniSubType.Value
                            Ani.Obj.Value = Ani.Obj.Value + Delta
                        Case AniSubType.Radius
                            Ani.Obj.Radius = Ani.Obj.Radius + Delta
                        Case AniSubType.StrokeThickness
                            Ani.Obj.StrokeThickness = MathRange(Ani.Obj.StrokeThickness + Delta, 0)
                        Case AniSubType.BorderThickness
                            Ani.Obj.BorderThickness = New Thickness(CType(Ani.Obj.BorderThickness, Thickness).Bottom + Delta)
                    End Select

                Case AniType.ColorAnimation
                    '利用Last记录了余下的小数值
                    Dim Delta As MyColor = MathPercent(New MyColor, Ani.Value, Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, Ani.Percent)) + Ani.Last
                    Select Case Ani.AniSubType
                        Case AniSubType.Background
                            Dim NewColor As MyColor = New MyColor(Ani.Obj.Background.Color) + Delta
                            Ani.Obj.Background = NewColor
                            Ani.Last = NewColor - New MyColor(Ani.Obj.Background.Color)
                        Case AniSubType.Foreground
                            Dim NewColor As MyColor = New MyColor(Ani.Obj.Foreground.Color) + Delta
                            Ani.Obj.Foreground = NewColor
                            Ani.Last = NewColor - New MyColor(Ani.Obj.Foreground.Color)
                        Case AniSubType.Stroke
                            Dim NewColor As MyColor = New MyColor(Ani.Obj.Stroke.Color) + Delta
                            Ani.Obj.Stroke = NewColor
                            Ani.Last = NewColor - New MyColor(Ani.Obj.Stroke.Color)
                        Case AniSubType.BorderBrush
                            Dim NewColor As MyColor = New MyColor(Ani.Obj.BorderBrush.Color) + Delta
                            Ani.Obj.BorderBrush = NewColor
                            Ani.Last = NewColor - New MyColor(Ani.Obj.BorderBrush.Color)
                        Case AniSubType.Fill
                            Dim NewColor As MyColor = New MyColor(Ani.Obj.Fill.Color) + Delta
                            Ani.Obj.Fill = NewColor
                            Ani.Last = NewColor - New MyColor(Ani.Obj.Fill.Color)
                    End Select

                Case AniType.Scale
                    Dim Delta As Double = Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, Ani.Percent)
                    Ani.Obj.Margin = New Thickness(Ani.Obj.Margin.Left + MathPercent(0, Ani.Value.Left, Delta), Ani.Obj.Margin.Top + MathPercent(0, Ani.Value.Top, Delta), Ani.Obj.Margin.Right, Ani.Obj.Margin.Bottom)
                    Ani.Obj.Width = Math.Max(Ani.Obj.Width + MathPercent(0, Ani.Value.Width, Delta), 0)
                    Ani.Obj.Height = Math.Max(Ani.Obj.Height + MathPercent(0, Ani.Value.Height, Delta), 0)

                Case AniType.TextAppear
                    Dim NewText As String = Mid(Ani.Value(0), 1, If(Ani.Value(1), Len(Ani.Value(0)), 0) + Math.Round(Len(Ani.Value(0)) * If(Ani.Value(1), -1, 1) * Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, 0)))
                    If Ani.Obj.GetType.Name = "TextBlock" Then
                        Ani.Obj.Text = NewText
                    Else
                        Ani.Obj.Context = NewText
                    End If

                Case AniType.Code
                    AniCodeList.Add(Ani.Value)

                Case AniType.CodeEvent
                    Select Case Ani.Value(0)
                        Case "Close"
                            Ani.Value(1).Close()
                        Case "Clear"
                            Ani.Value(1).Clear()
                        Case "Remove"
                            Ani.Value(1).Remove(Ani.Value(2))
                        Case "Add"
                            Ani.Value(1).Add(Ani.Value(2))
                        Case "Set"
                            Ani.Value(1) = Ani.Value(2)
                        Case "Tag"
                            Ani.Value(1).Tag = Ani.Value(2)
                        Case "Enabled"
                            Ani.Value(1).IsEnabled = Ani.Value(2)
                        Case "State"
                            Ani.Value(1).State = Ani.Value(2)
                        Case "IsHitTestVisible"
                            Ani.Value(1).IsHitTestVisible = Ani.Value(2)
                        Case "ShowInTaskbar"
                            Ani.Value(1).ShowInTaskbar = Ani.Value(2)
                        Case "Nothing"
                            Ani.Value(1) = Nothing
                        Case "Visible"
                            Ani.Value(1).Visibility = If(Ani.Value(2), Visibility.Visible, Visibility.Collapsed)
                        Case "End"
                            End
                    End Select

                Case AniType.ScaleTransform
                    Dim Delta As Double = MathPercent(0, Ani.Value, Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, Ani.Percent))
                    CType(Ani.Obj.RenderTransform, ScaleTransform).ScaleX = CType(Ani.Obj.RenderTransform, ScaleTransform).ScaleX + Delta
                    CType(Ani.Obj.RenderTransform, ScaleTransform).ScaleY = CType(Ani.Obj.RenderTransform, ScaleTransform).ScaleY + Delta

                Case AniType.RotateTransform
                    Dim Delta As Double = MathPercent(0, Ani.Value, Ani.Ease.GetDelta(Ani.FinishLength / Ani.TotalLength, Ani.Percent))
                    CType(Ani.Obj.RenderTransform, RotateTransform).Angle = CType(Ani.Obj.RenderTransform, RotateTransform).Angle + Delta

            End Select
            Ani.Percent = Ani.FinishLength / Ani.TotalLength '修改执行百分比
        Catch ex As Exception
            ExShow(ex, "执行动画失败：" & GetStringFromEnum(Ani.AniType))
        End Try

        Return Ani
    End Function

End Module '动画

Public Module BitDrawer

    Public GifList As New ArrayList 'Gif列表

    'Timer
    Public Sub GifStartRun()
        Dim time As New DispatcherTimer With {.Interval = TimeSpan.FromMilliseconds(1)}
        AddHandler time.Tick, AddressOf GifTimer
        time.Start()
    End Sub
    Public Sub GifTimer()
        For i = 0 To GifList.Count - 1
            Dim gif As MyBitmap = GifList(i)
            '切换Gif的帧
            If My.Computer.Clock.TickCount - gif.RoundLastTime >= gif.RoundTime Then
                gif.RoundLastTime = My.Computer.Clock.TickCount
                If gif.CurrentFlame = gif.MaxFlame - 1 Then
                    If gif.PlayOnce Then
                        '如果只播放一次那么拜拜
                        GifList.Remove(gif)
                    Else
                        '如果不只播放一次那么继续
                        gif.SetGifFrame(0)
                    End If
                Else
                    gif.SetGifFrame(gif.CurrentFlame + 1)
                End If
            End If
            '刷新绑定控件的显示
            If Not IsNothing(gif.Control) Then gif.Control.Source = New MyBitmap(gif.Pic)
        Next
    End Sub

    '类
    Public Class MyBitmap

        ''' <summary>
        ''' 存储的图片
        ''' </summary>
        ''' <remarks></remarks>
        Public Pic As System.Drawing.Bitmap
        ''' <summary>
        ''' (Gif)绑定的控件
        ''' </summary>
        ''' <remarks></remarks>
        Public Control As System.Windows.Controls.Image = Nothing
        ''' <summary>
        ''' (Gif)每一帧的时间
        ''' </summary>
        ''' <remarks></remarks>
        Public RoundTime As Integer
        ''' <summary>
        ''' (Gif)上一次改变时的时间
        ''' </summary>
        ''' <remarks></remarks>
        Public RoundLastTime As Integer
        ''' <summary>
        ''' (Gif)是否只播放一次
        ''' </summary>
        ''' <remarks></remarks>
        Public PlayOnce As Boolean = False
        ''' <summary>
        ''' (Gif)当前帧，从0开始
        ''' </summary>
        ''' <remarks></remarks>
        Public CurrentFlame As Integer = 0
        ''' <summary>
        ''' (Gif)最大帧数，从1开始
        ''' </summary>
        ''' <remarks></remarks>
        Public MaxFlame As Integer

        '类型转换
        '支持的类：Image   ImageSource   Bitmap   ImageBrush
        Public Shared Widening Operator CType(ByVal Image As System.Drawing.Image) As MyBitmap
            Return New MyBitmap(Image)
        End Operator
        Public Shared Widening Operator CType(ByVal Image As MyBitmap) As System.Drawing.Image
            Return Image.Pic
        End Operator
        Public Shared Widening Operator CType(ByVal Image As ImageSource) As MyBitmap
            Return New MyBitmap(Image)
        End Operator
        Public Shared Widening Operator CType(ByVal Image As MyBitmap) As ImageSource
            Dim ptr As IntPtr = Image.Pic.GetHbitmap()
            Dim re As ImageSource = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(
                ptr,
                IntPtr.Zero,
                Int32Rect.Empty,
                BitmapSizeOptions.FromEmptyOptions())
            DeleteObject(ptr)
            Return re
        End Operator
        Public Shared Widening Operator CType(ByVal Image As System.Drawing.Bitmap) As MyBitmap
            Return New MyBitmap(Image)
        End Operator
        Public Shared Widening Operator CType(ByVal Image As MyBitmap) As System.Drawing.Bitmap
            Return Image.Pic
        End Operator
        Public Shared Widening Operator CType(ByVal Image As ImageBrush) As MyBitmap
            Return New MyBitmap(Image)
        End Operator
        Public Shared Widening Operator CType(ByVal Image As MyBitmap) As ImageBrush
            Return New ImageBrush(New MyBitmap(Image.Pic))
        End Operator

        '构造函数
        Public Sub New()
        End Sub
        Public Sub New(ByVal FilePath As String)
            Try
                If FilePath.StartsWith(PATH_IMAGE) Then
                    '使用缓存
                    If BitmapCache.ContainsKey(FilePath) Then
                        Pic = BitmapCache(FilePath).Pic
                    Else
                        Pic = New MyBitmap(CType((New ImageSourceConverter).ConvertFromString(FilePath), ImageSource))
                        BitmapCache.Add(FilePath, Pic)
                    End If
                Else
                    Using inputStream As New FileStream(FilePath, FileMode.Open)
                        Pic = New System.Drawing.Bitmap(inputStream)
                    End Using
                End If
            Catch ex As Exception
                Pic = My.Application.TryFindResource(FilePath)
                If IsNothing(Pic) Then
                    Pic = New System.Drawing.Bitmap(1, 1)
                    ExShow(ex, "加载图片失败，该图片加载已被跳过：" & FilePath)
                End If
            End Try
        End Sub
        Public Sub New(ByVal Image As ImageSource)
            Using MS = New MemoryStream()
                Dim Encoder = New PngBitmapEncoder()
                Encoder.Frames.Add(BitmapFrame.Create(Image))
                Encoder.Save(MS)
                Pic = New System.Drawing.Bitmap(MS)
            End Using
        End Sub
        Public Sub New(ByVal Image As System.Drawing.Image)
            Pic = Image
        End Sub
        Public Sub New(ByVal Image As System.Drawing.Bitmap)
            Pic = Image
        End Sub
        Public Sub New(ByVal Image As ImageBrush)
            Using MS = New MemoryStream()
                Dim Encoder = New BmpBitmapEncoder()
                Encoder.Frames.Add(BitmapFrame.Create(Image.ImageSource))
                Encoder.Save(MS)
                Pic = New System.Drawing.Bitmap(MS)
            End Using
        End Sub

        ''' <summary>
        ''' 设置Gif的当前帧
        ''' </summary>
        ''' <param name="count">帧的编号，从0开始，超限制报错</param>
        ''' <remarks></remarks>
        Public Sub SetGifFrame(ByVal count As Integer)
            Pic.SelectActiveFrame(New System.Drawing.Imaging.FrameDimension(Pic.FrameDimensionsList()(0)), count)
            CurrentFlame = count
        End Sub

        ''' <summary>
        ''' 把这个对象视作Gif初始化
        ''' </summary>
        ''' <param name="round">两帧之间的间隔（毫秒）。</param>
        ''' <param name="once">是否只播放一次，True为是。</param>
        ''' <param name="control">绑定的控件。</param>
        ''' <remarks></remarks>
        Public Sub GifLoad(ByVal round As Integer, ByVal once As Boolean, Optional ByVal control As System.Windows.Controls.Image = Nothing)
            RoundTime = round
            RoundLastTime = My.Computer.Clock.TickCount
            PlayOnce = once
            Me.Control = control
            GifList.Add(Me)
            MaxFlame = Pic.GetFrameCount(New System.Drawing.Imaging.FrameDimension(Pic.FrameDimensionsList()(0)))
        End Sub

        ''' <summary>
        ''' 获取旋转的图片，这个方法不会导致原对象改变
        ''' </summary>
        ''' <param name="angle">旋转角度，单位为角度不是弧度啊</param>
        ''' <returns>旋转后的Bitmap</returns>
        ''' <remarks></remarks>
        Public Function Rotation(ByVal angle As Double) As System.Drawing.Bitmap
            With Me
                Dim img As System.Drawing.Image = Me.Pic
                Dim bitSize As Single = Math.Sqrt(img.Width ^ 2 + img.Height ^ 2)
                bitSize = img.Width
                Dim bmp As System.Drawing.Bitmap = New System.Drawing.Bitmap(CInt(bitSize), CInt(bitSize))
                Using g As System.Drawing.Graphics = System.Drawing.Graphics.FromImage(bmp)
                    g.TranslateTransform(bitSize / 2, bitSize / 2)
                    g.RotateTransform(angle)
                    g.TranslateTransform(-bitSize / 2, -bitSize / 2)
                    g.DrawImage(img, New System.Drawing.Rectangle(0, 0, img.Width, img.Width))
                End Using
                Return bmp
            End With
        End Function

        ''' <summary>
        ''' 获取左右翻转的图片，这个方法不会导致原对象改变
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function LeftRightFilp() As System.Drawing.Bitmap
            Dim bmp As System.Drawing.Bitmap = New System.Drawing.Bitmap(Me.Pic)
            bmp.RotateFlip(System.Drawing.RotateFlipType.RotateNoneFlipX)
            Return bmp
        End Function

    End Class

    '合并Bitmap
    Public Function BitmapAdd(ByVal background As System.Drawing.Bitmap, ByVal position As System.Drawing.Point, ByVal img As System.Drawing.Bitmap) As System.Drawing.Bitmap
        Dim graph As System.Drawing.Graphics = System.Drawing.Graphics.FromImage(background)
        graph.DrawImage(img, position)
        Return background
    End Function

    '裁剪Bitmap
    Public Function BitmapCut(ByVal img As BitmapSource, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer) As BitmapSource
        Return New CroppedBitmap(img, New Int32Rect(x, y, width, height))
    End Function

End Module '绘图
