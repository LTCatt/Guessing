Imports System.Security.Cryptography

Public Module Modules

#Region "声明"

    ''' <summary>
    ''' 程序的启动路径，以“\”结尾。
    ''' </summary>
    ''' <remarks></remarks>
    Public PATH As String = GetPathFromFullPath(Forms.Application.ExecutablePath)
    Public PATH_IMAGE As String = "pack://application:,,,/images/"
    Public Rnd As New Random
    Public frmMain As formMain

#End Region

#Region "API"

    ''' <summary>
    ''' 获取指定窗口的句柄。返回查找到的第一个窗口的句柄。
    ''' </summary>
    ''' <param name="lCenterassName">窗口的类名，使用Spy++可以查看。</param>
    ''' <param name="lpWindowName">窗口的标题。</param>
    ''' <returns>查找到的第一个窗口的句柄。</returns>
    ''' <remarks></remarks>
    Public Declare Function FindWindow Lib "user32" Alias "FindWindowA" (ByVal lCenterassName As String, ByVal lpWindowName As String) As Integer
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
                If Stack.Contains("Center.") Then GetStringFromException = GetStringFromException & vbCrLf & Stack
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
            Dim actualPath As String = If(isFullPath, "", Path) & filePath
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
            Dim actualpath As String = If(isFullPath, "", Path) & filePath
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
        If Not FileName.Contains(":\") Then FileName = Path & "Center\" & FileName & ".ini"
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
        FileName = If(FileName.Contains(":\"), FileName, Path & "Center\" & FileName & ".ini")
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
    Public Function GetEnumFromString(ByVal EnumData As Object, ByVal Value As String)
        Return [Enum].Parse(EnumData.GetType, Value)
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
    ''' 将数组随机打乱。
    ''' </summary>
    Public Function RandomChaos(ByVal array As ArrayList) As ArrayList
        RandomChaos = New ArrayList
        Do While array.Count > 0
            Dim i As Integer = RandomInteger(0, array.Count - 1)
            RandomChaos.Add(array(i))
            array.RemoveAt(i)
        Loop
    End Function
    Public Function RandomChaos(ByVal array As Object()) As ArrayList
        Dim arr As New ArrayList
        arr.AddRange(array)
        Return RandomChaos(arr)
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
