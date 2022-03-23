Public Class CMDUtility
    Shared Sub RunCommandCom(command As String, arguments As String, permanent As Boolean)
        Dim p As New Process()
        Dim pi As New ProcessStartInfo With {
            .Arguments = " " + If(permanent = True, "/K", "/C") + " " + command + " " + arguments,
            .FileName = "cmd.exe"
        }
        p.StartInfo = pi
        p.Start()
    End Sub
End Class
