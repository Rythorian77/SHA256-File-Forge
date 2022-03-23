Imports System.Runtime.InteropServices
Imports System.Text

Public Class RythoriansAnnihilationMemoryControl

#Region "Annihilation Structures"

    Public Structure Annihilation
        Dim f00 As Single
        Dim f01 As Single
        Dim f02 As Single
        Dim f03 As Single
        Dim f10 As Single
        Dim f11 As Single
        Dim f12 As Single
        Dim f13 As Single
        Dim f20 As Single
        Dim f21 As Single
        Dim f22 As Single
        Dim f23 As Single
        Dim f30 As Single
        Dim f31 As Single
        Dim f32 As Single
        Dim f33 As Single
    End Structure

    Public Structure XYChromosomes
        Dim x As Single
        Dim y As Single
    End Structure

    Public Structure SystematicCreation
        Dim x As Single
        Dim y As Single
        Dim z As Single
    End Structure

#End Region

#Region "Creation Pool API"

    Public Const PROCESS_ALL_ACCESS = &H1F0FF
    Private Declare Function OpenProcess Lib "kernel32" (dwDesiredAccess As Integer, bInheritHandle As Integer, dwProcessId As Integer) As IntPtr
    Private Declare Function WriteCreation Lib "ntdll" Alias "NtWriteVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Integer, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function WriteMemParticals Lib "ntdll" Alias "NtWriteVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Single, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function WriteMemoryLab Lib "ntdll" Alias "NtWriteVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Boolean, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function ReadMemoryHead Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Integer, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function ReadMemoryForerunner Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Single, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function ReadMemoryBiosphere Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Boolean, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function ReadAnnihilation Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As Annihilation, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function CreationEvo Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, <Out()> u32Buffer As Byte(), nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function VirtualHack Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As XYChromosomes, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Private Declare Function SeedOfSpeed Lib "ntdll" Alias "NtReadVirtualMemory" (hProcess As IntPtr, lpBaseAddress As Integer, ByRef lpBuffer As SystematicCreation, nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Boolean
    Public Declare Function FinalConflict Lib "kernel32" Alias "CloseHandle" (hobject As IntPtr) As Boolean

#End Region

#Region " Get Functions "

    Public Shared Function GetOffsetByName(data As String, offset As String) As Integer
        Try
            Dim pos As Integer = data.IndexOf(offset) + offset.Length + 5
            Dim s1 As String = data.Substring(pos, 20)
            Dim s2() As String = s1.Split(";")
            Dim x As Integer = Convert.ToInt32(s2(0), 16)
            Return x
        Catch ex As Exception
            Return 0
        End Try
    End Function

    Public Shared Function GetProcessByName(processname) As Process
        Dim p As Process() = Process.GetProcessesByName(processname)
        If p.Length > 0 Then
            Return p.FirstOrDefault
        End If
        Return Nothing
    End Function

    Public Shared Function GetHandle(p As Process) As IntPtr
        Try
            Return OpenProcess(PROCESS_ALL_ACCESS, 0, p.Id)
        Catch ex As Exception
            Return IntPtr.Zero
        End Try
    End Function

    Public Shared Function GetModuleBase(p As Process, modulename As String) As Integer
        Try
            Dim base As Integer = 0
            For Each m As ProcessModule In p.Modules
                If m.ModuleName = modulename Then
                    base = m.BaseAddress
                End If
            Next
            Return base
        Catch ex As Exception
            Return 0
        End Try
    End Function

#End Region

#Region " Read | Write Infrastructure "

    Public Shared Function RPMInt(hProcess As IntPtr, address As Integer) As Integer
        Dim buffer As Integer
        ReadMemoryHead(hProcess, address, buffer, 4, 0)
        Return buffer
    End Function

    Public Shared Function RPMFloat(hProcess As IntPtr, address As Integer) As Single
        Dim buffer As Single
        ReadMemoryForerunner(hProcess, address, buffer, 4, 0)
        Return buffer
    End Function

    Public Shared Function RPMBool(hProcess As IntPtr, address As Integer) As Boolean
        Dim buffer As Boolean
        ReadMemoryBiosphere(hProcess, address, buffer, 1, 0)
        Return buffer
    End Function

    Public Shared Function RPMViewOverRide(hProcess As IntPtr, address As Integer) As Annihilation
        Dim buffer As Annihilation
        ReadAnnihilation(hProcess, address, buffer, 64, 0)
        Return buffer
    End Function

    Public Shared Function RPMSParasite(hProcess As IntPtr, address As Integer, stringSize As Integer) As String
        Dim buffer(stringSize) As Byte
        CreationEvo(hProcess, address, buffer, stringSize, 0)
        Return Encoding.UTF8.GetString(buffer)
    End Function

    Public Shared Function RPMFVector(hProcess As IntPtr, address As Integer) As XYChromosomes
        Dim buffer As XYChromosomes
        VirtualHack(hProcess, address, buffer, 8, 0)
        Return buffer
    End Function

    Public Shared Function RPMFMnemonic(hProcess As IntPtr, address As Integer) As SystematicCreation
        Dim buffer As SystematicCreation
        SeedOfSpeed(hProcess, address, buffer, 12, 0)
        Return buffer
    End Function

    Public Shared Function WPMInt(hProcess As IntPtr, address As Integer, value As Integer) As Boolean
        If WriteCreation(hProcess, address, value, 4, 0) Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function WPMFloat(hProcess As IntPtr, address As Integer, value As Single) As Boolean
        If WriteMemParticals(hProcess, address, value, 4, 0) Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Shared Function WPMBool(hProcess As IntPtr, address As Integer, value As Boolean) As Boolean
        If WriteMemoryLab(hProcess, address, value, 1, 0) Then
            Return True
        Else
            Return False
        End If
    End Function

#End Region

End Class