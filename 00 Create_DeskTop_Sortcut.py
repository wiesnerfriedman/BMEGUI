import os, sys
import pythoncom
from win32com.shell import shell, shellcon





print 'sys.argv[0] =', sys.argv[0]             
pathname = os.path.dirname(sys.argv[0])        
print 'path =', pathname
print 'full path =', os.path.abspath(pathname)


shortcut = pythoncom.CoCreateInstance (
  shell.CLSID_ShellLink,
  None,
  pythoncom.CLSCTX_INPROC_SERVER,
  shell.IID_IShellLink
)
TargetFile = os.path.abspath(pathname)+'\\tgisstd.py'   #
#IconPicture = os.path.abspath(pathname)+'\\BMEGUIpic.exe'
IconPicture = os.path.abspath(pathname)+'\\BMEGUIpic6.bmp'
print IconPicture

#shortcut.SetPath (sys.executable)
shortcut.SetPath(TargetFile)

shortcut.SetDescription ("Python %s" % sys.version)

#shortcut.SetIconLocation (sys.executable, 0)
shortcut.SetIconLocation (IconPicture,0)
print (sys.executable, 0)

desktop_path = shell.SHGetFolderPath (0, shellcon.CSIDL_DESKTOP, 0, 0)
persist_file = shortcut.QueryInterface (pythoncom.IID_IPersistFile)
persist_file.Save (os.path.join (desktop_path, "BMEGUI.lnk"), 0)




#----- reset Working Directory 
shortcut = pythoncom.CoCreateInstance (
  shell.CLSID_ShellLink,
  None,
  pythoncom.CLSCTX_INPROC_SERVER,
  shell.IID_IShellLink
)

desktop_path = shell.SHGetFolderPath (0, shellcon.CSIDL_DESKTOP, 0, 0)
shortcut_path = os.path.join (desktop_path, "BMEGUI.lnk")
persist_file = shortcut.QueryInterface (pythoncom.IID_IPersistFile)
persist_file.Load (shortcut_path)


shortcut.SetDescription ("BMEGUI %s" % sys.version)
mydocs_path = shell.SHGetFolderPath (0, shellcon.CSIDL_PERSONAL, 0, 0)
shortcut.SetWorkingDirectory (os.path.abspath(pathname))

persist_file.Save (shortcut_path, 0)
