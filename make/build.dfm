object delphi_compile: TProject
  DefaultTarget = 'compile'
  object compile: TTarget
    object TDelphiCompileTask_0: TDelphiCompileTask
      Arguments.Strings = (
        '..\src\dante.dpr'
        '/U..\src\tasks;..\src\jcl;..\dcu'
        '/B'
        '/Q')
    end
    object del: TShellExecTask
      Executable = 'del ..\dist\*.*'
    end
    object rmdir: TShellExecTask
      Executable = 'rmdir ..\dist'
    end
    object mkdir: TShellExecTask
      Executable = 'mkdir ..\dist'
    end
    object copy_exe: TShellExecTask
      Executable = 'copy ..\src\dante.exe ..\dist\dante.exe'
    end
  end
end
