object delphi_compile: TProject
  DefaultTarget = 'compile'
  object compile: TTarget
    object TDelphiCompileTask_0: TDelphiCompileTask
      Arguments.Strings = (
        'C:\home\prj\dante\bin\..\src\dante.dpr'
        '/U..\src\tasks;..\src\jcl;..\dcu'
        '/B'
        '/Q')
    end
  end
end
