unit FileTasks;

interface

uses
  Classes, DanteClasses, ExecTasks;

type
  TDeleteTask = class(TCustomExecTask)
  end;

implementation

initialization
  RegisterTasks([TDeleteTask]);
end.

