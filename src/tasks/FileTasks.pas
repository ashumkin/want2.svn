unit FileTasks;

interface

uses
  Classes, DanteClasses, ExecTasks;

type
  TDeleteTask = class(TTask)
  private
    FDir: string;
  public
    procedure Execute; override;
    class function XMLTag :string; override;
  published
    property Dir: string read FDir write FDir;
  end;

implementation

uses JclFileUtils;

{ TDeleteTask }

procedure TDeleteTask.Execute;
begin
  if FDir <> '' then
  begin
    JclFileUtils.DelTree(FDir);
  end;
end;

class function TDeleteTask.XMLTag: string;
begin
  Result := 'delete';
end;

initialization
  RegisterTasks([TDeleteTask]);
end.

