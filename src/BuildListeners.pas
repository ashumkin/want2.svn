(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit BuildListeners;

interface
uses
  Classes,
  JclStrings,
  WantClasses;

const
  rcs_id :string = '#(@)$Id$';

type
  TBasicListener = class(TBuildListener)
  protected
    procedure LogLine(Msg: string; Level: TLogLevel = vlNormal);    virtual;
  public
    constructor Create;

    procedure Log(Level: TLogLevel; Msg: string = '');              override;

    procedure BuildFileLoaded(Project :TProject; FileName :string); override;

    procedure BuildStarted(Project :TProject);                      override;
    procedure BuildFinished(Project :TProject);                     override;
    procedure BuildFailed(Project :TProject; Msg :string = '');     override;

    procedure TargetStarted(Target :TTarget);                       override;
    procedure TargetFinished(Target :TTarget);                      override;

    procedure TaskStarted( Task :TTask);                            override;
    procedure TaskFinished(Task :TTask);                            override;
    procedure TaskFailed(  Task :TTask; Msg :string);               override;
  end;

implementation

{ TBasicListener }

procedure TBasicListener.BuildFileLoaded(Project: TProject; FileName: string);
begin

end;

procedure TBasicListener.BuildStarted(Project: TProject);
begin

end;

procedure TBasicListener.BuildFinished(Project: TProject);
begin

end;

procedure TBasicListener.BuildFailed(Project: TProject; Msg :string);
begin

end;

procedure TBasicListener.TargetStarted(Target: TTarget);
begin

end;

procedure TBasicListener.TargetFinished(Target: TTarget);
begin

end;

procedure TBasicListener.TaskStarted(Task: TTask);
begin

end;

procedure TBasicListener.TaskFinished(Task: TTask);
begin

end;

procedure TBasicListener.Log(Level: TLogLevel; Msg: string);
var
  Lines     :TStringList;
  i         :Integer;
begin
  if (Self.Level >= Level) then
  begin
    Lines := TStringList.Create;
    try
      Msg := Msg + ' ';
      JclStrings.StrToStrings(Msg, #10, Lines);
      for i := 0 to Lines.Count-1 do
      begin
        LogLine(Lines[i], Level);
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TBasicListener.TaskFailed(Task: TTask; Msg :string);
begin

end;

procedure TBasicListener.LogLine(Msg: string; Level: TLogLevel);
begin
  // do nothing
end;

constructor TBasicListener.Create;
begin
  inherited Create;
  FLevel := vlNormal;
end;

end.
