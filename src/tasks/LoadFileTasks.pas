(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{
  Contributors:
    Radim Novotny <radimnov@seznam.cz>
}

unit LoadFileTasks;

interface

uses
  SysUtils,
  Classes,
  WantClasses;

type
  TLoadFileTask = class(TTask)
  private
    FFailOnError :boolean;
    FProperty    :string;
    FSrcFile     :string;
  public
    constructor Create;

    procedure Init;    override;
    procedure Execute; override;
  published
    property srcfile   :string read FSrcfile  write FSrcfile;
    property _property :string read FProperty write FProperty;
    {  property encoding is not supported until Synapse or other
       third-party library with character set transltion will be
       included in WANT
    }
    // property encoding : string read fencoding write fencoding;
    property failonerror: boolean read FFailOnError write FFailOnError;
  end;

implementation

uses
  FilterElements;

{ TLoadFileTask }

constructor TLoadFileTask.Create;
begin
  FFailOnError := True;
end;

procedure TLoadFileTask.Execute;
var
  i    : integer;
  j    : integer;
  bSL  : TStringList;
  bFCE : TFilterChainElement;
begin
  inherited;
  bSL := TStringList.Create;
  try
    try
      bSL.LoadFromFile(FSrcFile);
    except
      if FFailOnError then raise;
    end;
    // process filterchains
    for i := 0 to ChildCount - 1 do
    begin
      if Children[i] is TFilterChainElement then
      begin
        // filterchain element does not have any attributes, only nested tags
        bFCE := Children[i] as TFilterChainElement;
        for j := 0 to bFCE.ChildCount - 1 do
        begin
          if bFCE.Children[j] is TCustomFilterElement then
          begin
            bSL.Text := (bFCE.Children[j] as TCustomFilterElement).Execute(bSL.Text);
          end;
        end;
      end;
    end;
    if Assigned(Owner) then 
    begin
      Owner.SetProperty(FProperty, bSL.Text);
      Owner.Configure;
    end;
  finally
    bSL.Free;
  end;
end;

procedure TLoadFileTask.Init;
begin
  inherited;
  RequireAttributes(['srcfile', 'property']);
end;

initialization
  RegisterTask(TLoadFileTask);
end.
