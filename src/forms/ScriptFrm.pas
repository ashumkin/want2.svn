unit ScriptFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, ComCtrls;

type
  TScriptForm = class(TForm)
    CentralPanel: TPanel;
    ScriptPanel: TPanel;
    AttributesPanel: TPanel;
    ScriptTree: TTreeView;
    AttributesGrid: TStringGrid;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptForm: TScriptForm;

implementation

{$R *.DFM}

procedure TScriptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
