{(#)$Id$}
unit JALUIHandler;

interface
uses
  Windows,
  Graphics,

  JalGeometry,

  Classes,
  Controls;

const
  rcs_id : string = '(#)$Id$';

type
  TUIHandler = class(TComponent)
  public
    Selecting   :Boolean;
    AmDragging  :Boolean;
    HaveRect    :Boolean;
    Anchor      :TPoint;
    CurrentPos  :TPoint;
    DragRect    :TRect;

    procedure NewSelection(const P :TPoint);

    procedure Click(Sender: TObject);    virtual;
    procedure DblClick(Sender: TObject); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);                        virtual;
    procedure MouseUp(  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);    virtual;
    procedure KeyDown(  Sender: TObject; var Key: Word; Shift: TShiftState);                          virtual;
    procedure KeyPress( Sender: TObject; var Key: Char);                                             virtual;
    procedure KeyUp(    Sender: TObject; var Key: Word; Shift: TShiftState);                            virtual;
  end;

implementation


type
  TCCCrack = class(TCustomControl);

procedure TUIHandler.NewSelection(const P: TPoint);
begin
  CurrentPos := P;
  DragRect := Rect([Anchor, P]);
  HaveRect := true;
end;

procedure TUIHandler.Click(Sender: TObject);
begin
end;

procedure TUIHandler.DblClick(Sender: TObject);
begin
end;

procedure TUIHandler.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button <> mbLeft then
      Exit;

  Self.Selecting    := True;
  Self.AmDragging   := False;

  Anchor   := Point(X, Y);
  CurrentPos := Anchor;

  if ssDouble in Shift then
    Exit; {!!!}

  DragRect := Rect([Anchor, CurrentPos]);
  HaveRect := false;
end;


procedure TUIHandler.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Self.Selecting and (ssLeft in Shift) then
      NewSelection(Point(X, Y))
  else
     Self.Selecting := False;
end;

procedure TUIHandler.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    if Selecting then
      NewSelection(Point(X, Y));
  finally
    Self.Selecting := False;
    Self.AmDragging  := False;
  end
end;

procedure TUIHandler.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
end;


procedure TUIHandler.KeyPress(Sender: TObject; var Key: Char);
begin
end;

procedure TUIHandler.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
end;

end.

