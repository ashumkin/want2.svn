{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca ®                                 }
{    ~                                         }
{  Copyright © 1995-2002 Juancarlo Añez        }
{  http://www.suigeneris.org/juanca            }
{  All rights reserved.                        }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit JALOwnedTrees;

interface
uses
  SysUtils,
  Classes;

type
  TTree = class
  protected
    FParent   :TTree;
    FChildren :TList;

    function GetChild(i :Integer):TTree;

    procedure SetParent(NewParent :TTree); virtual;

    procedure InsertNotification(Child : TTree); virtual;
    procedure RemoveNotification(Child : TTree); virtual;
  public
    constructor Create(Parent :TTree = nil);
    destructor  Destroy; override;

    function ChildCount :Integer;

    function Add(Child :TTree) :TTree;    virtual;
    function Remove(Child :TTree) :TTree; virtual;

    procedure Clear;

    property Parent :TTree read FParent write Setparent;

    property Children[i :Integer] :TTree read GetChild;
  end;


implementation

{ TTree }

constructor TTree.Create(Parent: TTree);
begin
  inherited Create;
  FChildren := TList.Create;

  SetParent(Parent);
end;

destructor TTree.Destroy;
begin
  SetParent(nil);
  Clear;
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
end;

function TTree.ChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TTree.GetChild(i: Integer): TTree;
begin
  if (i < 0) or (i >= FChildren.Count) then
    raise Exception.Create('Invalid position' + IntToStr(i));

  Result := FChildren[i];
end;

function TTree.Add(Child: TTree) :TTree;
begin
  Assert(Child <> nil);
  Child.SetParent(Self);
  Result := Child;
end;

function TTree.Remove(Child: TTree) :TTree;
var
  Index :Integer;
begin
  Index := FChildren.IndexOf(Child);
  if Index < 0 then
    raise Exception.Create('Child not found');

  Child.SetParent(nil);

  Result := Child;
end;

procedure TTree.SetParent(NewParent: TTree);
begin
  if NewParent = Self.Parent then
    EXIT;

  if Parent <> nil then
  begin
    Parent.RemoveNotification(Self);
    Parent.FChildren.Remove(Self);
  end;

  if NewParent <> nil then
  begin
    NewParent.FChildren.Add(Self);
    NewParent.InsertNotification(Self);
  end;

  Self.FParent := NewParent;
end;


procedure TTree.InsertNotification(Child: TTree);
begin

end;

procedure TTree.RemoveNotification(Child: TTree);
begin

end;

procedure TTree.Clear;
var
  i :Integer;
begin
  for i := ChildCount-1 downto 0 do
    Children[i].Free;
end;

end.
