{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclSscanf.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Pascal implementation of the C/C++ sscanf (String Scan Format) function.     }
{                                                                              }
{ Unit owner:                                                                  }
{ Last modified: January 30, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclSscanf;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils,
  JclBase;

function Sscanf(const Data: string; const Format: string; const Args: array of const): Integer;

type
  EJclSscanfError = class (EJclError);

implementation

uses
  JclResources, JclStrings;

type
  TFieldKind = (ftChar, ftSmallInt, ftInteger, ftInt64, ftFloating, ftCurrency,
    ftString, ftHex, ftPointer, ftCount, ftUnsigned, ftLiteral, ftWhiteSpace);

//------------------------------------------------------------------------------

procedure CreateSet(Source: string; var ResultSet: TSysCharSet);
var
  At: Integer;
  Token: Char;
  EndToken: Char;
  LoopToken: Char;
  Negate: Boolean;

  function GetToken(IsLiteral: Boolean): Char;
  begin
    if At <= Length(Source) then
    begin
      Result := Source[At];
      Inc(At);
      if not IsLiteral then
      begin
        if Result = '\' then  // Literal character.
          Result := GetToken(True);
        // Provides support for embedded control characters with the standard
        // Pascal ^a format. Removed because ^ has a special meaning for scanf
        // sets. Kept for future reference.
        (*
        else
        if Result ='^' then
        begin
          Result := UpCase(GetToken(True));
          if not Result in [#64..#95] then
            raise EJclSscanfError.Create(RsSscanfBadSet + Source);
          Result := Char(Byte(Result)-64);
        end;
        *)
      end;
    end
    else
      raise EJclSscanfError.Create(RsSscanfBadSet + Source);
  end;

begin
  ResultSet := [];
  At := 1;
  Negate := (Copy(Source, 1, 1) = '^');
  if Negate then
  begin
    Delete(Source, 1, 1);
    ResultSet := [#0..#255];
  end;
  while At <= Length(Source) do
  begin
    Token := GetToken(False);
    EndToken := Token;
    if (Copy(Source, At, 1) = '-') and (At <> Length(Source)) then  // This is a range.
    begin
      Inc(At, 1);  // Go past dash.
      if At > Length(Source) then
        raise EJclSscanfError.Create(RsSscanfBadSet + Source);
      EndToken := GetToken(False);
    end;
    if EndToken < Token then  // Z-A is probably the result of a missing letter.
      raise EJclSscanfError.Create(RsSscanfBadSet + Source);
    if Negate then
      for LoopToken := Token to EndToken do
        Exclude(ResultSet, LoopToken)
    else
      for LoopToken := Token to EndToken do
        Include(ResultSet, LoopToken);
  end;
end;

//------------------------------------------------------------------------------

function GetToken(const Str: string; var At: Integer): Char;
begin
  if At <= Length(Str) then
  begin
    Result := Str[At];
    Inc(At);
  end
  else
  begin
    At := Length(Str) + 1;
    raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
  end;
end;

//------------------------------------------------------------------------------

function PeekToken(const Str: string; const At: Integer): Char;
begin
  if At <= Length(Str) then
    Result := Str[At]
  else
    Result := #0;
end;

//------------------------------------------------------------------------------

function GetScanfToken(const Format: string; var At: Integer): string;
var
  Token: Char;
  TokenDone: Boolean;
  BuildingSet: Boolean;
begin
  Token := GetToken(Format, At);
  if Token = '%' then
  begin
    Result := Token;
    BuildingSet := False;
    TokenDone := False;
    repeat
      Token := GetToken(Format, At);
      Result := Result + Token;
      if (Token = '\') and BuildingSet then
      begin
        Token := GetToken(Format, At);
        Result := Result + Token;
      end
      else
      if Token = '[' then
        BuildingSet := True
      else
      if Token = ']' then
      begin
        BuildingSet := False;
        Token := PeekToken(Format, At);
        if Token in ['C', 'c'] then
        begin
          Token := GetToken(Format, At);
          Result := Result + Token;
        end;
        TokenDone := True;
      end
      else
      if (Token in ['*', '0'..'9']) or BuildingSet then
      begin
        // Data is accepted and added to the tag.
      end
      else
        TokenDone := True;
    until TokenDone;
  end
  else
  begin
    Result := Token;
    while Token in AnsiWhiteSpace do
    begin
      Token := PeekToken(Format, At);
      if Token in AnsiWhiteSpace then
        Token := GetToken(Format, At);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure InterpretScanToken(ScanToken: string; var FieldType: TFieldKind;
  var FieldWidth: Integer; var CharSet: TSysCharSet; var Stored: Boolean);
const
  NumberSet = ['0'..'9', '-'];
  HexSet = ['0'..'9','A'..'F', 'a'..'f', '$', 'x', 'X', '-'];
var
  TokenChar: Char;
  At: Integer;
  EndAt: Integer;
  Frag: string;
  Token: Char;
begin
  CharSet := [];
  if Copy(ScanToken, 1, 1) = '%' then
  begin
    Delete(ScanToken, 1, 1);
    TokenChar := ScanToken[Length(ScanToken)];
    if TokenChar <> ']' then
      Delete(ScanToken, Length(ScanToken), 1)
    else
      TokenChar := 's';
    At := Pos('[', ScanToken);
    if At <> 0 then
    begin
      EndAt := At;
      Token := GetToken(ScanToken, EndAt);
      while Token <> ']' do
      begin
        Token := GetToken(ScanToken, EndAt);
        if Token = '\' then
        begin
          Token := GetToken(ScanToken, EndAt);
          if Token = ']' then
            Token := #0; // Skip Literal ]'s.
        end;
      end;
      Dec(EndAt);
      Frag := Copy(ScanToken, At + 1, EndAt - At - 1);
      Delete(ScanToken, At, EndAt - At + 1);
      CreateSet(Frag, CharSet);
    end
    else
      CharSet := [];
    At := Pos('*', ScanToken);
    Stored := (At = 0);
    if not Stored then
      Delete(ScanToken, At, 1);
    if ScanToken <> '' then
    begin
      try
        FieldWidth := StrToInt(ScanToken);
      except
        raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
      end;
    end
    else
      FieldWidth := -1;
    case TokenChar of
      'c':
        begin
          FieldType := ftChar;
          if FieldWidth = -1 then
            FieldWidth := 1;
        end;
      'd':
        begin
          FieldType := ftInteger;
          CharSet := ['0'..'9', '-'];
        end;
      'e', 'f', 'g':
        begin
          FieldType := ftFloating;
          CharSet := ['0'..'9', '.', '+', '-', 'E', 'e'];
        end;
      {$IFDEF SUPPORTS_INT64}
      'i':
        begin
          FieldType := ftInt64;
          CharSet := NumberSet;
        end;
      {$ENDIF SUPPORTS_INT64}
      'h':
        begin
          FieldType := ftSmallInt;
          CharSet := NumberSet;
        end;
      'm':
        begin
          FieldType := ftCurrency;
          CharSet := NumberSet;
        end;
      's':
        begin
           FieldType := ftString;
        end;
      'x':
        begin
          FieldType := ftHex;
          CharSet := HexSet;
        end;
      'p':
        begin
          FieldType := ftPointer; // All pointers are 32 bit.
          CharSet := HexSet;
        end;
      'u':
        begin
          FieldType := ftUnsigned;
          CharSet := ['0'..'9'];
        end;
      '%':
        begin
          FieldType := ftLiteral;
          if (FieldWidth <> -1) or (not Stored) or (CharSet <> []) then
            raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
          Stored := False;
        end;
    else
      raise EJclSscanfError.CreateResRec(@RsSscanfBadFormat);
    end;
  end
  else
  begin
    if ScanToken[1] in AnsiWhiteSpace then
    begin
      FieldType := ftWhiteSpace;
      FieldWidth := -1;
      Stored := False;
      CharSet := [];
    end
    else
    begin
      FieldType := ftLiteral;
      FieldWidth := Length(ScanToken);
      Stored := False;
      CharSet := [];
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetDataToken(const Data: string; var DataAt: Integer;
  var OutOfData: Boolean; const FieldType: TFieldKind;
  const FieldWidth: Integer; CharSet: TSysCharSet): string;
var
  Token: Char;
begin
  OutOfData := False;
  Result := '';
  if FieldType = ftChar then
  begin
    try
      GetToken(Data, DataAt);
    except
      OutOfData := True;
      Exit; // Hit End of string.
    end;
  end
  else
  begin
    Token := #32;
    while Token in AnsiWhiteSpace do
    begin
      try
        Token := GetToken(Data, DataAt);
      except
        OutOfData := True;
        Exit; // Hit end of string.
      end;
    end;
    if FieldType = ftWhiteSpace then
    begin
      Dec(DataAt);  // Unget the last token.
      Exit;
    end;
  end;

  if CharSet = [] then
  begin
    CharSet := [#0..#255];
    if FieldType <> ftChar then
      CharSet := CharSet-AnsiWhiteSpace;
  end;

  Dec(DataAt);  // Unget the last token.
  while (FieldWidth = -1) or (Length(Result) < FieldWidth) do // Check Length
  begin
    Token := PeekToken(Data, DataAt);
    if Token in CharSet then
    begin
      try
        Token := GetToken(Data, DataAt);
        Result := Result+Token;
      except
        Break;
      end;
    end
    else
      Break;
  end;
end;

//------------------------------------------------------------------------------

function CheckFieldData(Data: string; const FieldType: TFieldKind): Boolean;
begin
  try
    case FieldType of
//      ftChar:
//        begin
//        end;
      ftSmallInt:
        StrToInt(Data);
      ftInteger:
        StrToInt(Data);
      {$IFDEF SUPPORTS_INT64}
      ftInt64:
        StrToInt64(Data);
      {$ENDIF SUPPORTS_INT64}
      ftFloating:
        StrToFloat(Data);
      ftCurrency:
        StrToFloat(Data);
//      ftString:
//        begin
//        end;
      ftHex:
        begin
          if AnsiUpperCase(Copy(Data, 1, 2)) = '0X' then
            Delete(Data, 1, 2);
          if Copy(Data, 1, 1) <> '$' then
            Data := '$'+Data;
          StrToInt(Data);
        end;
      ftPointer:
        StrToInt(Data);
//      ftCount:
//        begin
//        end;
      ftUnsigned:
        StrToInt(Data);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function StoreData(Data: string; const FieldType: TFieldKind;
  const FieldWidth: Integer; P: Pointer; VType: Integer): Boolean;
begin
  try
    case FieldType of
      ftChar:
        Move(Data[1], P^, FieldWidth);
      ftSmallInt:
        SmallInt(P^) := StrToInt(Data);
      ftInteger:
        Integer(P^) := StrToInt(Data);
      {$IFDEF SUPPORTS_INT64}
      ftInt64:
        Int64(P^) := StrToInt64(Data);
      {$ENDIF SUPPORTS_INT64}
      ftFloating:
        Extended(P^) := StrToFloat(Data);
      ftCurrency:
        Currency(P^) := StrToFloat(Data);
      ftString:
        begin
          if VType = vtString then
            ShortString(P^) := Data
          else
            string(P^) := Data;
        end;
      ftHex:
        begin
          if AnsiUpperCase(Copy(Data, 1, 2)) ='0X' then
            Delete(Data, 1, 2);
          if Copy(Data, 1, 1) <> '$' then
            Data := '$'+Data;
          Integer(P^) := StrToInt(Data);
        end;
      ftPointer:
        Integer(P^) := StrToInt(Data);
      ftCount:
        begin
        end;
      ftUnsigned:
        Cardinal(P^) := StrToInt(Data);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function Sscanf(const Data: string; const Format: string;
  const Args: array of const): Integer;
var
  At: Integer;
  DataAt: Integer;
  Results: Integer;
  ScanToken: string;
  StringToken: string;
  FieldType: TFieldKind;
  FieldWidth: Integer;
  CharSet: TSysCharSet;
  Stored: Boolean;
  OutOfData: Boolean;
  P: Pointer;
begin
  At := 1;
  DataAt := 1;
  Results := 0;

  while At <= Length(Format) do
  begin
    try
      ScanToken := GetScanfToken(Format, At);
      InterpretScanToken(ScanToken, FieldType, FieldWidth, CharSet, Stored);
    except
      Break;
    end;
    if FieldType <> ftCount then
      StringToken := GetDataToken(Data, DataAt, OutOfData, FieldType, FieldWidth, CharSet);

    if FieldType = ftLiteral then
      if StringToken <> ScanToken then
        Break;

    if not OutOfData then
    begin
      if not CheckFieldData(StringToken, FieldType) then
        Break;
      if Stored then
      begin
        if (Results + Low(Args)) > High(Args) then
          raise EJclSscanfError.CreateResRec(@RsSscanfInsufficient);
        P := Args[Results + Low(Args)].VPointer;
        StoreData(StringToken, FieldType, FieldWidth, P, Args[Results + Low(Args)].VType);
        Inc(Results);
      end;
    end;
  end;
  Result := Results;
end;

end.
