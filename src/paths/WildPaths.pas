{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Juancarlo Añez, Caracas, Venezuela.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, Dante and the names of contributors to this software
may not be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit WildPaths;
{:
@TODO: File-by-file matching currently relies too much on the OS, so it may
       not work consistently in Windows and Linux.

       To make it consistent:
         * Allways retreive lists of files with an '*' pattern
         * Filter the resulting list using IsMatch and our own pattern.

       Besides consistency, our own matching would allow us to use extended,
       Unix-like file matching ([-] ranges, etc.) on Windows.

       Do not make these changes until specific user stories prove them
       necessary.
}

interface
uses
  SysUtils,
  Classes;

const
  InvalidPathChars = ''; //';:,';
  WildChars        = '?*';

  SystemPathDelimiter :string = '\';

type
  TPath  = string;
  TSpec  = TPath;
  TPaths = array of TPath;
  TSpecs = TPaths;

  EWildPathsException = class(Exception);
  EWildPathsError     = class(EWildPathsException);

function PathConcat(P1, P2 :TPath) :TPath;

function ToSystemPath(Path :TPath; BasePath :string = ''):string;
function ToPath(SystemPath :string; BasePath :string = ''):TPath;

function ToSystemPaths(Paths :TPaths; BasePath :string = '') :TPaths;
function ToPaths(OSPaths :TPaths; BasePath :string = '') :TPaths;


function StringsToPaths(S :TStrings):TPaths;
function SplitPath(Path :TPath) :TPaths;

function  MovePath(Path :TPath; FromBase :TPath; ToBase :TPath = '') :TPath;
function  MovePaths(Paths :TPaths; FromBase :TPath; ToBase :TPath = '') :TPaths;
function  AbsoluteToRelativePath(Path, BasePath :TPath):TPath;
function  AbsoluteToRelativePaths(Paths :TPaths; BasePath :TPath):TPaths;
function  PathIsAbsolute(Path :TPath) :boolean;
procedure ForceRelativePath(var Path, BasePath :TPath);

// use JCL for this
function FindPaths(Path: TPath; BasePath: string;
                   IncludeAttr :Integer = faAnyFile;
                   ExcludeAttr :Integer = 0): TPaths;
function FindFiles(Path: TPath; BasePath: string = ''): TPaths;
function FindDirs(Path: TPath; BasePath: string  = ''): TPaths;

function  Wild(Spec :TPath; BasePath :string = ''):TPaths; overload;
procedure Wild(Files :TStrings; Spec :TPath; BasePath :string = ''); overload;


function IsMatch(Path, Spec :TPath):boolean; overload;
function IsMatch(Paths :TPaths; Specs :TSpecs; p :Integer = 0; s :Integer = 0):boolean; overload;

function  IsDir(Path :TPath):boolean;
function  IsFile(Path :TPath):boolean;
function  SuperPath(Path :TPath) :TPath;


implementation

procedure Wild(Files :TStrings; Specs :TSpecs; BasePath: TPath = ''; Index :Integer = 0);
  overload; forward;


function PathConcat(P1, P2 :TPath) :TPath;
begin
   if (Length(P1) = 0)
   or PathIsAbsolute(P2) then
     Result := P2
   else if Length(P2) = 0 then
     Result := P1
   else if P1[Length(P1)] = '/' then
     Result := P1 + P2
   else
     Result := P1 + '/' + P2;
end;

function ToPath(SystemPath: string; BasePath :string): TPath;
begin
  if LastDelimiter(InvalidPathChars, SystemPath) > 0 then
    raise EWildPathsException.Create('invalid path chars in ' + SystemPath)
  else
  begin
    Result := SystemPath;
    if BasePath <> '' then
      Result := ExtractRelativePath(BasePath, Result);
    if (Length(Result) >= 2)
    and (Result[2] = ':')
    and (Result[1] in ['a'..'z', 'A'..'Z'])
    then
    begin
      if (Length(Result) >= 3)
      and (Result[3] = SystemPathDelimiter) then
        Result := SystemPathDelimiter + Result
      else
        raise EWildPathsException.Create('invalid absolute path ' + SystemPath)
    end;
    Result := StringReplace(Result, SystemPathDelimiter, '/', [rfReplaceAll]);
  end;
end;

function ToSystemPath(Path: TPath; BasePath :string): string;
begin
   Result := PathConcat(BasePath, Path);
   Result := StringReplace(Result, '/', SystemPathDelimiter, [rfReplaceAll]);
   Result := ExpandFileName(Result);
end;

function ToSystemPaths(Paths :TPaths; BasePath :string = '') :TPaths;
var
  i :Integer;
begin
  SetLength(Result, Length(Paths));
  for i := 0 to High(Result) do
    Result[i] := ToSystemPath(Paths[i], BasePath);
end;

function ToPaths(OSPaths :TPaths; BasePath :string = '') :TPaths;
var
  i :Integer;
begin
  SetLength(Result, Length(OSPaths));
  for i := 0 to High(Result) do
    Result[i] := ToSystemPath(OSPaths[i], BasePath);
end;


function FindPaths( Path: TPath; BasePath :string;
                    IncludeAttr :Integer;
                    ExcludeAttr :Integer):TPaths;
var
  S :TStringList;
  Search :TSearchRec;
  SearchResult :Integer;
begin
  S := TStringList.Create;
  S.Sorted := True;
  try
    SearchResult := FindFirst(ToSystemPath(Path, BasePath), faAnyFile, Search);
    try
      while SearchResult = 0 do
      begin
        if ((Search.Attr and IncludeAttr) <> 0)
        and ((Search.Attr and ExcludeAttr) = 0)
        and (Search.Name <> '.' )
        and (Search.Name <> '..' )
        then
          S.Add(ToPath(Search.Name, BasePath));
        SearchResult := FindNext(Search);
      end;
    finally
      FindClose(Search);
    end;
    Result := StringsToPaths(S);
  finally
    S.Free;
  end;
end;

function FindDirs(Path: TPath; BasePath: string): TPaths;
begin
   Result := FindPaths(Path, BasePath, faDirectory, 0);
end;

function FindFiles(Path: TPath; BasePath: string): TPaths;
begin
   Result := FindPaths(Path, BasePath, faAnyFile, faDirectory);
end;

function SplitPath(path: TPath): TPaths;
var
  S :TStrings;
begin
  S := TStringList.Create;
  try
    S.CommaText := StringReplace(path, '/', ',', [rfReplaceAll]);
    Result := StringsToPaths(S);
  finally
    S.Free;
  end;
end;

function StringsToPaths(S: TStrings): TPaths;
var
  i :Integer;
begin
  SetLength(Result, S.Count);
  for i := 0 to S.Count-1 do
    Result[i] := S[i];
end;

function MovePath(Path :TPath; FromBase :TPath; ToBase :TPath) :TPath;
begin
   if Pos(FromBase, Path) = 1 then
     Result := PathConcat(ToBase, Copy(Path, 2+Length(FromBase), Length(Path)))
   else if PathIsAbsolute(Path) then
     Result :=  Path
   else
     Result := PathConcat(ToBase, Path);
end;


function MovePaths(Paths :TPaths; FromBase :TPath; ToBase :TPath) :TPaths;
var
  i :Integer;
begin
   SetLength(Result, Length(Paths));
   for i := Low(Paths) to High(Paths) do
     Result[i] := MovePath(Paths[i], FromBase, ToBase);
end;

function  AbsoluteToRelativePath(Path, BasePath :TPath):TPath;
begin
  Result := MovePath(Path, BasePath, '');
end;

function  AbsoluteToRelativePaths(Paths :TPaths; BasePath :TPath):TPaths;
begin
  Result := MovePaths(Paths, BasePath, '');
end;

function  PathIsAbsolute(Path :TPath) :boolean;
begin
   Result :=    (Length(Path) > 0) and (Path[1] = '/')
             or (Length(Path) >= 3) and (Path[2] = ':') and (Path[3] = '/')
end;

procedure ForceRelativePath(var Path, BasePath :TPath);
var
  p :Integer;
begin
  if PathIsAbsolute(Path) then
  begin
    p := Pos('/', Path);
    BasePath := BasePath + Copy(Path, 1, p);
    Delete(Path, 1, p);
    if PathIsAbsolute(Path) then
    begin // must be UNC, URI, or C:/ style
      p := 1+Pos('/', Copy(Path, 2, Length(Path)));
      BasePath := BasePath + Copy(Path, 1, p-1);
      Delete(Path, 1, p);
    end;
  end;
end;

function Wild(Spec: TPath; BasePath: string):TPaths;
var
  Files :TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Sorted := True;
    Wild(Files, Spec, BasePath);
    Result := StringsToPaths(Files);
  finally
    Files.Free;
  end;
end;

procedure Wild(Files :TStrings; Spec :TPath; BasePath :string = '');
begin
  ForceRelativePath(Spec, BasePath);
  Wild(Files, SplitPath(Spec), BasePath);
end;

procedure Wild(Files :TStrings; Specs: TSpecs; BasePath: TPath; Index: Integer);
var
  i       :Integer;
  Matches :TPaths;
begin
  if Index > High(Specs) then
    EXIT;
  // absorb all non-wildcard specs
  while (Index < High(Specs))
  and (LastDelimiter(WildChars, Specs[Index]) = 0) do
  begin
    BasePath := PathConcat(BasePath, Specs[Index]);
    Inc(Index);
  end;
  Assert(Index <= High(Specs));
  if Index = High(Specs) then
  begin // add files (works for '**' too)
    Matches := FindPaths(Specs[Index], BasePath);
    for i := Low(Matches) to High(Matches) do
      Files.Add(PathConcat(BasePath, Matches[i]))
  end;
  // handle wildcards
  if Specs[Index] = '**' then
  begin // match anything and recurse
    Wild(Files, Specs, BasePath, Index+1);
    Matches := FindDirs('*', BasePath);
    // use same Index ('**') to recurse
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Specs, PathConcat(BasePath, Matches[i]), Index);
  end
  else if Index < High(Specs) then
  begin // match directories
    Matches := FindDirs(Specs[Index], BasePath);
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Specs, PathConcat(BasePath, Matches[i]), Index+1);
  end;
end;

function Matches(A, B :string; i :Integer = 1; j :Integer = 1):boolean;
begin
  while (i <= Length(A))
  and   (j <= Length(B))
  and   (UpCase(A[i]) = UpCase(B[j])) do
  begin
    Inc(i);
    Inc(j);
  end;
  if j > Length(B) then
    Result := i > Length(A)
  else if i > Length(A) then
    Result := False
  else if B[j] = '?' then
    Result :=    Matches(A, B, i+1, j+1)
              or Matches(A, B, i,   j+1)
  else if B[j] = '*' then
    Result :=    Matches(A, B, i+1, j+1)
              or Matches(A, B, i+1, j)
  else
    Result := False;
end;

function IsMatch(Paths :TPaths; Specs :TSpecs; p :Integer = 0; s :Integer = 0):boolean;

begin
  while (p <= High(Paths))
  and   (s <= High(Specs))
  and   (Specs[s] <> '**')
  and   Matches(Paths[p], Specs[s]) do
  begin
    Inc(p);
    Inc(s);
  end;
  if s > High(Specs) then
    Result := p > High(Paths)
  else if p > High(Paths) then
    Result := False
  else if Specs[s] = '**' then
    Result :=    IsMatch(Paths, Specs, p+1, s+1)
              or IsMatch(Paths, Specs, p,   s+1)
              or IsMatch(Paths, Specs, p+1, s)
  else
    Result := False;
end;

function IsMatch(Path, Spec :TPath):boolean;
begin
  Result := IsMatch(SplitPath(Path), SplitPath(Spec));
end;

function  IsDir(Path :TPath):boolean;
begin
  Result := Length(FindDirs(Path)) = 1;
end;

function  IsFile(Path :TPath):boolean;
begin
  Result := Length(FindFiles(Path)) = 1;
end;

function  SuperPath(Path :TPath) :TPath;
var
  Base :TPath;
  p    :Integer;
begin
  Base := '';
  ForceRelativePath(Path, Base);
  p := LastDelimiter('/', path);
  if p = 0 then
    raise Exception.Create(Path + ' does not have a super directory')
  else
    Result := PathConcat(Base, Copy(Path, 1, p-1));
end;

end.
