{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo Añez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit WildPaths;
{:
@BUG:  SplitPath is not consisten with PathConcat. The output of the first
       can not be put back together with the second. This is a problem for
       UNC paths.

@TODO: File-by-file matching currently relies too much on the OS, so it may
       not work consistently in Windows and Linux.

       To make it consistent:
         * Allways retreive lists of files with an '*' pattern
         * Filter the resulting list using IsMatch and our own pattern.

       Besides consistency, our own matching would allow us to use extended,
       Unix-like file matching ([-] ranges, etc.) on Windows.

       Do not make these changes until specific user stories prove them
       necessary.

@TODO: Factor out stuff that JCL already handles well.
}

interface

uses
  Windows,
  SysUtils,
  Math,
  Classes,

  WantUtils;

const
  InvalidPathChars = ''; //';:,';
  WildChars        = '?*';

  {$IFDEF LINUX}
  SystemPathDelimiter: string = '/';
  {$ELSE}
  SystemPathDelimiter: string = '\';
  {$ENDIF}

type
  TPath  = type string;
  TPattern  = TPath;
  TPaths = array of TPath;
  TPatterns = TPaths;

  TSystemPath  = string;
  TSystemPaths = array of TSystemPath;

  EPathException = class(Exception);
  EFileOpException    = class(EPathException);

  TFileAttribute = (
    ReadOnly,  {= $00000001 }
    Hidden,    {= $00000002 }
    SysFile,   {= $00000004 }
    VolumeID,  {= $00000008 }
    Directory, {= $00000010 }
    Archive,   {= $00000020 }
    NoFile     {= -1}
  );
  TFileAttributes = set of TFileAttribute;

const
  AnyFileAttribute = [ReadOnly..Archive];

function  IsSystemIndependentPath(const Path: TPath): boolean;
procedure AssertIsSystemIndependentPath(const Path: TPath);

function IsWindowsPath(const Path: TPath):boolean;

function PathDrive(const Path: TPath): string;
function PathServer(const Path: TPath): string;

function RemovePathDrive(Path: TPath):TPath;
function RemovePathServer(Path: TPath):TPath;

function PathConcat(const Path1, Path2: TPath): TPath;

function ToSystemPath(const Path: TPath; const BasePath: TPath = ''):TSystemPath;
function ToPath(SystemPath: TSystemPath;  const BasePath: TPath = ''):TPath;

function ToSystemPaths(const Paths: TPaths; const BasePath: TPath = ''): TSystemPaths; overload;
function ToPaths(OSPaths: TSystemPaths; const BasePath: TPath = ''): TPaths;

procedure ToSystemPaths(Paths: TStrings; const BasePath: TPath = ''); overload;


function StringsToPaths(S: TStrings):TPaths;
function SplitPath(Path: TPath): TPaths;

function  MovePath(const Path: TPath; const FromBase: TPath; const ToBase: TPath = ''): TPath;
function  MovePaths(const Paths: TPaths; const FromBase: TPath; const ToBase: TPath = ''): TPaths;

function  ToRelativePath(const Path, BasePath: TPath):TPath;
function  ToRelativePaths(const Paths: TPaths; const BasePath: TPath):TPaths; overload;
function  ToRelativePaths(Paths: TStrings; const BasePath: TPath):TPaths; overload;

function  PathIsAbsolute(const Path: TPath): boolean;
procedure ForceRelativePath(var Path, BasePath: TPath);

function FindPaths(const Path: TPath; const BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                   ): TPaths;
function FindFiles(const Path: TPath; const BasePath: TPath = ''): TPaths;
function FindDirs(const  Path: TPath; const BasePath: TPath  = ''): TPaths;

function  Wild(const Pattern: TPath; const BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                ):TPaths; overload;
procedure Wild(Files: TStrings; const Pattern: TPath; const BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
          ); overload;


function IsMatch(const Path, Pattern: TPath):boolean; overload;
function IsMatch(const Paths: TPaths; const Patterns: TPatterns; p: Integer = 0; s: Integer = 0):boolean; overload;

function  PathExists(const Path: TPath):boolean;
function  PathIsDir(const Path: TPath):boolean;
function  PathIsFile(const Path: TPath):boolean;
function  SuperPath(const Path: TPath): TPath;


// file operations

procedure MakeDir(const Path: TPath);
procedure ChangeDir(const Path: TPath);
function  CurrentDir: TPath;

procedure CopyFile(const Src, Dst: TPath);
procedure CopyFiles(const Sources, Dests: TPaths);  overload;
procedure CopyFiles(const Files: TPaths; FromPath, ToPath: TPath);  overload;
procedure CopyFiles(const Pattern: TPattern; const FromPath, ToPath: TPath); overload;

procedure MoveFile(const Src, Dst: TPath);
procedure MoveFiles(const Sources, Dests: TPaths);  overload;
procedure MoveFiles(const Files: TPaths; const FromPath, ToPath: TPath);  overload;
procedure MoveFiles(const Pattern: TPattern; const FromPath, ToPath: TPath); overload;

procedure DeleteFile(const Path: TPath; DeleteReadOnly: boolean = false);
procedure DeleteFiles(const Files: TPaths; DeleteReadOnly: boolean = false);  overload;
procedure DeleteFiles(const Pattern: TPath; const BasePath: TPath= ''; DeleteReadOnly: boolean = false);  overload;

procedure TouchFile(const Path: TPath; When: TDateTime = 0); overload;
procedure TouchFile(const Path: TPath; When: string); overload;

function  FileAttributes(const Path: TPath):TFileAttributes;
procedure SetFileAttributes(const Path: TPath; const Attr: TFileAttributes);

function  FileTime(const Path: TPath): TDateTime;

function  SystemFileAttributes(const Path: TPath): Integer;
function  SystemFileTime(const Path: TPath): Longint;

function  TimeToSystemFileTime(const Time: TDateTime):Integer;
function  FileAttributesToSystemAttributes(const Attr: TFileAttributes):Byte;
function  SystemAttributesToFileAttributes(Attr: Integer) :TFileAttributes;


implementation


procedure Wild( Files: TStrings; const Patterns: TPatterns; const BasePath: TPath = '';
                Index: Integer = 0;
                IncludeAttr: TFileAttributes = AnyFileAttribute;
                ExcludeAttr: TFileAttributes = []
                );
  overload; forward;


function IsSystemIndependentPath(const Path: TPath): boolean;
begin
  Result := ( Pos(SystemPathDelimiter, Path) = 0 );
end;

function IsWindowsPath(const Path: TPath):boolean;
begin
  Result := Pos(':', Path) <> 0;
end;

function PathDrive(const Path: TPath): string;
var
  p: Integer;
begin
  if not IsWindowsPath(Path) then
    Result := ''
  else
  begin
    p := Pos(':', Path);
    Result := Copy(Path, p-1, 1);
  end;
end;

function PathServer(const Path: TPath): string;
var
  P: string;
begin
  if StrLeft(Path, 2) <> '//' then
    Result := ''
  else
  begin
    P := Copy(Path, 3, Length(Path));
    Result := StrToken(P, '/');
  end;
end;

function RemovePathDrive(Path: TPath):TPath;
var
  p: Integer;
begin
  Result := Path;
  p := Pos(':', Result);
  Delete(Result, 1, p);
end;

function RemovePathServer(Path: TPath):TPath;
var
  Server: string;
begin
  Result := Path;
  Server := PathServer(Path);
  if Server <> '' then
    Delete(Result, 1, 3 + Length(Server));
end;


procedure AssertIsSystemIndependentPath(const Path: TPath);
begin
  if Pos(SystemPathDelimiter, Path) <> 0 then
    raise EPathException.Create( Format( '"%s" looks like a system path. Expected a system independent one.',
                                  [Path])
                            );
end;

function PathConcat(const Path1, Path2: TPath): TPath;
var
  Parts: TPaths;
  i    : Integer;
  P1   : TPath;
  P2   : TPath;
begin
  AssertIsSystemIndependentPath(Path1);
  AssertIsSystemIndependentPath(Path2);

  Parts := nil;
  if (Length(Path1) = 0)
  //or (P1 = '.')
  or PathIsAbsolute(Path2) then
    Result := Path2
  else if Length(Path2) = 0 then
    Result := Path1
  else begin
    P1 := Path1;
    P2 := Path2;
    if P1[Length(P1)] = '/' then
      Delete(P1, Length(P1), 1);
    Result := P1;
    Parts := SplitPath(P2);
    for i := Low(Parts) to High(Parts) do
    begin
      if Parts[i] = '..' then
        Result := SuperPath(Result)
      else if Parts[i] = '.' then
        // do nothing
      else
        Result := Result + '/' + Parts[i];
    end;
  end;
end;

function ToPath(SystemPath: TSystemPath; const BasePath: TPath): TPath;
begin
  if Pos(InvalidPathChars, SystemPath) <> 0 then
    raise EPathException.Create('invalid path chars in ' + SystemPath)
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
      begin
        Result[1] := LowerCase(''+Result[1])[1];
        Result := SystemPathDelimiter + Result;
      end
      else
        raise EPathException.Create('invalid absolute path ' + SystemPath)
    end;
    Result := StringReplace(Result, SystemPathDelimiter, '/', [rfReplaceAll]);
  end;
end;

function ToSystemPath(const Path: TPath; const BasePath: TPath): string;
begin
   AssertIsSystemIndependentPath(Path);
   //!!!AssertIsSystemIndependentPath(BasePath);

   Result := MovePath(Path, '', BasePath);
   if (PathDrive(Result) <> '') and (StrLeft(Result, 1) = '/') then
     Delete(Result,1, 1);
   if (Length(Result) >= 1) and (Result[Length(Result)] = '/') then
     Delete(Result,Length(Result), 1);
   Result := StringReplace(Result, '/', SystemPathDelimiter, [rfReplaceAll]);
   if PathIsAbsolute(Path) then
     Result := ExpandFileName(Result);
end;

function ToSystemPaths(const Paths: TPaths; const BasePath: TPath = ''): TSystemPaths;
var
  i: Integer;
begin
  SetLength(Result, Length(Paths));
  for i := 0 to High(Result) do
    Result[i] := ToSystemPath(Paths[i], BasePath);
end;

procedure ToSystemPaths(Paths: TStrings; const BasePath: TPath = '');
var
  i: Integer;
begin
  for i := 0 to Paths.Count-1 do
    Paths[i] := ToSystemPath(Paths[i], BasePath);
end;



function ToPaths(OSPaths: TSystemPaths; const BasePath: TPath = ''): TPaths;
var
  i: Integer;
begin
  SetLength(Result, Length(OSPaths));
  for i := 0 to High(Result) do
    Result[i] := ToPath(OSPaths[i], BasePath);
end;


function FindPaths( const Path: TPath; const BasePath: TPath;
                    IncludeAttr: TFileAttributes;
                    ExcludeAttr: TFileAttributes
                    ):TPaths;
var
  S: TStringList;
  Search: TSearchRec;
  SearchResult: Integer;
begin
  AssertIsSystemIndependentPath(Path);

  S := TStringList.Create;
  S.Sorted := True;
  try
    SearchResult := FindFirst(ToSystemPath(Path, BasePath), faAnyFile, Search);
    try
      while SearchResult = 0 do
      begin
        if  ((SystemAttributesToFileAttributes(Search.Attr) * IncludeAttr) <> [])
        and ((SystemAttributesToFileAttributes(Search.Attr) * ExcludeAttr) =  [])
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
    FreeAndNil(S);
  end;
end;

function FindDirs(const Path: TPath; const BasePath: TPath): TPaths;
begin
   Result := FindPaths(Path, BasePath, [Directory]);
end;

function FindFiles(const Path: TPath; const BasePath: TPath): TPaths;
begin
   Result := FindPaths(Path, BasePath, AnyFileAttribute, [Directory]);
end;

function SplitPath(Path: TPath): TPaths;
var
  S   : TStrings;
  n,
  i   : Integer;
  Base: TPath;
begin
  AssertIsSystemIndependentPath(Path);

  S := TStringList.Create;
  try
    n := 0;
    if PathIsAbsolute(Path) then
    begin
      ForceRelativePath(Path, Base);
      SetLength(Result, 1);
      Result[0] := Base;
      n := 1;
    end;

    StrToStrings(Path, '/', S);

    SetLength(Result, n+S.Count);
    for i := 0 to S.Count-1 do
      Result[i+n] := S[i];
  finally
    FreeAndNil(S);
  end;
end;

function StringsToPaths(S: TStrings): TPaths;
var
  i: Integer;
begin
  SetLength(Result, S.Count);
  for i := 0 to S.Count-1 do
    Result[i] := S[i];
end;

function MovePath(const Path: TPath; const FromBase: TPath; const ToBase: TPath): TPath;
begin
  AssertIsSystemIndependentPath(Path);
  //!!!AssertIsSystemIndependentPath(FromBase);
  //!!!AssertIsSystemIndependentPath(ToBase);

  if (FromBase <> '') and (Pos(FromBase+'/', Path) = 1) then
    Result := PathConcat(ToBase, Copy(Path, 2+Length(FromBase), Length(Path)))
  else if PathIsAbsolute(Path) then
    Result :=  Path
  else
    Result := PathConcat(ToBase, Path);
end;


function MovePaths(const Paths: TPaths; const FromBase: TPath; const ToBase: TPath): TPaths;
var
  i: Integer;
begin
   SetLength(Result, Length(Paths));
   for i := Low(Paths) to High(Paths) do
     Result[i] := MovePath(Paths[i], FromBase, ToBase);
end;

function  ToRelativePath(const Path, BasePath: TPath):TPath;
var
  P, B  : TPaths;
  i, j: Integer;
begin
  AssertIsSystemIndependentPath(Path);
  AssertIsSystemIndependentPath(BasePath);

  P := nil;
  B := nil;
  if (PathDrive(Path) <> PathDrive(BasePath))
  then
    Result := Path
  else
  begin
    Result := '';
    P := SplitPath(Path);
    B := SplitPath(BasePath);
    i := 0;
    j := 0;
    while (i <= High(P))
    and   (j <= High(B))
    and   (P[i] = B[j])
    do begin
      Inc(i);
      Inc(j);
    end;

    if j > High(B) then
      Result := '.'
    else
      while  j <= High(B) do
      begin
        Result := Result + '../';
        Inc(j);
      end;
    while i <= High(P) do
    begin
      Result := PathConcat(Result, P[i]);
      Inc(i);
    end;
  end;

  if Result = '' then
    Result := '.';
end;

function  ToRelativePaths(const Paths: TPaths; const BasePath: TPath):TPaths;
begin
  Result := MovePaths(Paths, BasePath, '');
end;

function  ToRelativePaths(Paths: TStrings; const BasePath: TPath):TPaths;
var
  i :Integer;
begin
  for i := 0 to Paths.Count-1 do
    Paths[i] := ToRelativePath(Paths[i], BasePath);
end;

function  PathIsAbsolute(const Path: TPath): boolean;
begin
  AssertIsSystemIndependentPath(Path);

  Result :=    (Length(Path) > 0) and (Path[1] = '/')
             or (Length(Path) >= 3) and (Path[2] = ':') and (Path[3] = '/');
end;

procedure ForceRelativePath(var Path, BasePath: TPath);
var
  p: Integer;
begin
  AssertIsSystemIndependentPath(Path);
  AssertIsSystemIndependentPath(BasePath);

  if PathIsAbsolute(Path) then
  begin
    BasePath := '';
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

function Wild(const Pattern: TPath; const BasePath: TPath;
                    IncludeAttr: TFileAttributes;
                    ExcludeAttr: TFileAttributes
                    ):TPaths;
var
  Files: TStringList;
begin
  AssertIsSystemIndependentPath(Pattern);
  AssertIsSystemIndependentPath(BasePath);

  Files := TStringList.Create;
  try
    Files.Sorted := True;
    Wild(Files, Pattern, BasePath, IncludeAttr, ExcludeAttr);
    Result := StringsToPaths(Files);
  finally
    FreeAndNil(Files);
  end;
end;

procedure Wild(Files: TStrings; const Pattern: TPath; const BasePath: TPath;
               IncludeAttr: TFileAttributes;
               ExcludeAttr: TFileAttributes
               );
var
  Pats: string;
  Pat : TPath;
begin
  AssertIsSystemIndependentPath(Pattern);
  AssertIsSystemIndependentPath(BasePath);

  Pats := Pattern;
  Pat  := StrToken(Pats, ',');
  while Pat <> '' do
  begin
    //ForceRelativePath(Pattern, BasePath);
    if PathIsAbsolute(Pattern) then
      Wild(Files, SplitPath(Pat), '',       0, IncludeAttr, ExcludeAttr)
    else
      Wild(Files, SplitPath(Pat), BasePath, 0, IncludeAttr, ExcludeAttr);
    Pat  := StrToken(Pats, ',');
  end;
end;

procedure Wild( Files: TStrings; const Patterns: TPatterns; const BasePath: TPath;
                Index: Integer;
                IncludeAttr: TFileAttributes;
                ExcludeAttr: TFileAttributes
                );
var
  i      : Integer;
  Matches: TPaths;
  NewBase: TPath;
begin
  AssertIsSystemIndependentPath(BasePath);

  if Index > High(Patterns) then
    EXIT;

  NewBase := BasePath;
  // absorb all non-wildcard patterns
  while (Index < High(Patterns))
  and ((LastDelimiter(WildChars, Patterns[Index]) = 0)
       or (Patterns[Index] = '.')
      )
  do
  begin
    NewBase := PathConcat(NewBase, Patterns[Index]);
    Inc(Index);
  end;
  Assert(Index <= High(Patterns));
  if Index = High(Patterns) then
  begin // add files (works for '**' too)
    Matches := FindPaths(Patterns[Index], NewBase, IncludeAttr, ExcludeAttr);
    for i := Low(Matches) to High(Matches) do
      Files.Add(PathConcat(NewBase, Matches[i]))
  end;
  Matches := nil;
  // handle wildcards
  if Patterns[Index] = '**' then
  begin // match anything and recurse
    Wild(Files, Patterns, NewBase, Index+1, IncludeAttr, ExcludeAttr);
    Matches := FindDirs('*', NewBase);
    // use same Index ('**') to recurse
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Patterns, PathConcat(NewBase, Matches[i]), Index, IncludeAttr, ExcludeAttr);
  end
  else if Index < High(Patterns) then
  begin // match directories
    Matches := FindDirs(Patterns[Index], NewBase);
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Patterns, PathConcat(NewBase, Matches[i]), Index+1, IncludeAttr, ExcludeAttr);
  end;
end;

function Matches(A, B: TPath; i: Integer = 1; j: Integer = 1):boolean;
begin
  while (i <= Length(A))
  and   (j <= Length(B))
  and   (UpCase(A[i]) = UpCase(B[j])) do
  begin
    Inc(i);
    Inc(j);
  end;
  if B = '.' then
    Result := True
  else if j > Length(B) then
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

function IsMatch(const Paths: TPaths; const Patterns: TPatterns; p: Integer = 0; s: Integer = 0):boolean;

begin
  while (p <= High(Paths))
  and   (s <= High(Patterns))
  and   (Patterns[s] <> '**')
  and   Matches(Paths[p], Patterns[s]) do
  begin
    Inc(p);
    Inc(s);
  end;
  if s > High(Patterns) then
    Result := p > High(Paths)
  else if p > High(Paths) then
    Result := False
  else if Patterns[s] = '**' then
    Result :=    IsMatch(Paths, Patterns, p+1, s+1)
              or IsMatch(Paths, Patterns, p,   s+1)
              or IsMatch(Paths, Patterns, p+1, s)
  else
    Result := False;
end;

function IsMatch(const Path, Pattern: TPath):boolean;
begin
  AssertIsSystemIndependentPath(Path);
  AssertIsSystemIndependentPath(Pattern);

  Result := IsMatch(SplitPath(Path), SplitPath(Pattern));
end;

function  PathExists(const Path: TPath):boolean;
begin
  AssertIsSystemIndependentPath(Path);

  Result := Length(FindPaths(Path)) >= 1;
end;

function  PathIsDir(const Path: TPath):boolean;
begin
  AssertIsSystemIndependentPath(Path);

  Result := (FileAttributes(Path) * [NoFile, Directory]) = [Directory];
end;

function  PathIsFile(const Path: TPath):boolean;
begin
  AssertIsSystemIndependentPath(Path);
  Result := (FileAttributes(Path) * [NoFile, Directory]) = [];
end;

function  SuperPath(const Path: TPath): TPath;
var
  p   : Integer;
begin
  AssertIsSystemIndependentPath(Path);

  if (Path = '.') or (Path = '') then
    Result := '..'
  else
  begin
    Result := Path;
    p := LastDelimiter('/', Result);
    if p = Length(Path) then
    begin
      Result := Copy(Path, 1, p-1);
      p := LastDelimiter('/', Result);
    end;
    Result := Copy(Path, 1, p-1);
  end;
end;



// file operations


procedure MakeDir(const Path: TPath);
begin
  if (Length(Path) > 0)
  and (Path[Length(Path)] <> ':')  // Oops! Windows specific!
  and not PathIsDir(Path) then
  begin
    MakeDir(SuperPath(Path));
    SysUtils.CreateDir(ToSystemPath(Path));
    if not PathIsDir(Path) then
      raise EFileOpException.Create(Format('Could not create directory "%s"', [Path]));
  end;
end;

procedure ChangeDir(const Path: TPath);
begin
  if (Path <> '') and (Path <> CurrentDir) then
    SetCurrentDir(ToSystemPath(Path));
end;

function  CurrentDir: TPath;
begin
  Result := ToPath(SysUtils.GetCurrentDir);
end;


procedure CopyFile(const Src, Dst: TPath);
begin
   MakeDir(SuperPath(Dst));
   if PathIsDir(Src) then
     MakeDir(Dst)
   else if not Windows.CopyFile( PChar(ToSystemPath(Src)),
                                 PChar(ToSystemPath(Dst)),
                                 False)
     then
       raise EFileOpException.Create(SysErrorMessage(GetLastError));
end;

procedure CopyFiles(const Pattern: TPattern; const FromPath, ToPath: TPath);
begin
  CopyFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure CopyFiles(const Files: TPaths; FromPath, ToPath: TPath);
begin
  CopyFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure CopyFiles(const Sources, Dests: TPaths);
var
  f  : Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    CopyFile(Sources[f], Dests[f]);
  end;
end;

procedure MoveFile(const Src, Dst: TPath);
begin
  if PathIsDir(Src) then
  begin
    raise EFileOpException.Create(Format('Don''t know how to move dir "%s" to "%s"', [Src, Dst]));
  end;

  (* MoveFileEx not implemented on Win9x. Some help files say it is, but see
     this link for current doc:
       http://msdn.microsoft.com/library/psdk/winbase/filesio_9oe0.htm

     In addition, MOVEFILE_REPLACE_EXISTING does not work with read only files
  if not Windows.MoveFileEx(PChar(ToSystemPath(Src)), PChar(ToSystemPath(Dst)),
                            MOVEFILE_COPY_ALLOWED or
                            MOVEFILE_REPLACE_EXISTING or
                            MOVEFILE_WRITE_THROUGH)  -- Chrismo *)

  WildPaths.CopyFile(Src, Dst);
  WildPaths.DeleteFile(Src);
end;

procedure MoveFiles(const Pattern: TPattern; const FromPath, ToPath: TPath);
begin
  MoveFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure MoveFiles(const Files: TPaths; const FromPath, ToPath: TPath);
begin
  MoveFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure MoveFiles(const Sources, Dests: TPaths);
var
  f  : Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    MoveFile(Sources[f], Dests[f]);
  end;
end;

procedure DeleteFile(const Path: TPath; DeleteReadOnly: boolean = false);
var
  SysPath: string;
  FileAttr: Integer;
begin
  SysPath := ToSystemPath(Path);
  if not PathIsDir(Path) then
  begin
    if DeleteReadOnly then
    begin
      { take off read only attribute if it exists }
      FileAttr := SysUtils.FileGetAttr(SysPath);
      FileAttr := FileAttr and (not faReadOnly);
      SysUtils.FileSetAttr(SysPath, FileAttr);
    end;
    SysUtils.DeleteFile(ToSystemPath(Path))
  end
  else
    SysUtils.RemoveDir(ToSystemPath(Path));
end;

procedure DeleteFiles(const Pattern: TPath; const BasePath: TPath; DeleteReadOnly :boolean);
begin
  DeleteFiles(Wild(Pattern, BasePath), DeleteReadOnly);
end;

procedure DeleteFiles(const Files: TPaths; DeleteReadOnly: boolean = false);
var
  f: Integer;
begin
  for f := Low(Files) to High(Files) do
    if not PathIsDir(Files[f]) then
      DeleteFile(Files[f], DeleteReadOnly);
  for f := Low(Files) to High(Files) do
    if PathIsDir(Files[f]) then
      DeleteFile(Files[f], DeleteReadOnly);
end;

procedure TouchFile(const Path: TPath; When: string);
begin
  //!!! StrToDateTime changes with locale and platform!!
  TouchFile(Path, StrToDateTime(When));
end;


procedure TouchFile(const Path: TPath; When: TDateTime);
var
  Handle: Integer;
begin
   if When = 0 then
     When := Now;

   MakeDir(SuperPath(Path));
   
   Handle := FileOpen(ToSystemPath(Path), fmOpenWrite or fmShareDenyNone);
   if Handle < 0 then
     Handle := FileCreate(ToSystemPath(Path));
   try
     FileSetDate(Handle, DateTimeToFileDate(When))
   finally
     FileClose(Handle);
   end;
end;

function FileAttributes(const Path: TPath):TFileAttributes;
var
  Attr :Integer;
begin
  Attr := SystemFileAttributes(Path);
  if Attr < 0 then
    Result := [NoFile]
  else
    Result := TFileAttributes(Byte(Attr));
end;

procedure SetFileAttributes(const Path: TPath; const Attr: TFileAttributes);
begin
  SysUtils.FileSetAttr(ToSystemPath(Path), FileAttributesToSystemAttributes(Attr));
end;

function  FileTime(const Path: TPath): TDateTime;
var
  SystemTime: Longint;
begin
  SystemTime := SystemFileTime(Path);
  if SystemTime <= 0 then
    Result := 0
  else
    Result := FileDateToDateTime(SystemTime);
end;

function  SystemFileAttributes(const Path: TPath): Integer;
begin
  Result := Byte(SysUtils.FileGetAttr(ToSystemPath(Path)));
end;

function  SystemFileTime(const Path: TPath)    : Longint;
begin
  Result := SysUtils.FileAge(ToSystemPath(Path));
  if Result < 0 then
    Result := 0;
end;

function  TimeToSystemFileTime(const Time: TDateTime):Integer;
begin
  Result := DateTimeToFileDate(Time);
end;

function  FileAttributesToSystemAttributes(const Attr: TFileAttributes):Byte;
begin
  Result := Byte(Attr);
end;

function  SystemAttributesToFileAttributes(Attr: Integer) :TFileAttributes;
begin
  Result := TFileAttributes(Byte(Attr));
end;


end.
