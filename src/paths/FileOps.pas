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
unit FileOps;

interface
uses
  Windows,
  SysUtils,
  Math,

  JclBase,

  WildPaths;

type
  EFileOpException = class(Exception);

  TFileAttribute = (
    ReadOnly,  {= $00000001 }
    Hidden,    {= $00000002 }
    SysFile,   {= $00000004 }
    VolumeID,  {= $00000008 }
    Directory, {= $00000010 }
    Archive    {= $00000020 }
  );
  TFileAttributes = set of TFileAttribute;

const
  AnyFileAttribute = [ReadOnly..Archive];

procedure MakeDir(Path :TPath);
procedure ChangeDir(Path :TPath);
function  CurrentDir :TPath;

procedure CopyFile(Src, Dst :TPath);
procedure CopyFiles(const Sources, Dests :TPaths);  overload;
procedure CopyFiles(const Files :TPaths; FromPath, ToPath :TPath);  overload;
procedure CopyFiles(Pattern :TPattern; FromPath, ToPath :TPath); overload;

procedure MoveFile(Src, Dst :TPath);
procedure MoveFiles(const Sources, Dests :TPaths);  overload;
procedure MoveFiles(const Files :TPaths; FromPath, ToPath :TPath);  overload;
procedure MoveFiles(Pattern :TPattern; FromPath, ToPath :TPath); overload;

procedure DeleteFile(Path :TPath);
procedure DeleteFiles(const Files :TPaths);  overload;
procedure DeleteFiles(Pattern :TPath; BasePath :TPath= '');  overload;

procedure TouchFile(Path :TPath; When :TDateTime = 0); overload;
procedure TouchFile(Path :TPath; When :string); overload;

function  FileAttributes(Path :TPath):TFileAttributes;
function  FileTime(Path :TPath) :TDateTime;

function  SystemFileAttributes(Path :TPath) :Byte;
function  SystemFileTime(Path :TPath) :Longint;

function  TimeToSystemFileTime(const Time :TDateTime):Integer;
function  FileAttributesToSystemAttributes(const Attr :TFileAttributes):Byte;

implementation

procedure MakeDir(Path :TPath);
begin
  if (Length(Path) > 0)
  and (Path[Length(Path)] <> ':')  // Oops! Windows specific!
  and not IsDir(Path) then
  begin
    MakeDir(SuperPath(Path));
    SysUtils.CreateDir(ToSystemPath(Path));
  end;
end;

procedure ChangeDir(Path :TPath);
begin
  SetCurrentDir(ToSystemPath(Path));
end;

function  CurrentDir :TPath;
begin
  Result := ToPath(SysUtils.GetCurrentDir);
end;


procedure CopyFile(Src, Dst :TPath);
begin
   MakeDir(SuperPath(Dst));
   if not Windows.CopyFile( PChar(ToSystemPath(Src)),
                            PChar(ToSystemPath(Dst)),
                            False)
   then
     raise EFileOpException.Create(SysErrorMessage(GetLastError));
end;

procedure CopyFiles(Pattern :TPattern; FromPath, ToPath :TPath);
begin
  CopyFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure CopyFiles(const Files :TPaths; FromPath, ToPath :TPath);
begin
  CopyFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure CopyFiles(const Sources, Dests :TPaths);
var
  f   :Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    CopyFile(Sources[f], Dests[f]);
  end;
end;

procedure MoveFile(Src, Dst :TPath);
begin
   MakeDir(SuperPath(Dst));
   writeln('move ',ToSystemPath(Src), '->', ToSystemPath(Dst));
end;

procedure MoveFiles(Pattern :TPattern; FromPath, ToPath :TPath);
begin
  MoveFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure MoveFiles(const Files :TPaths; FromPath, ToPath :TPath);
begin
  MoveFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure MoveFiles(const Sources, Dests :TPaths);
var
  f   :Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    MoveFile(Sources[f], Dests[f]);
  end;
end;

procedure DeleteFile(Path :TPath);
begin
  if not IsDir(Path) then
    SysUtils.DeleteFile(ToSystemPath(Path))
  else
    SysUtils.RemoveDir(ToSystemPath(Path));
end;

procedure DeleteFiles(Pattern :TPath; BasePath :TPath);
begin
  DeleteFiles(Wild(Pattern, BasePath));
end;

procedure DeleteFiles(const Files :TPaths);
var
  f :Integer;
begin
  for f := Low(Files) to High(Files) do
    if not IsDir(Files[f]) then
      DeleteFile(Files[f]);
  for f := Low(Files) to High(Files) do
    if IsDir(Files[f]) then
      DeleteFile(Files[f]);
end;

procedure TouchFile(Path :TPath; When :string);
begin
  //!!! StrToDateTime changes with locale and platform!!
  TouchFile(Path, StrToDateTime(When));
end;


procedure TouchFile(Path :TPath; When :TDateTime);
var
  Handle :Integer;
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

function FileAttributes(Path :TPath):TFileAttributes;
begin
  Result := TFileAttributes(SystemFileAttributes(ToSystemPath(Path)));
end;

function  FileTime(Path :TPath) :TDateTime;
var
  SystemTime :Longint;
begin
  SystemTime := SystemFileTime(Path);
  if SystemTime <= 0 then
    Result := 0
  else
    Result := FileDateToDateTime(SystemTime);
end;

function  SystemFileAttributes(Path :TPath) :Byte;
begin
  Result := Byte(SysUtils.FileGetAttr(ToSystemPath(Path)));
end;

function  SystemFileTime(Path :TPath)     :Longint;
begin
  Result := SysUtils.FileAge(ToSystemPath(Path));
  if Result < 0 then
    Result := 0;
end;

function  TimeToSystemFileTime(const Time :TDateTime):Integer;
begin
  Result := DateTimeToFileDate(Time);
end;

function  FileAttributesToSystemAttributes(const Attr :TFileAttributes):Byte;
begin
  Result := Byte(Attr);
end;

end.
