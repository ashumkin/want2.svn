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
  WildPaths,

  SysUtils,
  Math;

procedure MakeDir(Path :TPath);

procedure CopyFile(Src, Dst :TPath);
procedure CopyFiles(const Sources, Dests :TPaths);  overload;
procedure CopyFiles(const Files :TPaths; FromPath, ToPath :TPath);  overload;
procedure CopyFiles(Spec :TSpec; FromPath, ToPath :TPath); overload;

procedure MoveFile(Src, Dst :TPath);
procedure MoveFiles(const Sources, Dests :TPaths);  overload;
procedure MoveFiles(const Files :TPaths; FromPath, ToPath :TPath);  overload;
procedure MoveFiles(Spec :TSpec; FromPath, ToPath :TPath); overload;

procedure DeleteFile(Path :TPath);
procedure DeleteFiles(const Files :TPaths);  overload;
procedure DeleteFiles(Spec :TPath; BasePath :TPath= '');  overload;

procedure TouchFile(Path :TPath; When :TDateTime = 0); overload;
procedure TouchFile(Path :TPath; When :string); overload;

implementation

procedure MakeDir(Path :TPath);
begin
  if (Length(Path) > 0)
  and (Path[Length(Path)] <> ':')  // Oops! Windows specific!
  and not IsDir(Path) then
  begin
    MakeDir(SuperPath(Path));
    writeln('mkdir ',ToSystemPath(Path));
  end;
end;

procedure CopyFile(Src, Dst :TPath);
begin
   MakeDir(SuperPath(Dst));
   writeln('copy ',ToSystemPath(Src), '->', ToSystemPath(Dst));
end;

procedure CopyFiles(Spec :TSpec; FromPath, ToPath :TPath);
begin
  CopyFiles(Wild(Spec, FromPath), FromPath, ToPath);
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

procedure MoveFiles(Spec :TSpec; FromPath, ToPath :TPath);
begin
  MoveFiles(Wild(Spec, FromPath), FromPath, ToPath);
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
    writeln('del ' + ToSystemPath(Path))
  else
    writeln('rmdir ' + ToSystemPath(Path));
end;

procedure DeleteFiles(Spec :TPath; BasePath :TPath);
begin
  DeleteFiles(Wild(Spec, BasePath));
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
  //!!! StrToDateTime changes with Local and platform!!
  TouchFile(Path, StrToDateTime(When));
end;


procedure TouchFile(Path :TPath; When :TDateTime);
var
  Handle :Integer;
begin
   Handle := FileOpen(ToSystemPath(Path), fmOpenWrite or fmShareDenyNone);
   if Handle < 0 then
     Handle := FileCreate(ToSystemPath(Path));
   try
     if When <> 0 then
       FileSetDate(Handle, DateTimeToFileDate(When))
     else
       FileSetDate(Handle, DateTimeToFileDate(Now))
   finally
     FileClose(Handle);
   end;
end;



end.
