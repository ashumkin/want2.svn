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
unit ZipStreams;

interface
uses
  SysUtils,
  Classes,
  ZipUtils,
  ZIP;

type
  EZipFileException = class(Exception);
  EZipFileError = class(EZipFileException);

  TCompressionLevel = (
     zlDefault,
     zlNone,
     zlBestSpeed,
     zlBestCompression
   );


  TZipStream = class(TStream)
  protected
    FZipFileName :string;
    FEntryName   :string;
    FEntryOpen   :boolean;
    FComment     :string;

    FCompression :TCompressionLevel;

    FZipFile     :ZipUtils.ZipFile;

    FPaths       :TStringList;

    procedure Error(Msg :string);
    procedure NotImplementedError(Msg :string);
  public
    constructor Create(const ZipFileName :string);
    destructor  Destroy; override;

    procedure NewEntry(const EntryName :string; const Comment :string = ''; Age :Integer = 0);            overload;
    procedure NewEntry(const EntryName :string; Age:Integer; const Comment :string = ''); overload;

    procedure CloseEntry;

    function Read(var Buffer; Count: Longint): Longint;    override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure WriteStream(const EntryName :string; Stream :TStream; const Comment :string = ''; Age :Integer = 0);   overload;
    procedure WriteStream(const EntryName :string; Stream :TStream; Age :Integer; const Comment :string = '');       overload;
    procedure WriteFile(const FileName :string; const Comment :string = '');

  public
    property ZipFileName      :string  read FZipFileName write FZipFileName;
    property EntryName        :string  read FEntryName   write FEntryName;
    property EntryOpen        :boolean read FEntryOpen   write FEntryOpen;
    property Comment          :string  read FComment     write FComment;

    property CompressionLevel :TCompressionLevel read FCompression write FCompression;
  end;

implementation
uses
  ZLIB;

const
  CompressionMap : array[TCompressionLevel] of Integer = (
   Z_DEFAULT_COMPRESSION,
   Z_NO_COMPRESSION,
   Z_BEST_SPEED,
   Z_BEST_COMPRESSION
   );

{ TZipStream }

constructor TZipStream.Create(const ZipFileName: string);
begin
  inherited Create;
  FPaths := TStringList.Create;
  FPaths.Sorted := True;
  FPaths.Duplicates := dupIgnore;

  FZipFileName := ZipFileName;

  FZipFile  := ZIP.ZipOpen(PChar(ZipFileName), 0 {DO NOT APPEND});
  if FZipFile = nil then
     Error(Format('Could not open zip file "%s"', [ZipFileName]));
end;

destructor TZipStream.Destroy;
var
  Err :Integer;
begin
  FPaths.Free;
  if EntryOpen then
     CloseEntry;
  Err := ZipClose(FZipFile, PChar(Comment));
  inherited Destroy;
  if Err <> ZIP_OK then
    Error('Could not close zip file');
end;

procedure TZipStream.Error(Msg: string);
begin
  raise EZipFileError.Create(Msg);
end;

procedure TZipStream.NotImplementedError(Msg: string);
begin
  Error(Format('"%s" not implemented in %s', [Msg, ClassName]));
end;

procedure TZipStream.NewEntry(const EntryName: string; Age: Integer; const Comment: string);
begin
  NewEntry(EntryName, Comment, Age);
end;


procedure TZipStream.NewEntry(const EntryName, Comment: string; Age: Integer);
var
  Err         :Integer;
  ZipFileInfo :zip_fileinfo;
begin
  FEntryName := EntryName;

  FillChar(ZipFileInfo, SizeOf(ZipFileInfo), 0);
  ZipFileInfo.dosDate := Age;

  Err := zipOpenNewFileInZip( FZipFile,
                              PChar(EntryName),
                              @ZipFileInfo,
                              NIL,             { const extrafield_local : voidp; }
                              0,               { size_extrafield_local : uInt; }
                              NIL,             { const extrafield_global : voidp; }
                              0,               { size_extrafield_global : uInt; }
                              PChar(Comment),  { const comment : PChar;}
                              Z_DEFLATED,
                              CompressionMap[CompressionLevel]);
  if Err <> ZIP_OK then
    Error(Format('Error creating zip file entry "%s"', [EntryName]));

  FEntryOpen := True;
end;

procedure TZipStream.CloseEntry;
var
  Err :Integer;
begin
  FEntryOpen := False;
  Err := ZipCloseFileInZip (FZipFile);
  if Err <> ZIP_OK then
    Error('Could not close zip file entry');
end;

function TZipStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := -1;
  NotImplementedError('Read');
end;

function TZipStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := -1;
  NotImplementedError('Seek');
end;

function TZipStream.Write(const Buffer; Count: Integer): Longint;
var
  Err         :Integer;
begin
  Result := -1;
  if not EntryOpen then
    Error('Need to open a zip entry first')
  else
  begin
    Err := zipWriteInFileInZip (FZipFile, @Buffer, Count);
    if Err < 0 then
      Error('Could not write to zip file entry');
    Result := Count;
  end;
end;

procedure TZipStream.WriteStream(const EntryName :string; Stream :TStream; Age :Integer; const Comment: string);
begin
  WriteStream(EntryName, Stream, Comment, Age);
end;

procedure TZipStream.WriteStream(const EntryName :string; Stream :TStream; const Comment: string; Age :Integer);
begin
  NewEntry(EntryName, Comment, Age);
  try
    Self.CopyFrom(Stream, Stream.Size);
  finally
    CloseEntry;
  end;
end;


procedure TZipStream.WriteFile(const FileName: string; const Comment :string);
var
  Stream :TFileStream;
  Path   :string;
begin
  Path := ExtractFileDir(FileName);
  if FPaths.IndexOf(Path) < 0 then
  begin
    try
      {:TODO Find a way to add directory entries to .ZIP files }
      FPaths.Add(Path);
    finally
    end;
  end;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    WriteStream(FileName, Stream, Comment, FileAge(FileName));
  finally
    Stream.Free;
  end;
end;




end.
