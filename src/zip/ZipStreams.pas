{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

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
}
unit ZipStreams;

interface
uses
  SysUtils,
  Classes,
  ZipUtils,
  ZIP,
  UNZIP,
  WildPaths;

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
  private
  protected
    FZipFileName :TPath;
    FEntryName   :TPath;
    FEntryOpen   :boolean;
    FComment     :string;

    FCompress    :boolean;
    FCompression :TCompressionLevel;

    FZipFile     :ZipUtils.ZipFile;

    FPaths       :TStringList;

    procedure Error(Msg :string);
    procedure NotImplementedError(Msg :string);

    function CheckFileTime(const FileName :TPath; Time :TDateTime = 0) :TDateTime;

  public
    constructor Create(const ZipFileName :TPath);
    destructor  Destroy; override;

    function Read(var Buffer; Count: Longint): Longint;    override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure NewEntry(  EntryName    :TPath;
                         Attributes   :TFileAttributes;
                         Time         :TDateTime;
                         Compress     :boolean;
                         Comment      :string = ''); overload;
    procedure CloseEntry;

    procedure WriteDirEntry( DirName :TPath; Time:TDateTime = 0;  Comment :string = '');

    procedure WriteStream( EntryName  :TPath;
                           Stream     :TStream;
                           Attributes :TFileAttributes;
                           Time       :TDateTime;
                           Comment    :string = ''); overload;

    procedure WriteFile( FileName :TPath;  Comment :string = '');

  public
    property ZipFileName      :TPath    read FZipFileName write FZipFileName;
    property EntryName        :TPath    read FEntryName   write FEntryName;
    property EntryOpen        :boolean  read FEntryOpen   write FEntryOpen;
    property Comment          :string   read FComment     write FComment;

    property Compress         :boolean  read FCompress    write FCompress default true;
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

constructor TZipStream.Create(const ZipFileName: TPath);
begin
  inherited Create;
  FPaths := TStringList.Create;
  FPaths.Sorted := True;
  FPaths.Duplicates := dupIgnore;

  FCompress := True;

  FZipFileName := ZipFileName;

  FZipFile  := ZIP.ZipOpen(PChar(ToSystemPath(ZipFileName)), 0 {DO NOT APPEND});
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


function TZipStream.CheckFileTime(const FileName :TPath; Time :TDateTime = 0) :TDateTime;
begin
  if Time <= 0 then
    Result := FileTime(FileName)
  else
    Result := Time;

  if Time <= 0 then
    Result := Now;
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



procedure TZipStream.NewEntry(  EntryName    :TPath;
                                Attributes   :TFileAttributes;
                                Time         :TDateTime;
                                Compress     :boolean;
                                Comment :string = '');
var
  Err            :Integer;
  ZipFileInfo    :zip_fileinfo;
  TrashBase      :TPath;
  CompressMethod :Integer;
begin
  FEntryName := EntryName;

  FillChar(ZipFileInfo, SizeOf(ZipFileInfo), 0);

  ZipFileInfo.external_fa := FileAttributesToSystemAttributes(Attributes);
  ZipFileInfo.dosDate     := TimeToSystemFileTime(Time);

  WildPaths.ForceRelativePath(EntryName, TrashBase);
  // Yank Base!!

  if Compress then
    CompressMethod := Z_DEFLATED
  else
    CompressMethod := 0 {Z_STORED};

  Err := zipOpenNewFileInZip( FZipFile,
                              PChar(EntryName),
                              @ZipFileInfo,
                              NIL,             { const extrafield_local : voidp; }
                              0,               { size_extrafield_local : uInt; }
                              NIL,             { const extrafield_global : voidp; }
                              0,               { size_extrafield_global : uInt; }
                              PChar(Comment),  { const comment : PChar;}
                              CompressMethod,
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




procedure TZipStream.WriteStream( EntryName  :TPath;
                                  Stream     :TStream;
                                  Attributes   :TFileAttributes;
                                  Time         :TDateTime;
                                  Comment    :string );
begin
  NewEntry(EntryName, Attributes, Time, FCompress, Comment);
  try
    Self.CopyFrom(Stream, Stream.Size);
  finally
    CloseEntry;
  end;
end;


procedure TZipStream.WriteFile( FileName: TPath;  Comment :string);
var
  Stream  :TFileStream;
  FileDir :TPath;
begin
  if IsDir(FileName) then
    WriteDirEntry(FileName, CheckFileTime(FileName), Comment)
  else
  begin
    FileDir := SuperPath(FileName);
    if IsDir(FileDir) then
       WriteDirEntry(FileDir);
    Stream := TFileStream.Create(ToSystemPath(FileName), fmOpenRead or fmShareDenyWrite);
    try
      WriteStream( FileName, Stream,
                             FileAttributes(FileName),
                             CheckFileTime(FileName),
                             Comment);
    finally
      Stream.Free;
    end;
  end;
end;




procedure TZipStream.WriteDirEntry( DirName: TPath; Time:TDateTime; Comment :string);
begin
  if (Length(DirName) > 0) and (FPaths.IndexOf(DirName) < 0) then
  begin
    NewEntry( DirName+'/',
              [Directory] + FileAttributes(DirName),
              CheckFileTime(DirName, Time),
              False, { do not compress }
              Comment);
    try
      FPaths.Add(DirName);
    finally
      CloseEntry;
    end;
  end;
end;



end.
