unit LogMgr;

{-------------------------------------------------------------------------}
{  Copyright 1999-2001 by Mike Taylor                                     }
{-------------------------------------------------------------------------}
{                                                                         }
{  LogMgr is a self-contained unit that allows for the                    }
{  creation, use and handling of program logging.                         }
{                                                                         }
{  Logging can be directed to a text file and/or the screen.              }
{                                                                         }
{  Changes:                                                               }
{                                                                         }
{    - added line wrapping logic                                          }
{        credit to: Juancarlo Añez, Caracas, Venezuela                    }
{                   Dante Project                                         }
{    - changed log levels to match Dante Project verbosity levels         }
{    - added Log overrides to match Dante Project needs                   }
{    - added echo to file code                                            }
{    - added shortname property                                           }
{    - added prefix parameter                                             }
{                                                                         }
{-------------------------------------------------------------------------}

interface

type
  TLogLevel = ( vlErrors,
                vlWarnings,
                vlNormal,
                vlVerbose,
                vlDebug     );
const
  vlVeryQuiet = vlErrors;
  vlQuiet     = vlWarnings;

  LogLevelText: array[TLogLevel] of String = ( 'Errors',
                                               'Warnings',
                                               'Normal',
                                               'Verbose',
                                               'Debug'       );

type
  TLogMethod = procedure(Tag, Msg :string; Level :TLogLevel) of object;

  TLogManager = class
    private
      FIncludeDate:      Boolean;
      FIncludeTime:      Boolean;
      FIncludeShortName: Boolean;
      FHeaderPresent:    Boolean;


      FLogLevel: TLogLevel;

      FRightMargin: Integer;

      FShortName: String;
      FLogHeader: String;

    protected
      procedure   SetIncludeDate(const aValue: Boolean);
      procedure   SetIncludeTime(const aValue: Boolean);

      procedure   BuildHeader;

      procedure   OutputLog(   const aLine: String; const aFlush: Boolean;   const aLevel: TLogLevel);  virtual;
      procedure   OutputPrefix(const aPrefix: String; const aFlush: Boolean; const aLevel: TLogLevel);  virtual;

    public
      constructor Create;

      procedure   LogMsg(const aLevel: TLogLevel; const aMsg: String; const aPrefix: String); virtual;

      procedure   Log(const aMsg: String = ''; const aLevel: TLogLevel = vlNormal); overload; virtual;
      procedure   Log(const aLevel: TLogLevel; const aMsg: String = ''); overload;
      procedure   Log(const aPrefix: String; const aMsg: String; const aLevel: TLogLevel = vlNormal); overload; virtual;

      property    Level:    TLogLevel read FLogLevel write FLogLevel;

      property    ShortName: String read FShortName write FShortName;
      property    LogHeader: String read FLogHeader write FLogHeader;

      property    IncludeDate:      Boolean read FIncludeDate      write SetIncludeDate;
      property    IncludeTime:      Boolean read FIncludeTime      write SetIncludeTime;
      property    IncludeShortName: Boolean read FIncludeShortName write FIncludeShortName;

      property    RightMargin: Integer read FRightMargin write FRightMargin;
  end;

  TNullLogManager = class(TLogManager);

  TFileLogManager = class(TLogManager)
  protected
    FActive:           Boolean;
    FFileOpen:         Boolean;
    FAlwaysFlush:      Boolean;
    FLogHandle: TextFile;
    FFileName:  String;

    procedure   OutputLog(   const aLine: String; const aFlush: Boolean;   const aLevel: TLogLevel);  override;
    procedure   OutputPrefix(const aPrefix: String; const aFlush: Boolean; const aLevel: TLogLevel);  override;

    procedure   OpenLogFile;
    procedure   CloseLogFile;
  public
    destructor  Destroy; override;
    procedure   Start; virtual;
    procedure   Stop;  virtual;

    procedure   LogMsg(const aLevel: TLogLevel; const aMsg: String; const aPrefix: String); override;

    property    Active:   Boolean read FActive;
    property    FileOpen: Boolean read FFileOpen;
    property    AlwaysFlush: Boolean read FAlwaysFlush write FAlwaysFlush;
    property    FileName:  String read FFileName  write FFileName;
  end;


implementation

uses
  Classes,
  SysUtils,
  JclStrings;

  { TLogManager }

constructor TLogManager.Create;
begin
  inherited Create;
  FLogLevel := vlNormal;

  FRightMargin := 78;
end;

procedure TLogManager.BuildHeader;
begin
  FLogHeader := '';

  if IncludeDate then
    FLogHeader := 'yyyymmdd';

  if IncludeTime then
    FLogHeader := FLogHeader + 'hhmmss';

  if Length(FLogHeader) > 0 then
    FLogHeader := FLogHeader + ' ';

  FHeaderPresent := Length(FLogHeader) > 0;
end;

procedure TLogManager.SetIncludeDate(const aValue: Boolean);
begin
  FIncludeDate := aValue;

  BuildHeader;
end;

procedure TLogManager.SetIncludeTime(const aValue: Boolean);
begin
  FIncludeTime := aValue;

  BuildHeader;
end;

procedure TLogManager.OutputLog(const aLine: String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  // do nothing
end;

procedure TLogManager.OutputPrefix(const aPrefix : String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  // do nothing
end;

procedure TLogManager.LogMsg(const aLevel: TLogLevel; const aMsg: String; const aPrefix: String);
var
  Header: String;
  Lines:  TStringList;
  Fragments:TStringList;
  i,n:    Integer;
  Msg:    String;
begin
  if (Level >= aLevel) then
    begin
      Header := '';

      if FHeaderPresent then
        Header := FormatDateTime(FLogHeader, Now);

      if IncludeShortName then
        if FHeaderPresent then
          Header := Header + ' ' + ShortName + ' '
        else
          Header := ShortName + ' ';

      if Length(aPrefix) > 0 then
        Header := Header + aPrefix;

      Lines := TStringList.Create;
      Fragments := TStringList.Create;
      try
        Msg := aMsg + ' ';
        Lines.Text := Msg;
        for i := 0 to Lines.Count-1 do
        begin
          Fragments.Clear;
          Lines[i] := WrapText(Lines[i], '@@', [' ',#13,#10,#9], RightMargin - Length(Header));
          JclStrings.StrToStrings(Lines[i], '@@', Fragments);
          for n := 0 to Fragments.Count-1 do
          begin
            OutputPrefix(Header, False, aLevel);
            OutputLog(Fragments[n], False,  aLevel);
          end;
        end;

      finally
        Lines.Free;
        Fragments.Free;
      end;
    end;
end;

procedure TLogManager.Log(const aMsg: String; const aLevel: TLogLevel);
begin
  LogMsg(aLevel, aMsg, '');
end;

procedure TLogManager.Log(const aLevel: TLogLevel; const aMsg: String);
begin
  LogMsg(aLevel, aMsg, '');
end;

procedure TLogManager.Log(const aPrefix: String; const aMsg: String; const aLevel: TLogLevel);
begin
  LogMsg(aLevel, aMsg, aPrefix);
end;


{ TFileLogManager }

destructor TFileLogManager.Destroy;
begin
  if Active then
    Stop;

  inherited Destroy;
end;

procedure TFileLogManager.OpenLogFile;
begin
  if FFileOpen then
    CloseLogFile;

  if Length(FileName) > 0 then
    begin
      AssignFile(FLogHandle, Filename);

      if FileExists(Filename) then
        Append(FLogHandle)
      else
        Rewrite(FLogHandle);

      FFileOpen := True;
    end;
end;

procedure TFileLogManager.CloseLogFile;
begin
  if FFileOpen then
    CloseFile(FLogHandle);

  FFileOpen := False;
end;

procedure TFileLogManager.OutputLog(const aLine: String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  if not FileOpen then
    OpenLogFile;

  WriteLn(FLogHandle, aLine);

  if aFlush then
    CloseLogFile;
end;

procedure TFileLogManager.OutputPrefix(const aPrefix: String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  if not FileOpen then
    OpenLogFile;

  Write(FLogHandle, aPrefix);

  if aFlush then
    CloseLogFile;
end;

procedure TFileLogManager.Start;
begin
  if Active then
    Stop;
end;

procedure TFileLogManager.Stop;
begin
  CloseLogFile;
  FActive := False;
end;

procedure TFileLogManager.LogMsg(const aLevel: TLogLevel; const aMsg, aPrefix: String);
begin
  if Active then
   inherited LogMsg(aLevel, aMsg, aPrefix);
  if AlwaysFlush then
    CloseLogFile;
end;

end.
