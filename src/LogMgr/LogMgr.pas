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
                vlVeryQuiet,
                vlQuiet,
                vlNormal,
                vlVerbose,
                vlDebug     );

  TLogManager = class
    private
      FActive:           Boolean;
      FFileOpen:         Boolean;
      FEchoToFile:       Boolean;
      FIncludeDate:      Boolean;
      FIncludeTime:      Boolean;
      FIncludeShortName: Boolean;
      FAlwaysFlush:      Boolean;
      FHeaderPresent:    Boolean;

      FLogLevel: TLogLevel;

      FRightMargin: Integer;

      FLogHandle: TextFile;

      FFileName:  String;
      FShortName: String;
      FLogHeader: String;

    protected
      procedure   SetIncludeDate(const aValue: Boolean);
      procedure   SetIncludeTime(const aValue: Boolean);

      procedure   BuildHeader;

      procedure   OpenLogFile;
      procedure   CloseLogFile;

      procedure   OutputLog(const aLine: String; const aFlush: Boolean);

    public
      constructor Create;
      destructor  Destroy; override;

      procedure   Start;
      procedure   Stop;

      procedure   LogMsg(const aLevel: TLogLevel; const aMsg: String; const aPrefix: String); virtual;

      procedure   Log(const aMsg: String = ''; const aLevel: TLogLevel = vlNormal); overload; virtual;
      procedure   Log(const aLevel: TLogLevel; const aMsg: String = ''); overload;
      procedure   Log(const aPrefix: String; const aMsg: String; const aLevel: TLogLevel = vlNormal); overload; virtual;

      property    Active:   Boolean read FActive;
      property    FileOpen: Boolean read FFileOpen;

      property    Level:    TLogLevel read FLogLevel write FLogLevel;

      property    EchoToFile:  Boolean read FEchoToFile  write FEchoToFile;
      property    AlwaysFlush: Boolean read FAlwaysFlush write FAlwaysFlush;

      property    FileName:  String read FFileName  write FFileName;
      property    ShortName: String read FShortName write FShortName;
      property    LogHeader: String read FLogHeader write FLogHeader;

      property    IncludeDate:      Boolean read FIncludeDate      write SetIncludeDate;
      property    IncludeTime:      Boolean read FIncludeTime      write SetIncludeTime;
      property    IncludeShortName: Boolean read FIncludeShortName write FIncludeShortName;

      property    RightMargin: Integer read FRightMargin write FRightMargin;
  end;

const
  LogLevelText: array[TLogLevel] of String = ( 'Errors',
                                               'Warnings',
                                               'Very Quiet',
                                               'Quiet',
                                               'Normal',
                                               'Verbose',
                                               'Debug'       );

implementation

uses
  Classes,
  SysUtils,
  JclStrings;

  { TLogManager }

constructor TLogManager.Create;
begin
  inherited Create;

  FActive           := False;
  FFileOpen         := False;
  FEchoToFile       := False;
  FAlwaysFlush      := False;
  FIncludeDate      := False;
  FIncludeTime      := False;
  FIncludeShortName := False;
  FHeaderPresent    := False;

  FFileName  := '';
  FShortName := '';
  FLogHeader := '';

  FLogLevel := vlNormal;

  FRightMargin := 55;
end;

destructor TLogManager.Destroy;
begin
  if Active then
    Stop;

  inherited Destroy;
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

procedure TLogManager.OpenLogFile;
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

procedure TLogManager.CloseLogFile;
begin
  if FFileOpen then
    CloseFile(FLogHandle);

  FFileOpen := False;
end;

procedure TLogManager.Start;
begin
  if Active then
    Stop;

  FActive := True;
end;

procedure TLogManager.Stop;
begin
  CloseLogFile;

  FActive := False;
end;

procedure TLogManager.OutputLog(const aLine: String; const aFlush: Boolean);
begin
  WriteLn(aLine);

  if EchoToFile then
    begin
      if not FileOpen then
        OpenLogFile;

      WriteLn(FLogHandle, aLine);

      if aFlush then
        CloseLogFile;
    end;
end;

procedure TLogManager.LogMsg(const aLevel: TLogLevel; const aMsg: String; const aPrefix: String);
var
  Header: String;
  Lines:  TStringList;
  i:      Integer;
begin
  if (Level >= aLevel) and Active then
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
        Header := Header + '[' + aPrefix + '] ';

      if Length(Header + aMsg) > RightMargin then
        begin
          Lines := TStringList.Create;

          try
            JclStrings.StrToStrings(WrapText(aMsg, '...@@... ', [' ',#13,#10,#9], RightMargin - Length(Header)),
                                    '@@', Lines);

            for i := 0 to Pred(Lines.Count) do
              OutputLog(Header + Lines[i], False);

          finally
            Lines.Free;

            if AlwaysFlush then
              CloseLogFile;
          end;
        end
      else
        OutputLog(Header + aMsg, AlwaysFlush);
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

end.
