(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{ Notes:
    Code idea (and fragments) are taken from Ant java classes

    Cvs             attributes "error" and "append" are not implemented.
                    attributes "passfile" and "port" are not implemented too,
                    because CVSNT does not use these environment variables
                    (http://ant.apache.org/manual/CoreTasks/cvs.html)
    CvsTagDiff      attribute  "rootdir" is not implemented
                    (http://ant.apache.org/manual/CoreTasks/cvstagdiff.html)
    CvsPass         Added property emptypassword for login to CVS servers without
                    password specified (for example sourceforge CVS).
                    Only acceptable value for "emptypassword" is "true".
                    if CVSNT is used, default password file is not regular
                    file, but passwords are stored in Windows registry key
                    HKEY_CURRENT_USER/Software/Cvsnt/cvspass
                    This task (on MsWindows) is trying to write password to
                    registry and when it fails, then write password
                    to $HOME/.cvspass
                    On Linux it tries only $HOME/.cvspass
                    (http://ant.apache.org/manual/CoreTasks/cvspass.html)
    CvsChangeLog    Added two properties - dateformat and timeformat
                    These properties are used to format date or time written
                    into changelog (XML file). Default: yyyy-mm-dd and hh:mm
                    Format string is compatible with Delphi date-time format
                    string (see "Date-Time values, formatting" in Delphi help)
                    (http://ant.apache.org/manual/CoreTasks/changelog.html)

}

unit CVSTasks;

interface

uses
  SysUtils,
  Classes,
  DateUtils,

  {$IFDEF MSWINDOWS}
  JclRegistry,
  Windows,
  {$ENDIF}
  JclSysInfo,
  JclFileUtils,
  JclUnicode,

  XPerlRE,

  WildPaths,
  ExecTasks,
  WantClasses,
  IniFiles, {Hashed stringlist}
  Contnrs;  {TObjectList}

type
  // used in CvsChangelog
  TRCSFile = class
    private
       fFile : string;
       fRevision : string;
       fPrevRevision : string;
    public
       constructor Create(aName, aRevision : string); overload;
       constructor Create(aName, aRevision, aPrevRevision : string); overload;

       property FileName : string read fFile write fFile;
       property Revision : string read fRevision write fRevision;
       property PrevRevision : string read fPrevRevision write fPrevRevision;
  end;

  // used in CvsChangelog
  TCvsEntry = class
     private
       fDate : TDateTime;
       fAuthor : string;
       fComment : string;
       fFiles : TObjectList;
     public
       constructor Create(aDate : TDateTime; aAuthor, aComment : string);
       destructor  Destroy; override;
       procedure  AddFile(aFile, aRevision : string); overload;
       procedure  AddFile(aFile, aRevision, aPreviousRevision : string); overload;
       function   OutAsXML(aDateFormat, aTimeFormat : string) : string;
  end;

  // used in CvsTagDiff
  TCvsTagEntry = class
     private
        fFileName : TFileName;
        fPrevRevision : string;
        fRevision: string;
     public
        constructor Create(aFileName: TFileName); overload;
        constructor Create(aFileName: TFileName; aRevision : string); overload;
        constructor Create(aFileName: TFileName; aRevision : string; aPrevRevision : string); overload;
        property FileName : TFileName read fFileName write fFileName;
        property PrevRevision : string read fPrevRevision write fPrevRevision;
        property Revision : string read fRevision write fRevision;
        function ToString : string;
  end;

  // used in CvsChangelog
  TCvsChangeLogParser = class
     private
       fFile : string;
       fDate : string;
       fAuthor : string;
       fComment : string;
       fRevision : string;
       fPreviousRevision : string;

       fEntries : THashedStringList;

       fStatus  : integer;
       procedure processComment(aLine : string);
       procedure processFile(aLine : string);
       procedure processDate(aLine : string);
       procedure processGetPreviousRevision(aLine : string);
       procedure processRevision(aLine : string);
       procedure SaveEntry;
       procedure Reset;
     public
       constructor Create;
       destructor Destroy; override;
       procedure ProcessInputFile(aFile : string);
       class function Parse(aInputFile : string) : THashedStringList;
       class procedure OutputEntriesToXML(aOutputFile : string; aEntries : THashedStringList; aDateFormat, aTimeFormat : string);
  end;


  // used in CvsChangelog
  TCvsChangeLogUserElement = class(TScriptElement)
    protected
      FUserId :string;
      FDisplayName :string;
    published
      property userid :string  read FUserID write FUserID;
      property displayname :string  read FDisplayName write FDisplayName;
    public
      class function TagName :string; override;
      procedure Init; override;
    end;

  // Custom CVS Task - base class for other Cvs Tasks
  TCustomCVSTask = class(TCustomExecTask)
  protected
    fCompression: boolean;
    fCompressionLevel: integer;
    ftag: string;
    fcvsRoot: string;
    fcvsRsh: string;
    fdate: string;
    fpackage: string;
    fCommand: string;
    fdest: string;
    fnoexec: boolean;
    fquiet: boolean;

    function AddOption(aOption : string; aValue : string =''; aForceQuote : boolean = false) : string;
    function BuildArguments  :string; override;

    function BuildArgumentsGlobal : string; virtual;
    function BuildArgumentsCommand : string; virtual; abstract;
    function BuildArgumentsSpecific : string; virtual;
  public
    procedure Init; override;
  protected
    property command : string read fCommand write fCommand;
    property compression : boolean read fCompression write fCompression;
    property compressionlevel : integer read fCompressionLevel write fCompressionLevel;
    property cvsRoot : string read fcvsRoot write fcvsRoot;
    property cvsRsh : string read fcvsRsh write fcvsRsh;
    property dest : string read fdest write fdest;
    property package : string read fpackage write fpackage;
    property tag : string read ftag write ftag;
    property date : string read fdate write fdate;
    property quiet : boolean read fquiet write fquiet;
    property noexec : boolean read fnoexec write fnoexec;
    property output;
    property failonerror;
  end;

  // this class is used internally to log most recent module tag
  // it is not globally visible Task (not registered task)
  TCvsMostRecentTag = class(TCustomCvsTask)
    private
      fMostRecentTag : string;
      fModuleName: string;
    public
      procedure Execute; override;
      procedure Init; override;

      property ModuleName : string read fModuleName write fModuleName;
      property MostRecentTag : string read fMostRecentTag;
    protected
      function  BuildArgumentsCommand : string; override;
      function  BuildArgumentsSpecific : string; override;
  end;

  TCvsTask = class(TCustomCvsTask)
    public
      procedure Execute; override;
      function  BuildArgumentsCommand : string; override;
    published
      property command;
      property compression;
      property compressionlevel;
      property cvsRoot;
      property cvsRsh;
      property dest;
      property package;
      property tag;
      property date;
      property quiet;
      property noexec;
      property output;
      property failonerror;
  end;

  TCvsTagDiffTask = class(TCustomCvsTask)
    private
      fstartTag : string;
      fstartDate : string;
      fendTag : string;
      fendDate : string;
      fdestfile : string;
      fMostRecentModule: string;

      function CopyToEnd(aString : string; aFrom : integer) : string;
      function ParseRDiffOutput(aOutput : string; var aParsedOutput : TObjectList) : boolean;
      procedure WriteTagDiff(const aParsedOutput : TObjectList);
      function WriteTagEntry(const aEntry : TCvsTagEntry) : string;
      function FindMostRecentTag : string;
    public
      procedure Execute; override;
      procedure Init; override;
    protected
      function  BuildArgumentsCommand : string; override;
      function  BuildArgumentsSpecific : string; override;
    published
      property compression;
      property compressionlevel;
      property cvsRoot;
      property cvsRsh;
      property package;
      property quiet;
      property failonerror;

      property startTag : string read fstartTag write fstartTag;
      property startDate : string read fstartDate write fstartDate;
      property endTag : string read fendTag write fendTag;
      property endDate : string read fendDate write fendDate;
      property destfile : string read fdestfile write fdestfile;
      // this property is required only when starttag or endtag contains
      // text "MOST RECENT", because of I don't know how to find most recent tag
      // across all modules. This shoul be set to module (filename) which is
      // in repository from project start (for example DPR file)
      property mostrecentmodulename : string read fMostRecentModule write fMostRecentModule;
  end;

  TCvsPassTask = class(TCustomCvsTask)
    private
      fPassword: string;
      fEmptyPassword: boolean;
      procedure ChangeCvsPassInHome;
      function ScrambleCvsPassword(const aPassword : string) : string;
      procedure WritePasswordTo(aFileName : string);
      {$IFDEF MSWINDOWS}
      procedure ChangeCvsPassInRegistry;
      {$ENDIF}
    public
      procedure Init; override;
      procedure Execute; override;
    published
      property cvsRoot;
      property password : string read fPassword write fPassword;
      property emptypassword : boolean read fEmptyPassword write fEmptyPassword;
  end;

  TCvsChangeLogTask = class(TCustomCvsTask)
    private
      fUserList : TList;
      fstart: string;
      fusersfile: string;
      fdaysinpast: string;
      fdir: string;
      fdestfile: string;
      fend: string;
      fDateFormat: string;
      fTimeFormat: string;
    public
      constructor Create(aOwner : TScriptElement); override;
      destructor Destroy; override;
      procedure Init; override;
      procedure Execute; override;
      function CreateUser(aUserID, aDisplayName : string) : TCvsChangeLogUserElement; overload;
    protected
      function  BuildArgumentsCommand : string; override;
      function  BuildArgumentsSpecific : string; override;
    published
      function CreateUser : TCvsChangeLogUserElement; overload;

      property dest : string read fdest write fdest;
      property dir : string read fdir write fdir;
      property destfile : string read fdestfile write fdestfile;
      property usersfile : string read fusersfile write fusersfile;
      property daysinpast : string read fdaysinpast write fdaysinpast;
      property start : string read fstart write fstart;
      property _end : string read fend write fend;
      // following properties are not included in Ant
      property dateformat : string read fDateFormat write fDateFormat;
      property timeformat : string read fTimeFormat write fTimeFormat;
  end;


implementation

const
    FILE_IS_NEW = ' is new; current revision ';
    FILE_HAS_CHANGED = ' changed from revision ';
    FILE_WAS_REMOVED = ' is removed';
    GET_FILE = 1;
    GET_DATE = 2;
    GET_COMMENT = 3;
    GET_REVISION = 4;
    GET_PREVIOUS_REV = 5;

var
    // for scramble cvs password
    shifts : array [0..255] of byte = (
          0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
         16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
        114, 120,  53,  79,  96, 109,  72, 108,  70,  64,  76,  67, 116,  74,  68,  87,
        111,  52,  75, 119,  49,  34,  82,  81,  95,  65, 112,  86, 118, 110, 122, 105,
         41,  57,  83,  43,  46, 102,  40,  89,  38, 103,  45,  50,  42, 123,  91,  35,
        125,  55,  54,  66, 124, 126,  59,  47,  92,  71, 115,  78,  88, 107, 106,  56,
         36, 121, 117, 104, 101, 100,  69,  73,  99,  63,  94,  93,  39,  37,  61,  48,
         58, 113,  32,  90,  44,  98,  60,  51,  33,  97,  62,  77,  84,  80,  85, 223,
        225, 216, 187, 166, 229, 189, 222, 188, 141, 249, 148, 200, 184, 136, 248, 190,
        199, 170, 181, 204, 138, 232, 218, 183, 255, 234, 220, 247, 213, 203, 226, 193,
        174, 172, 228, 252, 217, 201, 131, 230, 197, 211, 145, 238, 161, 179, 160, 212,
        207, 221, 254, 173, 202, 146, 224, 151, 140, 196, 205, 130, 135, 133, 143, 246,
        192, 159, 244, 239, 185, 168, 215, 144, 139, 165, 180, 157, 147, 186, 214, 176,
        227, 231, 219, 169, 175, 156, 206, 198, 129, 164, 150, 210, 154, 177, 134, 127,
        182, 128, 158, 208, 162, 132, 167, 209, 149, 241, 153, 251, 237, 236, 171, 195,
        243, 233, 253, 240, 194, 250, 191, 155, 142, 137, 245, 235, 163, 242, 178, 152
    );

{ Local function }
function ParseCVSDate(aDate : string) : TDateTime;
var
   bConverted : boolean;
   bMonth : word;
begin
   Result := Now;
   // try to create TDateTime from string date in different format
   // Supported formats:
   // format used in system by current locale
   // [d]d.mm.yyyy            (24.9.2001)
   // yyyy-[m]m-[d]d [h]h:[m]m:[s]s   (2001-09-24 10:15:45)
   // yyyy/[m]m/[d]d [h]h:[m]m:[s]s   (2001/09/24 10:15:45)
   // yyyy-[m]m-[d]d            (2001-09-24)
   // yyyy/[m]m/[d]d            (2001/09/24)
   // [d]d MMM yyyy           (24 Sep 2001)

   bConverted := False;
   try { current Locale settings }
     Result := StrToDate(aDate);
     bConverted := True;
   except end;

   if not bConverted then begin { dd.mm.yyyy }
      try
        if regex.Match('(\d?\d)\.(\d?\d)\.(\d\d\d\d)', aDate) then begin
           Result := EncodeDate(StrToInt(regex.SubExp[3].Text), StrToInt(regex.SubExp[2].Text), StrToInt(regex.SubExp[1].Text));
           bConverted := True;
        end;
      except
      end;
   end;

   if not bConverted then begin { yyyy/mm/dd hh:mm:ss }
      try
        if regex.Match('(\d\d\d\d)/(\d?\d)/(\d?\d) (\d?\d):(\d?\d):(\d?\d)', aDate) then begin
           Result := EncodeDate(StrToInt(regex.SubExp[1].Text), StrToInt(regex.SubExp[2].Text), StrToInt(regex.SubExp[3].Text));
           Result := Result + EncodeTime(StrToInt(regex.SubExp[4].Text), StrToInt(regex.SubExp[5].Text), StrToInt(regex.SubExp[6].Text), 0);
           bConverted := True;
        end;
      except
      end;
   end;

   if not bConverted then begin { yyyy-mm-dd hh:mm:ss }
      try
        if regex.Match('(\d\d\d\d)-(\d?\d)-(\d?\d) (\d?\d):(\d?\d):(\d?\d)', aDate) then begin
           Result := EncodeDate(StrToInt(regex.SubExp[1].Text), StrToInt(regex.SubExp[2].Text), StrToInt(regex.SubExp[3].Text));
           Result := Result + EncodeTime(StrToInt(regex.SubExp[4].Text), StrToInt(regex.SubExp[5].Text), StrToInt(regex.SubExp[6].Text), 0);
           bConverted := True;
        end;
      except
      end;
   end;

   if not bConverted then begin { yyyy-mm-dd }
      try
        if regex.Match('(\d\d\d\d)-(\d?\d)-(\d?\d)', aDate) then begin
           Result := EncodeDate(StrToInt(regex.SubExp[1].Text), StrToInt(regex.SubExp[2].Text), StrToInt(regex.SubExp[3].Text));
           bConverted := True;
        end;
      except
      end;
   end;

   if not bConverted then begin { yyyy/mm/dd }
      try
        if regex.Match('(\d\d\d\d)/(\d?\d)/(\d?\d)', aDate) then begin
           Result := EncodeDate(StrToInt(regex.SubExp[1].Text), StrToInt(regex.SubExp[2].Text), StrToInt(regex.SubExp[3].Text));
           bConverted := True;
        end;
      except
      end;
   end;

   if not bConverted then begin { dd MMM yyyy}
      try
        if regex.Match('(\d?\d) ([A-Z][a-z][a-z]) (\d\d\d\d)', aDate) then begin
           bMonth := 0;
           if regex.SubExp[2].Text = 'Jan' then bMonth := 1;
           if regex.SubExp[2].Text = 'Feb' then bMonth := 2;
           if regex.SubExp[2].Text = 'Mar' then bMonth := 3;
           if regex.SubExp[2].Text = 'Apr' then bMonth := 4;
           if regex.SubExp[2].Text = 'May' then bMonth := 5;
           if regex.SubExp[2].Text = 'Jun' then bMonth := 6;
           if regex.SubExp[2].Text = 'Jul' then bMonth := 7;
           if regex.SubExp[2].Text = 'Aug' then bMonth := 8;
           if regex.SubExp[2].Text = 'Sep' then bMonth := 9;
           if regex.SubExp[2].Text = 'Oct' then bMonth := 10;
           if regex.SubExp[2].Text = 'Nov' then bMonth := 11;
           if regex.SubExp[2].Text = 'Dec' then bMonth := 12;
           if bMonth <> 0 then begin
             Result := EncodeDate(StrToInt(regex.SubExp[3].Text), bMonth, StrToInt(regex.SubExp[1].Text));
             bConverted := True;
           end;
        end;
      except
      end;
   end;
   if not bConverted then Result := Now;
end;

{ TCustomCvsTask }

function TCustomCvsTask.AddOption(aOption, aValue: string; aForceQuote : boolean): string;
begin
   Result :=' ' + aOption;
   if aValue <> '' then begin
      if (Pos(' ', aValue) > 0) or aForceQuote  then Result := Result + '"' + aValue + '"'
                                                else Result := Result + aValue;
   end;
end;

function TCustomCvsTask.BuildArguments: string;
begin
  BuildArgumentsGlobal;
  BuildArgumentsCommand;
  BuildArgumentsSpecific;

  Result := inherited BuildArguments;
end;

function TCustomCVSTask.BuildArgumentsGlobal: string;
begin
  // first add global CVS options
  if fquiet then begin
    Log(vlVerbose, 'quiet=true');
    ArgumentList.Add('-q');
  end;
  if fnoexec then begin
    Log(vlVerbose, 'noexec=true');
    ArgumentList.Add('-n');
  end;
  if fCompression then begin
     if HasAttribute('compressionlevel') then begin
        if fCompressionLevel in [1..9] then begin
           Log(vlVerbose, 'compression=true');
           Log(vlVerbose, 'compressionlevel='+IntToStr(fCompressionLevel));
           ArgumentList.Add(AddOption('-z', IntToStr(fCompressionLevel)));
        end else begin
           Log(vlWarnings, 'Invalid compressionlevel value (not in range 1-9): '+IntToStr(fCompressionLevel));
        end;
     end else begin
        Log(vlVerbose, 'compression=true');
        ArgumentList.Add(AddOption('-z', '3'));
     end;
  end;
  if fcvsRoot <> '' then begin
     Log(vlVerbose, 'CVSROOT='+fcvsRoot);
     ArgumentList.Add(AddOption('-d', fcvsRoot));
  end;
  if fcvsRsh <> '' then begin
     Log(vlVerbose, 'CVS_RSH='+fcvsRsh);
     JclSysInfo.SetEnvironmentVar('CVS_RSH', fcvsRsh);
  end else begin
     JclSysInfo.SetEnvironmentVar('CVS_RSH', '');
  end;
end;

function TCustomCVSTask.BuildArgumentsSpecific: string;
begin
  if ftag <> '' then begin
     Log(vlVerbose, 'tag='+ftag);
     ArgumentList.Add(AddOption('-r', ftag, true));
  end;
  if fdate <> '' then begin
     Log(vlVerbose, 'date='+fdate);
     ArgumentList.Add(AddOption('-D', fdate, true));
  end;
  if fpackage <> '' then begin
     Log(vlVerbose, 'package='+fpackage);
     ArgumentList.Add(AddOption(fpackage));
  end;
end;

procedure TCustomCvsTask.Init;
begin
  {$IFDEF LINUX}
  Executable := 'cvs';
  {$ELSE}
  Executable := 'cvs.exe';
  {$ENDIF}
  inherited;
end;

{ TCvsTask }

function TCvsTask.BuildArgumentsCommand: string;
begin
  if fCommand <> '' then begin
     Log(vlVerbose, 'command='+fCommand);
     ArgumentList.Add(AddOption(fCommand));
  end else begin
     Log(vlVerbose, 'command=checkout');
     ArgumentList.Add(AddOption('checkout'));
  end;
end;

procedure TCvsTask.Execute;
var
   bOldDir : TPath;
begin
  bOldDir := CurrentDir;
  if fdest <> '' then begin
     ChangeDir(fdest, true);
  end;
  inherited;
  ChangeDir(bOldDir);
end;

{ TCvsTagDiffTask }

function TCvsTagDiffTask.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=rdiff');
  ArgumentList.Add(AddOption('rdiff'));
end;

function TCvsTagDiffTask.BuildArgumentsSpecific: string;
begin
  ArgumentList.Add(AddOption('-s')); // short listing
  if fstartTag <> '' then begin
     if fstartTag='MOST RECENT' then begin
       Log(vlVerbose, 'trying to find most recent tag');
       fstartTag := FindMostRecentTag;
     end;
     Log(vlVerbose, 'starttag='+fstartTag);
     ArgumentList.Add(AddOption('-r', fstartTag,true));
  end else if fstartDate <> '' then begin
     Log(vlVerbose, 'startdate='+fstartDate);
     ArgumentList.Add(AddOption('-D', fstartDate,true));
  end;
  if fendTag <> '' then begin
     if fendTag='MOST RECENT' then begin
        Log(vlVerbose, 'trying to find most recent tag');
        fendTag := FindMostRecentTag;
     end;
     Log(vlVerbose, 'endtag='+fendTag);
     ArgumentList.Add(AddOption('-r', fendTag,true));
  end else if fendDate <> '' then begin
     Log(vlVerbose, 'enddate='+fendDate);
     ArgumentList.Add(AddOption('-D', fendDate,true));
  end;
  inherited BuildArgumentsSpecific;
end;

function TCvsTagDiffTask.CopyToEnd(aString: string;
  aFrom: integer): string;
begin
  Result := Copy(aString, aFrom, Length(aString));
end;

procedure TCvsTagDiffTask.Execute;
var
   bRDiffOutput : TObjectList;
begin
  output := FileGetTempName('cvs');
  inherited;
  bRDiffOutput := TObjectList.Create;
  try
     if ParseRDiffOutput(output, bRDiffOutput) then WriteTagDiff(bRDiffOutput);
  finally
     SysUtils.DeleteFile(output);
     bRDiffOutput.Free;
  end;
end;

function TCvsTagDiffTask.FindMostRecentTag: string;
var
   bMRT : TCvsMostRecentTag;
begin
   bMRT := TCvsMostRecentTag.Create(self);
   try
     bMRT.Init;
     // copy attribute values from CvsTagDiffTask
     bMRT.compression := fcompression;
     bMRT.compressionlevel := fcompressionlevel;
     bMRT.cvsRoot := fcvsRoot;
     bMRT.cvsRsh := fcvsRsh;
     bMRT.package := fpackage;
     bMRT.quiet := fquiet;
     bMRT.failonerror := ffailonerror;
     bMRT.ModuleName := fMostRecentModule;

     bMRT.Execute;
     Result := bMRT.MostRecentTag;
   finally
     bMRT.Free;
   end;
end;

procedure TCvsTagDiffTask.Init;
begin
  inherited;
  RequireAttribute('destfile');
  RequireAttribute('package');
  if (fstartTag = 'MOST RECENT') or (fendtag = 'MOST RECENT') then
     RequireAttribute('mostrecentmodulename'); 
  RequireAttribute('starttag|startdate');
  RequireAttribute('endtag|enddate');

end;

function TCvsTagDiffTask.ParseRDiffOutput(aOutput: string; var aParsedOutput : TObjectList): boolean;
var
   i, index, new_index, headerLength : integer;
   revSeparator : integer;
   SL : TStringList;
   revision, prevRevision, line, fname : string;
begin
   Result := False;
   if Assigned(aParsedOutput) then begin
     headerLength := 5 + Length(package) + 2;
     SL := TStringList.Create;
     try
       SL.LoadFromFile(aOutput);
       for i := 0 to SL.Count - 1 do begin
         line := CopyToEnd(SL[i], headerLength-1);
         index := Pos(FILE_IS_NEW, line);
         if index <> 0 then begin
            // it is a new file
            new_index := aParsedOutput.Add(TCvsTagEntry.Create(Copy(line, 1, index-1), CopyToEnd(line, index+Length(FILE_IS_NEW))));
            Log(vlVerbose, TCvsTagEntry(aParsedOutput[new_index]).ToString);
         end else begin
            index := Pos(FILE_HAS_CHANGED, line);
            if index <> 0 then begin
              // it is modified file
              fname := Copy(line, 1, index-1);
              revSeparator := Pos(' to ', line);
              prevRevision := Copy(line, index + Length(FILE_HAS_CHANGED), revSeparator-(index + Length(FILE_HAS_CHANGED)));
              // 4 is " to " length
              revision := CopyToEnd(line, revSeparator + 4);
              new_index := aParsedOutput.Add(TCvsTagEntry.Create(fname, revision, prevRevision));
              Log(vlVerbose, TCvsTagEntry(aParsedOutput[new_index]).ToString);
            end else begin
              index := Pos(FILE_WAS_REMOVED, line);
              if index <> 0 then begin
                // it is a removed file
                fname := Copy(line, 1, index-1);
                new_index := aParsedOutput.Add(TCvsTagEntry.Create(fname));
                Log(vlVerbose, TCvsTagEntry(aParsedOutput[new_index]).ToString);
              end;
            end;
         end;
       end;
       Result := True;
     finally
       SL.Free;
     end;
  end;
end;

procedure TCvsTagDiffTask.WriteTagDiff(
  const aParsedOutput: TObjectList);
var
   FS : TFileStream;
   i : integer;

   procedure StreamWriteString(const aString : string);
   var
      s : string;
   begin
      s := WideStringToUTF8(aString);
      FS.WriteBuffer(s[1], Length(s));
   end;
begin
   FS := TFileStream.Create(destfile, fmCreate);
   try
     StreamWriteString('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
     StreamWriteString('<tagdiff ');
     if fstartTag <> '' then StreamWriteString('starttag="'+fstartTag+'" ')
                        else StreamWriteString('startdate="'+fstartDate+'" ');
     if fendTag <> ''   then StreamWriteString('endtag="'+fendTag+'" ')
                        else StreamWriteString('enddate="'+fendDate+'" ');
     StreamWriteString('>'#13#10);
     for i := 0 to aParsedOutput.Count - 1 do begin
        StreamWriteString(WriteTagEntry(TCvsTagEntry(aParsedOutput[i])));
     end;
     StreamWriteString('</tagdiff>'#13#10);
   finally
     FS.Free;
   end;
end;

function TCvsTagDiffTask.WriteTagEntry(const aEntry: TCvsTagEntry): string;
begin
   Result := #9'<entry>'#13#10;
   Result := Result + #9#9'<file>'#13#10;
   Result := Result + #9#9#9'<name>'+aEntry.FileName+'</name>'#13#10;
   if aEntry.Revision <> '' then
      Result := Result + #9#9#9'<revision>'+aEntry.Revision+'</revision>'#13#10;
   if aEntry.PrevRevision <> '' then
      Result := Result + #9#9#9'<prevrevision>'+aEntry.PrevRevision+'</prevrevision>'#13#10;
   Result := Result + #9#9'</file>'#13#10;
   Result := Result + #9'</entry>'#13#10;
end;

{ TCvsTagEntry }

constructor TCvsTagEntry.Create(aFileName: TFileName);
begin
  fFileName := aFileName;
  fPrevRevision := '';
  fRevision := '';
end;

constructor TCvsTagEntry.Create(aFileName: TFileName;
  aRevision: string);
begin
  fFileName := aFileName;
  fRevision := aRevision;
  fPrevRevision := '';
end;

constructor TCvsTagEntry.Create(aFileName: TFileName; aRevision,
  aPrevRevision: string);
begin
  fFileName := aFileName;
  fPrevRevision := aPrevRevision;
  fRevision := aRevision;
end;

function TCvsTagEntry.ToString: string;
begin
   Result := '';
   Result := Result + fFileName;
   if (fRevision = '') and (fPrevRevision = '') then
      Result := Result + ' was removed'
   else if (fRevision <> '') and (fPrevRevision = '') then
      Result := Result + ' is new; current revision is ' + fRevision
   else if (fRevision <> '') and (fPrevRevision <> '') then
      Result := Result + ' has changed from ' + fPrevRevision + ' to ' + fRevision;
end;

{ TCvsPassTask }

// used in both, MsWindows and Linux
procedure TCvsPassTask.ChangeCvsPassInHome;
var
   bHomeDir : string;
begin
  if JclSysInfo.GetEnvironmentVar('HOME', bHomeDir, true)
     then WritePasswordTo(PathAddSeparator(bHomeDir)+'.cvspass')
     else Log(vlErrors, 'Cannot determine $HOME direcotry');
end;

{$IFDEF MSWINDOWS}
procedure TCvsPassTask.ChangeCvsPassInRegistry;
begin
  if RegKeyExists(HKEY_CURRENT_USER, 'Software\Cvsnt\cvspass') then begin
     RegWriteString(HKEY_CURRENT_USER, 'Software\Cvsnt\cvspass', fcvsRoot, ScrambleCvsPassword(fPassword));
     Log(vlVerbose, 'Password for repository '+fcvsRoot+' stored do registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass');
  end else begin
     Log(vlWarnings, 'Could not find registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass. Trying $HOME');
     ChangeCvsPassInhome;
  end;
end;
{$ENDIF}

procedure TCvsPassTask.Execute;
begin
  // no call to inherited, because of this task does not call cvs executable

  // if CVSNT is used, default password file is not regular file, but passwords
  // are stored in registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass
  // in format:  KeyName = repository; KeyValue = scrambled password

  // in linux is used ~/.cvspass
  {$IFDEF LINUX}
     ChangeCvsPassInHome;
  {$ELSE}
     ChangeCvsPassInRegistry;
  {$ENDIF}
end;

procedure TCvsPassTask.Init;
begin
  inherited;
  RequireAttribute('cvsroot');
  RequireAttribute('password|emptypassword');
  if GetAttribute('emptypassword') <> '' then begin
     if fEmptyPassword then fPassword := ''; 
  end;
end;

function TCvsPassTask.ScrambleCvsPassword(const aPassword: string): string;
var
   i : integer;
begin
   Result := 'A';
   for i := 1 to Length(aPassword) do begin
      Result := Result + Chr(shifts[Ord(aPassword[i])]);
   end;
end;

procedure TCvsPassTask.WritePasswordTo(aFileName: string);
var
   SL : TStringList;
   bExists, bFound  : boolean;
   i : integer;
begin
  bExists := False;
  if FileExists(aFileName) then begin
     bExists := True;
     if FileIsReadOnly(aFileName) then begin
       Log(vlErrors, 'Cannot write to '+aFileName);
       exit;
     end;
  end;
  SL := TStringList.Create;
  try
    if bExists then SL.LoadFromFile(aFileName);
    bFound := False;
    for i := 0 to SL.Count - 1 do begin
      if Copy(SL[i], 1, Length(fcvsRoot)) = fcvsRoot then begin
         SL[i] := fcvsRoot + ' ' + ScrambleCvsPassword(fPassword);
         bFound := true;
         break;
      end;
    end;
    if not bFound then SL.Add(fcvsRoot + ' ' + ScrambleCvsPassword(fPassword));
    SL.SaveToFile(aFileName);
  finally
    SL.Free;
  end;
end;

{ TCvsChangeLogUserElement }

procedure TCvsChangeLogUserElement.Init;
begin
  inherited;
  RequireAttribute('userid');
  RequireAttribute('displayname');
end;

class function TCvsChangeLogUserElement.TagName: string;
begin
  Result := 'user';
end;

{ TCvsChangeLogTask }

function TCvsChangeLogTask.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=log');
  ArgumentList.Add(AddOption('log'));
end;

function TCvsChangeLogTask.BuildArgumentsSpecific: string;
var
   s : string;
begin
  if fdaysinpast <> '' then begin
     DateTimeToString(fstart, 'yyyy-mm-dd', Now - StrToInt(fdaysinpast) * 24 * 60 * 60);
     Log(vlVerbose, 'daysinpast ('+fdaysinpast+') converted to '+fstart);
  end;
  if fstart <> '' then begin
    DateTimeToString(s, 'yyyy-mm-dd', ParseCVSDate(fstart));
    s := '>='+s;
    ArgumentList.Add(AddOption('-d', s, true));
    Log(vlVerbose, 'date'+s);
  end;
  if fdir = '' then fdir := basedir;
  inherited BuildArgumentsSpecific;
end;

constructor TCvsChangeLogTask.Create(aOwner: TScriptElement);
begin
   fDateFormat := 'yyyy-mm-dd';
   fTimeFormat := 'hh:mm';
   inherited Create(aOwner);
   fUserList := TList.Create;
end;

function TCvsChangeLogTask.CreateUser: TCvsChangeLogUserElement;
begin
  Result := TCvsChangeLogUserElement.Create(self);
  fUserList.Add(Result);
end;

function TCvsChangeLogTask.CreateUser(aUserID,
  aDisplayName: string): TCvsChangeLogUserElement;
begin
  Result := TCvsChangeLogUserElement.Create(self);
  Result.userid := aUserID;
  Result.displayname := aDisplayName;
  fUserList.Add(Result);
end;

destructor TCvsChangeLogTask.Destroy;
begin
  fUserList.Free;
  inherited;
end;

procedure TCvsChangeLogTask.Execute;
var
   SL : TStringList;
   i,j : integer;
   bOldDir : TPath;
   bEntries : THashedStringList;
begin
  bOldDir := CurrentDir;
  if fdir <> '' then begin
     Log(vlVerbose, 'directory changed to '+dir);
     ChangeDir(fdir, true);
  end;
  output := FileGetTempName('cvs');
  inherited;
  ChangeDir(bOldDir);
  Log(vlVerbose, 'directory changed back to '+bOldDir);

  // append to user list from file
  if fusersfile <> '' then begin
    if FileExists(fusersfile) then begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(fusersfile);
        for i := 0 to SL.Count - 1 do begin
           if SL.Values[SL.Names[i]] <> '' then begin
              CreateUser(SL.Names[i], SL.Values[SL.Names[i]]);
           end;
        end;
      finally
        SL.Free;
      end;
    end else Log(vlWarnings, 'Userfile ' + fUsersfile + ' does not exists');
  end;
  bEntries := TCvsChangeLogParser.Parse(output);
  DeleteFile(output);

  // filter start/end dates and replace username
  for i := bEntries.Count - 1 downto 0 do begin
    if fstart <> '' then begin
       if TCvsEntry(bEntries.Objects[i]).fDate < ParseCVSDate(fStart) then begin
          bEntries.Delete(i);
          continue;
       end;
    end;
    if fend <> '' then begin
       if TCvsEntry(bEntries.Objects[i]).fDate > ParseCVSDate(fend) then begin
          bEntries.Delete(i);
          continue;
       end;
    end;
    for j := 0 to fUserList.Count - 1 do begin
       if TCvsChangeLogUserElement(fUserList[j]).userid = TCvsEntry(bEntries.Objects[i]).fAuthor then begin
          TCvsEntry(bEntries.Objects[i]).fAuthor := TCvsChangeLogUserElement(fUserList[j]).displayname;
          break;
       end;
    end;
  end;
  TCvsChangeLogParser.OutputEntriesToXML(fdestfile, bEntries, fDateFormat, fTimeFormat);
end;

procedure TCvsChangeLogTask.Init;
begin
  inherited;
  RequireAttribute('destfile');
end;

{ TRCSFile }

constructor TRCSFile.Create(aName, aRevision: string);
begin
   fFile := aNAme;
   fRevision := aRevision;
   fPrevRevision := '';
end;

constructor TRCSFile.Create(aName, aRevision, aPrevRevision: string);
begin
   fFile := aName;
   fRevision := aRevision;
   fPrevRevision := '';
   if aRevision <> aPrevRevision then fPrevRevision := aPrevRevision;
end;

{ TCvsEntry }

procedure TCvsEntry.AddFile(aFile, aRevision, aPreviousRevision: string);
begin
   fFiles.Add(TRCSFile.Create(aFile, aRevision, aPreviousRevision));
end;

procedure TCvsEntry.AddFile(aFile, aRevision: string);
begin
   fFiles.Add(TRCSFile.Create(aFile, aRevision));
end;

constructor TCvsEntry.Create(aDate : TDateTime; aAuthor, aComment: string);
begin
  fDate := aDate;
  fAuthor := aAuthor;
  fComment := aComment;
  fFiles := TObjectList.Create;
end;

destructor TCvsEntry.Destroy;
begin
  fFiles.Free;
  inherited;
end;

function TCvsEntry.OutAsXML(aDateFormat, aTimeFormat : string): string;
var
   i : integer;
   s, bOutput : string;
   bRF : TRCSFile;

   function AddLine(aText : string) : string;
   begin
      bOutput := bOutput + aText + #13#10;
   end;
begin
   bOutput := '';
   AddLine(#9'<entry>');
   DateTimeToString(s, aDateFormat, fDate);
   AddLine(#9#9'<date>' + s + '</date>');
   DateTimeToString(s, aTimeFormat, fDate);
   AddLine(#9#9'<time>' + s + '</time>');
   AddLine(#9#9'<author><![CDATA[' + fAuthor + ']]></author>');
   for i := 0 to fFiles.Count - 1 do begin
     bRF := TRCSFile(fFiles[i]);
     AddLine(#9#9'<file>');
     AddLine(#9#9#9'<name>' + bRF.FileName + '</name>');
     AddLine(#9#9#9'<revision>' + bRF.Revision + '</revision>');
     if bRF.PrevRevision <> '' then begin
        AddLine(#9#9#9'<prevrevision>' + bRF.PrevRevision + '</prevrevision>');
     end;
     AddLine(#9#9'</file>');
   end;
   AddLine(#9#9'<msg><![CDATA[' + fComment + ']]></msg>');
   AddLine(#9'</entry>');
   Result := bOutput;
end;

{ TCvsChangeLogParser }

constructor TCvsChangeLogParser.Create;
begin
  fStatus := GET_FILE;
  fEntries := THashedStringList.Create;
end;

destructor TCvsChangeLogParser.Destroy;
begin
  fEntries.Free;
  inherited;
end;

class procedure TCvsChangeLogParser.OutputEntriesToXML(aOutputFile : string;
  aEntries: THashedStringList; aDateFormat, aTimeformat : string);
var
   i : integer;
   FS : TFileStream;

   procedure StreamWriteString(const aString : string);
   var
      s : string;
   begin
      s := WideStringToUTF8(aString);
      FS.WriteBuffer(s[1], Length(s));
   end;
begin
   FS := TFileStream.Create(aOutputFile, fmCreate);
   try
     StreamWriteString('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
     StreamWriteString('<changelog>'#13#10);
     for i := 0 to aEntries.Count - 1 do begin
        StreamWriteString(TCvsEntry(aEntries.Objects[i]).OutAsXML(aDateFormat, aTimeFormat));
     end;
     StreamWriteString('</changelog>'#13#10);
   finally
     FS.Free;
   end;
end;

class function TCvsChangeLogParser.Parse(aInputFile : string) : THashedStringList;
begin
   with TCvsChangeLogParser.Create do begin
      ProcessInputFile(aInputfile);
      Result := fEntries;
   end;
end;

procedure TCvsChangeLogParser.processComment(aLine: string);
var
   lineSeparator : string;
   bEnd : integer;
begin
   {$IFDEF LINUX}
     lineSeparator := #$0A;
   {$ELSE}
     lineSeparator := #$0D#$0A;
   {$ENDIF}
   if Pos('======', aLine) = 1 then begin
      //We have ended changelog for that particular file
      //so we can save it
      bEnd := Length(fComment) - Length(lineSeparator);
      fComment := Copy(fComment, 1, bEnd);
      saveEntry;
      fStatus := GET_FILE;
   end else if Pos('----------------------------', aLine) = 1 then begin
      bEnd := Length(fComment) - Length(lineSeparator);
      fComment := Copy(fComment, 1, bEnd);
      fStatus := GET_PREVIOUS_REV;
   end else if Pos('branches:', aLine) = 1 then begin
      // - this was not in original Ant implementation
      // ignore this line; continue in Comment parsing
   end else begin
      fComment := fComment + aLine + lineSeparator;
   end;
end;

procedure TCvsChangeLogParser.processDate(aLine: string);
var
   lineData : string;
begin
   if Pos('date:', aLine) = 1 then begin
      fDate := Copy(aLine, 7, 19);
      lineData := Copy(aLine, Pos(';', aLine)+1, Length(aLine));
      fAuthor := Copy(lineData, 11, Pos(';', lineData)-11);
      fStatus := GET_COMMENT;
      //Reset comment to empty here as we can accumulate multiple lines
      //in the processComment method
      fComment := '';
   end;
end;

procedure TCvsChangeLogParser.processFile(aLine: string);
begin
   if Pos('Working file:', aLine) = 1 then begin
      fFile := Copy(aLine, 15, Length(aLine));
      fStatus := GET_REVISION;
   end;
end;

procedure TCvsChangeLogParser.processGetPreviousRevision(aLine: string);
begin
  if Pos('revision', aLine) = 0 then begin
     raise Exception.Create('Unexpected line from CVS: ' + aLine);
  end;
  fPreviousRevision := Copy(aLine, 10, Length(aLine));
  saveEntry;
  fRevision := fPreviousRevision;
  fStatus := GET_DATE;
end;

procedure TCvsChangeLogParser.ProcessInputFile(aFile: string);
var
   i : integer;
   SL : TStringList;
begin
   SL := TStringList.Create;
   try
     SL.LoadFromFile(aFile);
     for i := 0 to SL.Count - 1 do begin
        case fStatus of
            GET_FILE: begin
                // make sure attributes are reset when
                // working on a 'new' file.
                reset;
                processFile(SL[i]);
            end;
            GET_REVISION: begin
                processRevision(SL[i]);
            end;
            GET_DATE: begin
                processDate(SL[i]);
            end;
            GET_COMMENT: begin
                processComment(SL[i]);
            end;
            GET_PREVIOUS_REV: begin
                processGetPreviousRevision(SL[i]);
            end;
        end;
     end;
   finally
     SL.Free;
   end;

end;

procedure TCvsChangeLogParser.processRevision(aLine: string);
begin
  if Pos('revision', aLine) = 1 then begin
     fRevision := Copy(aLine, 10, Length(aLine));
     fStatus := GET_DATE;
  end else if Pos('======', aLine) = 1 then begin
     //There was no revisions in this changelog
     //entry so lets move unto next file
     fStatus := GET_FILE;
  end;
end;

procedure TCvsChangeLogParser.Reset;
begin
   fFile := '';
   fDate := '';
   fAuthor := '';
   fComment := '';
   fRevision := '';
   fPreviousRevision := '';
end;

procedure TCvsChangeLogParser.SaveEntry;
var
   bEntryKey : string;
   bEntry : TCvsEntry;
   i : integer;
begin
   bEntryKey := fDate + fAuthor + fComment;
   i := fEntries.IndexOf(bEntryKey);
   if i = -1 then begin
       bEntry := TCvsEntry.Create(ParseCVSDate(fDate), fAuthor, fComment);
       fEntries.AddObject(bEntryKey, bEntry);
     end else begin
       bEntry := TCVSEntry(fEntries.Objects[i]);
   end;
   bEntry.AddFile(fFile, fRevision, fPreviousRevision);
end;

{ TCvsMostRecentTag }

function TCvsMostRecentTag.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=log');
  ArgumentList.Add(AddOption('log'));
end;

function TCvsMostRecentTag.BuildArgumentsSpecific: string;
begin
  ArgumentList.Add(AddOption('-h')); // headers only
  ArgumentList.Add(AddOption(fModuleName));
end;

procedure TCvsMostRecentTag.Execute;
var
   SL : TStringList;
   i : integer;
begin
  fMostRecentTag := '';
  output := FileGetTempName('cvs');
  inherited;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(output);
    i := SL.IndexOf('symbolic names:');
    if i <> -1 then begin
       if SL[i+1][1] = #9 then begin
         fMostRecentTag := Trim(Copy(SL[i+1], 1, Pos(':', SL[i+1])-1));
       end;
    end;
  finally
    SL.Free;
    DeleteFile(output);
  end;
end;

procedure TCvsMostRecentTag.Init;
begin
  inherited;
end;

initialization
  RegisterTask(TCvsTask);
  RegisterTask(TCvsTagDiffTask);
  RegisterTask(TCvsPassTask);
  RegisterTask(TCvsChangeLogTask);
  RegisterElement( TCvsChangeLogTask, TCvsChangeLogUserElement );
end.


