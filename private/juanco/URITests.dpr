{$APPTYPE CONSOLE}
program URITests;

uses
  SysUtils,
  URIs in 'URIs.pas';

{$R *.RES}

procedure WriteURI(URI :string);
begin
  writeln(URI);
  with SplitURI(URI) do
    writeln(Format('[%s][%s][%s][%s][%s]', [Protocol, Server, Path, Resource, Extension]));
  writeln;
end;

begin
  WriteURI('dante/src/dante.dpr');
  WriteURI('/dante/src/dante.dpr');
  WriteURI('/c:/dante/src/dante.dpr');
  WriteURI('http:/c:/dante/src/dante.dpr');
  WriteURI('http://localhost/c:/dante/src/dante.dpr');
  WriteURI('http://remote.com/c:/dante/src/dante.dpr');
  WriteURI('http://remote.com/dante.dpr');
  WriteURI('http:/dante/src/dante.dpr');
  WriteURI('http:dante.dpr');
  WriteURI('http:.cvsignore');
  WriteURI('.cvsignore');
  WriteURI('/.cvsignore');

  WriteURI('file:/c:/.cvsignore');
  WriteURI('file:///c:/.cvsignore');
  WriteURI('file://c:/.cvsignore');

  ReadLn;
end.
