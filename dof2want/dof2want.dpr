program dof2want;

{$APPTYPE CONSOLE}



uses
  SysUtils,
  dofCompilerFlags,
  dofDirectoriesFlags,
  dofLinkerFlags,
  dofreader in 'dofreader.pas',
  wantWriter in 'wantWriter.pas',
  typ_dofReader in 'typ_dofReader.pas',
  const_dofReader in 'const_dofReader.pas';

procedure outputStr(str : string);
  begin
    writeln(str);
  end;
  procedure usage;
  begin
    outputStr('dof2want delphiproject.dof');
  end;

  procedure CreateWantFile(dofFilename : string);
  const
    StandardOutputName = 'want_compile.xml';
  var
    reader : TDelphiDofReader;
    wantWriter : TWantCompileWriter;
  begin
    outputStr('Converting '+DofFilename+' ==> '+StandardOutputName);
    reader := TDelphiDofReader.create;
    try
       try
         reader.LoadFromFile(dofFilename);
         wantWriter := TWantCompileWriter.create;
         wantWriter.NameOfProject := ChangeFileExt(dofFilename,'.dpr');
         wantWriter.writeFile(reader,StandardOutputName);
         outputStr('Conversion Completed Successfully.');
       except
         outputStr('could not recognize '+dofFilename+' as a valid delphi .dof file');
       end;
       
    finally
      reader.free;
    end;
  end;

  procedure Banner;
  begin
    outputStr('.Dof 2 Want Converter v.1.2');
    outputStr('');
  end;
begin
  Banner;
  if paramCount <1 then
    begin
      outputStr('not enough parameters');
      usage;
    end
  else  
  if not fileExists(paramstr(1)) then
     begin
        outputStr('project file not found');
        usage;
     end
  else
    begin
      CreateWantFile(ExpandFilename(paramStr(1)));
    end;   
end.
