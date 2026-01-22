program steel;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, steel_types, steel_utils, steel_parser, steel_resolve, steel_emit;

const
  STEEL_VERSION = 'steel-pascal 0.2.0';

type
  TCommand = (
    cmdBuild,
    cmdResolve,
    cmdCheck,
    cmdPrint,
    cmdRun,
    cmdDoctor,
    cmdCache,
    cmdGraph,
    cmdFmt,
    cmdDecompile,
    cmdHelp,
    cmdVersion
  );

  TCacheMode = (
    cacheStatus,
    cacheClear
  );

var
  RootPath: string;
  FilePath: string;
  EmitPath: string;
  LogPath: string;
  Command: TCommand;
  CacheMode: TCacheMode;
  Options: TResolveOptions;
  PrintOnly: Boolean;
  RunAll: Boolean;
  BakeList: TStringList;
  OverrideOS: string;
  OverrideArch: string;
  LogMode: string;

procedure PrintUsage;
begin
  WriteLn('steel (pascal)');
  WriteLn('Usage:');
  WriteLn('  steel [command] [options]');
  WriteLn('');
  WriteLn('Commands:');
  WriteLn('  steel            (alias for build steelconf)');
  WriteLn('  build            Parse + resolve steelconf, emit steel.log');
  WriteLn('  resolve          Alias of build');
  WriteLn('  check            Validate steelconf (no emit unless --emit)');
  WriteLn('  print            Emit + print summary');
  WriteLn('  run              Parse and list bakes (stub)');
  WriteLn('  doctor           Diagnostics for config/tools');
  WriteLn('  cache status     Show .steel-cache size');
  WriteLn('  cache clear      Clear .steel-cache');
  WriteLn('  graph            Placeholder graph output');
  WriteLn('  fmt              Placeholder formatter');
  WriteLn('  decompile        Placeholder decompile');
  WriteLn('  help             Show help');
  WriteLn('  version          Show version');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --root <path>        Workspace root');
  WriteLn('  --file <path>        steelconf path');
  WriteLn('  --emit <path>        Output steel.log path');
  WriteLn('  --profile <name>     Select profile');
  WriteLn('  --target <name>      Select target');
  WriteLn('  -D KEY=VALUE         Override variable');
  WriteLn('  --os <name>          Override host OS');
  WriteLn('  --arch <name>        Override host arch');
  WriteLn('  --log <path>         Run log path (run)');
  WriteLn('  --log-mode <mode>    append|truncate (run)');
  WriteLn('  --bake <name>        Select bake (repeatable)');
  WriteLn('  --all                Select all bakes (run)');
end;

procedure PrintVersion;
begin
  WriteLn(STEEL_VERSION);
end;

function HasPrefix(const S, Prefix: string): Boolean;
begin
  Result := (Length(S) >= Length(Prefix)) and (Copy(S, 1, Length(Prefix)) = Prefix);
end;

function ShouldIgnoreDir(const Name: string): Boolean;
begin
  Result := (Name = '.git') or (Name = '.hg') or (Name = '.svn') or
            (Name = 'target') or (Name = 'node_modules') or (Name = 'dist') or
            (Name = 'build') or (Name = '.steel') or (Name = '.steel-cache');
end;

function ScanForSteelconf(const Root: string; Depth: Integer): string;
var
  SR: TSearchRec;
  Entries: TStringList;
  i: Integer;
  Path: string;
begin
  Result := '';
  if Depth > 16 then
    Exit;
  Entries := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(Root) + '*', faAnyFile, SR) = 0 then begin
      repeat
        if (SR.Name = '.') or (SR.Name = '..') then
          Continue;
        Entries.Add(SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    Entries.Sort;
    for i := 0 to Entries.Count - 1 do begin
      Path := IncludeTrailingPathDelimiter(Root) + Entries[i];
      if DirectoryExists(Path) then begin
        if ShouldIgnoreDir(Entries[i]) then
          Continue;
        Result := ScanForSteelconf(Path, Depth + 1);
        if Result <> '' then
          Exit;
      end else begin
        if (Entries[i] = 'steelconf') or (Entries[i] = 'steel') or (Entries[i] = 'build.muf') then begin
          Result := Path;
          Exit;
        end;
      end;
    end;
  finally
    Entries.Free;
  end;
end;

function FindSteelconf(const Root: string; const ExplicitPath: string): string;
var
  Candidate: string;
begin
  if ExplicitPath <> '' then begin
    if FileExists(ExplicitPath) then
      Exit(ExplicitPath);
    Exit('');
  end;
  Candidate := IncludeTrailingPathDelimiter(Root) + 'steelconf';
  if FileExists(Candidate) then
    Exit(Candidate);
  Candidate := IncludeTrailingPathDelimiter(Root) + 'steel';
  if FileExists(Candidate) then
    Exit(Candidate);
  Candidate := IncludeTrailingPathDelimiter(Root) + 'build.muf';
  if FileExists(Candidate) then
    Exit(Candidate);
  Result := ScanForSteelconf(Root, 0);
end;

procedure ParseArgs;
var
  i: Integer;
  Arg, NextArg: string;
begin
  RootPath := GetCurrentDir;
  FilePath := '';
  EmitPath := '';
  LogPath := '';
  PrintOnly := False;
  RunAll := False;
  CacheMode := cacheStatus;
  Command := cmdBuild;
  OverrideOS := '';
  OverrideArch := '';
  LogMode := 'truncate';
  BakeList := TStringList.Create;
  InitResolveOptions(Options);

  if ParamCount = 0 then
    Exit;

  Arg := ParamStr(1);
  if (Arg = 'help') or (Arg = '--help') or (Arg = '-h') then begin
    Command := cmdHelp;
    Exit;
  end;
  if (Arg = 'version') or (Arg = '--version') or (Arg = '-V') then begin
    Command := cmdVersion;
    Exit;
  end;
  if Arg = 'build' then begin
    Command := cmdBuild;
    i := 2;
  end else if Arg = 'resolve' then begin
    Command := cmdResolve;
    i := 2;
  end else if Arg = 'check' then begin
    Command := cmdCheck;
    i := 2;
  end else if Arg = 'print' then begin
    Command := cmdPrint;
    i := 2;
  end else if Arg = 'run' then begin
    Command := cmdRun;
    i := 2;
  end else if Arg = 'doctor' then begin
    Command := cmdDoctor;
    i := 2;
  end else if Arg = 'cache' then begin
    Command := cmdCache;
    i := 2;
  end else if Arg = 'graph' then begin
    Command := cmdGraph;
    i := 2;
  end else if Arg = 'fmt' then begin
    Command := cmdFmt;
    i := 2;
  end else if Arg = 'decompile' then begin
    Command := cmdDecompile;
    i := 2;
  end else begin
    Command := cmdBuild;
    i := 1;
  end;

  while i <= ParamCount do begin
    Arg := ParamStr(i);
    if Arg = '--root' then begin
      Inc(i);
      if i <= ParamCount then
        RootPath := ParamStr(i);
    end else if (Arg = '--file') or (Arg = '--input') then begin
      Inc(i);
      if i <= ParamCount then
        FilePath := ParamStr(i);
    end else if Arg = '--emit' then begin
      Inc(i);
      if i <= ParamCount then
        EmitPath := ParamStr(i);
    end else if Arg = '--profile' then begin
      Inc(i);
      if i <= ParamCount then
        Options.OverrideProfile := ParamStr(i);
    end else if Arg = '--target' then begin
      Inc(i);
      if i <= ParamCount then
        Options.OverrideTarget := ParamStr(i);
    end else if Arg = '--os' then begin
      Inc(i);
      if i <= ParamCount then
        OverrideOS := ParamStr(i);
    end else if Arg = '--arch' then begin
      Inc(i);
      if i <= ParamCount then
        OverrideArch := ParamStr(i);
    end else if (Arg = '--log') then begin
      Inc(i);
      if i <= ParamCount then
        LogPath := ParamStr(i);
    end else if (Arg = '--log-mode') then begin
      Inc(i);
      if i <= ParamCount then
        LogMode := ParamStr(i);
    end else if (Arg = '--bake') then begin
      Inc(i);
      if i <= ParamCount then
        BakeList.Add(ParamStr(i));
    end else if Arg = '--all' then begin
      RunAll := True;
    end else if Arg = '--print' then begin
      PrintOnly := True;
    end else if (Arg = '-D') then begin
      Inc(i);
      if i <= ParamCount then
        Options.Defines.Add(ParamStr(i));
    end else if HasPrefix(Arg, '-D') then begin
      Options.Defines.Add(Copy(Arg, 3, Length(Arg)));
    end else if (Arg = 'status') and (Command = cmdCache) then begin
      CacheMode := cacheStatus;
    end else if (Arg = 'clear') and (Command = cmdCache) then begin
      CacheMode := cacheClear;
    end else if (Arg <> '') and (Arg[1] <> '-') and (FilePath = '') and (Command in [cmdBuild, cmdResolve, cmdCheck, cmdPrint, cmdRun]) then begin
      FilePath := Arg;
    end else begin
      NextArg := Arg;
      if (NextArg <> '') and (NextArg[1] = '-') then begin
        WriteLn('error: unknown flag ', NextArg);
        PrintUsage;
        Halt(1);
      end;
    end;
    Inc(i);
  end;
end;

procedure FreeOptions;
begin
  if Options.Defines <> nil then
    Options.Defines.Free;
  if BakeList <> nil then
    BakeList.Free;
end;

procedure PrintSummary(const Res: TResolvedConfig);
begin
  WriteLn('workspace: ', Res.WorkspaceName);
  WriteLn('root: ', Res.WorkspaceRoot);
  WriteLn('profile: ', Res.SelectedProfile);
  WriteLn('target: ', Res.SelectedTarget);
  WriteLn('os: ', Res.HostOS);
  WriteLn('arch: ', Res.HostArch);
end;

procedure CollectAllBakes(const FileIn: TMufFile; List: TStringList);
var
  i: Integer;
begin
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    if FileIn.Blocks[i].Tag = 'bake' then
      List.Add(FileIn.Blocks[i].Name);
  end;
end;

procedure Doctor(const FileIn: TMufFile);
var
  i, j: Integer;
  Block: TBlock;
  Dir: TDirective;
  ExecPath: string;
begin
  WriteLn('doctor:');
  if FilePath = '' then
    WriteLn('  config: missing')
  else
    WriteLn('  config: ', FilePath);
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if Block.Tag = 'tool' then begin
      ExecPath := '';
      for j := 0 to Length(Block.Directives) - 1 do begin
        Dir := Block.Directives[j];
        if Dir.Kind = dkExec then begin
          if Length(Dir.Args) > 0 then
            ExecPath := Dir.Args[0];
          Break;
        end;
      end;
      if ExecPath <> '' then begin
        if FindInPath(ExecPath) <> '' then
          WriteLn('  tool ', Block.Name, ': ok')
        else
          WriteLn('  tool ', Block.Name, ': missing (', ExecPath, ')');
      end else begin
        WriteLn('  tool ', Block.Name, ': no exec');
      end;
    end;
  end;
end;

procedure DoCacheStatus(const Root: string);
var
  CacheRoot: string;
  SR: TSearchRec;
  TotalSize: Int64;
  Count: Int64;
  function Walk(const Dir: string): Boolean;
  var
    Path: string;
  begin
    Result := True;
    if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, SR) = 0 then begin
      repeat
        if (SR.Name = '.') or (SR.Name = '..') then
          Continue;
        Path := IncludeTrailingPathDelimiter(Dir) + SR.Name;
        if DirectoryExists(Path) then begin
          Walk(Path);
        end else begin
          Inc(Count);
          Inc(TotalSize, SR.Size);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
begin
  CacheRoot := IncludeTrailingPathDelimiter(Root) + '.steel-cache';
  TotalSize := 0;
  Count := 0;
  if DirectoryExists(CacheRoot) then begin
    Walk(CacheRoot);
    WriteLn('cache: ', CacheRoot);
    WriteLn('files: ', Count);
    WriteLn('bytes: ', TotalSize);
  end else begin
    WriteLn('cache: none');
  end;
end;

procedure ClearDir(const Dir: string);
var
  SR: TSearchRec;
  Path: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, SR) = 0 then begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      Path := IncludeTrailingPathDelimiter(Dir) + SR.Name;
      if DirectoryExists(Path) then begin
        ClearDir(Path);
        RemoveDir(Path);
      end else begin
        DeleteFile(Path);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure DoCacheClear(const Root: string);
var
  CacheRoot: string;
begin
  CacheRoot := IncludeTrailingPathDelimiter(Root) + '.steel-cache';
  if DirectoryExists(CacheRoot) then begin
    ClearDir(CacheRoot);
    WriteLn('cache cleared: ', CacheRoot);
  end else begin
    WriteLn('cache: none');
  end;
end;

var
  FileIn: TMufFile;
  ParseErr: string;
  Res: TResolvedConfig;
  TargetsToRun: TStringList;
begin
  ParseArgs;

  if Command = cmdHelp then begin
    PrintUsage;
    FreeOptions;
    Halt(0);
  end;
  if Command = cmdVersion then begin
    PrintVersion;
    FreeOptions;
    Halt(0);
  end;

  if Command = cmdCache then begin
    if CacheMode = cacheClear then
      DoCacheClear(RootPath)
    else
      DoCacheStatus(RootPath);
    FreeOptions;
    Halt(0);
  end;

  if Command in [cmdGraph, cmdFmt, cmdDecompile] then begin
    WriteLn('stub: command not implemented in pascal yet');
    FreeOptions;
    Halt(0);
  end;

  FilePath := FindSteelconf(RootPath, FilePath);
  if FilePath = '' then begin
    WriteLn('error: steelconf not found');
    FreeOptions;
    Halt(1);
  end;

  if not ParseMufFile(FilePath, FileIn, ParseErr) then begin
    WriteLn('error: ', ParseErr);
    FreeOptions;
    Halt(1);
  end;

  Options.OverrideOS := OverrideOS;
  Options.OverrideArch := OverrideArch;
  ResolveConfig(FileIn, Options, Res);

  if Command = cmdDoctor then begin
    Doctor(FileIn);
    FreeOptions;
    Halt(0);
  end;

  if Command in [cmdBuild, cmdResolve, cmdPrint, cmdCheck] then begin
    if EmitPath = '' then begin
      EmitPath := GetEnvironmentVariable('MUFFIN_EMIT');
      if EmitPath = '' then
        EmitPath := IncludeTrailingPathDelimiter(RootPath) + 'steel.log';
    end;

    if (Command <> cmdCheck) or (EmitPath <> '') then
      EmitSteelLog(EmitPath, FileIn, Res);

    if Command = cmdPrint then
      PrintSummary(Res);

    if Command = cmdCheck then
      WriteLn('ok: check');
    FreeOptions;
    Halt(0);
  end;

  if Command = cmdRun then begin
    TargetsToRun := TStringList.Create;
    try
      if PrintOnly then
        PrintSummary(Res);
      if RunAll then
        CollectAllBakes(FileIn, TargetsToRun)
      else if BakeList.Count > 0 then
        TargetsToRun.AddStrings(BakeList);
      WriteLn('run (stub):');
      if TargetsToRun.Count = 0 then
        WriteLn('  no bakes selected')
      else
        WriteLn('  bakes: ', TargetsToRun.DelimitedText);

      if LogPath = '' then
        LogPath := IncludeTrailingPathDelimiter(RootPath) + 'target' + PathDelim + 'steel_run.mff';
      if LogMode = 'append' then begin
        if FileExists(LogPath) then begin
          EmitRunLog(LogPath, FileIn, Res, TargetsToRun);
        end else begin
          EmitRunLog(LogPath, FileIn, Res, TargetsToRun);
        end;
      end else begin
        EmitRunLog(LogPath, FileIn, Res, TargetsToRun);
      end;
    finally
      TargetsToRun.Free;
    end;
    FreeOptions;
    Halt(0);
  end;

  FreeOptions;
end.
