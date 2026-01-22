program steel;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DateUtils, Process, steel_types, steel_utils, steel_parser, steel_resolve, steel_emit;

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
  RunLogPath: string;
  Command: TCommand;
  CacheMode: TCacheMode;
  Options: TResolveOptions;
  PrintOnly: Boolean;
  RunAll: Boolean;
  DryRun: Boolean;
  Verbose: Boolean;
  NoCache: Boolean;
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
  WriteLn('  --dry-run            Print actions without executing (run)');
  WriteLn('  --verbose, -v        Verbose run output');
  WriteLn('  --no-cache           Disable run cache/skip logic (run)');
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
  RunLogPath := '';
  PrintOnly := False;
  RunAll := False;
  DryRun := False;
  Verbose := False;
  NoCache := False;
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
    end else if Arg = '--dry-run' then begin
      DryRun := True;
    end else if (Arg = '--verbose') or (Arg = '-v') then begin
      Verbose := True;
    end else if Arg = '--no-cache' then begin
      NoCache := True;
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

function CollectBakeDeps(const Block: TBlock): TStringList;
var
  i, j: Integer;
  Dir: TDirective;
begin
  Result := TStringList.Create;
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    if Dir.Kind = dkNeeds then begin
      for j := 0 to Length(Dir.Args) - 1 do
        Result.Add(Dir.Args[j]);
    end;
  end;
end;

procedure AddUniqueStrings(const Src: TStringList; Dest: TStringList);
var
  i: Integer;
begin
  for i := 0 to Src.Count - 1 do begin
    if Dest.IndexOf(Src[i]) < 0 then
      Dest.Add(Src[i]);
  end;
end;

function FindBakeBlockIndex(const FileIn: TMufFile; const Name: string): Integer; forward;
function FindToolBlockIndex(const FileIn: TMufFile; const Name: string): Integer; forward;

function TopoOrderBakes(const FileIn: TMufFile; const Targets: TStringList; Order: TStringList): Boolean;
var
  Visiting: TStringList;
  Visited: TStringList;
  ErrMsg: string;
  function Visit(const Name: string): Boolean;
  var
    Idx: Integer;
    Deps: TStringList;
    i: Integer;
  begin
    Result := False;
    if Visited.IndexOf(Name) >= 0 then begin
      Result := True;
      Exit;
    end;
    if Visiting.IndexOf(Name) >= 0 then begin
      ErrMsg := 'dependency cycle detected at bake `' + Name + '`';
      Exit;
    end;
    Idx := FindBakeBlockIndex(FileIn, Name);
    if Idx < 0 then begin
      ErrMsg := 'unknown bake `' + Name + '`';
      Exit;
    end;
    Visiting.Add(Name);
    Deps := CollectBakeDeps(FileIn.Blocks[Idx]);
    try
      for i := 0 to Deps.Count - 1 do begin
        if not Visit(Deps[i]) then
          Exit;
      end;
    finally
      Deps.Free;
    end;
    Visiting.Delete(Visiting.IndexOf(Name));
    Visited.Add(Name);
    Order.Add(Name);
    Result := True;
  end;
var
  i: Integer;
begin
  Result := False;
  Order.Clear;
  Visiting := TStringList.Create;
  Visited := TStringList.Create;
  try
    for i := 0 to Targets.Count - 1 do begin
      if not Visit(Targets[i]) then begin
        WriteLn('error: ', ErrMsg);
        Exit;
      end;
    end;
  finally
    Visiting.Free;
    Visited.Free;
  end;
  Result := True;
end;

function FindBakeBlockIndex(const FileIn: TMufFile; const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    if (FileIn.Blocks[i].Tag = 'bake') and (FileIn.Blocks[i].Name = Name) then begin
      Result := i;
      Exit;
    end;
  end;
end;

function BakeCachePath(const Root, BakeName: string): string;
var
  i: Integer;
  Name: string;
begin
  Name := '';
  for i := 1 to Length(BakeName) do begin
    if BakeName[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'] then
      Name := Name + BakeName[i]
    else
      Name := Name + '_';
  end;
  if Name = '' then
    Name := 'bake';
  Result := IncludeTrailingPathDelimiter(Root) + '.steel-cache' + PathDelim + 'run' + PathDelim + Name + '.fp';
end;

function ReadCacheFingerprint(const Path: string; out Value: QWord): Boolean;
var
  Lines: TStringList;
  i: Integer;
  Line, Hex: string;
begin
  Result := False;
  Value := 0;
  if not FileExists(Path) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Path);
    for i := 0 to Lines.Count - 1 do begin
      Line := Trim(Lines[i]);
      if Pos('fingerprint ', Line) = 1 then begin
        Hex := Trim(Copy(Line, 13, Length(Line)));
        Value := StrToQWordDef('$' + Hex, 0);
        Result := True;
        Exit;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure WriteCacheFingerprint(const Path: string; Value: QWord);
var
  DirPath: string;
  Text: TStringList;
begin
  DirPath := ExtractFileDir(Path);
  if (DirPath <> '') and (not DirectoryExists(DirPath)) then
    ForceDirectories(DirPath);
  Text := TStringList.Create;
  try
    Text.Add(Format('fingerprint %.16x', [Value]));
    Text.SaveToFile(Path);
  finally
    Text.Free;
  end;
end;

function Fnv1aInit: QWord;
begin
  Result := QWord($CBF29CE484222325);
end;

function Fnv1aUpdate(H: QWord; const S: string): QWord;
var
  i: Integer;
begin
  Result := H;
  for i := 1 to Length(S) do begin
    Result := Result xor QWord(Ord(S[i]));
    Result := Result * QWord($100000001B3);
  end;
end;

function Fnv1aUpdateU64(H: QWord; V: QWord): QWord;
var
  i: Integer;
  B: Byte;
begin
  Result := H;
  for i := 0 to 7 do begin
    B := Byte((V shr (i * 8)) and $FF);
    Result := Result xor QWord(B);
    Result := Result * QWord($100000001B3);
  end;
end;

procedure PutFileStamp(var H: QWord; const Path: string);
var
  Info: TSearchRec;
  MTime: TDateTime;
  Secs: Int64;
begin
  if FindFirst(Path, faAnyFile, Info) = 0 then begin
    H := Fnv1aUpdateU64(H, QWord(Info.Size));
    MTime := FileDateToDateTime(Info.Time);
    Secs := DateTimeToUnix(MTime);
    H := Fnv1aUpdateU64(H, QWord(Secs));
    FindClose(Info);
  end else begin
    H := Fnv1aUpdateU64(H, 0);
    H := Fnv1aUpdateU64(H, 0);
  end;
end;

function KindLabel(Kind: TDirectiveKind): string;
begin
  case Kind of
    dkSet: Result := 'set';
    dkExec: Result := 'exec';
    dkMake: Result := 'make';
    dkOutput: Result := 'output';
    dkTakes: Result := 'takes';
    dkEmits: Result := 'emits';
    dkRef: Result := 'ref';
    dkUse: Result := 'use';
    dkRun: Result := 'run';
    dkNeeds: Result := 'needs';
  else
    Result := 'unknown';
  end;
end;

procedure HashDirective(var H: QWord; const Dir: TDirective);
var
  i: Integer;
begin
  H := Fnv1aUpdate(H, KindLabel(Dir.Kind));
  H := Fnv1aUpdate(H, Dir.Name);
  for i := 0 to Length(Dir.Args) - 1 do
    H := Fnv1aUpdate(H, Dir.Args[i]);
end;

function BakeFingerprint(const FileIn: TMufFile; const Block: TBlock; const Res: TResolvedConfig; const Sources: TStringList; const OutputRel: string): QWord;
var
  H: QWord;
  i, j, k: Integer;
  Dir: TDirective;
  ToolIdx: Integer;
  ToolExec: string;
  Sorted: TStringList;
begin
  H := Fnv1aInit;
  H := Fnv1aUpdate(H, 'bake');
  H := Fnv1aUpdate(H, Block.Name);
  H := Fnv1aUpdate(H, OutputRel);
  H := Fnv1aUpdate(H, Res.SelectedProfile);
  for i := 0 to Res.Vars.Count - 1 do begin
    H := Fnv1aUpdate(H, Res.Vars.Names[i]);
    H := Fnv1aUpdate(H, Res.Vars.ValueFromIndex[i]);
  end;
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    HashDirective(H, Dir);
    if Dir.Kind = dkUse then begin
      for j := 0 to Length(Dir.Args) - 1 do begin
        ToolIdx := FindToolBlockIndex(FileIn, Dir.Args[j]);
        if ToolIdx >= 0 then begin
          ToolExec := '';
          for k := 0 to Length(FileIn.Blocks[ToolIdx].Directives) - 1 do begin
            if FileIn.Blocks[ToolIdx].Directives[k].Kind = dkExec then begin
              if Length(FileIn.Blocks[ToolIdx].Directives[k].Args) > 0 then
                ToolExec := FileIn.Blocks[ToolIdx].Directives[k].Args[0];
              Break;
            end;
          end;
          H := Fnv1aUpdate(H, ToolExec);
          if ToolExec <> '' then
            PutFileStamp(H, ToolExec);
        end;
      end;
    end;
  end;
  for i := 0 to Length(Block.Children) - 1 do begin
    if FileIn.Blocks[Block.Children[i]].Tag = 'run' then begin
      for j := 0 to Length(FileIn.Blocks[Block.Children[i]].Directives) - 1 do begin
        Dir := FileIn.Blocks[Block.Children[i]].Directives[j];
        HashDirective(H, Dir);
      end;
    end;
  end;
  Sorted := TStringList.Create;
  try
    Sorted.AddStrings(Sources);
    Sorted.Sort;
    for i := 0 to Sorted.Count - 1 do begin
      H := Fnv1aUpdate(H, Sorted[i]);
      PutFileStamp(H, IncludeTrailingPathDelimiter(Res.WorkspaceRoot) + Sorted[i]);
    end;
  finally
    Sorted.Free;
  end;
  Result := H;
end;

function ShouldSkipMtime(const OutputAbs: string; const Sources: TStringList; const Root: string): Boolean;
var
  OutInfo, InInfo: TSearchRec;
  OutTime, InTime: Integer;
  i: Integer;
  Path: string;
begin
  Result := False;
  if FindFirst(OutputAbs, faAnyFile, OutInfo) <> 0 then
    Exit;
  OutTime := OutInfo.Time;
  FindClose(OutInfo);
  for i := 0 to Sources.Count - 1 do begin
    Path := IncludeTrailingPathDelimiter(Root) + Sources[i];
    if FindFirst(Path, faAnyFile, InInfo) <> 0 then
      Exit;
    InTime := InInfo.Time;
    FindClose(InInfo);
    if InTime > OutTime then
      Exit;
  end;
  Result := True;
end;

function ResolveExecutable(const Cmd: string): string;
begin
  if (Pos(PathDelim, Cmd) > 0) or (Pos('/', Cmd) > 0) then
    Result := Cmd
  else
    Result := FindInPath(Cmd);
end;

function TouchFile(const Path: string; out ErrMsg: string): Boolean;
var
  F: TextFile;
  DirPath: string;
  Code: Integer;
begin
  Result := False;
  ErrMsg := '';
  DirPath := ExtractFileDir(Path);
  if (DirPath <> '') and (not DirectoryExists(DirPath)) then
    ForceDirectories(DirPath);
  AssignFile(F, Path);
  {$I-}
  Rewrite(F);
  {$I+}
  Code := IOResult;
  if Code = 0 then begin
    CloseFile(F);
    Result := True;
  end else begin
    ErrMsg := SysErrorMessage(Code);
  end;
end;

function ExecuteExecDirective(const Dir: TDirective; Vars: TStringList; const WorkDir: string; DoRun: Boolean; Verbose: Boolean; const ContextLabel: string; out CmdShown: string; out OutText: string; out ErrText: string; out DurationMs: QWord): Integer;
var
  Proc: TProcess;
  Cmd, ResolvedCmd, Arg, Shell: string;
  i: Integer;
  OutputBuf: TStringStream;
  ErrorBuf: TStringStream;
  RunStdout: Boolean;
  CmdLine: string;
  StartTick: QWord;
begin
  Result := 0;
  CmdShown := '';
  OutText := '';
  ErrText := '';
  DurationMs := 0;
  if Length(Dir.Args) = 0 then
    Exit;
  Cmd := ExpandVars(Dir.Args[0], Vars);
  CmdShown := Cmd;
  if not DoRun then begin
    WriteLn('dry-run: exec ', Cmd);
    for i := 1 to Length(Dir.Args) - 1 do begin
      Arg := ExpandVars(Dir.Args[i], Vars);
      WriteLn('dry-run: arg ', Arg);
    end;
    Exit;
  end;
  Proc := TProcess.Create(nil);
  try
    if (Length(Dir.Args) = 1) and (Pos(' ', Cmd) > 0) then begin
      Shell := GetEnvironmentVariable('COMSPEC');
      if Shell = '' then
        Shell := '/bin/sh';
      ResolvedCmd := ResolveExecutable(Shell);
      if ResolvedCmd = '' then begin
        WriteLn('error: shell not found: ', Shell);
        Exit(127);
      end;
      Proc.Executable := ResolvedCmd;
      if Pos('cmd', LowerCase(ExtractFileName(Shell))) = 1 then begin
        Proc.Parameters.Add('/C');
        Proc.Parameters.Add(Cmd);
      end else begin
        Proc.Parameters.Add('-c');
        Proc.Parameters.Add(Cmd);
      end;
    end else begin
      ResolvedCmd := ResolveExecutable(Cmd);
      if ResolvedCmd = '' then begin
        WriteLn('error: command not found: ', Cmd);
        Exit(127);
      end;
      Proc.Executable := ResolvedCmd;
      for i := 1 to Length(Dir.Args) - 1 do begin
        Arg := ExpandVars(Dir.Args[i], Vars);
        Proc.Parameters.Add(Arg);
      end;
    end;
    if Verbose then begin
      CmdLine := Cmd;
      for i := 1 to Length(Dir.Args) - 1 do begin
        Arg := ExpandVars(Dir.Args[i], Vars);
        CmdLine := CmdLine + ' ' + Arg;
      end;
      if (ContextLabel <> '') and (Pos('tool ', ContextLabel) = 1) then begin
        { skip tool/run step lines to match Rust output }
      end else if ContextLabel <> '' then
        WriteLn('run ', ContextLabel, ': ', CmdLine)
      else
        WriteLn('run: ', CmdLine);
    end;
    Proc.Options := [poWaitOnExit, poUsePipes];
    if WorkDir <> '' then
      Proc.CurrentDirectory := WorkDir;
    StartTick := GetTickCount64;
    Proc.Execute;
    OutputBuf := TStringStream.Create('');
    ErrorBuf := TStringStream.Create('');
    try
      OutputBuf.CopyFrom(Proc.Output, 0);
      ErrorBuf.CopyFrom(Proc.Stderr, 0);
      OutText := OutputBuf.DataString;
      ErrText := ErrorBuf.DataString;
      RunStdout := (GetEnvironmentVariable('MUFFIN_RUN_STDOUT') <> '') and DoRun;
      if RunStdout then begin
        if OutText <> '' then
          Write(OutText);
        if ErrText <> '' then
          Write(ErrText);
      end;
    finally
      ErrorBuf.Free;
      OutputBuf.Free;
    end;
    DurationMs := GetTickCount64 - StartTick;
    Result := Proc.ExitStatus;
    if Result <> 0 then begin
      WriteLn('error: exec failed (', Result, '): ', Cmd);
    end;
  finally
    Proc.Free;
  end;
end;

function HasGlob(const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do begin
    if (S[i] = '*') or (S[i] = '?') or (S[i] = '[') then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure AddPathMatch(const BaseDir, Pattern: string; List: TStringList);
var
  SR: TSearchRec;
  SearchDir: string;
begin
  SearchDir := BaseDir;
  if SearchDir = '' then
    SearchDir := '.';
  if FindFirst(IncludeTrailingPathDelimiter(SearchDir) + Pattern, faAnyFile, SR) = 0 then begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      List.Add(IncludeTrailingPathDelimiter(SearchDir) + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function IsAbsolutePath(const Path: string): Boolean;
begin
  Result := False;
  if Path = '' then
    Exit;
  if (Length(Path) >= 1) and (Path[1] = PathDelim) then begin
    Result := True;
    Exit;
  end;
  if (Length(Path) >= 2) and (Path[2] = ':') then begin
    Result := True;
    Exit;
  end;
end;

function ResolveTargetDir(const Res: TResolvedConfig): string;
begin
  if (Res.WorkspaceTargetDir <> '') and IsAbsolutePath(Res.WorkspaceTargetDir) then
    Result := Res.WorkspaceTargetDir
  else
    Result := IncludeTrailingPathDelimiter(Res.WorkspaceRoot) + Res.WorkspaceTargetDir;
end;

function ExpandPaths(const RawPath, TargetDir: string; List: TStringList): Integer;
var
  Path, DirPart, NamePart: string;
  Temp: TStringList;
begin
  Result := 0;
  if RawPath = '' then
    Exit;
  if IsAbsolutePath(RawPath) then
    Path := RawPath
  else
    Path := IncludeTrailingPathDelimiter(TargetDir) + RawPath;
  if HasGlob(Path) then begin
    Temp := TStringList.Create;
    try
      DirPart := ExtractFileDir(Path);
      NamePart := ExtractFileName(Path);
      AddPathMatch(DirPart, NamePart, Temp);
      if Temp.Count > 0 then begin
        List.AddStrings(Temp);
        Result := Temp.Count;
      end;
    finally
      Temp.Free;
    end;
  end else begin
    if FileExists(Path) or DirectoryExists(Path) then begin
      List.Add(Path);
      Result := 1;
    end;
  end;
end;

function ExpandOutputPath(const RawPath, TargetDir: string; List: TStringList): Integer;
var
  Path: string;
begin
  Result := 0;
  if RawPath = '' then
    Exit;
  if IsAbsolutePath(RawPath) then
    Path := RawPath
  else
    Path := IncludeTrailingPathDelimiter(TargetDir) + RawPath;
  List.Add(Path);
  Result := 1;
end;

function RelativizePath(const Root, Path: string): string;
var
  RootAbs, PathAbs: string;
begin
  Result := Path;
  if (Root = '') or (Path = '') then
    Exit;
  RootAbs := IncludeTrailingPathDelimiter(ExpandFileName(Root));
  PathAbs := ExpandFileName(Path);
  if Pos(RootAbs, PathAbs) = 1 then
    Result := ExtractRelativePath(RootAbs, PathAbs);
end;

procedure GatherBakeSources(const FileIn: TMufFile; const Block: TBlock; const Res: TResolvedConfig; Sources: TStringList; out OutputRel: string);
var
  i, j: Integer;
  Dir: TDirective;
  Path: string;
  Paths: TStringList;
begin
  OutputRel := '';
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    if Dir.Kind = dkMake then begin
      Paths := TStringList.Create;
      try
        for j := 0 to Length(Dir.Args) - 1 do begin
          Path := ExpandVars(Dir.Args[j], Res.Vars);
          ExpandPaths(Path, Res.WorkspaceRoot, Paths);
        end;
        for j := 0 to Paths.Count - 1 do
          Sources.Add(RelativizePath(Res.WorkspaceRoot, Paths[j]));
      finally
        Paths.Free;
      end;
    end else if (Dir.Kind = dkOutput) and (OutputRel = '') then begin
      if Length(Dir.Args) > 0 then begin
        Path := ExpandVars(Dir.Args[0], Res.Vars);
        if IsAbsolutePath(Path) then
          OutputRel := RelativizePath(Res.WorkspaceRoot, Path)
        else
          OutputRel := Path;
      end;
    end;
  end;
end;

procedure GatherBakeSummary(const FileIn: TMufFile; const Block: TBlock; const Res: TResolvedConfig; out SourceCount: Integer; out OutputPath: string);
var
  i, j: Integer;
  Dir: TDirective;
  Path: string;
  Paths: TStringList;
begin
  SourceCount := 0;
  OutputPath := '';
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    if Dir.Kind = dkMake then begin
      Paths := TStringList.Create;
      try
        for j := 0 to Length(Dir.Args) - 1 do begin
          Path := ExpandVars(Dir.Args[j], Res.Vars);
          Inc(SourceCount, ExpandPaths(Path, Res.WorkspaceRoot, Paths));
        end;
      finally
        Paths.Free;
      end;
    end else if (Dir.Kind = dkOutput) and (OutputPath = '') then begin
      if Length(Dir.Args) > 0 then begin
        Path := ExpandVars(Dir.Args[0], Res.Vars);
        if IsAbsolutePath(Path) then
          OutputPath := Path
        else
          OutputPath := IncludeTrailingPathDelimiter(ResolveTargetDir(Res)) + Path;
        OutputPath := RelativizePath(Res.WorkspaceRoot, OutputPath);
      end;
    end;
  end;
end;

function BlockLabel(const Block: TBlock): string;
begin
  if Block.Name <> '' then
    Result := Block.Tag + ' ' + Block.Name
  else
    Result := Block.Tag;
end;

function BakeContextLabel(const FileIn: TMufFile; const Block: TBlock): string;
var
  ParentIndex: Integer;
begin
  if Block.Tag = 'bake' then
    Exit(Block.Name);
  ParentIndex := Block.Parent;
  while (ParentIndex >= 0) and (FileIn.Blocks[ParentIndex].Tag <> 'bake') do
    ParentIndex := FileIn.Blocks[ParentIndex].Parent;
  if ParentIndex >= 0 then
    Result := FileIn.Blocks[ParentIndex].Name
  else
    Result := Block.Name;
end;

function EscapeLog(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;

function GetFileSize(const Path: string): Int64;
var
  FS: TFileStream;
begin
  Result := 0;
  if not FileExists(Path) then
    Exit;
  FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    Result := FS.Size;
  finally
    FS.Free;
  end;
end;

procedure EnsureRunLogHeader(const Path: string);
var
  F: TextFile;
begin
  if FileExists(Path) then begin
    if GetFileSize(Path) > 0 then
      Exit;
  end;
  if ExtractFileDir(Path) <> '' then
    ForceDirectories(ExtractFileDir(Path));
  AssignFile(F, Path);
  if FileExists(Path) then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, '[log meta]');
    WriteLn(F, 'format "steel-runlog-1"');
    WriteLn(F, 'tool "steel"');
    WriteLn(F, 'version "', STEEL_VERSION, '"');
    WriteLn(F, 'ts_iso "', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', Now), '"');
    WriteLn(F, '..');
  finally
    CloseFile(F);
  end;
end;

procedure AppendBakeLogStart(const Path, BakeName: string);
var
  F: TextFile;
begin
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, '[bake log "', EscapeLog(BakeName), '"]');
  finally
    CloseFile(F);
  end;
end;

procedure AppendBakeLogSources(const Path, OutputRel: string; Sources: TStringList);
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, 'output "', EscapeLog(OutputRel), '"');
    WriteLn(F, 'sources_count ', Sources.Count);
    for i := 0 to Sources.Count - 1 do
      WriteLn(F, 'source "', EscapeLog(Sources[i]), '"');
  finally
    CloseFile(F);
  end;
end;

procedure AppendBakeLogSummary(const Path: string; Runs: Integer; DurationMs: QWord);
var
  F: TextFile;
begin
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, 'runs ', Runs);
    WriteLn(F, 'duration_ms ', DurationMs);
  finally
    CloseFile(F);
  end;
end;

procedure AppendBakeLogEnd(const Path: string);
var
  F: TextFile;
begin
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, '..');
  finally
    CloseFile(F);
  end;
end;

procedure AppendRunLog(const Path, Cmd, Cwd: string; Status: Integer; const OutText, ErrText: string; DurationMs: QWord);
var
  F: TextFile;
  OkStr: string;
begin
  if Status = 0 then
    OkStr := 'true'
  else
    OkStr := 'false';
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, '[run log]');
    WriteLn(F, 'ts ', DateTimeToUnix(Now));
    WriteLn(F, 'ts_iso "', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', Now), '"');
    WriteLn(F, 'duration_ms ', DurationMs);
    WriteLn(F, 'cmd "', EscapeLog(Cmd), '"');
    WriteLn(F, 'cwd "', EscapeLog(Cwd), '"');
    WriteLn(F, 'status ', Status);
    WriteLn(F, 'ok ', OkStr);
    WriteLn(F, 'stdout_bytes ', Length(OutText));
    WriteLn(F, 'stderr_bytes ', Length(ErrText));
    if OutText <> '' then
      WriteLn(F, 'stdout "', EscapeLog(OutText), '"');
    if ErrText <> '' then
      WriteLn(F, 'stderr "', EscapeLog(ErrText), '"');
    WriteLn(F, '..');
  finally
    CloseFile(F);
  end;
end;

procedure AppendRunSummary(const Path: string);
var
  F: TextFile;
begin
  AssignFile(F, Path);
  Append(F);
  try
    WriteLn(F, '[run summary]');
    WriteLn(F, 'ts_iso "', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', Now), '"');
    WriteLn(F, '..');
  finally
    CloseFile(F);
  end;
end;
function ExecuteBlockDirectives(const FileIn: TMufFile; const Block: TBlock; const Res: TResolvedConfig; DoRun: Boolean; Verbose: Boolean; var Runs: Integer; var TotalDurationMs: QWord): Integer;
var
  i, j: Integer;
  Dir: TDirective;
  Status: Integer;
  Path: string;
  Paths: TStringList;
  ToolIndex: Integer;
  ErrMsg: string;
  CmdShown: string;
  OutText: string;
  ErrText: string;
  DurationMs: QWord;
  CmdLine: string;
  TotalMatches: Integer;
begin
  Result := 0;
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    if Dir.Kind = dkExec then begin
      if (Block.Tag = 'bake') or (Block.Tag = 'run') then
        Status := ExecuteExecDirective(Dir, Res.Vars, Res.WorkspaceRoot, DoRun, Verbose, BakeContextLabel(FileIn, Block), CmdShown, OutText, ErrText, DurationMs)
      else
        Status := ExecuteExecDirective(Dir, Res.Vars, Res.WorkspaceRoot, DoRun, Verbose, BlockLabel(Block), CmdShown, OutText, ErrText, DurationMs);
      if (RunLogPath <> '') and DoRun and ((Block.Tag = 'bake') or (Block.Tag = 'run')) then begin
        CmdLine := CmdShown;
        for j := 1 to Length(Dir.Args) - 1 do
          CmdLine := CmdLine + ' ' + ExpandVars(Dir.Args[j], Res.Vars);
        AppendRunLog(RunLogPath, CmdLine, Res.WorkspaceRoot, Status, OutText, ErrText, DurationMs);
      end;
      if DoRun then begin
        Inc(Runs);
        Inc(TotalDurationMs, DurationMs);
      end;
      if Status <> 0 then begin
        WriteLn('error: in ', BlockLabel(Block), ': exec ', CmdShown);
        Result := Status;
        Exit;
      end;
    end else if Dir.Kind = dkUse then begin
      for j := 0 to Length(Dir.Args) - 1 do begin
        ToolIndex := FindToolBlockIndex(FileIn, Dir.Args[j]);
        if ToolIndex < 0 then begin
          WriteLn('error: in ', BlockLabel(Block), ': tool not found: ', Dir.Args[j]);
          Result := 1;
          Exit;
        end;
        Status := ExecuteBlockDirectives(FileIn, FileIn.Blocks[ToolIndex], Res, DoRun, Verbose, Runs, TotalDurationMs);
        if Status <> 0 then begin
          WriteLn('error: tool failed: ', Dir.Args[j]);
          Result := Status;
          Exit;
        end;
      end;
    end else if (Dir.Kind = dkMake) or (Dir.Kind = dkOutput) then begin
      Paths := TStringList.Create;
      try
        TotalMatches := 0;
        for j := 0 to Length(Dir.Args) - 1 do begin
          Path := ExpandVars(Dir.Args[j], Res.Vars);
          if (Dir.Kind = dkOutput) and HasGlob(Path) then begin
            WriteLn('error: in ', BlockLabel(Block), ': .output does not allow globs: ', Path);
            Result := 1;
            Exit;
          end;
          if Dir.Kind = dkMake then
            Inc(TotalMatches, ExpandPaths(Path, Res.WorkspaceRoot, Paths))
          else
            Inc(TotalMatches, ExpandOutputPath(Path, ResolveTargetDir(Res), Paths));
        end;
        if TotalMatches = 0 then begin
          WriteLn('error: in ', BlockLabel(Block), ': no matches for .make/.output');
          Result := 1;
          Exit;
        end;
        if Dir.Kind = dkOutput then begin
          for j := 0 to Paths.Count - 1 do begin
            if not DoRun then begin
              WriteLn('dry-run: file ', Paths[j]);
            end else if not TouchFile(Paths[j], ErrMsg) then begin
              WriteLn('error: failed to create file: ', Paths[j], ' (', ErrMsg, ')');
              Result := 1;
              Exit;
            end;
          end;
        end else begin
          for j := 0 to Paths.Count - 1 do begin
            if not DoRun then
              WriteLn('dry-run: source ', Paths[j]);
          end;
        end;
      finally
        Paths.Free;
      end;
    end;
  end;
end;

function FindToolBlockIndex(const FileIn: TMufFile; const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    if (FileIn.Blocks[i].Tag = 'tool') and (FileIn.Blocks[i].Name = Name) then begin
      Result := i;
      Exit;
    end;
  end;
end;

function RunBlockRecursive(const FileIn: TMufFile; BlockIndex: Integer; const Res: TResolvedConfig; DoRun: Boolean; Verbose: Boolean; var Runs: Integer; var TotalDurationMs: QWord): Integer;
var
  Block, Child: TBlock;
  i: Integer;
  SourceCount: Integer;
  OutputPath: string;
  ParentIndex: Integer;
begin
  Result := 0;
  if (BlockIndex < 0) or (BlockIndex >= Length(FileIn.Blocks)) then
    Exit(1);
  Block := FileIn.Blocks[BlockIndex];
  if Block.Tag = 'run' then begin
    { no per-step header to match Rust output }
  end;
  if Verbose and (Block.Tag = 'run') then begin
    ParentIndex := Block.Parent;
    while (ParentIndex >= 0) and (FileIn.Blocks[ParentIndex].Tag <> 'bake') do
      ParentIndex := FileIn.Blocks[ParentIndex].Parent;
    if ParentIndex >= 0 then
      GatherBakeSummary(FileIn, FileIn.Blocks[ParentIndex], Res, SourceCount, OutputPath);
  end;
  Result := ExecuteBlockDirectives(FileIn, Block, Res, DoRun, Verbose, Runs, TotalDurationMs);
  if Result <> 0 then
    Exit;
  for i := 0 to Length(Block.Children) - 1 do begin
    Child := FileIn.Blocks[Block.Children[i]];
    if Child.Tag = 'run' then begin
      Result := RunBlockRecursive(FileIn, Block.Children[i], Res, DoRun, Verbose, Runs, TotalDurationMs);
      if Result <> 0 then
        Exit;
    end;
  end;
end;

function RunBake(const FileIn: TMufFile; BakeIndex: Integer; const Res: TResolvedConfig; DoRun: Boolean; Verbose: Boolean; var Runs: Integer; var TotalDurationMs: QWord): Integer;
var
  Block: TBlock;
  i: Integer;
  SourceCount: Integer;
  OutputPath: string;
begin
  Result := 0;
  if (BakeIndex < 0) or (BakeIndex >= Length(FileIn.Blocks)) then
    Exit(1);
  Block := FileIn.Blocks[BakeIndex];
  if Verbose then begin
    GatherBakeSummary(FileIn, Block, Res, SourceCount, OutputPath);
    if OutputPath <> '' then
      WriteLn('bake ', Block.Name, ': ', SourceCount, ' sources -> ', OutputPath)
    else
      WriteLn('bake ', Block.Name, ': ', SourceCount, ' sources');
  end;
  Result := ExecuteBlockDirectives(FileIn, Block, Res, DoRun, Verbose, Runs, TotalDurationMs);
  if Result <> 0 then
    Exit;
  for i := 0 to Length(Block.Children) - 1 do begin
    if FileIn.Blocks[Block.Children[i]].Tag = 'run' then begin
      Result := RunBlockRecursive(FileIn, Block.Children[i], Res, DoRun, Verbose, Runs, TotalDurationMs);
      if Result <> 0 then
        Exit;
    end;
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
  BlockIndex: Integer;
  BakeIndex: Integer;
  Order: TStringList;
  Sources: TStringList;
  OutputRel: string;
  OutputAbs: string;
  CachePath: string;
  CachedFp: QWord;
  Fingerprint: QWord;
  Runs: Integer;
  DurationMs: QWord;
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
    Order := TStringList.Create;
    try
      if (LogMode <> 'append') and (LogMode <> 'truncate') then begin
        WriteLn('error: invalid --log-mode ', LogMode);
        FreeOptions;
        Halt(1);
      end;
      if PrintOnly then
        PrintSummary(Res);
      if RunAll then
        CollectAllBakes(FileIn, TargetsToRun)
      else if BakeList.Count > 0 then
        TargetsToRun.AddStrings(BakeList);
      if TargetsToRun.Count = 0 then begin
        BlockIndex := FindBakeBlockIndex(FileIn, 'app');
        if BlockIndex >= 0 then
          TargetsToRun.Add('app')
        else begin
          for BakeIndex := 0 to Length(FileIn.Blocks) - 1 do begin
            if FileIn.Blocks[BakeIndex].Tag = 'bake' then begin
              TargetsToRun.Add(FileIn.Blocks[BakeIndex].Name);
              Break;
            end;
          end;
        end;
      end;
      if TargetsToRun.Count = 0 then begin
        WriteLn('run: no bakes selected');
        FreeOptions;
        Halt(1);
      end;

      if LogPath = '' then
        LogPath := IncludeTrailingPathDelimiter(RootPath) + 'target' + PathDelim + 'steel_run.mff';
      RunLogPath := LogPath;
      if LogMode = 'truncate' then begin
        if FileExists(RunLogPath) then
          DeleteFile(RunLogPath);
      end;
      EnsureRunLogHeader(RunLogPath);

      if not TopoOrderBakes(FileIn, TargetsToRun, Order) then begin
        FreeOptions;
        Halt(1);
      end;

      for BlockIndex := 0 to Order.Count - 1 do begin
        BakeIndex := FindBakeBlockIndex(FileIn, Order[BlockIndex]);
        if BakeIndex < 0 then begin
          WriteLn('error: bake not found: ', Order[BlockIndex]);
          FreeOptions;
          Halt(1);
        end;
        Sources := TStringList.Create;
        try
          GatherBakeSources(FileIn, FileIn.Blocks[BakeIndex], Res, Sources, OutputRel);
          if Sources.Count = 0 then begin
            WriteLn('error: bake `', Order[BlockIndex], '`: no sources matched');
            FreeOptions;
            Halt(1);
          end;
          if OutputRel = '' then begin
            WriteLn('error: bake `', Order[BlockIndex], '`: missing .output');
            FreeOptions;
            Halt(1);
          end;
          if IsAbsolutePath(OutputRel) then
            OutputAbs := OutputRel
          else
            OutputAbs := IncludeTrailingPathDelimiter(ResolveTargetDir(Res)) + OutputRel;

          if not DryRun then begin
            if not NoCache then begin
              CachePath := BakeCachePath(Res.WorkspaceRoot, Order[BlockIndex]);
              Fingerprint := BakeFingerprint(FileIn, FileIn.Blocks[BakeIndex], Res, Sources, OutputRel);
              if FileExists(OutputAbs) and ReadCacheFingerprint(CachePath, CachedFp) and (CachedFp = Fingerprint) then begin
                if Verbose then
                  WriteLn('skip ', Order[BlockIndex], ' (up to date)');
                Continue;
              end;
              if FileExists(OutputAbs) and ShouldSkipMtime(OutputAbs, Sources, Res.WorkspaceRoot) then begin
                if Verbose then
                  WriteLn('skip ', Order[BlockIndex], ' (up to date)');
                Continue;
              end;
            end;
          end;

          if not DryRun then begin
            AppendBakeLogStart(RunLogPath, Order[BlockIndex]);
            AppendBakeLogSources(RunLogPath, OutputRel, Sources);
          end;

          Runs := 0;
          DurationMs := 0;
          if RunBake(FileIn, BakeIndex, Res, not DryRun, Verbose, Runs, DurationMs) <> 0 then begin
            WriteLn('error: bake failed: ', Order[BlockIndex]);
            FreeOptions;
            Halt(1);
          end;

          if not DryRun then begin
            AppendBakeLogSummary(RunLogPath, Runs, DurationMs);
            AppendBakeLogEnd(RunLogPath);
          end;

          if not DryRun and (not NoCache) then begin
            WriteCacheFingerprint(CachePath, Fingerprint);
          end;
        finally
          Sources.Free;
        end;
      end;

      if not DryRun then
        AppendRunSummary(RunLogPath);
    finally
      TargetsToRun.Free;
      Order.Free;
    end;
    FreeOptions;
    Halt(0);
  end;

  FreeOptions;
end.
