unit steel_emit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, steel_types;

procedure EmitSteelLog(const Path: string; const FileIn: TMufFile; const Res: TResolvedConfig);
procedure EmitRunLog(const Path: string; const FileIn: TMufFile; const Res: TResolvedConfig; const BakeNames: TStringList; AppendMode: Boolean);

implementation

uses
  steel_utils;

procedure EmitBlockDirectives(var F: TextFile; const Block: TBlock; Indent: string; Vars: TStringList);
var
  i, j: Integer;
  Dir: TDirective;
  Line: string;
begin
  for i := 0 to Length(Block.Directives) - 1 do begin
    Dir := Block.Directives[i];
    if Dir.Kind = dkSet then begin
      Line := Indent + '.set ' + QuoteIfNeeded(Dir.Name);
      if Length(Dir.Args) > 0 then
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[0], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkExec then begin
      Line := Indent + '.exec';
      if Length(Dir.Args) > 0 then
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[0], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkMake then begin
      Line := Indent + '.make';
      for j := 0 to Length(Dir.Args) - 1 do
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[j], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkOutput then begin
      Line := Indent + '.output';
      for j := 0 to Length(Dir.Args) - 1 do
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[j], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkTakes then begin
      Line := Indent + '.takes';
      for j := 0 to Length(Dir.Args) - 1 do
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[j], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkEmits then begin
      Line := Indent + '.emits';
      for j := 0 to Length(Dir.Args) - 1 do
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[j], Vars));
      WriteLn(F, Line);
    end else if Dir.Kind = dkRef then begin
      Line := Indent + '.ref';
      for j := 0 to Length(Dir.Args) - 1 do
        Line := Line + ' ' + QuoteIfNeeded(ExpandVars(Dir.Args[j], Vars));
      WriteLn(F, Line);
    end else begin
      Line := Indent + Dir.Raw;
      WriteLn(F, Line);
    end;
  end;
end;

procedure EmitBlocksDump(var F: TextFile; const FileIn: TMufFile; Vars: TStringList);
var
  i: Integer;
  TagLine: string;
begin
  if Length(FileIn.Blocks) = 0 then
    Exit;
  WriteLn(F, '');
  WriteLn(F, '[blocks]');
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    TagLine := '  block ' + FileIn.Blocks[i].Tag;
    if FileIn.Blocks[i].Name <> '' then
      TagLine := TagLine + ' ' + QuoteIfNeeded(FileIn.Blocks[i].Name);
    WriteLn(F, TagLine);
    EmitBlockDirectives(F, FileIn.Blocks[i], '    ', Vars);
  end;
  WriteLn(F, '..');
end;

procedure EmitTools(var F: TextFile; const FileIn: TMufFile; Vars: TStringList);
var
  i: Integer;
  Block: TBlock;
begin
  WriteLn(F, '');
  WriteLn(F, '[tools]');
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if Block.Tag = 'tool' then begin
      WriteLn(F, '  tool ', QuoteIfNeeded(Block.Name));
      EmitBlockDirectives(F, Block, '    ', Vars);
    end;
  end;
  WriteLn(F, '..');
end;

procedure EmitExports(var F: TextFile; const FileIn: TMufFile; Vars: TStringList);
var
  i: Integer;
  Block: TBlock;
begin
  WriteLn(F, '');
  WriteLn(F, '[exports]');
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if Block.Tag = 'export' then
      EmitBlockDirectives(F, Block, '  ', Vars);
  end;
  WriteLn(F, '..');
end;

procedure EmitBakes(var F: TextFile; const FileIn: TMufFile; Vars: TStringList);
var
  i, j: Integer;
  Block, Child: TBlock;
begin
  WriteLn(F, '');
  WriteLn(F, '[bakes]');
  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if Block.Tag = 'bake' then begin
      WriteLn(F, '  bake ', QuoteIfNeeded(Block.Name));
      EmitBlockDirectives(F, Block, '    ', Vars);
      for j := 0 to Length(Block.Children) - 1 do begin
        Child := FileIn.Blocks[Block.Children[j]];
        if Child.Tag = 'run' then begin
          WriteLn(F, '    run ', QuoteIfNeeded(Child.Name));
          EmitBlockDirectives(F, Child, '      ', Vars);
        end;
      end;
    end;
  end;
  WriteLn(F, '..');
end;

procedure EmitSteelLog(const Path: string; const FileIn: TMufFile; const Res: TResolvedConfig);
var
  F: TextFile;
  Root: string;
begin
  Root := Res.WorkspaceRoot;
  AssignFile(F, Path);
  Rewrite(F);
  try
    WriteLn(F, 'mff 1');
    WriteLn(F, '');
    WriteLn(F, '[host]');
    WriteLn(F, '  os "', Res.HostOS, '"');
    WriteLn(F, '  arch "', Res.HostArch, '"');
    WriteLn(F, '..');
    WriteLn(F, '');
    WriteLn(F, '[workspace]');
    WriteLn(F, '  name "', Res.WorkspaceName, '"');
    WriteLn(F, '  root "', Root, '"');
    WriteLn(F, '  target_dir "', Res.WorkspaceTargetDir, '"');
    WriteLn(F, '..');
    WriteLn(F, '');
    WriteLn(F, 'profile "', Res.SelectedProfile, '"');
    WriteLn(F, '');
    WriteLn(F, '[target]');
    WriteLn(F, '  name "', Res.SelectedTarget, '"');
    WriteLn(F, '..');
    WriteLn(F, '');
    WriteLn(F, '[paths]');
    WriteLn(F, '  root "', Root, '"');
    WriteLn(F, '  dist "', IncludeTrailingPathDelimiter(Root) + 'dist', '"');
    WriteLn(F, '  cache "', IncludeTrailingPathDelimiter(Root) + '.steel-cache', '"');
    WriteLn(F, '..');
    EmitTools(F, FileIn, Res.Vars);
    EmitBakes(F, FileIn, Res.Vars);
    EmitExports(F, FileIn, Res.Vars);
    EmitBlocksDump(F, FileIn, Res.Vars);
  finally
    CloseFile(F);
  end;
end;

procedure EmitRunLog(const Path: string; const FileIn: TMufFile; const Res: TResolvedConfig; const BakeNames: TStringList; AppendMode: Boolean);
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, Path);
  if AppendMode and FileExists(Path) then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, 'mff 1');
    WriteLn(F, '');
    WriteLn(F, '[run]');
    WriteLn(F, '  profile "', Res.SelectedProfile, '"');
    WriteLn(F, '  target "', Res.SelectedTarget, '"');
    WriteLn(F, '  bakes');
    for i := 0 to BakeNames.Count - 1 do
      WriteLn(F, '    ', QuoteIfNeeded(BakeNames[i]));
    WriteLn(F, '..');
  finally
    CloseFile(F);
  end;
end;

end.
