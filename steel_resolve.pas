unit steel_resolve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, steel_types;

type
  TResolveOptions = record
    OverrideProfile: string;
    OverrideTarget: string;
    OverrideOS: string;
    OverrideArch: string;
    Defines: TStringList;
  end;

procedure InitResolveOptions(out Opt: TResolveOptions);
procedure ResolveConfig(const FileIn: TMufFile; const Opt: TResolveOptions; out Res: TResolvedConfig);

implementation

uses
  steel_utils;

procedure InitResolveOptions(out Opt: TResolveOptions);
begin
  Opt.OverrideProfile := '';
  Opt.OverrideTarget := '';
  Opt.OverrideOS := '';
  Opt.OverrideArch := '';
  Opt.Defines := TStringList.Create;
  Opt.Defines.NameValueSeparator := '=';
end;

procedure ResolveConfig(const FileIn: TMufFile; const Opt: TResolveOptions; out Res: TResolvedConfig);
var
  i, j: Integer;
  Block: TBlock;
  Dir: TDirective;
  Key, Val: string;
  ProfileName: string;
  TargetName: string;
begin
  Res.WorkspaceName := '';
  Res.WorkspaceRoot := '';
  Res.WorkspaceTargetDir := '';
  Res.SelectedProfile := '';
  Res.SelectedTarget := '';
  Res.DefaultTarget := '';
  Res.HostOS := '';
  Res.HostArch := '';
  Res.Vars := TStringList.Create;
  Res.Vars.NameValueSeparator := '=';

  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if Block.Tag = 'workspace' then begin
      for j := 0 to Length(Block.Directives) - 1 do begin
        Dir := Block.Directives[j];
        if Dir.Kind = dkSet then begin
          Key := Dir.Name;
          if Length(Dir.Args) > 0 then
            Val := Dir.Args[0]
          else
            Val := '';
          if Key = 'name' then
            Res.WorkspaceName := Val
          else if Key = 'root' then
            Res.WorkspaceRoot := Val
          else if Key = 'target_dir' then
            Res.WorkspaceTargetDir := Val
          else if Key = 'profile' then
            Res.SelectedProfile := Val;
          if Key <> '' then
            Res.Vars.Values[Key] := Val;
        end;
      end;
    end else if Block.Tag = 'default' then begin
      for j := 0 to Length(Block.Directives) - 1 do begin
        Dir := Block.Directives[j];
        if (Dir.Kind = dkSet) and (Dir.Name = 'target') then begin
          if Length(Dir.Args) > 0 then
            Res.DefaultTarget := Dir.Args[0];
        end;
      end;
    end;
  end;

  ProfileName := Res.SelectedProfile;
  if Opt.OverrideProfile <> '' then
    ProfileName := Opt.OverrideProfile
  else if ProfileName = '' then
    ProfileName := GetEnvironmentVariable('MUFFIN_PROFILE');
  if ProfileName = '' then
    ProfileName := 'debug';
  Res.SelectedProfile := ProfileName;

  TargetName := Opt.OverrideTarget;
  if TargetName = '' then
    TargetName := GetEnvironmentVariable('MUFFIN_TARGET');
  if TargetName = '' then
    TargetName := Res.DefaultTarget;
  if TargetName = '' then begin
    for i := 0 to Length(FileIn.Blocks) - 1 do begin
      if FileIn.Blocks[i].Tag = 'target' then begin
        TargetName := FileIn.Blocks[i].Name;
        Break;
      end;
    end;
  end;
  if TargetName = '' then
    TargetName := 'default';
  Res.SelectedTarget := TargetName;

  for i := 0 to Length(FileIn.Blocks) - 1 do begin
    Block := FileIn.Blocks[i];
    if (Block.Tag = 'profile') and (Block.Name = ProfileName) then begin
      for j := 0 to Length(Block.Directives) - 1 do begin
        Dir := Block.Directives[j];
        if Dir.Kind = dkSet then begin
          Key := Dir.Name;
          if Length(Dir.Args) > 0 then
            Val := Dir.Args[0]
          else
            Val := '';
          if Key <> '' then
            Res.Vars.Values[Key] := Val;
        end;
      end;
    end;
  end;

  for i := 0 to Opt.Defines.Count - 1 do begin
    Key := Opt.Defines.Names[i];
    Val := Opt.Defines.ValueFromIndex[i];
    if Key <> '' then begin
      if Key = 'profile' then
        Res.SelectedProfile := Val
      else if Key = 'target' then
        Res.SelectedTarget := Val
      else
        Res.Vars.Values[Key] := Val;
    end;
  end;

  Res.HostOS := Opt.OverrideOS;
  if Res.HostOS = '' then
    Res.HostOS := DetectOS;
  if (Res.HostOS = '') or (Res.HostOS = 'unknown') then
    Res.HostOS := EnvFallbackOS;
  if Res.HostOS = '' then
    Res.HostOS := 'unknown';

  Res.HostArch := Opt.OverrideArch;
  if Res.HostArch = '' then
    Res.HostArch := DetectArch;
  if (Res.HostArch = '') or (Res.HostArch = 'unknown') then
    Res.HostArch := EnvFallbackArch;
  if Res.HostArch = '' then
    Res.HostArch := 'unknown';

  if Res.WorkspaceName = '' then
    Res.WorkspaceName := 'workspace';
  if Res.WorkspaceRoot = '' then
    Res.WorkspaceRoot := '.';
  if Res.WorkspaceTargetDir = '' then
    Res.WorkspaceTargetDir := 'target';
end;

end.
