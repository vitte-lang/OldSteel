unit steel_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDirectiveKind = (
    dkSet,
    dkExec,
    dkMake,
    dkOutput,
    dkTakes,
    dkEmits,
    dkRef,
    dkUse,
    dkRun,
    dkNeeds,
    dkUnknown
  );

  TDirective = record
    Kind: TDirectiveKind;
    Name: string;
    Args: array of string;
    Raw: string;
  end;

  TDirectiveArray = array of TDirective;

  TBlock = record
    Tag: string;
    Name: string;
    Parent: Integer;
    Children: array of Integer;
    Directives: TDirectiveArray;
  end;

  TBlockArray = array of TBlock;

  TMufFile = record
    HeaderVersion: Integer;
    Blocks: TBlockArray;
    TopDirectives: TDirectiveArray;
  end;

  TResolvedConfig = record
    WorkspaceName: string;
    WorkspaceRoot: string;
    WorkspaceTargetDir: string;
    SelectedProfile: string;
    SelectedTarget: string;
    DefaultTarget: string;
    HostOS: string;
    HostArch: string;
    Vars: TStringList;
  end;

procedure AddDirective(var List: TDirectiveArray; const Dir: TDirective);
procedure AddChild(var Blocks: TBlockArray; Parent, Child: Integer);

implementation

procedure AddDirective(var List: TDirectiveArray; const Dir: TDirective);
var
  L: Integer;
begin
  L := Length(List);
  SetLength(List, L + 1);
  List[L] := Dir;
end;

procedure AddChild(var Blocks: TBlockArray; Parent, Child: Integer);
var
  L: Integer;
begin
  if (Parent < 0) or (Parent >= Length(Blocks)) then
    Exit;
  L := Length(Blocks[Parent].Children);
  SetLength(Blocks[Parent].Children, L + 1);
  Blocks[Parent].Children[L] := Child;
end;

end.
