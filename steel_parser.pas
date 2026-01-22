unit steel_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, steel_types;

function ParseMufFile(const Path: string; out FileOut: TMufFile; out ErrorMsg: string): Boolean;

implementation

uses
  steel_utils;

function ParseDirective(const Line: string): TDirective;
var
  Tokens, Tokens2: TStringList;
  Op, Rest, Key, Val: string;
  i: Integer;
begin
  Result.Kind := dkUnknown;
  Result.Name := '';
  SetLength(Result.Args, 0);
  Result.Raw := Line;
  if (Length(Line) = 0) or (Line[1] <> '.') then
    Exit;

  Tokens := SplitTokens(Trim(Copy(Line, 2, Length(Line))));
  try
    if Tokens.Count = 0 then
      Exit;
    Op := Tokens[0];
    Rest := JoinTokens(Tokens, 1);

    if Op = 'set' then begin
      Result.Kind := dkSet;
      if Tokens.Count >= 2 then begin
        Key := Unquote(Tokens[1]);
        Val := '';
        if Tokens.Count > 2 then
          Val := Unquote(JoinTokens(Tokens, 2));
        Result.Name := Key;
        SetLength(Result.Args, 1);
        Result.Args[0] := Val;
      end;
      Exit;
    end;

    if Op = 'exec' then
      Result.Kind := dkExec
    else if Op = 'make' then
      Result.Kind := dkMake
    else if Op = 'output' then
      Result.Kind := dkOutput
    else if Op = 'takes' then
      Result.Kind := dkTakes
    else if Op = 'emits' then
      Result.Kind := dkEmits
    else if Op = 'ref' then
      Result.Kind := dkRef
    else if Op = 'use' then
      Result.Kind := dkUse
    else if Op = 'run' then
      Result.Kind := dkRun
    else if Op = 'needs' then
      Result.Kind := dkNeeds
    else
      Result.Kind := dkUnknown;

    Result.Name := Op;
    if Rest <> '' then begin
      Tokens2 := SplitTokens(Rest);
      try
        SetLength(Result.Args, Tokens2.Count);
        for i := 0 to Tokens2.Count - 1 do
          Result.Args[i] := Unquote(Tokens2[i]);
      finally
        Tokens2.Free;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

function ParseBlockHeader(const Line: string; out Tag, Name: string): Boolean;
var
  Inside: string;
  p: Integer;
begin
  Result := False;
  Tag := '';
  Name := '';
  if (Length(Line) < 2) then
    Exit;
  if (Line[1] <> '[') or (Line[Length(Line)] <> ']') then
    Exit;
  Inside := Trim(Copy(Line, 2, Length(Line) - 2));
  if Inside = '' then
    Exit;
  p := Pos(' ', Inside);
  if p > 0 then begin
    Tag := Trim(Copy(Inside, 1, p - 1));
    Name := Trim(Copy(Inside, p + 1, Length(Inside)));
  end else begin
    Tag := Inside;
    Name := '';
  end;
  Name := Unquote(Name);
  Result := True;
end;

function ParseMufFile(const Path: string; out FileOut: TMufFile; out ErrorMsg: string): Boolean;
var
  F: TextFile;
  Line, Trimmed: string;
  HeaderSeen: Boolean;
  Stack: array of Integer;
  TopIndex: Integer;
  Tag, Name: string;
  BlockIndex: Integer;
  Dir: TDirective;
  VersionStr: string;
  Tokens: TStringList;
begin
  Result := False;
  ErrorMsg := '';
  FileOut.HeaderVersion := 0;
  SetLength(FileOut.Blocks, 0);
  SetLength(FileOut.TopDirectives, 0);
  HeaderSeen := False;
  SetLength(Stack, 0);
  TopIndex := -1;

  AssignFile(F, Path);
  Reset(F);
  try
    while not EOF(F) do begin
      ReadLn(F, Line);
      Trimmed := StripInlineComment(Line);
      if Trimmed = '' then
        Continue;

      if (Pos('!muf', Trimmed) = 1) or (Pos('muf', Trimmed) = 1) then begin
        Tokens := SplitTokens(Trimmed);
        try
          if Tokens.Count >= 2 then begin
            VersionStr := Tokens[1];
            FileOut.HeaderVersion := StrToIntDef(VersionStr, 0);
          end;
        finally
          Tokens.Free;
        end;
        HeaderSeen := True;
        Continue;
      end;

      if (Trimmed = '..') or (Trimmed = '.end') then begin
        if TopIndex >= 0 then begin
          SetLength(Stack, TopIndex);
          Dec(TopIndex);
        end;
        Continue;
      end;

      if ParseBlockHeader(Trimmed, Tag, Name) then begin
        BlockIndex := Length(FileOut.Blocks);
        SetLength(FileOut.Blocks, BlockIndex + 1);
        FileOut.Blocks[BlockIndex].Tag := Tag;
        FileOut.Blocks[BlockIndex].Name := Name;
        FileOut.Blocks[BlockIndex].Parent := -1;
        SetLength(FileOut.Blocks[BlockIndex].Children, 0);
        SetLength(FileOut.Blocks[BlockIndex].Directives, 0);
        if TopIndex >= 0 then begin
          FileOut.Blocks[BlockIndex].Parent := Stack[TopIndex];
          AddChild(FileOut.Blocks, Stack[TopIndex], BlockIndex);
        end;
        SetLength(Stack, TopIndex + 2);
        Stack[TopIndex + 1] := BlockIndex;
        Inc(TopIndex);
        Continue;
      end;

      Dir := ParseDirective(Trimmed);
      if (TopIndex >= 0) and (Dir.Raw <> '') then
        AddDirective(FileOut.Blocks[Stack[TopIndex]].Directives, Dir)
      else if Dir.Raw <> '' then
        AddDirective(FileOut.TopDirectives, Dir);
    end;
  finally
    CloseFile(F);
  end;

  if not HeaderSeen then begin
    ErrorMsg := 'invalid or missing MUF header';
    Exit;
  end;
  if FileOut.HeaderVersion <> 4 then begin
    ErrorMsg := 'unsupported MUF version (expected 4)';
    Exit;
  end;
  Result := True;
end;

end.
