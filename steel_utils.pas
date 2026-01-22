unit steel_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StripInlineComment(const S: string): string;
function Unquote(const S: string): string;
function QuoteIfNeeded(const S: string): string;
function SplitTokens(const S: string): TStringList;
function JoinTokens(const Tokens: TStringList; StartIndex: Integer): string;
function ExpandVars(const S: string; Vars: TStringList): string;
function DetectOS: string;
function DetectArch: string;
function EnvFallbackOS: string;
function EnvFallbackArch: string;
function FindInPath(const FileName: string): string;

implementation

function StripInlineComment(const S: string): string;
var
  p: Integer;
begin
  p := Pos(';;', S);
  if p > 0 then
    Result := Trim(Copy(S, 1, p - 1))
  else
    Result := Trim(S);
end;

function Unquote(const S: string): string;
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L >= 2) and (Result[1] = '"') and (Result[L] = '"') then
    Result := Copy(Result, 2, L - 2);
end;

function QuoteIfNeeded(const S: string): string;
var
  i: Integer;
  NeedsQuote: Boolean;
begin
  NeedsQuote := False;
  for i := 1 to Length(S) do begin
    if S[i] = ' ' then begin
      NeedsQuote := True;
      Break;
    end;
  end;
  if NeedsQuote then
    Result := '"' + S + '"'
  else
    Result := S;
end;

function SplitTokens(const S: string): TStringList;
var
  i: Integer;
  Cur: string;
  InQuote: Boolean;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := ' ';
  Cur := '';
  InQuote := False;
  i := 1;
  while i <= Length(S) do begin
    if S[i] = '"' then begin
      InQuote := not InQuote;
      Cur := Cur + S[i];
      Inc(i);
      Continue;
    end;
    if (S[i] = ' ') and (not InQuote) then begin
      if Cur <> '' then begin
        Result.Add(Cur);
        Cur := '';
      end;
      while (i <= Length(S)) and (S[i] = ' ') do
        Inc(i);
      Continue;
    end;
    Cur := Cur + S[i];
    Inc(i);
  end;
  if Cur <> '' then
    Result.Add(Cur);
end;

function JoinTokens(const Tokens: TStringList; StartIndex: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if Tokens = nil then
    Exit;
  for i := StartIndex to Tokens.Count - 1 do begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Tokens[i];
  end;
end;

function ExpandVars(const S: string; Vars: TStringList): string;
var
  i, j: Integer;
  Key, Val: string;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do begin
    if (S[i] = '$') and (i < Length(S)) and (S[i + 1] = '{') then begin
      j := i + 2;
      while (j <= Length(S)) and (S[j] <> '}') do
        Inc(j);
      if (j <= Length(S)) and (S[j] = '}') then begin
        Key := Copy(S, i + 2, j - i - 2);
        Val := Vars.Values[Key];
        Result := Result + Val;
        i := j + 1;
        Continue;
      end;
    end;
    Result := Result + S[i];
    Inc(i);
  end;
end;

function DetectOS: string;
begin
  {$IF defined(WINDOWS)}
  Result := 'windows';
  {$ELSEIF defined(DARWIN)}
  Result := 'macos';
  {$ELSEIF defined(LINUX)}
  Result := 'linux';
  {$ELSEIF defined(FREEBSD)}
  Result := 'freebsd';
  {$ELSEIF defined(OPENBSD)}
  Result := 'openbsd';
  {$ELSEIF defined(NETBSD)}
  Result := 'netbsd';
  {$ELSEIF defined(HAIKU)}
  Result := 'haiku';
  {$ELSEIF defined(AMIGA)}
  Result := 'amiga';
  {$ELSEIF defined(AROS)}
  Result := 'aros';
  {$ELSEIF defined(MORPHOS)}
  Result := 'morphos';
  {$ELSEIF defined(OS2)}
  Result := 'os2';
  {$ELSEIF defined(QNX)}
  Result := 'qnx';
  {$ELSEIF defined(ANDROID)}
  Result := 'android';
  {$ELSEIF defined(IOS)}
  Result := 'ios';
  {$ELSEIF defined(EMSCRIPTEN)}
  Result := 'emscripten';
  {$ELSEIF defined(SUNOS)}
  Result := 'solaris';
  {$ELSEIF defined(AIX)}
  Result := 'aix';
  {$ELSEIF defined(HPUX)}
  Result := 'hpux';
  {$ELSEIF defined(CYGWIN)}
  Result := 'cygwin';
  {$ELSE}
  Result := 'unknown';
  {$ENDIF}
end;

function DetectArch: string;
begin
  {$IF defined(CPUX86_64)}
  Result := 'x86_64';
  {$ELSEIF defined(CPUX86)}
  Result := 'x86';
  {$ELSEIF defined(CPUARM)}
  Result := 'arm';
  {$ELSEIF defined(CPUAARCH64)}
  Result := 'aarch64';
  {$ELSEIF defined(CPUPPC)}
  Result := 'ppc';
  {$ELSEIF defined(CPUPPC64)}
  Result := 'ppc64';
  {$ELSEIF defined(CPUM68K)}
  Result := 'm68k';
  {$ELSEIF defined(CPUSPARC)}
  Result := 'sparc';
  {$ELSEIF defined(CPUMIPS)}
  Result := 'mips';
  {$ELSEIF defined(CPUALPHA)}
  Result := 'alpha';
  {$ELSEIF defined(CPUPOWERPC)}
  Result := 'powerpc';
  {$ELSEIF defined(CPUVAX)}
  Result := 'vax';
  {$ELSEIF defined(CPURISCV32)}
  Result := 'riscv32';
  {$ELSEIF defined(CPURISCV64)}
  Result := 'riscv64';
  {$ELSE}
  Result := 'unknown';
  {$ENDIF}
end;

function EnvFallbackOS: string;
var
  V: string;
begin
  Result := '';
  V := GetEnvironmentVariable('STEEL_OS');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('OSTYPE');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('OS');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('UNAME_S');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
end;

function EnvFallbackArch: string;
var
  V: string;
begin
  Result := '';
  V := GetEnvironmentVariable('STEEL_ARCH');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('HOSTTYPE');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
  V := GetEnvironmentVariable('MACHTYPE');
  if V <> '' then begin
    Result := V;
    Exit;
  end;
end;

function FindInPath(const FileName: string): string;
var
  P, Part: string;
  Parts: TStringList;
  i: Integer;
begin
  Result := '';
  if (Pos(PathDelim, FileName) > 0) or (Pos('/', FileName) > 0) then begin
    if FileExists(FileName) then
      Result := FileName;
    Exit;
  end;
  P := GetEnvironmentVariable('PATH');
  if P = '' then
    Exit;
  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := PathSeparator;
    Parts.DelimitedText := P;
    for i := 0 to Parts.Count - 1 do begin
      Part := IncludeTrailingPathDelimiter(Parts[i]) + FileName;
      if FileExists(Part) then begin
        Result := Part;
        Exit;
      end;
    end;
  finally
    Parts.Free;
  end;
end;

end.
