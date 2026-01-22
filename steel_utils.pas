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
function NormalizeOS(const S: string): string;
function NormalizeArch(const S: string): string;
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
  {$ELSEIF defined(ATARI)}
  Result := 'atari';
  {$ELSEIF defined(AROS)}
  Result := 'aros';
  {$ELSEIF defined(MORPHOS)}
  Result := 'morphos';
  {$ELSEIF defined(OS2)}
  Result := 'os2';
  {$ELSEIF defined(MSDOS)}
  Result := 'msdos';
  {$ELSEIF defined(DOS)}
  Result := 'dos';
  {$ELSEIF defined(GO32V2)}
  Result := 'go32v2';
  {$ELSEIF defined(NETWARE)}
  Result := 'netware';
  {$ELSEIF defined(MACOS)}
  Result := 'macos-classic';
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
  {$ELSEIF defined(CPUSPARC64)}
  Result := 'sparc64';
  {$ELSEIF defined(CPUMIPS)}
  Result := 'mips';
  {$ELSEIF defined(CPUMIPSEL)}
  Result := 'mipsel';
  {$ELSEIF defined(CPUMIPS64)}
  Result := 'mips64';
  {$ELSEIF defined(CPUMIPS64EL)}
  Result := 'mips64el';
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
  {$ELSEIF defined(CPUAVR)}
  Result := 'avr';
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

function NormalizeOS(const S: string): string;
var
  L: string;
begin
  L := LowerCase(Trim(S));
  if (L = 'darwin') or (L = 'mac os x') or (L = 'macosx') or (L = 'macos') then
    Exit('macos');
  if (L = 'macos-classic') or (L = 'mac os') or (L = 'macos9') or (L = 'mac os 9') or (L = 'mac os 8') or (L = 'mac os 7') then
    Exit('macos-classic');
  if (L = 'a/ux') or (L = 'aux') then
    Exit('a/ux');
  if (L = 'win32') or (L = 'win64') or (L = 'windows_nt') or (L = 'windows') then
    Exit('windows');
  if (L = 'mingw') or (L = 'msys') then
    Exit('windows');
  if (L = 'cygwin') then
    Exit('cygwin');
  if (L = 'windows 1.0') or (L = 'windows 2.0') or (L = 'windows 3.0') or (L = 'windows 3.1') or (L = 'windows 3.11') then
    Exit('windows');
  if (L = 'windows 95') or (L = 'windows 98') or (L = 'windows 98 se') or (L = 'windows me') then
    Exit('windows');
  if (L = 'windows nt 3.1') or (L = 'windows nt 3.5') or (L = 'windows nt 4.0') then
    Exit('windows');
  if (L = 'windows 2000') or (L = 'windows xp') or (L = 'windows vista') then
    Exit('windows');
  if (L = 'msdos') or (L = 'dos') or (L = 'go32v2') then
    Exit('dos');
  if (L = 'pc-dos') or (L = 'puntos') or (L = 'pc dos') or (L = 'dr-dos') or (L = 'freedos') then
    Exit('dos');
  if (L = 'dosbox') then
    Exit('dos');
  if (L = 'os/2') then
    Exit('os2');
  if (L = 'sunos') or (L = 'sun') then
    Exit('solaris');
  if (L = 'svr4') or (L = 'sysv') then
    Exit('solaris');
  if (L = 'aix') or (L = 'hpux') or (L = 'irix') then
    Exit(L);
  if (L = 'irix64') then
    Exit('irix');
  if (L = 'tru64') or (L = 'osf1') or (L = 'osf/1') then
    Exit('tru64');
  if (L = 'ultrix') then
    Exit('ultrix');
  if (L = 'xenix') then
    Exit('xenix');
  if (L = 'minix') then
    Exit('minix');
  if (L = 'plan 9') or (L = 'plan9') then
    Exit('plan9');
  if (L = 'haiku') or (L = 'beos') then
    Exit(L);
  if (L = 'amigaos') then
    Exit('amiga');
  if (L = 'morphos') or (L = 'aros') or (L = 'netware') or (L = 'qnx') then
    Exit(L);
  if (L = 'android') or (L = 'ios') then
    Exit(L);
  if (L = 'linux') or (L = 'gnu/linux') then
    Exit('linux');
  if (L = 'linux 1.x') or (L = 'linux 2.0') or (L = 'linux 2.2') or (L = 'linux 2.4') then
    Exit('linux');
  if (L = 'debian') or (L = 'slackware') or (L = 'red hat') or (L = 'redhat') or (L = 'mandrake') then
    Exit('linux');
  if (L = 'freebsd') or (L = 'openbsd') or (L = 'netbsd') or (L = 'dragonfly') then
    Exit(L);
  Result := L;
end;

function NormalizeArch(const S: string): string;
var
  L: string;
begin
  L := LowerCase(Trim(S));
  if (L = 'x86_64') or (L = 'amd64') or (L = 'x64') or (L = 'x86-64') then
    Exit('x86_64');
  if (L = 'i386') or (L = 'i486') or (L = 'i586') or (L = 'i686') or (L = 'ia32') then
    Exit('x86');
  if (L = 'x86') or (L = '386') or (L = '486') or (L = '586') or (L = '686') then
    Exit('x86');
  if (L = '8086') or (L = '8088') or (L = '80186') or (L = '80286') then
    Exit('x86');
  if (L = 'nec v20') or (L = 'nec v30') or (L = 'v20') or (L = 'v30') then
    Exit('x86');
  if (L = 'am286') or (L = 'am386') or (L = 'am486') then
    Exit('x86');
  if (L = 'pentium') or (L = 'p5') or (L = 'pentium mmx') or (L = 'pentium ii') or (L = 'pentium iii') or (L = 'pentium 4') or (L = 'pentium m') then
    Exit('x86');
  if (L = 'celeron') or (L = 'atom') then
    Exit('x86');
  if (L = 'cyrix 486') or (L = 'cyrix 6x86') or (L = 'winchip') then
    Exit('x86');
  if (L = 'k5') or (L = 'k6') or (L = 'k6-2') or (L = 'k6-iii') or (L = 'athlon') or (L = 'athlon xp') or (L = 'duron') or (L = 'opteron') or (L = 'sempron') then
    Exit('x86');
  if (L = 'i86pc') then
    Exit('x86');
  if (L = 'ia64') then
    Exit('ia64');
  if (L = 'arm64') then
    Exit('aarch64');
  if (L = 'armv8') or (L = 'aarch64') then
    Exit('aarch64');
  if (L = 'armv7') or (L = 'armv7l') or (L = 'armhf') or (L = 'armel') then
    Exit('arm');
  if (L = 'armv7a') or (L = 'armv7r') or (L = 'armv7m') or (L = 'armv7em') then
    Exit('arm');
  if (L = 'armv6') or (L = 'armv6l') or (L = 'armv5') or (L = 'armv4') or (L = 'armv4t') then
    Exit('arm');
  if (L = 'xscale') or (L = 'strongarm') then
    Exit('arm');
  if (L = 'ppc') or (L = 'powerpc') then
    Exit('ppc');
  if (L = 'ppc64') then
    Exit('ppc64');
  if (L = 'ppc64le') then
    Exit('ppc64');
  if (L = 'powerpc64') or (L = 'powerpc64le') then
    Exit('ppc64');
  if (L = 'powerpc 601') or (L = 'powerpc 603') or (L = 'powerpc 603e') or (L = 'powerpc 604') then
    Exit('ppc');
  if (L = 'powerpc g3') or (L = 'powerpc g4') or (L = 'powerpc g5') then
    Exit('ppc');
  if (L = 'm68k') or (L = '68k') then
    Exit('m68k');
  if (L = 'mc68000') or (L = 'mc68020') or (L = 'mc68030') or (L = 'mc68040') or (L = 'mc68060') then
    Exit('m68k');
  if (L = 'sparc64') or (L = 'sparcv9') then
    Exit('sparc64');
  if (L = 'sparc') then
    Exit('sparc');
  if (L = 'sparc32') then
    Exit('sparc');
  if (L = 'sh3') or (L = 'sh4') then
    Exit(L);
  if (L = 'sh2') or (L = 'sh2a') or (L = 'sh5') or (L = 'superh') then
    Exit('sh4');
  if (L = 'm88k') then
    Exit('m88k');
  if (L = 's390x') then
    Exit('s390x');
  if (L = 's390x') then
    Exit('s390x');
  if (L = 's390') then
    Exit('s390');
  if (L = 'mipsel') or (L = 'mipsle') then
    Exit('mipsel');
  if (L = 'mips64el') then
    Exit('mips64el');
  if (L = 'mips64') then
    Exit('mips64');
  if (L = 'mips') or (L = 'mipseb') then
    Exit('mips');
  if (L = 'mipsisa32') then
    Exit('mips');
  if (L = 'mipsisa64') then
    Exit('mips64');
  if (L = 'r3000') or (L = 'r4000') then
    Exit('mips');
  if (L = 'riscv64') then
    Exit('riscv64');
  if (L = 'riscv32') then
    Exit('riscv32');
  if (L = 'alpha') or (L = 'dec alpha') or (L = 'alpha ev4') or (L = 'alpha ev5') or (L = 'alpha ev6') then
    Exit('alpha');
  if (L = 'vax') then
    Exit('vax');
  if (L = 'hppa') or (L = 'parisc') then
    Exit('hppa');
  if (L = 'm32r') then
    Exit('m32r');
  if (L = 'microblaze') then
    Exit('microblaze');
  if (L = 'nios2') then
    Exit('nios2');
  if (L = 'avr') or (L = 'avr32') then
    Exit('avr');
  if (L = 'msp430') then
    Exit('msp430');
  if (L = 'tricore') then
    Exit('tricore');
  if (L = 'tilegx') or (L = 'tilepro') then
    Exit('tile');
  if (L = 'xtensa') then
    Exit('xtensa');
  Result := L;
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
