unit KLib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  ALPHABET_LEN = 255;

type
  TDeltaArray = array [0..ALPHABET_LEN] of Integer;

  TBMContext = record
    FDelta1: TDeltaArray;
    FDelta2: array of Integer;
    FPattern: string;
  end;

procedure FreeBMContext(C: TBMContext);
procedure PrepareBMContext(pat: string; var Context: TBMContext);
function BoyerMooreSearch(S: PChar; Len: Integer; var C: TBMContext): Integer; overload;
function BoyerMooreSearch(S: string; var C: TBMContext): Integer;

function AtoI(const S: string): Integer;
function HexToInt(HexStr: string): DWord;

function FormatFreq(F: Integer): string;

implementation

function AtoI(const S: string): integer;
var
  T: string;
begin
  T := S;
  Result := 0;
  if T = '' then
    Exit;
  if T[1] = '$' then
  begin
    Delete(T, 1, 1);
    Result := HexToInt(T);
  end
  else if (Length(T) > 2) and (T[1] in ['X', 'x']) then
  begin
    Delete(T, 1, 2);
    Result := HexToInt(T);
  end
  else
    Result := StrToIntDef(T, 0);
end;

function HexToInt(HexStr: string): DWord;
var
  I: integer;
begin
  HexStr := UpperCase(HexStr);
  Result := 0;

  for i := 1 to length(HexStr) do
  begin
    Result := Result shl 4;
    if HexStr[i] in ['0'..'9'] then
      Result := Result + DWord(byte(HexStr[i]) - 48)
    else
    if HexStr[i] in ['A'..'F'] then
      Result := Result + DWord(byte(HexStr[i]) - 55)
    else
    begin
      Break;
    end;
  end;
end;

// delta1 table: delta1[c] contains the distance between the last
// character of pat and the rightmost occurence of c in pat.
// If c does not occur in pat, then delta1[c] = patlen.
// If c is at string[i] and c != pat[patlen-1], we can
// safely shift i over by delta1[c], which is the minimum distance
// needed to shift pat forward to get string[i] lined up
// with some character in pat.
// this algorithm runs in alphabet_len+patlen time.
procedure make_delta1(var delta1: TDeltaArray; pat: PChar; const patlen: Integer);
var
  I: Integer;
begin
    for I := Low(delta1) to High(delta1) do
        delta1[I] := patlen;

    for I := 0 to patlen - 2 do
        delta1[Ord(pat[I])] := Length(pat) - 1 - I;
end;

// true if the suffix of word starting from word[pos] is a prefix
// of word
function is_prefix(word: PChar; const wordlen, pos: Integer): Boolean;
begin
  Result := CompareByte(word[0], word[pos], wordlen * SizeOf(word[0])) = 0;
end;

// length of the longest suffix of word ending on word[pos].
// suffix_length("dddbcabc", 8, 4) = 2
function suffix_length(word: PChar; const wordlen, pos: Integer): Integer;
var
  I: Integer;
begin
  // increment suffix length i to the first mismatch or beginning
  // of the word
  I := 0;
  while (i <= pos) and (word[pos - I] = word[wordlen - 1 - i]) do Inc(I);
  Result := I;
end;

// delta2 table: given a mismatch at pat[pos], we want to align
// with the next possible full match could be based on what we
// know about pat[pos+1] to pat[patlen-1].
//
// In case 1:
// pat[pos+1] to pat[patlen-1] does not occur elsewhere in pat,
// the next plausible match starts at or after the mismatch.
// If, within the substring pat[pos+1 .. patlen-1], lies a prefix
// of pat, the next plausible match is here (if there are multiple
// prefixes in the substring, pick the longest). Otherwise, the
// next plausible match starts past the character aligned with
// pat[patlen-1].
//
// In case 2:
// pat[pos+1] to pat[patlen-1] does occur elsewhere in pat. The
// mismatch tells us that we are not looking at the end of a match.
// We may, however, be looking at the middle of a match.
//
// The first loop, which takes care of case 1, is analogous to
// the KMP table, adapted for a 'backwards' scan order with the
// additional restriction that the substrings it considers as
// potential prefixes are all suffixes. In the worst case scenario
// pat consists of the same letter repeated, so every suffix is
// a prefix. This loop alone is not sufficient, however:
// Suppose that pat is "ABYXCDEYX", and text is ".....ABYXCDEYX".
// We will match X, Y, and find B != E. There is no prefix of pat
// in the suffix "YX", so the first loop tells us to skip forward
// by 9 characters.
// Although superficially similar to the KMP table, the KMP table
// relies on information about the beginning of the partial match
// that the BM algorithm does not have.
//
// The second loop addresses case 2. Since suffix_length may not be
// unique, we want to take the minimum value, which will tell us
// how far away the closest potential match is.
procedure make_delta2(var delta2: array of Integer; pat: PChar; const patlen: Integer);
var
  P: Integer;
  last_prefix_index: Integer;
  slen: Integer;
begin
  last_prefix_index := patlen - 1;

  // first loop
  for p := patlen - 1 downto 0 do
  begin
      if is_prefix(pat, patlen, p + 1) then
          last_prefix_index := p + 1;

      delta2[p] := last_prefix_index + (patlen - 1 - p);
  end;

  // second loop
  for p := 0 to patlen - 2 do
  begin
    slen := suffix_length(pat, patlen, p);
    if pat[p - slen] <> pat[patlen - 1 - slen] then
          delta2[patlen- 1 - slen] := patlen - 1 - p + slen;
  end
end;

procedure FreeBMContext(C: TBMContext);
begin
  SetLength(C.FDelta2, 0);
end;

procedure PrepareBMContext(pat: string; var Context: TBMContext);
begin
  with Context do
  begin
    FPattern := pat;
    SetLength(FDelta2, Length(pat));
    make_delta1(FDelta1, PChar(pat), Length(pat));
    make_delta2(FDelta2, PChar(pat), Length(pat));
  end;
end;

function BoyerMooreSearch(S: PChar; Len: Integer; var C: TBMContext): Integer;
var
  I: Integer;
  J: Integer;
begin
  Result := -1;

  with C do
  begin
    I := Length(FPattern) - 1;
    if I < 0 then Exit;

    while I < len do
    begin
      J := Length(FPattern);
      while (J >= 1) and (s[I] = FPattern[J])  do
      begin
        Dec(I);
        Dec(J);
      end;

      if J < 1 then
      begin
        Result := I + 1;
        Exit;
      end;

      Inc(I, Max(FDelta1[Ord(s[I])], FDelta2[J - 1]));
    end;
  end;
end;

function BoyerMooreSearch(S: string; var C: TBMContext): Integer;
begin
  Result := BoyerMooreSearch(PChar(S), Length(S), C);
end;

function FormatFreq(F: Integer): string;
const
  U: array [1..3] of string = ('K', 'M', 'G');
var
  I: Integer;
  C: Integer = 0;
begin
  Result := IntToStr(F);
  I := Length(Result);
  while (I > 0) and (Result[I] = '0') do
  begin
    Inc(C);
    Dec(I);
  end;
  I := Round(C / 3);
  if I > 0 then
  begin
    I := Min(I, High(U));
    Result := FloatToStr(F / power(1000, I)) + U[I];
  end;
end;

end.

