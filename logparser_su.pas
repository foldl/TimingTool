unit logparser_su;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, ExtCtrls, LteGraph,
  ImgList, LteTimingReader, RegExpr, Math, superobject, TimingGraph;

type

  { TLteTimingReaderSu }

  TLteTimingReaderSu = class(TTimingReader)
  private
    FLegend: TTreeView;
    FLoadFirstFrame: Boolean;
    FTimerEventIndex: Integer;
    procedure SetLegend(AValue: TTreeView);
    procedure ReceiveSFN(const SFN: Integer);
    procedure GetTheme(const TSG, SIG: Integer; out PS: TPenStyle;
                   out C: TColor);
  protected
    procedure BeforeRead; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Legend: TTreeView read FLegend write SetLegend;
  end;

implementation

uses
  KLib;

const
  THEME: array [0..15] of TColor =
    (482559, 505739, 6450981, 15631106, 8199463, 9847672,
     2895066, 3911660, 3507588, 3898092, 9276742, 3846574, 11630200, 11645302, 7887025, 4764541);
  TSG_NAME: array [0..4] of string =
    ('TSG0-Pri Rx', 'TSG1-Pri Tx', 'TSG2-Sec Rx', 'TSG3-Sec Tx', 'TSG4-IRAT  ');
  TIMER_NAME: array [0..4] of array [0..15] of string =
    (
    {$include "tsg.inc"}
     );

  GENERIC_EVENT_INDEX = 0;

type

  { TSigParser }

  TSigParser = class(TLineParserRegExp)
  private
    FReaderSu: TLteTimingReaderSu;
  public
    constructor Create;
    procedure Matched(ARe: TRegExpr; const ALine: string;
                   const LineNo: Cardinal); override;
  end;

  { THWISRParser }

  THWISRParser = class(TLineParserRegExp)
  private
    FReaderSu: TLteTimingReaderSu;
  public
    constructor Create;
    procedure Matched(ARe: TRegExpr; const ALine: string;
                   const LineNo: Cardinal); override;
  end;

{ THWISRParser }

constructor THWISRParser.Create;
begin
  inherited Create('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9xX]+)/([0-9])/([0-9]+)\} +: +\[HW\] ([^ ]+),');
end;

procedure THWISRParser.Matched(ARe: TRegExpr; const ALine: string;
  const LineNo: Cardinal);
var
  SFN, TTI, Sample: Integer;
  K: Int64;
begin
  if not Assigned(FReaderSu) then FReaderSu := (Reader as TLteTimingReaderSu);
  SFN    := AtoI(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  FReaderSu.ReceiveSFN(SFN);
  K := Reader.FEventMap.I[ARe.Match[4]];
  Reader.FEvents[K and $FFFF].Add(TTI, Sample, ARe.Match[4],
                        THEME[K shr 16],
                        psSolid);
end;

{ TSigParser }

constructor TSigParser.Create;
begin
  inherited Create('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9]+)/([0-9])/([0-9]+)\} +: +\[HW\] tsg([0-9])_time_sig\[([0-9]+)\]');
end;

procedure TSigParser.Matched(ARe: TRegExpr; const ALine: string;
  const LineNo: Cardinal);
var
  SFN, TTI, Sample, TSG, SIG: Integer;
  PS: TPenStyle;
  C: TColor;
begin
  if not Assigned(FReaderSu) then FReaderSu := (Reader as TLteTimingReaderSu);

  SFN    := StrToInt(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  TSG    := StrToInt(ARe.Match[4]);
  SIG    := EnsureRange(StrToInt(ARe.Match[5]), 0, 15);
  FReaderSu.ReceiveSFN(SFN);
  if TSG <= High(TSG_NAME) then
  begin
    FReaderSu.GetTheme(TSG, SIG, PS, C);
    Reader.FEvents[(Reader as TLteTimingReaderSu).FTimerEventIndex + TSG].Add(TTI, Sample, TIMER_NAME[TSG, SIG], C,
                          TEventStyle(PS));
  end;
end;

{ TLteTimingReaderSu }

procedure TLteTimingReaderSu.SetLegend(AValue: TTreeView);
var
  I, J, K: Integer;
  N, C: TTreeNode;
  M: TCustomImageList;
  X: TBitmap;
  PS: TPenStyle;
  Cl: TColor;
begin
  if FLegend = AValue then Exit;
  FLegend := AValue;
  FLegend.Items.Clear;
  M := FLegend.Images;
  M.Clear;
  X := TBitmap.Create;
  X.SetSize(M.Height, M.Height);
  X.Canvas.Brush.Color := FLegend.BackgroundColor;
  X.Canvas.Pen.Width := 2;
  with X.Canvas do
  begin
    FillRect(0, 0, Width, Height);
    Pen.Color := clRed;
    Line(0, X.Height div 2, X.Width, X.Height div 2);
  end;
  M.Add(X, nil);

  for I := 0 to High(TSG_NAME) do
  begin
    N := FLegend.Items.Add(nil, TSG_NAME[I]);
    for J := 0 to High(TIMER_NAME[I]) do
    begin
      GetTheme(I, J, PS, Cl);
      with X.Canvas do
      begin
        FillRect(0, 0, Width, Height);
        Pen.Style := PS;
        Pen.Color := Cl;
        Line(0, X.Height div 2, X.Width, X.Height div 2);
      end;
      K := M.Add(X, nil);
      C := FLegend.Items.AddChild(N, TIMER_NAME[I][J]);
      C.StateIndex := K;
    end;
    N.Expand(False);
  end;
end;

procedure TLteTimingReaderSu.ReceiveSFN(const SFN: Integer);
begin
  if FLoadFirstFrame then
  begin
    Graph.SFN := SFN;
    FLoadFirstFrame := False;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
  end
  else if SFN <> Graph.SFN then
  begin
    Graph.NewFrame;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
    Graph.SFN := SFN;
  end;
end;

procedure TLteTimingReaderSu.GetTheme(const TSG, SIG: Integer; out
  PS: TPenStyle; out C: TColor);
begin
  C := THEME[(SIG + 10) mod Length(THEME)]; // div 2];
  PS := psSolid; // TPenStyle(SIG mod 2);
end;

procedure TLteTimingReaderSu.BeforeRead;
begin
  inherited BeforeRead;
  FLoadFirstFrame := True;
end;

constructor TLteTimingReaderSu.Create;
var
  L, O: TStringList;
  S, T: string;
  I: Integer;
begin
  inherited;
  L := TStringList.Create;
  O := TStringList.Create;
  L.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'group.ini');
  for S in L do
  begin
    I := Pos(':', S);
    if I < 1 then Continue;
    T := Copy(S, 1, I - 1);
    I := O.IndexOf(T);
    if I < 0 then O.Add(T);
  end;
  O.Sort;
  for S in L do
  begin
    I := Pos(':', S);
    if I < 1 then Continue;
    T := Copy(S, 1, I - 1);
    Delete(S, 1, I); S := Trim(S);
    FEventMap.I[S] := ((TSuperAvlEntry.Hash(S) mod Length(THEME)) shl 16) + O.IndexOf(T) + 1;
  end;
  L.Free;

  FTimerEventIndex := O.Count + 1;
  SetLength(FEvents, Length(TSG_NAME) + O.Count + 1);
  for I := 0 to High(FEvents) do
  begin
    FEvents[I] := TLteEventList.Create;
    Graph.AddEventList(FEvents[I]);
  end;

  FEvents[GENERIC_EVENT_INDEX].Caption := 'Generic';
  for I := 0 to O.Count - 1 do
    FEvents[I + 1].Caption := O[I];
  for I := 0 to High(TSG_NAME) do
    FEvents[I + FTimerEventIndex].Caption := TSG_NAME[I];

  O.Free;

  AddParser(TSigParser.Create);
  AddParser(THWISRParser.Create);
end;

destructor TLteTimingReaderSu.Destroy;
begin
  inherited Destroy;
end;

end.

