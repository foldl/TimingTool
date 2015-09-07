unit ex_logparser_su;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Types, Graphics, ExtCtrls, LteGraph,
  ImgList, TimingReader, RegExpr, Math, superobject, TimingGraph;

type

  { TLteGraph2 }

  TLteGraph2 = class(TLteGraph)
  private
    FSleepEvent: TEventList;
  protected
    procedure DoDrawFgSimple; override;
  end;

  { TLteTimingReaderSu }

  TLteTimingReaderSu = class(TTimingReader)
  private
    FLegend: TTreeView;
    FLoadFirstFrame: Boolean;
    FTheGraph: TLteGraph2;
    FTimerEventIndex: Integer;
    FEvents: array of TEventList;
    FEnterTime: Integer;
    FEnterLine: Integer;
    function GetEvents(const Index: Integer): TLteEventList;
    procedure SetLegend(AValue: TTreeView);
    procedure ReceiveSFN(const SFN: Integer);
    procedure GetTheme(const TSG, SIG: Integer; out PS: TPenStyle;
                   out C: TColor);

    procedure AddSleepEvent(const ASFN, ASample: Integer;
                   IsEnter: Boolean);

    procedure HWISRMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
    procedure SigMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
    procedure EnterSleepMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
    procedure ExitSleepMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
  protected
    function CreateGraph: TTimingGraph; override;
    property TheGraph: TLteGraph2 read FTheGraph;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeforeRead; override;

    property Legend: TTreeView read FLegend write SetLegend;
    property Events[const Index: Integer]: TLteEventList read GetEvents;

    property Graph: TLteGraph2 read FTheGraph;
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

{ TLteGraph2 }

procedure TLteGraph2.DoDrawFgSimple;
var
  S: string;
  C: TCanvas;
  Sleep: Double;
  X: TSize;
  function EventSum(List: TEventList): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    if not Assigned(List) then Exit;
    for I := 0 to List.Count - 1 do
      Result := Result + List[I]^.Duration.Sample;
  end;
begin
  Sleep := EventSum(FSleepEvent);
  S := Format('idle: %.1f%%', [Sleep * 100 / FFrameLength.Sample]);
  C := FDoubleBuffer.PaintBuffer.Canvas;
  X := C.TextExtent(S);
  C.TextOut(FDoubleBuffer.Width - Max(100, X.cx) - 20, 3, S);
  inherited;
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

function TLteTimingReaderSu.GetEvents(const Index: Integer): TLteEventList;
begin
  Result := TLteEventList(FEvents[Index]);;
end;

procedure TLteTimingReaderSu.ReceiveSFN(const SFN: Integer);
var
  P: PTimingEvent;
begin
  if FLoadFirstFrame then
  begin
    FTheGraph.SFN := SFN;
    FLoadFirstFrame := False;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
  end
  else if SFN <> FTheGraph.SFN then
  begin
    if FEnterTime >= 0 then
    begin
      P := Graph.FSleepEvent.NewEvent;
      P^.Time.Sample := FEnterTime;
      P^.Duration.Sample := FTheGraph.FrameLength.Sample - FEnterTime;
      Graph.FSleepEvent.InsertEvent(P);
      FEnterTime := 0;
    end;
    FTheGraph.NewFrame;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
    FTheGraph.SFN := SFN;
  end;
end;

procedure TLteTimingReaderSu.GetTheme(const TSG, SIG: Integer; out
  PS: TPenStyle; out C: TColor);
begin
  C := THEME[(SIG + 10) mod Length(THEME)]; // div 2];
  PS := psSolid; // TPenStyle(SIG mod 2);
end;

procedure TLteTimingReaderSu.AddSleepEvent(const ASFN, ASample: Integer;
  IsEnter: Boolean);
var
  P: PTimingEvent;
begin
  ReceiveSFN(ASFN);
  if IsEnter then
  begin
    FEnterTime := ASample;
  end
  else begin
    P := Graph.FSleepEvent.NewEvent;
    P^.Time.Sample := FEnterTime;
    P^.Duration.Sample := ASample - FEnterTime;
    Graph.FSleepEvent.InsertEvent(P);
    FEnterTime := -1;
  end;
end;

procedure TLteTimingReaderSu.HWISRMatched(ARe: TRegExpr; const ALine: string;
  const LineNo: Cardinal);
var
  SFN, TTI, Sample: Integer;
  K: Int64;
begin
  SFN    := AtoI(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  ReceiveSFN(SFN);
  K := FEventMap.I[ARe.Match[4]];
  Events[K and $FFFF].Add(TTI, Sample, ARe.Match[4],
                        THEME[K shr 16],
                        psSolid);
end;

procedure TLteTimingReaderSu.SigMatched(ARe: TRegExpr; const ALine: string;
  const LineNo: Cardinal);
var
  SFN, TTI, Sample, TSG, SIG: Integer;
  PS: TPenStyle;
  C: TColor;
begin
  SFN    := StrToInt(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  TSG    := StrToInt(ARe.Match[4]);
  SIG    := EnsureRange(StrToInt(ARe.Match[5]), 0, 15);
  ReceiveSFN(SFN);
  if TSG <= High(TSG_NAME) then
  begin
    GetTheme(TSG, SIG, PS, C);
    Events[FTimerEventIndex + TSG].Add(TTI, Sample, TIMER_NAME[TSG, SIG], C,
                          TEventStyle(PS));
  end;
end;

procedure TLteTimingReaderSu.EnterSleepMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
var
  SFN, TTI, Sample: Integer;
begin
  SFN    := AtoI(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  FEnterLine := LineNo;
  AddSleepEvent(SFN, TTI * Graph.TTISamples + Sample, True);
end;

procedure TLteTimingReaderSu.ExitSleepMatched(ARe: TRegExpr; const ALine: string;
              const LineNo: Cardinal);
var
  SFN, TTI, Sample: Integer;
begin
  SFN    := AtoI(ARe.Match[1]);
  TTI    := StrToInt(ARe.Match[2]);
  Sample := StrToInt(ARe.Match[3]);
  if FEnterLine + 100 < LineNo then
    FEnterLine := 1;
  AddSleepEvent(SFN, TTI * Graph.TTISamples + Sample, False);
end;

procedure TLteTimingReaderSu.BeforeRead;
begin
  inherited BeforeRead;
  FLoadFirstFrame := True;
  FTheGraph.SFN := -1;
  FEnterTime := -1;
end;

function TLteTimingReaderSu.CreateGraph: TTimingGraph;
begin
  FTheGraph := TLteGraph2.Create;
  Result := FTheGraph;
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
    if Pos('//', S) = 1 then Continue;
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
  with Graph do
  begin
    FSleepEvent := TLteEventList.Create;
    FSleepEvent.HasDuration := True;
    FSleepEvent.Color := 482559;
    FSleepEvent.Caption := 'Sleep';
  end;
  Graph.AddEventList(Graph.FSleepEvent);

  AddParser('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9a-fA-FxX]+)/([0-9])/([0-9]+)\} +: +\[HW\] tsg([0-9])_time_sig\[([0-9]+)\]',
            TReParserCB(@SigMatched));
  AddParser('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9a-fA-FxX]+)/([0-9])/([0-9]+)\} +: +\[HW\] ([^ ]+),',
            TReParserCB(@HWISRMatched));
  AddParser('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9a-fA-FxX]+)/([0-9])/([0-9]+)\} +: +PM_DEBUG_PAT=>  USER_EVENT: ENTER_CORE_SLEEP',
            TReParserCB(@EnterSleepMatched));
  AddParser('^[0-9\.]+\s*\([0-9]+\)\s*\{L: ([0-9a-fA-FxX]+)/([0-9])/([0-9]+)\} +: +PM_DEBUG_PAT=>  USER_EVENT: EXIT_CORE_SLEEP',
            TReParserCB(@ExitSleepMatched));
end;

destructor TLteTimingReaderSu.Destroy;
begin
  inherited Destroy;
end;

end.

