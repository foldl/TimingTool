unit TdsGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo, Controls, Graphics, ExtCtrls, Math,
  TimingGraph;

const
  DATA_LEN           = 352;
  MIDAMBLE_LEN       = 144;
  SLOT_LEN           = (2 * DATA_LEN + MIDAMBLE_LEN + 16);
  SYNC_DL_LEN        = 64;
  SYNC_UL_LEN        = 128;
  SYNC_DL_GAP        = 32;
  SYNC_UL_GAP        = 96;
  SYNC_UL_GAP2       = 32;
  SPECIAL_LEN        = SYNC_DL_GAP + SYNC_DL_LEN + SYNC_UL_GAP + SYNC_UL_LEN + SYNC_UL_GAP2;
  SUBFRAME_LEN       = 7 * SLOT_LEN + SPECIAL_LEN; // 6400
  SAMPLE_RATE        = 1280000;

type

  TTdsSlot = (ts0, tsSpecial, ts1, ts2, ts3, ts4, ts5, ts6);

  TTdsGraph = class;

  { TTdsEventList }

  TTdsEventList = class(TEventList)
  private
  public
    function Add(const ASlot: TTdsSlot; const AChip: Word;
      const ATag: string = ''; const AColor: TColor = clDefault): Integer; overload;

    function Add(const ASlot: TTdsSlot; const AChip: Word;
      const ATag: string; const AColor: TColor; const AStyle: TEventStyle): Integer;
    function Add(const ASlot: TTdsSlot; const AChip: Word;
      const AColor: TColor; const AStyle: TEventStyle): Integer;
  end;

  { TTdsGraph }

  TTdsGraph = class(TTimingGraph)
  private
    procedure DrawSlots;
    function GetSFN: Integer;
    procedure SetSFN(AValue: Integer);

  protected
    procedure DoDrawBackground; override;
    procedure DoDrawFgSimple; override;
    procedure SetSampleRate(AValue: Cardinal); override;
  public
    constructor Create;
    destructor Destroy; override;

    // properies for subframe
    property SFN: Integer read GetSFN write SetSFN;
  end;

const
  TDS_SLOT_NAME: array [TTdsSlot] of string =
    ('0','Special','1','2','3','4','5','6');

implementation

uses
  KLib;

function StartSampleOfSlot(const ASlot: TTdsSlot): Integer;
begin
  if ASlot <= tsSpecial then
    Result := Ord(ASlot) * SLOT_LEN
  else
    Result := (Ord(ASlot) - 1) * SLOT_LEN + SPECIAL_LEN;
end;

function SlotLength(const ASlot: TTdsSlot): Integer;
begin
  Result := SLOT_LEN;
  if ASlot = tsSpecial then Result := SPECIAL_LEN;
end;

function EndSampleOfSlot(const ASlot: TTdsSlot): Integer;
begin
  Result := StartSampleOfSlot(ASlot) + SlotLength(ASlot);
end;

{ TTdsEventList }

function TTdsEventList.Add(const ASlot: TTdsSlot; const AChip: Word;
  const ATag: string; const AColor: TColor): Integer;
begin
  Result := Add(ASlot, AChip, ATag, AColor, Style);
end;

function TTdsEventList.Add(const ASlot: TTdsSlot; const AChip: Word;
  const AColor: TColor; const AStyle: TEventStyle): Integer;
begin
  Result := Add(ASlot, AChip, Tag, AColor, AStyle);
end;

function TTdsEventList.Add(const ASlot: TTdsSlot; const AChip: Word;
  const ATag: string; const AColor: TColor; const AStyle: TEventStyle
  ): Integer;
var
  P: PTimingEvent;
begin
  Result := -1;

  New(P);
  P^.Time.Sample := StartSampleOfSlot(ASlot) + AChip;
  if ATag = '' then P^.Tag := Tag else P^.Tag := ATag;
  if AColor = clDefault then
    P^.Color := Color
  else
    P^.Color := AColor;
  P^.Style := AStyle;

  Result := InsertEvent(P);

  Notify;
end;

{ TTdsGraph }

procedure TTdsGraph.DrawSlots;
var
  I, J: Integer;
  X, Y, W: Integer;
  S: TSize;
  SlotLen: Double;
  T: TTdsSlot;
  C: string;
  E: TEventTime;
  Canvas: TCanvas;
  PW: Integer;
  PS: TPenStyle;
  function TryDrawLine(const Sample: Integer): Boolean;
  var
    Ev: TEventTime;
    X0: Integer;
  begin
    Ev.Sample := Sample;
    X0 := TimeToScreen(Ev);
    Result := InRange(X0, FGridRect.Left, FGridRect.Right);
    if Result then
      Canvas.Line(X0, FGridRect.Top, X0, FGridRect.Bottom);
  end;

begin
  Canvas := FDoubleBuffer.DrawBuffer.Canvas;
  with Canvas do
  begin
    PW := Pen.Width;
    PS := Pen.Style;
  end;
  S := FDoubleBuffer.DrawBuffer.Canvas.TextExtent('0');
  Y := FGridRect.Bottom + (FMarginBottom - S.cy) div 2;
  X := - 2 * SUBFRAME_LEN;
  for I := -1 to 1 do
  begin
    Inc(X, SUBFRAME_LEN);

    for T in TTdsSlot do
    begin
      E.Sample := StartSampleOfSlot(T) + X;
      J := TimeToScreen(E);
      TryDrawLine(E.Sample);

      with Canvas do
      begin
        Pen.Width := 1;
        Pen.Style := psDot;
      end;

      if T = tsSpecial then
      begin
        TryDrawLine(E.Sample + SYNC_DL_GAP + SYNC_DL_LEN);
        TryDrawLine(E.Sample + SYNC_DL_GAP + SYNC_DL_LEN+ SYNC_UL_GAP + SYNC_UL_LEN);
      end
      else begin
        TryDrawLine(E.Sample + DATA_LEN);
        TryDrawLine(E.Sample + DATA_LEN + MIDAMBLE_LEN);
        TryDrawLine(E.Sample + DATA_LEN * 2 + MIDAMBLE_LEN);
      end;

      with Canvas do
      begin
        Pen.Width := PW;
        Pen.Style := PS;
      end;

      if I < 0 then
        C := '- ' + TDS_SLOT_NAME[T]
      else if I = 0 then
        C := TDS_SLOT_NAME[T]
      else
        C := '+ ' + TDS_SLOT_NAME[T];

      S := FDoubleBuffer.DrawBuffer.Canvas.TextExtent(C);
      SlotLen := FPixelsPerSample * SlotLength(T);
      if S.cx + 2 > SlotLen then Continue;

      W := Round(SlotLen);
      J := J + W div 2 - (S.cx div 2);
      if InRange(J, FGridRect.Left, FGridRect.Right) then
      begin
        Canvas.TextOut(J, Y, C);
      end;
    end;
  end;

  E.Sample := EndSampleOfSlot(ts6) + X;
  J := TimeToScreen(E);
  if InRange(J, FGridRect.Left, FGridRect.Right) then
    Canvas.Line(J, FGridRect.Top, J, FGridRect.Bottom);
end;

procedure TTdsGraph.DoDrawBackground;
var
  S: string;
begin
  // draw common parameters
  S := Format('TD-SCDMA         Sample Rate: %s', [FormatFreq(FSampleRate)]);
  with FDoubleBuffer.DrawBuffer.Canvas do
  begin
    TextOut(10, 10, S);
    //TextOut(10, 20, Format('%d, %d', [FViewStartTime.TTI, FViewStartTime.Sample]));
  end;

  DrawSlots;
end;

procedure TTdsGraph.DoDrawFgSimple;
var
  S: string;
  C: TCanvas;
begin
  S := Format('Frame: %d', [SFN]);
  C := FDoubleBuffer.PaintBuffer.Canvas;
  C.TextOut(3, FDoubleBuffer.Height - C.TextHeight(S) - 3, S);
end;

function TTdsGraph.GetSFN: Integer;
begin
  Result := FrameId;
end;

procedure TTdsGraph.SetSampleRate(AValue: Cardinal);
begin
  if AValue <> SAMPLE_RATE then raise Exception.Create('invalid sample rate for tds');
  if FSampleRate = AValue then Exit;
  FFrameLength.Sample := SUBFRAME_LEN;
  FPanMin.Sample := -FFrameLength.Sample;
  inherited SetSampleRate(AValue);
end;

procedure TTdsGraph.SetSFN(AValue: Integer);
begin
  FrameId := AValue;
end;

constructor TTdsGraph.Create;
begin
  inherited Create(ettSample);

  SampleRate := SAMPLE_RATE;
end;

destructor TTdsGraph.Destroy;
begin
  inherited Destroy;
end;

end.

