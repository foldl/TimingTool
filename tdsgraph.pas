unit TdsGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo, Controls, Graphics, ExtCtrls, Math,
  TimingGraph;

const
  DATA_LEN           = 352;
  MIDAMBLE_LEN       = 144;
  SLOT_LEN           = (2 * DATA_LEN + 128 + 16);
  SYNC_DL_LEN        = 64;
  SYNC_UL_LEN        = 128;
  SPECIAL_LEN        = 32 + SYNC_DL_LEN + 96 + SYNC_UL_LEN + 32;
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
  if ASlot <= tsSpecial then
    P^.Time.Sample := Ord(ASlot) * SLOT_LEN + AChip
  else
    P^.Time.Sample := (Ord(ASlot) - 1) * SLOT_LEN + SPECIAL_LEN + AChip;
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
  I: Integer;
  X, Y, W: Integer;
  S: TSize;
  TTI: Double;
  DX: Double;
  C: string;
  E: TEventTime;

begin   {
  TTI := FPixelsPerSample * FSamplesPerTTI;
  S := FDoubleBuffer.DrawBuffer.Canvas.TextExtent('0');
  W := Round(TTI);
  Y := FGridRect.Bottom + (FMarginBottom - S.cy) div 2;
  I := FViewStartTime.Sample div FSamplesPerTTI;
  if FViewStartTime.Sample < 0 then Dec(I);
  DX := FGridRect.Left - (FViewStartTime.Sample - I * FSamplesPerTTI) * FPixelsPerSample;
  Dec(I);
  while True do
  begin
    Inc(I);
    X := Round(DX);
    DX := DX + TTI;
    if (X > FGridRect.Right) or (I >= 2 * TTI_PER_FRAME) then Break;
    if InRange(X, FGridRect.Left, FGridRect.Right) then
    begin
      with FDoubleBuffer.DrawBuffer.Canvas do
      begin
        Line(X, FGridRect.Top, X, FGridRect.Bottom);
      end;
    end;
    if I < -TTI_PER_FRAME then Continue;

    if I < 0 then
      C := '- ' + FormatTTI(TTI_PER_FRAME + I)
    else if I < TTI_PER_FRAME then
      C := FormatTTI(I)
    else
      C := '+ ' + FormatTTI(I - TTI_PER_FRAME);

    S := FDoubleBuffer.DrawBuffer.Canvas.TextExtent(C);
    if S.cx + 10 > TTI then Continue;
    X := X + W div 2 - (S.cx div 2);
    if InRange(X, FGridRect.Left, FGridRect.Right) then
    begin
      with FDoubleBuffer.DrawBuffer.Canvas do
        TextOut(X, Y, C);
    end;
  end;  }
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

  with FDoubleBuffer.DrawBuffer.Canvas do
  begin
    Pen.Color := FGridColor;
    Pen.Width := 2;
    Rectangle(FGridRect);
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

