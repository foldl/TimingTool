unit LteGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo, Controls, Graphics, ExtCtrls, Math,
  TimingGraph;

const
  TTI_PER_FRAME  = 10;
  SUB_CARRIER_PER_RB = 12;
  RE_PER_SLOT_NORMAL = 7;
  RE_PER_SLOT_EXT    = 6;

type

  TFrameType = (ftFDD, ftTDD);

  TSubframeAlloc = 0..6;

  TSpecialSubframeConfig = 0..8;

  TCPMode = (cmNormal, cmExtended);

  TBandwidthRB = 6..110;

  TLteGraph = class;

  { TLteEventList }

  TLteEventList = class(TEventList)
  private
  public
    function Add(const ATTI: Byte; const ASample: Word;
      const ATag: string = ''; const AColor: TColor = clDefault): Integer; overload;

    function Add(const ATTI: Byte; const ASample: Word;
      const ATag: string; const AColor: TColor; const AStyle: TEventStyle): Integer;
    function Add(const ATTI: Byte; const ASample: Word;
      const AColor: TColor; const AStyle: TEventStyle): Integer;
  end;

  { TLteGraph }

  TLteGraph = class(TTimingGraph)
  private
    FBandwidthRB: TBandwidthRB;
    FCPMode: TCPMode;
    FFrameType: TFrameType;
    FSpecialSubframeConfig: TSpecialSubframeConfig;
    FSubframeAlloc: TSubframeAlloc;
    FYPixelsPerRE: Double;
    procedure SetBandwidthRB(AValue: TBandwidthRB);
    procedure SetCPMode(AValue: TCPMode);
    procedure SetFrameType(AValue: TFrameType);
    procedure SetSpecialSubframeConfig(AValue: TSpecialSubframeConfig);
    procedure SetSubframeAlloc(AValue: TSubframeAlloc);

    procedure DrawRE;
    procedure DrawRB;
    procedure DrawTTI;

  protected
    procedure DoDrawBackground; override;
    procedure DoDrawFgSimple; override;
    procedure SetSampleRate(AValue: Cardinal); override;
  private
    FREPerTTI: Integer;
    FShowRB: Boolean;
    FShowRE: Boolean;
    FSamplesPerTTI: Integer;

    function GetSFN: Integer;
    procedure SetSFN(AValue: Integer);
    procedure SetShowRB(AValue: Boolean);
    procedure SetShowRE(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property FrameType: TFrameType read FFrameType write SetFrameType;
    property SubframeAlloc: TSubframeAlloc read FSubframeAlloc write SetSubframeAlloc;
    property SpecialSubframeConfig: TSpecialSubframeConfig read FSpecialSubframeConfig write SetSpecialSubframeConfig;
    property CPMode: TCPMode read FCPMode write SetCPMode;
    property BandwidthRB: TBandwidthRB read FBandwidthRB write SetBandwidthRB;
    property TTISamples: Integer read FSamplesPerTTI;
    property REPerTTI: Integer read FREPerTTI;

    // properies for subframe
    property SFN: Integer read GetSFN write SetSFN;

    property ShowRE: Boolean read FShowRE write SetShowRE;
    property ShowRB: Boolean read FShowRB write SetShowRB;
  end;

const
  LTE_TDD_SUBFRAME_NAME: array [TSubframeAlloc] of array [0..TTI_PER_FRAME - 1] of string =
    (('0 ↓','1 S','2 ↑','3 ↑','4 ↑',  '5 ↓','6 S','7 ↑','8 ↑','9 ↑'),
     ('0 ↓','1 S','2 ↑','3 ↑','4 ↓',  '5 ↓','6 S','7 ↑','8 ↑','9 ↓'),
     ('0 ↓','1 S','2 ↑','3 ↓','4 ↓',  '5 ↓','6 S','7 ↑','8 ↓','9 ↓'),
     ('0 ↓','1 S','2 ↑','3 ↑','4 ↑',  '5 ↓','6 ↓','7 ↓','8 ↓','9 ↓'),
     ('0 ↓','1 S','2 ↑','3 ↑','4 ↓',  '5 ↓','6 ↓','7 ↓','8 ↓','9 ↓'),
     ('0 ↓','1 S','2 ↑','3 ↓','4 ↓',  '5 ↓','6 ↓','7 ↓','8 ↓','9 ↓'),
     ('0 ↓','1 S','2 ↑','3 ↑','4 ↑',  '5 ↓','6 S','7 ↑','8 ↑','9 ↓'));

implementation

uses
  KLib;

{ TLteEventList }

function TLteEventList.Add(const ATTI: Byte; const ASample: Word;
  const ATag: string; const AColor: TColor): Integer;
begin
  Result := Add(ATTI, ASample, ATag, AColor, Style);
end;

function TLteEventList.Add(const ATTI: Byte; const ASample: Word;
  const AColor: TColor; const AStyle: TEventStyle): Integer;
begin
  Result := Add(ATTI, ASample, Tag, AColor, AStyle);
end;

function TLteEventList.Add(const ATTI: Byte; const ASample: Word;
  const ATag: string; const AColor: TColor; const AStyle: TEventStyle
  ): Integer;
var
  P: PTimingEvent;
begin
  Result := -1;
  if not InRange(ATTI, 0, TTI_PER_FRAME - 1) then Exit;

  New(P);
  P^.Time.Sample := ATTI * (Graph as TLteGraph).TTISamples + ASample;
  if ATag = '' then P^.Tag := Tag else P^.Tag := ATag;
  if AColor = clDefault then
    P^.Color := Color
  else
    P^.Color := AColor;
  P^.Style := AStyle;

  Result := InsertEvent(P);

  Notify;
end;

{ TLteGraph }

procedure TLteGraph.SetBandwidthRB(AValue: TBandwidthRB);
begin
  if FBandwidthRB = AValue then Exit;
  FBandwidthRB := AValue;
  FullRedraw;
end;

procedure TLteGraph.SetCPMode(AValue: TCPMode);
begin
  if FCPMode = AValue then Exit;
  FCPMode := AValue;
  if FCPMode = cmNormal then FREPerTTI := 2 * RE_PER_SLOT_NORMAL
  else FREPerTTI := 2 * RE_PER_SLOT_EXT;
  FullRedraw;
end;

procedure TLteGraph.SetFrameType(AValue: TFrameType);
begin
  if FFrameType = AValue then Exit;
  FFrameType := AValue;
  FullRedraw;
end;

procedure TLteGraph.SetSpecialSubframeConfig(AValue: TSpecialSubframeConfig);
begin
  if FSpecialSubframeConfig = AValue then Exit;
  FSpecialSubframeConfig := AValue;
  FullRedraw;
end;

procedure TLteGraph.SetSubframeAlloc(AValue: TSubframeAlloc);
begin
  if FSubframeAlloc = AValue then Exit;
  FSubframeAlloc := AValue;
  FullRedraw;
end;

procedure TLteGraph.DrawRE;
begin

end;

procedure TLteGraph.DrawRB;
begin

end;

procedure TLteGraph.DrawTTI;
var
  I: Integer;
  X, Y, W: Integer;
  S: TSize;
  TTI: Double;
  DX: Double;
  C: string;
  E: TEventTime;
  function FormatTTI(const V: Integer): string;
  begin
    if FFrameType = ftFDD then
      Result := IntToStr(V)
    else
      Result := LTE_TDD_SUBFRAME_NAME[FSubframeAlloc][V];
  end;

begin
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
  end;
end;

procedure TLteGraph.DoDrawBackground;
var
  S: string;
begin
  // draw common parameters
  S := Format('Mode: %s               BW: %sHz         Sample Rate: %s',
              [GetEnumName(TypeInfo(TFrameType), Ord(FFrameType)),
               FormatFreq(FBandwidthRB * 180000), FormatFreq(FSampleRate)]);
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

  if FShowRE then DrawRE;
  if FShowRB then DrawRB;
  DrawTTI;
end;

procedure TLteGraph.DoDrawFgSimple;
var
  S: string;
  C: TCanvas;
begin
  S := Format('Frame: %d', [SFN]);
  C := FDoubleBuffer.PaintBuffer.Canvas;
  C.TextOut(3, FDoubleBuffer.Height - C.TextHeight(S) - 3, S);
end;

function TLteGraph.GetSFN: Integer;
begin
  Result := FrameId;
end;

procedure TLteGraph.SetSampleRate(AValue: Cardinal);
begin
  if FSampleRate = AValue then Exit;
  AValue := (Max(20000, AValue) div 1000) * 1000;
  FSamplesPerTTI := AValue div 1000;
  FFrameLength.Sample := FSamplesPerTTI * 10;
  FPanMin.Sample := -FFrameLength.Sample;
  inherited SetSampleRate(AValue);
end;

procedure TLteGraph.SetSFN(AValue: Integer);
begin
  FrameId := AValue;
end;

procedure TLteGraph.SetShowRB(AValue: Boolean);
begin
  if FShowRB = AValue then Exit;
  FShowRB := AValue;
end;

procedure TLteGraph.SetShowRE(AValue: Boolean);
begin
  if FShowRE = AValue then Exit;
  FShowRE := AValue;
  FullRedraw;
end;

constructor TLteGraph.Create;
begin
  inherited Create(ettSample);
  FREPerTTI := 2 * RE_PER_SLOT_NORMAL;

  FFrameType := ftTDD;
  FBandwidthRB := 110;

  SampleRate := 15000*2048;
end;

destructor TLteGraph.Destroy;
begin
  inherited Destroy;
end;

end.

