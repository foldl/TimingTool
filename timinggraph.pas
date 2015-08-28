unit TimingGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo, Controls, Graphics, ExtCtrls, Math, fgl, MouseTool;

type

  { TDoubleBuffer }

  TDoubleBuffer = class
  private
    FDrawBuffer: TBitmap;
    FHeight: Integer;
    FPaintBox: TPaintBox;
    FPaintBuffer: TBitmap;
    FWidth: Integer;
    procedure OnPaint(Sender: TObject);
    procedure SetPaintBox(AValue: TPaintBox); virtual;
    procedure Draw2Paint2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw2Paint;
    procedure Repaint;

    procedure SetSize(W, H: Integer); virtual;
    property DrawBuffer: TBitmap read FDrawBuffer;
    property PaintBuffer: TBitmap read FPaintBuffer;
    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TEventStyle = TPenStyle;

  TEventTimeType = (ettSample, ettMilliSec, ettTime);
  TEventTime = record
    case TEventTimeType of
      ettSample: // Frame -> Sample
        (Sample: Integer);
      ettMilliSec:
        (TimeMS: Double);
      ettTime:
        (Time: TTime)
  end;

  TTimingEvent = record
    Time: TEventTime;
    Duration: TEventTime;
    Tag: string;
    Color: TColor;
    Style: TEventStyle;
    X: Integer;
    X1: Integer; // end of duration
  end;
  PTimingEvent = ^TTimingEvent;

  TTimingGraph = class;

  TFPListList = specialize TFPGObjectList<TFPList>;

  { TFrameList }

  generic TFrameList<T> = class
  private
    type
      PT = ^T;
  private
    FDispose: Boolean;
    FList: TFPList;
    FFrames: TFPListList;
    FFrameIndex: Integer;
    function GetFrameCount: Integer;
    procedure SetFrameIndex(AValue: Integer);
    procedure FreeFun(Data, Arg: Pointer);
  public
    constructor Create(DisposeMem: Boolean = True);
    destructor Destroy; override;

    procedure NewFrame;

    procedure ClearAll;
    procedure Clear;

    property FrameCount: Integer read GetFrameCount;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex;
    property Frame: TFPList read FList;
  end;

  TEventFrameList = specialize TFrameList<TTimingEvent>;

  { TEventList }

  TEventList = class
  private
    FCaption: string;
    FColor: TColor;
    FHasDuration: Boolean;
    FFrames: TEventFrameList;
    FMarkerSize: Integer;
    FPosition: Integer;
    FSize: Integer;
    FStyle: TEventStyle;
    FTag: string;
    FGraph: TTimingGraph;
    function GetCount: Integer;
    function GetEvent(const Index: Integer): PTimingEvent;
    function GetFrameCount: Integer;
    function GetFrameIndex: Integer;
    function GetList: TFPList;
    function GetRemoteEvent(const FrameIndex, Index: Integer): PTimingEvent;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetDrawDuration(AValue: Boolean);
    procedure SetFrameIndex(AValue: Integer);
    procedure SetMarkerSize(AValue: Integer);
    procedure SetSize(AValue: Integer);
    procedure SetStyle(AValue: TEventStyle);
    procedure SetTag(AValue: string);

    procedure NewFrame;
    property FrameCount: Integer read GetFrameCount;
    property FrameIndex: Integer read GetFrameIndex write SetFrameIndex;
  protected
    procedure Notify;
    function GetTimeLabel(ATime: TEventTime): string; virtual;
    procedure DrawSimple(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawDuration(ACanvas: TCanvas; ARect: TRect); virtual;
    function FindSimpleEvent(const X: Integer; const Tolerance: Integer = 5): Integer;
    function FindDurationEvent(const X: Integer; const Tolerance: Integer = 5): Integer;
    procedure LayoutSimple(const ARect: TRect); virtual;
    procedure LayoutDuration(const ARect: TRect); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function NewEvent: PTimingEvent;
    function InsertEvent(P: PTimingEvent): Integer;
    function Add(const Event: TTimingEvent): Integer;
    procedure ClearAll;
    procedure Clear;

    function FindEvent(const X: Integer; const Tolerance: Integer = 5): Integer;
    function FindFirstEvent(const X: Integer; const Tolerance: Integer = 5): Integer;

    procedure DrawRainbowLine(ACanvas: TCanvas; const AX, AY, AHeight: Integer);
    procedure Layout(const ARect: TRect);
    procedure Draw(ACanvas: TCanvas; ARect: TRect); virtual;

    property Tag: string read FTag write SetTag;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Position: Integer read FPosition;
    property Size: Integer read FSize write SetSize;
    property Style: TEventStyle read FStyle write SetStyle;
    property MarkerSize: Integer read FMarkerSize write SetMarkerSize;
    property HasDuration: Boolean read FHasDuration write SetDrawDuration;

    property Graph: TTimingGraph read FGraph;
    property Count: Integer read GetCount;
    property Event[const Index: Integer]: PTimingEvent read GetEvent; default;
    property RemoteEvent[const AFrameIndex, Index: Integer]: PTimingEvent read GetRemoteEvent;
  end;

  TLteEventListList = specialize TFPGObjectList<TEventList>;

  TToolMeasure = class;

  TEventListForEach = procedure (EList: TEventList; P: PTimingEvent; Param: Pointer);
  TEventListForEachMethod = procedure (EList: TEventList; P: PTimingEvent; Param: Pointer) of object;

  { TTimingGraph }

  TTimingGraph = class
  const
    RANGE = 3;
  protected
    FPaintBox: TPaintBox;
    FDoubleBuffer: TDoubleBuffer;
    FGridRect: TRect;
    FPixelsPerSample: Double;  // x-axis
    FPixelsPerMS: Double;
    FViewStartTime: TEventTime;  // bottom-left of view
    FUpdateCounter: Integer;
    FNeedFullRedraw: Boolean;
    FToolMan: TMouseToolMan;
    FShowAll: Boolean;
    FFrames: array of Integer;   // Frame ID or No
    FMeasureTool: TToolMeasure;
    FBackgroundColor: TColor;
    FFrameLength: TEventTime;
    FGridColor: TColor;
    FLabelColor: TColor;
    FMarginBottom: Integer;
    FMarginEventList: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FPanMax: TEventTime;
    FPanMin: TEventTime;
    FSampleRate: Cardinal;
    FShowEventLabel: Boolean;
    FShowEventTiming: Boolean;
    FEvents: TLteEventListList;
    FFrameCount: Integer;
    FFrameIndex: Integer;
    FTimeType: TEventTimeType;
  private
    type
    TForEachParam = record
      Fun: TEventListForEach;
      Param: Pointer;
    end;
    PForEachParam = ^TForEachParam;
    procedure SetPaintBox(AValue: TPaintBox);

    procedure DrawBackground;
    procedure DrawEventsList;
    procedure DrawEvents(E: TEventList);

    procedure EventListChanged;

    function FindEventList(const X, Y: Integer): TEventList;
    function FindFirstEvent(const X, Y: Integer; out AList: TEventList; out AFrame: Integer): Integer;
    function FindEvent(const X, Y: Integer; out AList: TEventList; out AFrame: Integer): Integer;

    procedure ForEachCB(AList: TEventList; P: PTimingEvent; Param: PForEachParam);
    procedure ForEach(const X, Y: Integer;  const AFun: TEventListForEach; Param: Pointer;
      const Tolerance: Integer = 5); overload;
    procedure ForEach(const X, Y: Integer;  const AFun: TEventListForEachMethod; Param: Pointer;
      const Tolerance: Integer = 5);

    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ToolWindowZoom(Sender: TObject; Rect: TRect);
    procedure ToolWindowPan(Sender: TObject; Delta: TPoint);
  protected
    procedure DoDrawBackground; virtual;
    procedure DoDrawFgSimple; virtual;
    procedure DoResize; virtual;
    procedure FullRedraw;
    procedure Redraw;
  public
    function ScreenToTime(const X: Integer): TEventTime; virtual;
    function TimeToScreen(const T: TEventTime): Integer; virtual;
    function ScreenToTimeSec(const X: Integer): Double; virtual;
    function ScreenDiffToTimeSec(const X: Integer): Double; virtual;
    function TimeAdd(const A, B: TEventTime): TEventTime;
    function TimeSubtract(const A, B: TEventTime): TEventTime;
    function TimeNeg(const A: TEventTime): TEventTime;
    function TimeAbs(const A: TEventTime): TEventTime;
    function TimeDiffInSec(const A, B: TEventTime): Double;  // (A - B) sec
    function TimeDiff(const A, B: TEventTime): TEventTime;  // abs(A - B)
    function TimeCompare(const A, B: TEventTime): Integer;   // A - B
    function TimeIncSec(const A: TEventTime; const Off: Double; var B: TEventTime): Boolean;

    function FrameTimeMultiply(const N: Integer): Double; virtual;

    function FormatTimeShort(const T: TEventTime): string; virtual;
    procedure FormatTimeLong(const T: TEventTime; L: TStrings); virtual;
    function FormatSecTimeShort(const T: Double): string; virtual;
    procedure FormatSecTimeLong(const T: Double; L: TStrings); virtual;
  private
    FEventMaxHeight: Integer;
    function GetEventList(const Index: Integer): TEventList;
    function GetEventListCount: Integer;
    function GetFrameId: Integer;
    function GetPanMin: TEventTime;
    procedure PaintBoxResize(Sender: TObject);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetEventHeight(AValue: Integer);
    procedure SetFrameId(AValue: Integer);
    procedure SetFrameLength(AValue: TEventTime);
    procedure SetGridColor(AValue: TColor);
    procedure SetLabelColor(AValue: TColor);
    procedure SetMarginBottom(AValue: Integer);
    procedure SetMarginEventList(AValue: Integer);
    procedure SetMarginLeft(AValue: Integer);
    procedure SetMarginRight(AValue: Integer);
    procedure SetMarginTop(AValue: Integer);
    procedure SetShowEventLabel(AValue: Boolean);
    procedure SetShowEventTiming(AValue: Boolean);
    procedure SetFrameIndex(AValue: Integer);
    procedure UpdatePanMax;
  protected
    procedure SetSampleRate(AValue: Cardinal); virtual;
  protected
    property FrameId: Integer read GetFrameId write SetFrameId;
  public
    constructor Create(ATimeType: TEventTimeType);
    destructor Destroy; override;

    function AddEventList(AList: TEventList): Integer;

    procedure BeginUpdate;
    procedure EndUpdate(const ARedraw: Boolean = True);

    procedure Clear;
    procedure NewFrame;

    procedure ShowAll;

    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;

    property FrameCount: Integer read FFrameCount;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property GridColor: TColor read FGridColor write SetGridColor;
    property LabelColor: TColor read FLabelColor write SetLabelColor;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property EventMaxHeight: Integer read FEventMaxHeight write SetEventHeight;

    property MarginEventList: Integer read FMarginEventList write SetMarginEventList;

    property EventList[const Index: Integer]: TEventList read GetEventList;
    property EventListCount: Integer read GetEventListCount;
    property ShowEventLabel: Boolean read FShowEventLabel write SetShowEventLabel;
    property ShowEventTiming: Boolean read FShowEventTiming write SetShowEventTiming;

    property TimeType: TEventTimeType read FTimeType;
    property SampleRate: Cardinal read FSampleRate write SetSampleRate;
    property FrameLength: TEventTime read FFrameLength write SetFrameLength;

    property PanMin: TEventTime read FPanMin;
    property PanMax: TEventTime read FPanMax;
  end;

  { TToolMeasure }

  TToolMeasure = class(TMouseTool)
  private
    FArrowColor: TColor;
    FGraph: TTimingGraph;
    FLabelColor: TColor;
    FMarkerColor: TColor;
    FPt: TPoint;
    FToPt: TPoint;
    FFrameIndex: Integer;
    FEventList: TEventList;
    FIndex: Integer;
    FToFrameIndex: Integer;
    FToEventList: TEventList;
    FToIndex: Integer;
    procedure DoPaint(C: TCanvas);
    procedure SetArrowColor(AValue: TColor);
    procedure SetLabelColor(AValue: TColor);
    procedure SetMarkerColor(AValue: TColor);
  protected
    function OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    function OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; override;
    function OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    procedure Paint; override;
  public
    constructor Create(Graph: TTimingGraph);

    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property LabelColor: TColor read FLabelColor write SetLabelColor;
    property MarkerColor: TColor read FMarkerColor write SetMarkerColor;
  end;

implementation

{ TToolMeasure }

function TToolMeasure.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if not Assigned(FGraph) then Exit;
  if Button <> mbLeft then Exit;
  if not (ssShift in Shift) then Exit;
  FIndex := FGraph.FindEvent(X, Y, FEventList, FFrameIndex);
  FToIndex := -1;
  Result := FIndex >= 0;
  if Result then
  begin
    FPt.x := FEventList.RemoteEvent[FFrameIndex, FIndex]^.X;
    FPt.y := Y;
    FToPt := FPt;
  end;
end;

function TToolMeasure.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := False;
  if not Assigned(FGraph) then Exit;
  if not Active then Exit;
  FToPt.x := X;
  Result := True;
  FToIndex := FGraph.FindEvent(X, Y, FToEventList, FToFrameIndex);
  if FToIndex >= 0 then
  begin
    FToPt.x := FToEventList.RemoteEvent[FToFrameIndex, FToIndex]^.X;
  end;
end;

function TToolMeasure.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if not Assigned(FGraph) then Exit;
  if Active then
  begin
    if Assigned(Manager.PersistCanvas) then
      DoPaint(Manager.PersistCanvas);
    //FGraph.Redraw;
  end;
end;

procedure TToolMeasure.Paint;
begin
  DoPaint(Manager.PaintBox.Canvas);
end;

procedure TToolMeasure.DoPaint(C: TCanvas);
const
  ASIZE = 6;
  ASIZEY = 4;
  LINE_MARGIN = 2;
var
  P1, P2: PTimingEvent;
  X, Y: Integer;
  O: TEventTime;
  S: TStringList;
  E: TSize;
  T: TTextStyle;

  procedure DrawArrow(const X, Y, Dir: Integer);
  begin
    C.Line(X, Y, X + Dir * ASIZE, Y + ASIZEY);
    C.Line(X, Y, X + Dir * ASIZE, Y - ASIZEY);
  end;

  function Measure: TSize;
  var
    X: string;
    E: TSize;
  begin
    Result.cy := 0;
    Result.cx := 0;
    for X in S do
    begin
      E := C.TextExtent(X);
      Inc(Result.cy, LINE_MARGIN + E.cy);
      Result.cx := Max(Result.cx, E.cx);
    end;
  end;
  procedure Draw(X, Y: Integer);
  var
    A: string;
    E: TSize;
  begin
    for A in S do
    begin
      E := C.TextExtent(A);
      C.TextOut(X, Y, A);
      Inc(Y, LINE_MARGIN + E.cy);
    end;
  end;

begin
  T := C.TextStyle;
  T.SingleLine := False;
  C.TextStyle := T;
  C.Pen.Style := psSolid;
  S := TStringList.Create;

  if FToIndex < 0 then
    FGraph.FormatSecTimeLong(FGraph.ScreenDiffToTimeSec(Abs(FPt.x - FToPt.x)), S)
  else begin
    P1 := FEventList.RemoteEvent[FFrameIndex, FIndex];
    P2 := FToEventList.RemoteEvent[FToFrameIndex, FToIndex];
    O := FGraph.TimeDiff(P1^.Time, P2^.Time);
    FGraph.FormatTimeLong(O, S);

    if FEventList <> FToEventList then
    begin
      C.Pen.Color := FMarkerColor;
      C.Pen.Width := 1;
      if FEventList.Position < FToEventList.Position then
        C.Line(FToPt.x, FToPt.y - 10, FToPt.x, FToEventList.Position)
      else
        C.Line(FToPt.x, FToPt.y + 10, FToPt.x, FToEventList.Position + FToEventList.Size);
    end;
  end;

  C.Pen.Color := FArrowColor;
  C.Pen.Width := 1;
  C.Line(FPt, FToPt);
  if Abs(FPt.x - FToPt.x) > 3 * ASIZE then
  begin
    //C.Pen.Width := 1;
    DrawArrow(Min(FPt.x, FToPt.x), FPt.y, 1);
    DrawArrow(Max(FPt.x, FToPt.x), FPt.y, -1)
  end;
  C.Brush.Style := bsClear;
  E := Measure;
  if FToPt.x > FPt.x then
    X := FToPt.x + 2 //  (FPt.x + FToPt.x - E.cx) div 2;
  else
    X := FToPt.x - E.cx - 2;
  Y := FPt.y - 2 - E.cy;
  C.Font.Color := FLabelColor;
  Draw(X, Y);

  S.Free;
end;

procedure TToolMeasure.SetArrowColor(AValue: TColor);
begin
  if FArrowColor = AValue then Exit;
  FArrowColor := AValue;
end;

procedure TToolMeasure.SetLabelColor(AValue: TColor);
begin
  if FLabelColor = AValue then Exit;
  FLabelColor := AValue;
end;

procedure TToolMeasure.SetMarkerColor(AValue: TColor);
begin
  if FMarkerColor = AValue then Exit;
  FMarkerColor := AValue;
end;

constructor TToolMeasure.Create(Graph: TTimingGraph);
begin
  inherited Create('Measure');
  FGraph := Graph;
  FLabelColor := clPurple;
  FArrowColor := clBlue;
  FMarkerColor := clBlack;
end;

{ TFrameList }

function TFrameList.GetFrameCount: Integer;
begin
  Result := FFrames.Count;
end;

procedure TFrameList.SetFrameIndex(AValue: Integer);
begin
  if FFrameIndex = AValue then Exit;
  FFrameIndex := AValue;
  FList := FFrames[AValue];
end;

procedure TFrameList.FreeFun(Data, Arg: Pointer);
begin
  if FDispose then Dispose(PT(Data));
end;

constructor TFrameList.Create(DisposeMem: Boolean);
begin
  FDispose := DisposeMem;
  FFrames := TFPListList.Create(True);
  NewFrame;
end;

destructor TFrameList.Destroy;
begin
  ClearAll;
  FFrames.Free;
  inherited Destroy;
end;

procedure TFrameList.NewFrame;
begin
  FList := TFPList.Create;
  FFrameIndex := FFrames.Add(FList);
end;

procedure TFrameList.ClearAll;
var
  L: TFPList;
begin
  while FFrames.Count > 1 do
  begin
    L := FFrames.Last;
    L.ForEachCall(@FreeFun, nil);
    FFrames.Delete(FFrames.Count - 1);
  end;
  FFrameIndex := 0;
  FList := FFrames[0];
  Clear;
end;

procedure TFrameList.Clear;
begin
  FList.ForEachCall(@FreeFun, nil);
  FList.Clear;
end;

{ TEventList }

procedure TEventList.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Notify;
end;

procedure TEventList.SetDrawDuration(AValue: Boolean);
begin
  if FHasDuration = AValue then Exit;
  FHasDuration := AValue;
  Notify;
end;

procedure TEventList.SetFrameIndex(AValue: Integer);
begin
  if FFrames.FrameIndex = AValue then Exit;
  FFrames.FrameIndex := AValue;
  Notify;
end;

procedure TEventList.SetMarkerSize(AValue: Integer);
begin
  if FMarkerSize = AValue then Exit;
  FMarkerSize := AValue;
  Notify;
end;

procedure TEventList.SetCaption(AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  Notify;
end;

function TEventList.GetCount: Integer;
begin
  Result := FFrames.Frame.Count;
end;

function TEventList.GetEvent(const Index: Integer): PTimingEvent;
var
  FList: TFPList;
begin
  FList := FFrames.Frame;
  if InRange(Index, 0, FList.Count - 1) then
    Result := PTimingEvent(FList[Index])
  else
    Result := nil;
end;

function TEventList.GetFrameCount: Integer;
begin
  Result := FFrames.FrameCount;
end;

function TEventList.GetFrameIndex: Integer;
begin
  Result := FFrames.FrameIndex;
end;

function TEventList.GetList: TFPList;
begin
  Result := FFrames.Frame;
end;

function TEventList.GetRemoteEvent(const FrameIndex, Index: Integer
  ): PTimingEvent;
var
  T: Integer;
begin
  T := FFrames.FrameIndex;
  FFrames.FrameIndex := FrameIndex;
  Result := Event[Index];
  FFrames.FrameIndex := T;
end;

procedure TEventList.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  Notify;
end;

procedure TEventList.SetStyle(AValue: TEventStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;
  Notify;
end;

procedure TEventList.SetTag(AValue: string);
begin
  if FTag = AValue then Exit;
  FTag := AValue;
  Notify;
end;

procedure TEventList.Notify;
begin
  if Assigned(FGraph) then FGraph.EventListChanged;
end;

function TEventList.InsertEvent(P: PTimingEvent): Integer;
var
  T: PTimingEvent;
  I, J, M: Integer;
  C: Integer;
  FList: TFPList;
begin
  FList := FFrames.Frame;
  I := 0;
  J := FList.Count - 1;
  while I <= J do
  begin
    M := (I + J) div 2;
    T := FList[M];
    C := FGraph.TimeCompare(P^.Time, T^.Time);
    if C = 0 then
      Break;
    if C < 0 then
      J := M - 1
    else
      I := M + 1
  end;
  Result := IfThen(I <= J, M, I);
  FList.Insert(Result, P);

  Notify;
end;

function TEventList.GetTimeLabel(ATime: TEventTime): string;
begin
  Result := FGraph.FormatTimeShort(ATime);
end;

constructor TEventList.Create;
begin
  FSize := 30;
  FMarkerSize := 2;
  FFrames := TEventFrameList.Create(True);
end;

destructor TEventList.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;

function TEventList.NewEvent: PTimingEvent;
begin
  New(Result);
  Result^.Color := FColor;
  Result^.Style := FStyle;
end;

function TEventList.Add(const Event: TTimingEvent): Integer;
var
  P: PTimingEvent;
begin
  New(P);
  P^ := Event;
  Result := InsertEvent(P);
  Notify;
end;

function TEventList.FindEvent(const X: Integer; const Tolerance: Integer
  ): Integer;
begin
  if FHasDuration then Result := FindDurationEvent(X, Tolerance)
  else Result := FindSimpleEvent(X, Tolerance);
end;

function TEventList.FindFirstEvent(const X: Integer; const Tolerance: Integer
  ): Integer;
var
  FList: TFPList;
begin
  FList := FFrames.Frame;
  Result := FindEvent(X, Tolerance);
  if (Result > 0) and (not FHasDuration) then
  begin
    while Result > 0 do
    begin
      Dec(Result);
      if Abs(PTimingEvent(FList[Result])^.X - X) > Tolerance then
      begin
        Break;
      end;
    end;
    Inc(Result);
  end;
end;

procedure TEventList.DrawRainbowLine(ACanvas: TCanvas; const AX, AY,
  AHeight: Integer);
const
  COLORS: array [0..4] of TColor = (clRed, clFuchsia, clLime, clGreen, clBlue);
var
  Y, Y1, K, M: Integer;
begin
  ACanvas.Pen.Style := psSolid;
  Y := AY;
  Y1 := AY + AHeight;
  M := 0;
  K := Max(1, AHeight div Length(COLORS));
  while Y < Y1 do
  begin
    ACanvas.Pen.Color := COLORS[M - 1];
    ACanvas.Line(AX, Y, AX, Y1);
    Inc(Y, K);
    Inc(M);
  end;
end;

procedure TEventList.Layout(const ARect: TRect);
begin
  if FHasDuration then LayoutDuration(ARect)
  else LayoutSimple(ARect);
end;

procedure TEventList.LayoutSimple(const ARect: TRect);
var
  I, N, Q: Integer;
  P: PTimingEvent;
  Offset: TEventTime;
begin
  N := FFrames.FrameIndex;
  Offset := FGraph.TimeNeg(FGraph.TimeAdd(FGraph.FrameLength, FGraph.FrameLength));

  for Q := N - 1 to N + 1 do
  begin
    if (Q < 0) or (Q >= FrameCount) then Continue;
    Offset := FGraph.TimeAdd(Offset, FGraph.FrameLength);
    FFrames.FrameIndex := Q;

    for I := 0 to Count - 1 do
    begin
      P := PTimingEvent(FFrames.FList[I]);
      P^.X := FGraph.TimeToScreen(FGraph.TimeAdd(P^.Time, Offset));
    end;
  end;
  FFrames.FrameIndex := N;
end;

procedure TEventList.LayoutDuration(const ARect: TRect);
var
  I, N, Q: Integer;
  P: PTimingEvent;
  Offset: TEventTime;
  T: TEventTime;
begin
  N := FFrames.FrameIndex;
  Offset := FGraph.TimeNeg(FGraph.TimeAdd(FGraph.FrameLength, FGraph.FrameLength));

  for Q := N - 1 to N + 1 do
  begin
    if (Q < 0) or (Q >= FrameCount) then Continue;
    Offset := FGraph.TimeAdd(Offset, FGraph.FrameLength);
    FFrames.FrameIndex := Q;

    for I := 0 to Count - 1 do
    begin
      P := PTimingEvent(FFrames.FList[I]);
      T := FGraph.TimeAdd(P^.Time, Offset);
      P^.X  := FGraph.TimeToScreen(T);
      P^.X1 := FGraph.TimeToScreen(FGraph.TimeAdd(P^.Duration, T));
    end;
  end;
  FFrames.FrameIndex := N;
end;

procedure TEventList.Draw(ACanvas: TCanvas; ARect: TRect);
begin
  if FHasDuration then DrawDuration(ACanvas, ARect)
  else DrawSimple(ACanvas, ARect);
end;

procedure TEventList.DrawSimple(ACanvas: TCanvas; ARect: TRect
  );
var
  S: TSize;
  C: TCanvas;
  X, Y, I, Q, N: Integer;
  P: PTimingEvent;
  Rainbow: Boolean = False;
  LastX: Integer = -1;
  G: string;
begin
  C := ACanvas;
  N := FrameIndex;
  for Q := N - 1 to N + 1 do
  begin
    if (Q < 0) or (Q >= FrameCount) then Continue;
    FFrames.FrameIndex := Q;

    for I := 0 to Count - 1 do
    begin
      P := PTimingEvent(FFrames.FList[I]);
      X := P^.X;

      if not InRange(X, ARect.Left, ARect.Right) then Continue;

      if X = LastX then
      begin
        if not Rainbow then
        begin
          DrawRainbowLine(C, X, Position, Size);
          Rainbow := True;
        end;
      end
      else begin
        C.Pen.Color := P^.Color;
        C.Pen.Style := P^.Style;
        Y := Position;
        C.Line(X, Y, X, Y + Self.Size);

        // draw label
        if FGraph.ShowEventLabel then
        begin
          S := C.TextExtent(P^.Tag);
          if X - LastX > S.cx then
            C.TextOut(X - S.cx div 2, Y - 2 - S.cy, P^.Tag);;
        end;
        if FGraph.ShowEventTiming then
        begin
          G := GetTimeLabel(P^.Time); ;
          S := C.TextExtent(G);
          if X - LastX > S.cx then
            C.TextOut(X - S.cx div 2, Y + Size + 2, G);
        end;

        LastX := X;
        Rainbow := False;
      end;
    end;
  end;
  FFrames.FrameIndex := N;
end;

procedure TEventList.DrawDuration(ACanvas: TCanvas; ARect: TRect);
var
  S: TSize;
  C: TCanvas;
  X, X0, X1, Y, I, Q, N: Integer;
  P: PTimingEvent;
  LastX: Integer = -1;
  G: string;
begin
  C := ACanvas;
  N := FrameIndex;
  for Q := N - 1 to N + 1 do
  begin
    if (Q < 0) or (Q >= FrameCount) then Continue;
    FFrames.FrameIndex := Q;

    for I := 0 to Count - 1 do
    begin
      P := PTimingEvent(FFrames.FList[I]);
      X0 := Max(P^.X, ARect.Left);
      X1 := Min(Max(P^.X + FMarkerSize, P^.X1), ARect.Right);

      if X1 < X0 then Continue;

      X := (X0 + X1) div 2;
      C.Brush.Color := P^.Color;
      case P^.Style of
        psDot, psDash, psDashDot, psDashDotDot: C.Brush.Style := bsCross;
        else C.Brush.Style := bsSolid;
      end;
      //C.Pen.Color := P^.Color;
      Y := Position;
      C.FillRect(X0, Y, X1, Y + Self.Size);
      C.Brush.Style := bsClear;

      // draw label
      if FGraph.ShowEventLabel then
      begin
        S := C.TextExtent(P^.Tag);
        if X - LastX > S.cx then
          C.TextOut(X - S.cx div 2, Y - 2 - S.cy, P^.Tag);;
      end;
      if FGraph.ShowEventTiming then
      begin
        G := GetTimeLabel(P^.Time); ;
        S := C.TextExtent(G);
        if X - LastX > S.cx then
          C.TextOut(X - S.cx div 2, Y + Size + 2, G);
      end;

      LastX := X;
    end;
  end;
  FFrames.FrameIndex := N;
end;

function TEventList.FindSimpleEvent(const X: Integer; const Tolerance: Integer
  ): Integer;
var
  P: PTimingEvent;
  I, J, M: Integer;
  C: Integer;
  FList: TFPList;
begin
  FList := FFrames.Frame;
  Result := -1;
  I := 0;
  J := FList.Count - 1;
  while I <= J do
  begin
    M := (I + J) div 2;
    P := FList[M];

    if P^.X = X then
    begin
      Result := M;
      Exit;
    end
    else if P^.X > X then
      J := M - 1
    else
      I := M + 1
  end;
  M := I;
  I := Max(0, Min(M, J));
  J := Min(FList.Count - 1, Max(M, J));
  C := X;
  for M := I to J do
  begin
    P := FList[M];
    if Abs(P^.X - X) < C then
    begin
      Result := M;
      C := Abs(P^.X - X);
    end;
  end;
  if C >= Tolerance then Result := -1;
end;

function TEventList.FindDurationEvent(const X: Integer; const Tolerance: Integer
  ): Integer;
var
  P: PTimingEvent;
  I: Integer;
  FList: TFPList;
begin
  // TODO: optimize
  FList := FFrames.Frame;
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    P := FList[I];

    if InRange(X, P^.X - Tolerance, P^.X1 + Tolerance) then
    begin
      Result := I;
      Exit;
    end
  end;
end;

procedure TEventList.NewFrame;
begin
  FFrames.NewFrame;
end;

procedure FreeEvent(Data, Arg: Pointer);
begin
  Dispose(PTimingEvent(Data));
end;

procedure TEventList.ClearAll;
begin
  FFrames.ClearAll;
  Notify;
end;

procedure TEventList.Clear;
begin
  FFrames.Clear;
  Notify;
end;

{ TTimingGraph }

procedure TTimingGraph.SetPaintBox(AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
  FPaintBox.Constraints.MinHeight := 200;
  FPaintBox.Constraints.MinWidth := 300;
  FPaintBox.OnResize := @PaintBoxResize;
  FPaintBox.OnMouseMove := @PaintBoxMouseMove;
  FDoubleBuffer.PaintBox := AValue;
  FToolMan.PaintBox := AValue;
  FGridRect.Left := FMarginLeft;
  FGridRect.Bottom := FDoubleBuffer.Height - FMarginBottom;
  FGridRect.Right := FDoubleBuffer.Width - FMarginRight;
  FGridRect.Top := FMarginTop;
  ShowAll;
end;

procedure TTimingGraph.DrawBackground;
begin
  with FDoubleBuffer.DrawBuffer.Canvas do
  begin
    Font.Color := clBlack;
    Brush.Style := bsSolid;
    Brush.Color := FBackgroundColor;
    FillRect(0, 0, FDoubleBuffer.Width, FDoubleBuffer.Height);

    Pen.Color := FGridColor;
    Pen.Width := 2;
    Rectangle(FGridRect);
  end;
  DoDrawBackground;
end;

procedure TTimingGraph.DrawEventsList;
var
  I: Integer;
  P: Integer;
  H: Integer;
  C: TCanvas;
begin
  if FEvents.Count < 1 then Exit;
  C := FDoubleBuffer.PaintBuffer.Canvas;

  H := (FGridRect.Bottom - FGridRect.Top) div FEvents.Count;
  if H < FMarginEventList then Exit;
  H := Min(H, FEventMaxHeight);

  P := FGridRect.Bottom - FMarginEventList div 2;

  for I := 0 to FEvents.Count - 1 do
  begin
    C.Pen.Color := FGridColor;
    C.Pen.Width := 1;
    C.Pen.Style := psSolid;
    C.Line(FGridRect.Left, P, FGridRect.Right, P);

    FEvents[I].Size := H - FMarginEventList;
    FEvents[I].FPosition := P - FEvents[I].Size;
    P := P - H;
    DrawEvents(FEvents[I]);
  end;
end;

procedure TTimingGraph.DrawEvents(E: TEventList);
var
  S: TSize;
  C: TCanvas;
  X, Y: Integer;
  R: TRect;
begin
  C := FDoubleBuffer.PaintBuffer.Canvas;
  C.Brush.Style := bsClear;
  C.Pen.Width := E.MarkerSize;

  S := C.TextExtent(E.Caption);
  Y := E.Position + E.Size div 2 - S.cy div 2;
  X := FGridRect.Left - S.cx - 10;
  C.TextOut(X, Y, E.Caption);

  R := FGridRect;
  R.Top := E.Position - E.Size;
  R.Bottom := E.Position;

  E.Layout(R);
  E.Draw(C, R);
end;

procedure TTimingGraph.Redraw;
begin
  if FUpdateCounter > 0 then Exit;
  FDoubleBuffer.Draw2Paint;
  with FDoubleBuffer.PaintBuffer.Canvas do
  begin
    Font.Color := clBlack;
  end;
  DoDrawFgSimple;
  DrawEventsList;
  FDoubleBuffer.Repaint;
end;

procedure TTimingGraph.FullRedraw;
begin
  if FUpdateCounter > 0 then
  begin
    FNeedFullRedraw := True;
    Exit;
  end;
  FNeedFullRedraw := False;
  DrawBackground;
  Redraw;
end;

procedure TTimingGraph.EventListChanged;
begin
  Redraw;
end;

function TTimingGraph.FindEventList(const X, Y: Integer): TEventList;
var
  E: TEventList;
begin
  Result := nil;
  for E in FEvents do
  begin
    if InRange(Y, E.Position, E.Position + E.Size) then
    begin
      Result := E;
      Break;
    end;
  end;
end;

function TTimingGraph.FindFirstEvent(const X, Y: Integer; out
  AList: TEventList; out AFrame: Integer): Integer;
var
  E: TEventList;
  I, J, K: Integer;
begin
  Result := -1;
  for E in FEvents do
  begin
    if not InRange(Y, E.Position, E.Position + E.Size) then Continue;
    K := E.FrameIndex;
    for J := K - 1 to K + 1 do
    begin
      if (J < 0) or (J >= E.FrameCount) then Continue;
      E.FFrames.FrameIndex := J;
      if E.Count < 1 then Continue;

      I := E.FindFirstEvent(X, RANGE);
      if I < 0 then Continue;

      AList := E;
      Result := I;
      AFrame := J;
      Break;
    end;
    E.FFrames.FrameIndex := K;
    Break;
  end;
end;

function TTimingGraph.FindEvent(const X, Y: Integer; out AList: TEventList; out
  AFrame: Integer): Integer;
var
  I, J, M: Integer;
begin
  Result := FindFirstEvent(X, Y, AList, AFrame);
  if Result < 0 then Exit;
  J := AList.FFrames.FrameIndex;
  AList.FFrames.FrameIndex := AFrame;
  I := Result;
  M := Abs(AList.Event[I]^.X - X);
  Inc(I);
  while I < AList.Count do
  begin
    if Abs(AList.Event[I]^.X - X) < M then
    begin
      M := Abs(AList.Event[I]^.X - X);
      Result := I;
    end;
    Inc(I);
  end;
  AList.FFrames.FrameIndex := J;
end;

procedure TTimingGraph.ForEachCB(AList: TEventList; P: PTimingEvent; Param: PForEachParam);
begin
  Param^.Fun(AList, P, Param^.Param);
end;

procedure TTimingGraph.ForEach(const X, Y: Integer; const AFun: TEventListForEach;
  Param: Pointer; const Tolerance: Integer);
var
  T: TForEachParam;
begin
  T.Fun := AFun;
  T.Param := Param;
  ForEach(X, Y, TEventListForEachMethod(@ForEachCB), @T, Tolerance);
end;

procedure TTimingGraph.ForEach(const X, Y: Integer;
  const AFun: TEventListForEachMethod; Param: Pointer; const Tolerance: Integer
  );
var
  I, J: Integer;
  FList: TEventList;
  P: PTimingEvent;
  AFrame: Integer;
begin
  I := FindFirstEvent(X, Y, FList, AFrame);
  if I < 0 then Exit;
  BeginUpdate;
  J := FList.FrameIndex;
  FList.FrameIndex := AFrame;
  if not FList.HasDuration then
  begin
    repeat
      AFun(FList, FList.Event[I], Param);
      Inc(I);
    until (I >= FList.Count) or (Abs(FList[I]^.X - X) > Tolerance);
  end
  else begin
    while I < FList.Count do
    begin
      P := FList[I];
      if InRange(X, P^.X - Tolerance, P^.X1 + Tolerance) then
        AFun(FList, P, Param);
      Inc(I);
    end;
  end;
  FList.FrameIndex := J;
  EndUpdate(False);
end;

type
  TParam = record
    S: string;
  end;
  PParam = ^TParam;
procedure OnEvent(E: TEventList; PE: PTimingEvent; Param: PParam);
begin
  Param^.S := Param^.S + Format('%s@%s'#10, [PE^.Tag, E.GetTimeLabel(PE^.Time)]);
end;

procedure TTimingGraph.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
label
  SET_HINT;
var
  P: TPoint;
  AParam: TParam;
begin
  P.x := X;
  P.y := Y;
  if not PtInRect(FGridRect, P) then goto SET_HINT;

  ForEach(X, Y, TEventListForEach(@OnEvent), @AParam);

SET_HINT:
  PaintBox.Hint := Copy(AParam.S, 1, Length(AParam.S) - 1);
end;

procedure TTimingGraph.ToolWindowZoom(Sender: TObject; Rect: TRect);
var
  X: Double;
begin
  if (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top) then
  begin
    ShowAll;
    Exit;
  end;
  X := (FGridRect.Right - FGridRect.Left) / (Rect.Right - Rect.Left);
  FViewStartTime := ScreenToTime(Rect.Left);
  FPixelsPerSample := FPixelsPerSample * X;
  FPixelsPerMS := FPixelsPerMS * X;
  UpdatePanMax;
  FShowAll := False;
  FullRedraw;
end;

procedure TTimingGraph.ToolWindowPan(Sender: TObject; Delta: TPoint);
var
  X: Double;
begin
  if Delta.x = 0 then Exit;
  X := - Delta.x / FPixelsPerMS / 1000;
  if not TimeIncSec(FViewStartTime, X, FViewStartTime) then Exit;

  if TimeCompare(FViewStartTime, PanMin) < 0 then
    FViewStartTime := PanMin
  else if TimeCompare(FViewStartTime, PanMax) > 0 then
      FViewStartTime := PanMax;
  FShowAll := False;
  FullRedraw;
end;

procedure TTimingGraph.DoDrawBackground;
begin

end;

procedure TTimingGraph.DoDrawFgSimple;
begin

end;

procedure TTimingGraph.DoResize;
begin
  UpdatePanMax;
end;

function TTimingGraph.ScreenToTime(const X: Integer): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := Round((X - FGridRect.Left) / FPixelsPerSample + FViewStartTime.Sample);
    ettMilliSec:
      Result.TimeMS := (X - FGridRect.Left) / FPixelsPerMS + FViewStartTime.TimeMS;
    ettTime:
      Result.Time := (X - FGridRect.Left) / FPixelsPerMS / MSecsPerDay + FViewStartTime.Time;
  end;
end;

function TTimingGraph.TimeToScreen(const T: TEventTime): Integer;
begin
  case FTimeType of
    ettSample:
      Result := Round(FGridRect.Left + (T.Sample - FViewStartTime.Sample) * FPixelsPerSample);
    ettMilliSec:
      Result := Round(FGridRect.Left + (T.TimeMS - FViewStartTime.TimeMS) * FPixelsPerMS);
    ettTime:
      Result := Round(FGridRect.Left + (T.Time - FViewStartTime.Time) * MSecsPerDay * FPixelsPerMS);
  end;
end;

function TTimingGraph.ScreenToTimeSec(const X: Integer): Double;
begin
  case FTimeType of
    ettSample:
      Result := ((X - FGridRect.Left) / FPixelsPerSample + FViewStartTime.Sample) / FSampleRate;
    ettMilliSec:
      Result := ((X - FGridRect.Left) / FPixelsPerMS + FViewStartTime.TimeMS) / 1000;
    ettTime:
      Result := (X - FGridRect.Left) / FPixelsPerMS + FViewStartTime.Time * SecsPerDay;
  end;
end;

function TTimingGraph.ScreenDiffToTimeSec(const X: Integer): Double;
begin
  case FTimeType of
    ettSample:
      Result := X / (FPixelsPerSample * FSampleRate);
    ettMilliSec:
      Result := X / (FPixelsPerMS * 1000);
    ettTime:
      Result := X / (FPixelsPerMS * 1000);
  end;
end;

function TTimingGraph.TimeAdd(const A, B: TEventTime): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := A.Sample + B.Sample;
    ettMilliSec:
      Result.TimeMS := A.TimeMS + B.TimeMS;
    ettTime:
      Result.Time := A.Time + B.Time;
  end;
end;

function TTimingGraph.TimeSubtract(const A, B: TEventTime): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := A.Sample - B.Sample;
    ettMilliSec:
      Result.TimeMS := A.TimeMS - B.TimeMS;
    ettTime:
      Result.Time := A.Time - B.Time;
  end;
end;

function TTimingGraph.TimeNeg(const A: TEventTime): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := -A.Sample;
    ettMilliSec:
      Result.TimeMS := -A.TimeMS;
    ettTime:
      Result.Time := -A.Time;
  end;
end;

function TTimingGraph.TimeAbs(const A: TEventTime): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := Abs(A.Sample);
    ettMilliSec:
      Result.TimeMS := Abs(A.TimeMS);
    ettTime:
      Result.Time := Abs(A.Time);
  end;
end;

function TTimingGraph.FrameTimeMultiply(const N: Integer): Double;
begin
  case FTimeType of
    ettSample:
      Result := N * FFrameLength.Sample / FSampleRate;
    ettMilliSec:
      Result := N * FFrameLength.TimeMS / 1000;
    ettTime:
      Result := N * FFrameLength.Time * SecsPerDay;
  end;
end;

function TTimingGraph.TimeDiffInSec(const A, B: TEventTime): Double;
begin
  case FTimeType of
    ettSample:
      Result := (A.Sample - B.Sample) / FSampleRate;
    ettMilliSec:
      Result := (A.TimeMS - B.TimeMS) / 1000;
    ettTime:
      Result := (A.Time - B.Time) * SecsPerDay;
  end;
end;

function TTimingGraph.TimeDiff(const A, B: TEventTime): TEventTime;
begin
  case FTimeType of
    ettSample:
      Result.Sample := Abs(A.Sample - B.Sample);
    ettMilliSec:
      Result.TimeMS := Abs(A.TimeMS - B.TimeMS);
    ettTime:
      Result.Time := Abs(A.Time - B.Time);
  end;
end;

function TTimingGraph.TimeCompare(const A, B: TEventTime): Integer;
begin
  case FTimeType of
    ettSample:
      Result := A.Sample - B.Sample;
    ettMilliSec:
      Result := Sign(A.TimeMS - B.TimeMS);
    ettTime:
      Result := Sign(A.Time - B.Time);
  end;
end;

function TTimingGraph.TimeIncSec(const A: TEventTime; const Off: Double;
  var B: TEventTime): Boolean;
begin
  case FTimeType of
    ettSample:
      B.Sample := Round(A.Sample + Off * FSampleRate);
    ettMilliSec:
      B.TimeMS := A.TimeMS + Off * 1000;
    ettTime:
      B.Time := A.Time + Off / SecsPerDay;
  end;
  Result := True;
end;

function TTimingGraph.FormatTimeShort(const T: TEventTime): string;
begin
  case FTimeType of
    ettSample:
      Result := IntToStr(T.Sample);
    ettMilliSec:
      Result := FormatSecTimeShort(T.TimeMS / 1000);
    ettTime:
      Result := FormatSecTimeShort(T.Time * SecsPerDay);
  end;
end;

procedure TTimingGraph.FormatTimeLong(const T: TEventTime; L: TStrings);
begin
  case FTimeType of
    ettSample:
      begin
        L.Add(Format('%d Samples', [T.Sample]));
        L.Add(FormatSecTimeShort(T.Sample / FSampleRate));
      end;
    ettMilliSec:
      L.Add(FormatTimeShort(T));
    ettTime:
      L.Add(FormatTimeShort(T));
  end;
end;

function TTimingGraph.FormatSecTimeShort(const T: Double): string;
begin
  if T > 1 then Result := Format('%.2fs', [T])
  else if T > 0.001 then Result := Format('%.2fms', [T * 1000])
  else Result := Format('%.2fus', [T * 1000000]);
end;

procedure TTimingGraph.FormatSecTimeLong(const T: Double; L: TStrings);
begin
  case FTimeType of
    ettSample:
      begin
        L.Add(Format('%.1f Samples', [T * FSampleRate]));
        L.Add(FormatSecTimeShort(T));
      end;
    ettMilliSec:
      L.Add(FormatSecTimeShort(T));
    ettTime:
      L.Add(FormatSecTimeShort(T));
  end;
end;

procedure TTimingGraph.PaintBoxResize(Sender: TObject);
begin
  FDoubleBuffer.SetSize(FPaintBox.Width, FPaintBox.Height);
  FGridRect.Left := FMarginLeft;
  FGridRect.Bottom := FDoubleBuffer.Height - FMarginBottom;
  FGridRect.Right := FDoubleBuffer.Width - FMarginRight;
  FGridRect.Top := FMarginTop;
  DoResize;
  FullRedraw;
end;

function TTimingGraph.GetEventList(const Index: Integer): TEventList;
begin
  Result := FEvents[Index];
end;

function TTimingGraph.GetEventListCount: Integer;
begin
  Result := FEvents.Count;
end;

function TTimingGraph.GetFrameId: Integer;
begin
  Result := FFrames[FFrameIndex];
end;

function TTimingGraph.GetPanMin: TEventTime;
begin
  Result := FPanMin;
end;

procedure TTimingGraph.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;
  FullRedraw;
end;

procedure TTimingGraph.SetEventHeight(AValue: Integer);
begin
  if FEventMaxHeight = AValue then Exit;
  FEventMaxHeight := AValue;
  FullRedraw;
end;

procedure TTimingGraph.SetFrameId(AValue: Integer);
begin
  if FFrames[FFrameIndex] = AValue then Exit;
  FFrames[FFrameIndex] := AValue;
  Redraw;
end;

procedure TTimingGraph.SetFrameLength(AValue: TEventTime);
begin
  if CompareMem(@FFrameLength, @AValue, SizeOf(TEventTime)) then Exit;
  FFrameLength := AValue;
  UpdatePanMax;
end;

procedure TTimingGraph.SetGridColor(AValue: TColor);
begin
  if FGridColor = AValue then Exit;
  FGridColor := AValue;
  FullRedraw;
end;

procedure TTimingGraph.SetLabelColor(AValue: TColor);
begin
  if FLabelColor = AValue then Exit;
  FLabelColor := AValue;
  FullRedraw;
end;

procedure TTimingGraph.SetMarginBottom(AValue: Integer);
begin
  if FMarginBottom = AValue then Exit;
  FMarginBottom := Max(10, AValue);
  FullRedraw;
end;

procedure TTimingGraph.SetMarginEventList(AValue: Integer);
begin
  if FMarginEventList = AValue then Exit;
  FMarginEventList := Max(20, AValue);
  FullRedraw;
end;

procedure TTimingGraph.SetMarginLeft(AValue: Integer);
begin
  if FMarginLeft = AValue then Exit;
  FMarginLeft := Max(10, AValue);
  FullRedraw;
end;

procedure TTimingGraph.SetMarginRight(AValue: Integer);
begin
  if FMarginRight = AValue then Exit;
  FMarginRight := Max(1, AValue);
  FullRedraw;
end;

procedure TTimingGraph.SetMarginTop(AValue: Integer);
begin
  if FMarginTop = AValue then Exit;
  FMarginTop := Max(1, AValue);
  FullRedraw;
end;

procedure TTimingGraph.SetSampleRate(AValue: Cardinal);
begin
  if FSampleRate = AValue then Exit;
  if AValue < 1 then Exit;
  FSampleRate := AValue;
  UpdatePanMax;
end;

procedure TTimingGraph.SetShowEventLabel(AValue: Boolean);
begin
  if FShowEventLabel = AValue then Exit;
  FShowEventLabel := AValue;
  Redraw;
end;

procedure TTimingGraph.SetShowEventTiming(AValue: Boolean);
begin
  if FShowEventTiming = AValue then Exit;
  FShowEventTiming := AValue;
  Redraw;
end;

procedure TTimingGraph.Clear;
var
  L: TEventList;
begin
  BeginUpdate;
  FFrameCount := 1;
  FFrameIndex := 0;
  for L in FEvents do L.ClearAll;
  FFrames[FFrameIndex] := 0;
  EndUpdate;
end;

procedure TTimingGraph.SetFrameIndex(AValue: Integer);
var
  L: TEventList;
begin
  if not InRange(AValue, 0, FFrameCount - 1) then Exit;
  if FFrameIndex = AValue then Exit;
  BeginUpdate;
  FFrameIndex := AValue;
  for L in FEvents do L.FrameIndex := AValue;
  EndUpdate;
end;

procedure TTimingGraph.UpdatePanMax;
begin
  case FTimeType of
    ettSample:
      FPanMax.Sample := Round(FFrameLength.Sample * 2 - (FGridRect.Right - FGridRect.Left) / FPixelsPerSample);
    ettMilliSec:
      FPanMax.TimeMS := FFrameLength.TimeMS * 2 - (FGridRect.Right - FGridRect.Left) / FPixelsPerMS;
    ettTime:
      FPanMax.Time   := FFrameLength.Time * 2 - (FGridRect.Right - FGridRect.Left) / FPixelsPerMS / MSecsPerDay;
  end;
end;

constructor TTimingGraph.Create(ATimeType: TEventTimeType);
  procedure AddTools;
  var
    T: TMouseTool;
  begin
    T := TToolWindowZoom.Create;
    with T as TToolWindowZoom do
      OnWindowZoom := @ToolWindowZoom;
    FToolMan.AddTool(T);
    T := TToolWindowPan.Create;
    with T as TToolWindowPan do
      OnWindowPan := @ToolWindowPan;
    FToolMan.AddTool(T);
    T := TToolIdleMove.Create;
    with T as TToolIdleMove do
      OnMove := @PaintBoxMouseMove;
    FToolMan.AddTool(T);
    FMeasureTool := TToolMeasure.Create(Self);
    FToolMan.AddTool(FMeasureTool);
  end;

begin
  FDoubleBuffer := TDoubleBuffer.Create;
  FGridColor := TColor($cccccc);
  FLabelColor := clBlack;
  FBackgroundColor := clBtnFace; // TColor($404040);
  FMarginLeft := 80;
  FMarginRight := 5;
  FMarginTop := 30;
  FMarginBottom := 20;
  FMarginEventList := 20;
  FEventMaxHeight := 50;
  FShowAll := True;
  FShowEventLabel := True;
  FShowEventTiming := True;
  FFrameLength.Sample := 1000;
  FSampleRate := 1000;

  FTimeType := ATimeType;
  FSampleRate := 1;

  FPixelsPerSample := 1;
  FPixelsPerMS := 0.001;

  FEvents := TLteEventListList.Create;
  FFrameCount := 0;

  FToolMan := TMouseToolMan.Create;
  AddTools;

  FToolMan.PersistCanvas := FDoubleBuffer.PaintBuffer.Canvas;

  //SetLength(FFrames, 2048);
  NewFrame;
end;

destructor TTimingGraph.Destroy;
begin
  FToolMan.Free;
  FEvents.Free;
  FDoubleBuffer.Free;
  inherited Destroy;
end;

function TTimingGraph.AddEventList(AList: TEventList): Integer;
begin
  AList.FGraph := Self;
  Result := FEvents.Add(AList);
end;

procedure TTimingGraph.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TTimingGraph.EndUpdate(const ARedraw: Boolean);
begin
  Dec(FUpdateCounter);
  if FUpdateCounter <= 0 then
  begin
    FUpdateCounter := 0;
    if FNeedFullRedraw then
      FullRedraw
    else
      if ARedraw then Redraw;
  end;
end;

procedure TTimingGraph.NewFrame;
var
  L: TEventList;
begin
  BeginUpdate;
  FFrameIndex := FFrameCount;
  Inc(FFrameCount);
  if FFrameIndex > High(FFrames) then
    SetLength(FFrames, FFrameCount + 128);
  for L in FEvents do L.NewFrame;
  EndUpdate;
end;

procedure TTimingGraph.ShowAll;
begin
  FShowAll := True;
  FillChar(FViewStartTime, SizeOf(FViewStartTime), 0);
  case FTimeType of
    ettSample:
      FPixelsPerSample := (FGridRect.Right - FGridRect.Left) / FFrameLength.Sample;
    ettMilliSec:
      FPixelsPerMS := (FGridRect.Right - FGridRect.Left) / FFrameLength.TimeMS;
    ettTime:
      FPixelsPerMS := (FGridRect.Right - FGridRect.Left) / (MSecsPerDay * FFrameLength.Time);
  end;
  if FTimeType = ettSample then
    FPixelsPerMS := FPixelsPerSample * (FSampleRate / 1000)
  else
    FPixelsPerSample := 0.1;
  FullRedraw;
end;

{ TDoubleBuffer }

procedure TDoubleBuffer.OnPaint(Sender: TObject);
var
  R: TRect;
begin
  R.Left := 0; R.Top := 0;
  R.Bottom := FHeight;
  R.Right := FWidth;
  FPaintBox.Canvas.CopyRect(R, FPaintBuffer.Canvas, R);
end;

procedure TDoubleBuffer.SetPaintBox(AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
  with FDrawBuffer do
  begin
    Canvas.Font := FPaintBox.Font;
    Canvas.Font.Quality := fqCleartype;
  end;
  with FPaintBuffer do
  begin
    Canvas.Font := FPaintBox.Font;
  end;
  SetSize(AValue.Width, AValue.Height);
  FPaintBox.OnPaint := @OnPaint;
end;

procedure TDoubleBuffer.Draw2Paint2;
begin
  FPaintBox.Invalidate;
end;

constructor TDoubleBuffer.Create;
begin
  FDrawBuffer := TBitmap.Create;
  FPaintBuffer := TBitmap.Create;
  with FDrawBuffer do
  begin
    Canvas.Clipping:=False;
    Canvas.Font.Quality := fqCleartype;
  end;
  with FPaintBuffer do
  begin
    Canvas.Font.Quality := fqCleartype;
  end;
  SetSize(900, 600);
end;

destructor TDoubleBuffer.Destroy;
begin
  if Assigned(FPaintBox) then
    FPaintBox.OnPaint := nil;
  FDrawBuffer.Free;
  FPaintBuffer.Free;
end;

procedure TDoubleBuffer.Draw2Paint;
begin
  FPaintBuffer.Canvas.Draw(0, 0, FDrawBuffer);
end;

procedure TDoubleBuffer.Repaint;
begin
  if Assigned(FPaintBox) then FPaintBox.Refresh;
  //if Assigned(FPaintBox) then
  //  FPaintBox.Canvas.Draw(0, 0, FPaintBuffer);
  //TThread.Synchronize(nil, @Draw2Paint2);
end;

procedure TDoubleBuffer.SetSize(W, H: Integer);
begin
  FWidth := W;
  FHeight := H;

  if (FDrawBuffer.Width >= W) and (FDrawBuffer.Height >= H) then Exit;

  W := W + 200;
  H := H + 100;
  FDrawBuffer.SetSize(W, H);
  FPaintBuffer.SetSize(W, H);
end;

end.

