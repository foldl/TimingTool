unit ex_sleep_check;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo, Controls, Graphics, ExtCtrls, Math,
  TimingGraph, TimingReader;

type

  { TMilliSecGraph }

  TMilliSecGraph = class(TTimingGraph)
  private
    FSleepEvent: TEventList;
    procedure DrawTicks;
    function EventSum(List: TEventList): Double;
  protected
    procedure DoDrawBackground; override;
    procedure DoDrawFgSimple; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TSleepTimingReader }

  TSleepTimingReader = class(TTimingReader)
  private
    FSleepEvent: TEventList;
    FLoadFirstFrame: Boolean;
    FTheGraph: TMilliSecGraph;
    FEnterTime: Double;
    procedure AddSleepEvent(ATime: Double; IsEnter: Boolean);
    procedure ReceiveFrame(const Frame: Integer);

    procedure EnterSleepMatched(const ALine: string; const LineNo: Cardinal);
    procedure ExitSleepMatched(const ALine: string; const LineNo: Cardinal);
  protected
    function CreateGraph: TTimingGraph; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeRead; override;

    property Graph: TMilliSecGraph read FTheGraph;
  end;

implementation

{ TSleepTimingReader }

procedure TSleepTimingReader.AddSleepEvent(ATime: Double; IsEnter: Boolean);
var
  F: Integer;
  T: Double;
  P: PTimingEvent;

  function CaclIndex(const T: Double): Integer;
  begin
    Result := Trunc(T * 10000);
  end;

begin
  F := Trunc(ATime * 100);
  T := ATime * 1000 - F * 10;
  ReceiveFrame(F);

  if IsEnter then
  begin
    FEnterTime := T;
  end
  else begin
    P := FSleepEvent.NewEvent;
    P^.Time.TimeMS := FEnterTime;
    P^.Duration.TimeMS := T - FEnterTime;
    FSleepEvent.InsertEvent(P);
    FEnterTime := -1;
  end;
end;

procedure TSleepTimingReader.ReceiveFrame(const Frame: Integer);
var
  P: PTimingEvent;
begin
  if FLoadFirstFrame then
  begin
    FTheGraph.FrameId := Frame;
    FLoadFirstFrame := False;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, Frame]), nil);
  end
  else if Frame <> FTheGraph.FrameId then
  begin
    if FEnterTime >= 0.0 then
    begin
      P := FSleepEvent.NewEvent;
      P^.Time.TimeMS := FEnterTime;
      P^.Duration.TimeMS := FTheGraph.FrameLength.TimeMS - FEnterTime;
      FSleepEvent.InsertEvent(P);
      FEnterTime := 0.0;
    end;
    FTheGraph.NewFrame;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, Frame]), nil);
    FTheGraph.FrameId := Frame;
  end;
end;

procedure TSleepTimingReader.EnterSleepMatched(const ALine: string;
  const LineNo: Cardinal);
var
  I: Integer;
begin
  I := Pos(' (', ALine);
  if I < 1 then Exit;
  AddSleepEvent(StrToFloatDef(Copy(ALine, 1, I - 1), 0.0), True);
end;

procedure TSleepTimingReader.ExitSleepMatched(const ALine: string;
  const LineNo: Cardinal);
var
  I: Integer;
begin
  I := Pos(' (', ALine);
  if I < 1 then Exit;
  AddSleepEvent(StrToFloatDef(Copy(ALine, 1, I - 1), 0.0), False);
end;

function TSleepTimingReader.CreateGraph: TTimingGraph;
begin
  FTheGraph := TMilliSecGraph.Create;
  Result := FTheGraph;
end;

constructor TSleepTimingReader.Create;
begin
  inherited;
  FSleepEvent := TEventList.Create;
  FSleepEvent.HasDuration := True;
  FSleepEvent.Color := 482559;
  FSleepEvent.Caption := 'Sleep';
  FTheGraph.FSleepEvent := FSleepEvent;
  Graph.AddEventList(FSleepEvent);
  AddParser('ENTER_CORE_SLEEP', TSubstrParserCB(@EnterSleepMatched));
  AddParser('EXIT_CORE_SLEEP', TSubstrParserCB(@ExitSleepMatched));
end;

destructor TSleepTimingReader.Destroy;
begin
  inherited Destroy;
end;

procedure TSleepTimingReader.BeforeRead;
begin
  inherited BeforeRead;
  FLoadFirstFrame := True;
  FEnterTime := -1.0;
end;

{ TMilliSecGraph }

procedure TMilliSecGraph.DrawTicks;
var
  I, J, Y: Integer;
  E: TEventTime;
  Canvas: TCanvas;
  S: string;
  X: TSize;
  PreJ: Integer = -1;
begin
  Canvas := FDoubleBuffer.DrawBuffer.Canvas;
  Y := FGridRect.Bottom + (FMarginBottom - Canvas.TextHeight('0')) div 2;
  for I := -10 to 21 do
  begin
    E.TimeMS := I;
    J := TimeToScreen(E);
    if InRange(J, FGridRect.Left, FGridRect.Right) then
    begin
      Canvas.Line(J, FGridRect.Top, J, FGridRect.Bottom);
      if PreJ > 0 then
      begin
        S := IntToStr(I - 1);
        X := Canvas.TextExtent(S);
        Canvas.TextOut((PreJ + J) div 2 - X.cx div 2, Y, S);
      end;
      PreJ := J;
    end;
  end;
end;

function TMilliSecGraph.EventSum(List: TEventList): Double;
var
  I: Integer;
begin
  Result := 0.0;
  if not Assigned(List) then Exit;
  for I := 0 to List.Count - 1 do
    Result := Result + List[I]^.Duration.TimeMS;
end;

procedure TMilliSecGraph.DoDrawBackground;
begin
  with FDoubleBuffer.DrawBuffer.Canvas do
  begin
    TextOut(10, 10, Format('frame length = %f ms', [FFrameLength.TimeMS]));
  end;

  DrawTicks;
end;

procedure TMilliSecGraph.DoDrawFgSimple;
var
  S: string;
  C: TCanvas;
  Sleep: Double;
begin
  Sleep := EventSum(FSleepEvent);
  S := Format('idle: %.1f%%', [Sleep * 100 / 10]);
  C := FDoubleBuffer.PaintBuffer.Canvas;
  C.TextOut(3, FDoubleBuffer.Height - C.TextHeight(S) - 3, S);
end;

constructor TMilliSecGraph.Create;
begin
  inherited Create(ettMilliSec);
  FFrameLength.TimeMS := 10;
  FPanMin.TimeMS := -10;
end;

destructor TMilliSecGraph.Destroy;
begin
  inherited Destroy;
end;

end.

