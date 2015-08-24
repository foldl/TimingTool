unit mousetool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, typinfo, Graphics, ExtCtrls, Math, fgl;

type

  TMouseTool = class;
  TMouseToolList = specialize TFPGObjectList<TMouseTool>;

  { TMouseToolMan }

  TMouseToolMan = class
  private
    FPaintBox: TPaintBox;
    FList: TMouseToolList;
    FActive: TMouseTool;
    FPersistCanvas: TCanvas;
    procedure SetPaintBox(AValue: TPaintBox);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintTool;
    procedure ActivateTool(Tool: TMouseTool);
    procedure SetPersistCanvas(AValue: TCanvas);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTool(Tool: TMouseTool);

    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;
    property PersistCanvas: TCanvas read FPersistCanvas write SetPersistCanvas;
  end;

  TMouseTool = class
  private
    FActive: Boolean;
    FManager: TMouseToolMan;
    FName: string;
  protected
    function OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; virtual;
    function OnMouseEnter(Sender: TObject):Boolean; virtual;
    function OnMouseLeave(Sender: TObject):Boolean; virtual;
    function OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; virtual;
    function OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; virtual;
    function OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean):Boolean; virtual;
    constructor Create(const Name: string);
    procedure Paint; virtual;
    property Active: Boolean read FActive write FActive;
  public
    destructor Destroy; override;

    property Name: string read FName;
    property Manager: TMouseToolMan read FManager;
  end;

  TWindowZoomEvent = procedure (Sender: TObject; Rect: TRect) of object;
  TWindowPanEvent = procedure (Sender: TObject; Delta: TPoint) of object;

  { TToolWindowZoom }

  TToolWindowZoom = class(TMouseTool)
  private
    FOnWindowZoom: TWindowZoomEvent;
    FRect: TRect;
    procedure SetOnWindowZoom(AValue: TWindowZoomEvent);
  protected
    function OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    function OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; override;
    function OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    procedure Paint; override;
  public
    constructor Create;
    destructor Destroy; override;

    property OnWindowZoom: TWindowZoomEvent read FOnWindowZoom write SetOnWindowZoom;
  end;

  { TToolIdleMove }

  TToolIdleMove = class(TMouseTool)
  private
    FOnMove: TMouseMoveEvent;
    procedure SetOnMove(AValue: TMouseMoveEvent);
  protected
    function OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; override;
  public
    constructor Create;

    property OnMove: TMouseMoveEvent read FOnMove write SetOnMove;
  end;

  { TToolWindowPan }

  TToolWindowPan = class(TMouseTool)
  private
    FOnWindowPan: TWindowPanEvent;
    FRect: TRect;
    procedure SetOnWindowPan(AValue: TWindowPanEvent);
  protected
    function OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    function OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer):Boolean; override;
    function OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer):Boolean; override;
    function OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean):Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    property OnWindowPan: TWindowPanEvent read FOnWindowPan write SetOnWindowPan;
  end;

implementation

{ TToolIdleMove }

procedure TToolIdleMove.SetOnMove(AValue: TMouseMoveEvent);
begin
  if FOnMove = AValue then Exit;
  FOnMove := AValue;
end;

function TToolIdleMove.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if Assigned(FOnMove) then FOnMove(Sender, Shift, X, Y);
  Result := False;
end;

constructor TToolIdleMove.Create;
begin
  inherited Create('Idle Move');
end;

{ TToolWindowPan }

procedure TToolWindowPan.SetOnWindowPan(AValue: TWindowPanEvent);
begin
  if FOnWindowPan = AValue then Exit;
  FOnWindowPan := AValue;
end;

function TToolWindowPan.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := Button = mbRight;
  if Result then
  begin
    FRect.Left := X;
    FRect.Top := Y;
  end;
end;

function TToolWindowPan.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  S: TPoint;
begin
  Result := FActive;
  if FActive and Assigned(FOnWindowPan) then
  begin
    S.x := X - FRect.Left;
    S.y := Y - FRect.Top;
    FRect.Left := X;
    FRect.Top := Y;
    FOnWindowPan(Self, S);
  end;
end;

function TToolWindowPan.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TToolWindowPan.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean): Boolean;
var
  S: TPoint;
begin
  Result := False;
  if Assigned(FOnWindowPan) then
  begin
    S.x := WheelDelta div 3;
    S.y := 0;
    FOnWindowPan(Self, S);
  end;
  Handled := True;
end;

constructor TToolWindowPan.Create;
begin
  inherited Create('Pan');
end;

destructor TToolWindowPan.Destroy;
begin
  inherited Destroy;
end;

{ TToolWindowZoom }

procedure TToolWindowZoom.SetOnWindowZoom(AValue: TWindowZoomEvent);
begin
  if FOnWindowZoom = AValue then Exit;
  FOnWindowZoom := AValue;
end;

function TToolWindowZoom.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if ssCtrl in Shift then
  begin
    FRect.Left := X;
    FRect.Top := Y;
    FRect.BottomRight := FRect.TopLeft;
    Result := True;
  end
  else
    Result := False;
end;

function TToolWindowZoom.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  if FActive and (ssCtrl in Shift) then
  begin
    FRect.Right := X;
    FRect.Bottom := Y;
    Result := True;
  end
  else
    Result := False;
end;

function TToolWindowZoom.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if FActive then
    if Assigned(FOnWindowZoom) then FOnWindowZoom(Self, FRect);
  Result := False;
end;

procedure TToolWindowZoom.Paint;
var
  C: TCanvas;
begin
  C := FManager.FPaintBox.Canvas;
  C.Clipping := False;
  C.Pen.Color := clBlue;
  C.Pen.Style := psDot;
  C.Brush.Style := bsClear;
  C.Rectangle(FRect);
end;

constructor TToolWindowZoom.Create;
begin
  inherited Create('Zoom Window');
end;

destructor TToolWindowZoom.Destroy;
begin
  inherited Destroy;
end;

{ TMouseTool }

function TMouseTool.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FActive;
end;

function TMouseTool.OnMouseEnter(Sender: TObject): Boolean;
begin
  Result := False;
end;

function TMouseTool.OnMouseLeave(Sender: TObject): Boolean;
begin
  Result := FActive;
end;

function TMouseTool.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := FActive;
end;

function TMouseTool.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TMouseTool.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean): Boolean;
begin
  Result := False;
end;

constructor TMouseTool.Create(const Name: string);
begin
  FName := Name;
end;

destructor TMouseTool.Destroy;
begin
  inherited Destroy;
end;

procedure TMouseTool.Paint;
begin

end;

{ TMouseToolMan }

procedure TMouseToolMan.SetPaintBox(AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
  with FPaintBox do
  begin
    OnMouseUp := @PaintBoxMouseUp;
    OnMouseDown := @PaintBoxMouseDown;
    OnMouseEnter := @PaintBoxMouseEnter;
    OnMouseLeave := @PaintBoxMouseLeave;
    OnMouseMove := @PaintBoxMouseMove;
    OnMouseWheel := @PaintBoxMouseWheel;
  end;
end;

procedure TMouseToolMan.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  T: TMouseTool;
begin
  if Assigned(FActive) then
  begin
    FActive.Active := FActive.OnMouseDown(Sender, Button, Shift, X, Y);
    if not (FActive.Active) then FActive := nil;
    PaintTool;
  end
  else begin
    for T in FList do
    begin
      if T.OnMouseDown(Sender, Button, Shift, X, Y) then
      begin
        ActivateTool(T);
        Break;
      end;
    end;
  end;
end;

procedure TMouseToolMan.PaintBoxMouseEnter(Sender: TObject);
begin

end;

procedure TMouseToolMan.PaintBoxMouseLeave(Sender: TObject);
begin
  if Assigned(FActive) then
  begin
    FActive.Active := False;
    FActive := nil;
    PaintTool;
  end;
end;

procedure TMouseToolMan.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  T: TMouseTool;
begin
  if Assigned(FActive) then
  begin
    FActive.Active := FActive.OnMouseMove(Sender, Shift, X, Y);
    if not (FActive.Active) then FActive := nil;
    PaintTool;
  end
  else begin
    for T in FList do
    begin
      if T.OnMouseMove(Sender, Shift, X, Y) then
      begin
        ActivateTool(T);
        Break;
      end;
    end;
  end;
end;

procedure TMouseToolMan.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  T: TMouseTool;
begin
  if Assigned(FActive) then
  begin
    FActive.Active := FActive.OnMouseUp(Sender, Button, Shift, X, Y);
    if not (FActive.Active) then FActive := nil
    else PaintTool;
  end
  else begin
    for T in FList do
    begin
      if T.OnMouseUp(Sender, Button, Shift, X, Y) then
      begin
        ActivateTool(T);
        Break;
      end;
    end;
  end;
end;

procedure TMouseToolMan.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  T: TMouseTool;
begin
  if Assigned(FActive) then
  begin
    FActive.Active := FActive.OnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
    if not (FActive.Active) then FActive := nil;
    PaintTool;
  end
  else begin
    for T in FList do
    begin
      if T.OnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled) then
      begin
        ActivateTool(T);
        Break;
      end;
    end;
  end;
end;

procedure TMouseToolMan.PaintTool;
begin
  FPaintBox.Refresh;
  if Assigned(FActive) then FActive.Paint;
end;

procedure TMouseToolMan.ActivateTool(Tool: TMouseTool);
begin
  Tool.Active := True;
  FActive := Tool;
  PaintTool;
end;

procedure TMouseToolMan.SetPersistCanvas(AValue: TCanvas);
begin
  if FPersistCanvas = AValue then Exit;
  FPersistCanvas := AValue;
end;

constructor TMouseToolMan.Create;
begin
  FList := TMouseToolList.Create(True);
end;

destructor TMouseToolMan.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TMouseToolMan.AddTool(Tool: TMouseTool);
begin
  FList.Add(Tool);
  Tool.FManager := Self;
end;

end.

