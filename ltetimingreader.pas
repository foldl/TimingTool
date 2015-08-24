unit LteTimingReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Controls, ComCtrls, Graphics, ExtCtrls, LteGraph,
  ImgList, RegExpr, Math, superobject, KLib, fgl;

type

  TTimingReader = class;

  { TLineParser }

  TLineParser = class
  private
    FReader: TTimingReader;
  protected
    function Parse(Sender: TObject; const ALine: string;
                   const LineNo: Cardinal): Boolean; virtual;
  public
    property Reader: TTimingReader read FReader write FReader;
  end;

  { TLineParserRegExp }

  TLineParserRegExp = class(TLineParser)
  private
    FRe: TRegExpr;
    function GetRegExpr: string;
    procedure SetRegExpr(AValue: string);
  protected
    function Parse(Sender: TObject; const ALine: string;
                   const LineNo: Cardinal): Boolean; override;
  public
    constructor Create(const AExpr: string);
    destructor Destroy; override;

    procedure Matched(ARe: TRegExpr; const ALine: string;
                   const LineNo: Cardinal); virtual;

    property RegExpr: string read GetRegExpr write SetRegExpr;
  end;

  { TLineParserSubstr }

  TLineParserSubstr = class(TLineParser)
  private
    FContext: TBMContext;
    FSubstr: string;
    procedure SetSubstr(AValue: string);
  protected
    function Parse(Sender: TObject; const ALine: string;
                   const LineNo: Cardinal): Boolean; override;
  public
    constructor Create(const ASubstr: string);
    destructor Destroy; override;

    procedure Matched(const ALine: string; const LineNo: Cardinal); virtual;

    property Substr: string read FSubstr write SetSubstr;
  end;

  TParserList = specialize TFPGList<TLineParser>;

  { TTimingReader }

  TTimingReader = class
  private
    FParsers: TParserList;

    FGraph: TLteGraph;
    FTimer: TTimer;

    function GetPlaying: Boolean;
    function GetPlaySpeed: Integer;
    procedure SetFrameList(AValue: TListBox);
    procedure SetPaintBox(AValue: TPaintBox);
    procedure OnTimer(Sender: TObject);
    function DoLoadFile(const Fn: string): Boolean;
    function DoLoadLine(Sender: TObject; const ALine: string;
                              const LineNo: Cardinal): Boolean;
    procedure FrameSelected(Sender: TObject);
    procedure SetPlaying(AValue: Boolean);
    procedure SetPlaySpeed(AValue: Integer);
  public
    FEventMap: ISuperObject;
    FEvents: array of TLteEventList;
    FFrameList: TListBox;
  protected
    procedure BeforeRead; virtual;
    procedure AfterRead; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParser(AParser: TLineParser);
    function LoadFile(const Fn: string): Boolean;
    property FrameList: TListBox read FFrameList write SetFrameList;
    property PaintBox: TPaintBox write SetPaintBox;
    property Playing: Boolean read GetPlaying write SetPlaying;
    property PlayInterval: Integer read GetPlaySpeed write SetPlaySpeed;
    property Graph: TLteGraph read FGraph;
  end;

implementation

uses
  sutimingloadingformu;

{ TLineParser }

function TLineParser.Parse(Sender: TObject; const ALine: string;
  const LineNo: Cardinal): Boolean;
begin
  Result := False;
end;

{ TLineParserSubstr }

procedure TLineParserSubstr.SetSubstr(AValue: string);
begin
  if FSubstr = AValue then Exit;
  FSubstr := AValue;
  PrepareBMContext(AValue, FContext);
end;

function TLineParserSubstr.Parse(Sender: TObject; const ALine: string;
  const LineNo: Cardinal): Boolean;
begin
  Result := BoyerMooreSearch(ALine, FContext) >= 0;
  if Result then Matched(ALine, LineNo);
end;

constructor TLineParserSubstr.Create(const ASubstr: string);
begin
  FSubstr := ASubstr;
  PrepareBMContext(ASubstr, FContext);
end;

destructor TLineParserSubstr.Destroy;
begin
  FreeBMContext(FContext);
  inherited Destroy;
end;

procedure TLineParserSubstr.Matched(const ALine: string; const LineNo: Cardinal
  );
begin

end;

{ TLineParserRegExp }

procedure TLineParserRegExp.SetRegExpr(AValue: string);
begin
  if FRe.Expression = AValue then Exit;
  FRe.Expression := AValue;
  FRe.Compile;
end;

function TLineParserRegExp.GetRegExpr: string;
begin
  Result := FRe.Expression;
end;

constructor TLineParserRegExp.Create(const AExpr: string);
begin
  FRe := TRegExpr.Create;
  FRe.Expression := AExpr;
  FRe.Compile;
end;

destructor TLineParserRegExp.Destroy;
begin
  FRe.Free;
  inherited Destroy;
end;

procedure TLineParserRegExp.Matched(ARe: TRegExpr; const ALine: string;
  const LineNo: Cardinal);
begin

end;

function TLineParserRegExp.Parse(Sender: TObject; const ALine: string;
  const LineNo: Cardinal): Boolean;
begin
  Result := FRe.Exec(ALine);
  if Result then Matched(FRe, ALine, LineNo);
end;

{ TTimingReader }

procedure TTimingReader.SetFrameList(AValue: TListBox);
begin
  if FFrameList = AValue then Exit;
  FFrameList := AValue;
  FFrameList.OnClick := @FrameSelected;
end;

function TTimingReader.GetPlaySpeed: Integer;
begin
  Result := FTimer.Interval;
end;

function TTimingReader.GetPlaying: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TTimingReader.SetPaintBox(AValue: TPaintBox);
begin
  FGraph.PaintBox := AValue;
end;

procedure TTimingReader.OnTimer(Sender: TObject);
var
  I: Integer;
begin
  I := FFrameList.ItemIndex;
  if I < FFrameList.Count - 1 then
  begin
    FFrameList.ItemIndex := I + 1;
    FFrameList.OnClick(FFrameList);
  end
  else
    FTimer.Enabled := False;
end;

function TTimingReader.DoLoadFile(const Fn: string): Boolean;
var
  F: Text;
  S: string;
begin
  AssignFile(F, Fn);
  Reset(F);
  BeforeRead;
  while not EOF(F) do
  begin
    Readln(F, S);
    DoLoadLine(Self, S, 0);
  end;
  CloseFile(F);
  AfterRead;
  Result := True;
end;

function TTimingReader.DoLoadLine(Sender: TObject; const ALine: string;
  const LineNo: Cardinal): Boolean;
var
  P: TLineParser;
begin
  Result := True;
  for P in FParsers do
    if P.Parse(Sender, ALine, LineNo) then Break;
end;

procedure TTimingReader.FrameSelected(Sender: TObject);
begin
  FGraph.FrameIndex := FFrameList.ItemIndex;
end;

procedure TTimingReader.SetPlaying(AValue: Boolean);
begin
  FTimer.Enabled := AValue;
end;

procedure TTimingReader.SetPlaySpeed(AValue: Integer);
begin
  FTimer.Interval := Max(2, AValue);
end;

procedure TTimingReader.BeforeRead;
begin
  FGraph.BeginUpdate;
  FFrameList.Items.BeginUpdate;
  FGraph.Clear;
  FFrameList.Clear;
  FGraph.SFN := -1;
end;

procedure TTimingReader.AfterRead;
begin
  FFrameList.Items.EndUpdate;
  FGraph.EndUpdate;
end;

constructor TTimingReader.Create;
begin
  FEventMap := TSuperObject.Create(stObject);
  FGraph := TLteGraph.Create;
  FParsers := TParserList.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 30;
  FTimer.OnTimer := @OnTimer;
end;

destructor TTimingReader.Destroy;
begin
  FGraph.Free;
  FTimer.Free;
  FParsers.Free;
  inherited Destroy;
end;

procedure TTimingReader.AddParser(AParser: TLineParser);
begin
  FParsers.Add(AParser);
  AParser.Reader := Self;
end;

function TTimingReader.LoadFile(const Fn: string): Boolean;
begin
  BeforeRead;
  try
    SuTimingLoadingForm.LoadFile(Fn, @DoLoadLine);
    Result := True;
  finally
    AfterRead;
  end;
end;

end.

