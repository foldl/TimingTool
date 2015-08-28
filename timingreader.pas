unit TimingReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Controls, ComCtrls, Graphics, ExtCtrls, TimingGraph,
  ImgList, RegExpr, Math, superobject, KLib, fgl;

type

  TLineParserType = (ptRe, ptSubstr);
  TSubstrParserCB = procedure(const ALine: string; const LineNo: Cardinal) of object;
  TReParserCB = procedure(ARe: TRegExpr; const ALine: string;
    const LineNo: Cardinal) of object;

  TLineParser = record
    case ParserType: TLineParserType of
      ptRe:
        (
          Re: TRegExpr;
          ReCB: TReParserCB
        );
      ptSubstr:
        (
          Context: PBMContext;
          SubstrCB: TSubstrParserCB
        )
  end;

  { TTimingReader }

  TTimingReader = class
  private
    FParsers: array of TLineParser;

    FGraph: TTimingGraph;
    FTimer: TTimer;

    function GetPlaying: Boolean;
    function GetPlaySpeed: Integer;
    procedure SetFrameList(AValue: TListBox);
    procedure SetPaintBox(AValue: TPaintBox);
    procedure OnTimer(Sender: TObject);
    procedure FrameSelected(Sender: TObject);
    procedure SetPlaying(AValue: Boolean);
    procedure SetPlaySpeed(AValue: Integer);
  public
    FEventMap: ISuperObject;
    FFrameList: TListBox;
  protected
    function CreateGraph: TTimingGraph; virtual; abstract;
  public
    procedure BeforeRead; virtual;
    procedure AfterRead; virtual;
    function DoLoadLine(Sender: TObject; const ALine: string;
                              const LineNo: Cardinal): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFile(const Fn: string): Boolean;

    procedure AddParser(ARe: string; ACB: TReParserCB); overload;
    procedure AddParser(ASubstr: string; ACB: TSubstrParserCB);

    property FrameList: TListBox read FFrameList write SetFrameList;
    property PaintBox: TPaintBox write SetPaintBox;
    property Playing: Boolean read GetPlaying write SetPlaying;
    property PlayInterval: Integer read GetPlaySpeed write SetPlaySpeed;
  end;

implementation

uses
  timingloadingformu;

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

function TTimingReader.LoadFile(const Fn: string): Boolean;
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

procedure TTimingReader.AddParser(ARe: string; ACB: TReParserCB);
begin
  SetLength(FParsers, Length(FParsers) + 1);
  with FParsers[High(FParsers)] do
  begin
    ParserType := ptRe;
    Re := TRegExpr.Create;   Re.Expression := ARe; Re.Compile;
    ReCB := ACB;
  end;
end;

procedure TTimingReader.AddParser(ASubstr: string; ACB: TSubstrParserCB);
begin
  SetLength(FParsers, Length(FParsers) + 1);
  with FParsers[High(FParsers)] do
  begin
    ParserType := ptSubstr;
    New(Context);
    PrepareBMContext(ASubstr, Context^);
    SubstrCB := ACB;
  end;
end;

function TTimingReader.DoLoadLine(Sender: TObject; const ALine: string;
  const LineNo: Cardinal): Boolean;
var
  P: TLineParser;
begin
  Result := True;
  for P in FParsers do
  begin
    with P do
    begin
      case ParserType of
        ptRe:
          begin
            if Re.Exec(ALine) then
            begin
              ReCB(Re, ALine, LineNo);
              Break;
            end;
          end;
        ptSubstr:
          begin
            if BoyerMooreSearch(ALine, Context^) >= 0 then
            begin
              SubstrCB(ALine, LineNo);
              Break;
            end;
          end;
      end;
    end;
  end;
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
end;

procedure TTimingReader.AfterRead;
begin
  FFrameList.Items.EndUpdate;
  FGraph.EndUpdate;
end;

constructor TTimingReader.Create;
begin
  FEventMap := TSuperObject.Create(stObject);
  FGraph := CreateGraph;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 30;
  FTimer.OnTimer := @OnTimer;
end;

destructor TTimingReader.Destroy;
var
  I: Integer;
begin
  FGraph.Free;
  FTimer.Free;
  for I := 0 to High(FParsers) do
  begin
    with FParsers[I] do
    begin
      case ParserType of
        ptRe: Re.Free;
        ptSubstr:
          begin
            FreeBMContext(Context^);
            Dispose(Context);
          end;
      end;
    end;
  end;
  inherited Destroy;
end;

end.

