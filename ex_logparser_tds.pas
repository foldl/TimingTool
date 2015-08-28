unit ex_logparser_tds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, ExtCtrls, TdsGraph,
  ImgList, TimingReader, RegExpr, Math, superobject, TimingGraph;

type

  { TTdsTimingReader }

  TTdsTimingReader = class(TTimingReader)
  private
    FLoadFirstFrame: Boolean;
    FTheGraph: TTdsGraph;
    FEvents: array of TEventList;
    function GetEvents(const Index: Integer): TTdsEventList;
    procedure ReceiveSFN(const SFN: Integer);
    procedure GetTheme(const TSG, SIG: Integer; out PS: TPenStyle;
                   out C: TColor);
  protected
    procedure BeforeRead; override;
    function CreateGraph: TTimingGraph; override;
    property TheGraph: TTdsGraph read FTheGraph;
  public
    constructor Create;
    destructor Destroy; override;
    property Events[const Index: Integer]: TTdsEventList read GetEvents;

    property Graph: TTdsGraph read FTheGraph;
  end;

implementation

uses
  KLib;

const
  THEME: array [0..15] of TColor =
    (482559, 505739, 6450981, 15631106, 8199463, 9847672,
     2895066, 3911660, 3507588, 3898092, 9276742, 3846574, 11630200, 11645302, 7887025, 4764541);

  GENERIC_EVENT_INDEX = 0;

{ TTdsTimingReader }

function TTdsTimingReader.GetEvents(const Index: Integer): TTdsEventList;
begin
  Result := TTdsEventList(FEvents[Index]);;
end;

procedure TTdsTimingReader.ReceiveSFN(const SFN: Integer);
begin
  if FLoadFirstFrame then
  begin
    FTheGraph.SFN := SFN;
    FLoadFirstFrame := False;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
  end
  else if SFN <> FTheGraph.SFN then
  begin
    FTheGraph.NewFrame;
    FFrameList.AddItem(Format('%d-%d', [Graph.FrameIndex, SFN]), nil);
    FTheGraph.SFN := SFN;
  end;
end;

procedure TTdsTimingReader.GetTheme(const TSG, SIG: Integer; out
  PS: TPenStyle; out C: TColor);
begin
  C := THEME[(SIG + 10) mod Length(THEME)]; // div 2];
  PS := psSolid; // TPenStyle(SIG mod 2);
end;

procedure TTdsTimingReader.BeforeRead;
begin
  inherited BeforeRead;
  FLoadFirstFrame := True;
  FTheGraph.SFN := -1;
end;

function TTdsTimingReader.CreateGraph: TTimingGraph;
begin
  FTheGraph := TTdsGraph.Create;
  Result := FTheGraph;
end;

constructor TTdsTimingReader.Create;
begin
  inherited;
end;

destructor TTdsTimingReader.Destroy;
begin
  inherited Destroy;
end;

end.

