unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, StdCtrls, ExtCtrls, logparser_su, LteGraph, types;

type

  { TSuTimingForm }

  TSuTimingForm = class(TForm)
    ListBox1: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseEnter(Sender: TObject);
    procedure PaintBox1MouseLeave(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FReader: TLteTimingReaderSu;
  public
    { public declarations }
  end;

var
  SuTimingForm: TSuTimingForm;

implementation

uses
  helpformu, settingsu;

{$R *.lfm}

{ TSuTimingForm }

procedure TSuTimingForm.FormCreate(Sender: TObject);
begin
  FReader := TLteTimingReaderSu.Create;
  FReader.FrameList := ListBox1;
  FReader.PaintBox := PaintBox1;
  // something is strange
 // PaintBox1.OnResize(PaintBox1);    PaintBox1.OnResize(PaintBox1);
end;

procedure TSuTimingForm.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FReader.LoadFile(OpenDialog1.FileName);
    Caption := 'Sulog LTE Timing - ' + OpenDialog1.FileName;
    Application.Title := Caption;
  end;
end;

procedure TSuTimingForm.MenuItem4Click(Sender: TObject);
begin
  FReader.Playing := not FReader.Playing;
end;

procedure TSuTimingForm.MenuItem6Click(Sender: TObject);
begin
  FReader.Legend := SuTimingHelpForm.TreeView1;
  SuTimingHelpForm.Show;
end;

procedure TSuTimingForm.MenuItem7Click(Sender: TObject);
begin
  with SuTmingOptionForm do
  begin
    RadioFrameType.ItemIndex := Ord(FReader.Graph.FrameType);
    RadioCPType.ItemIndex := Ord(FReader.Graph.CPMode);
    ComboFrameAlloc.ItemIndex := FReader.Graph.SubframeAlloc;
    ComboSpecial.ItemIndex    := FReader.Graph.SpecialSubframeConfig;
    EditBw.Text := IntToStr(FReader.Graph.BandwidthRB);
    EditSampleRate.Text := IntToStr(FReader.Graph.SampleRate);
    EditPlayInterval.Text := IntToStr(FReader.PlayInterval);
    if ShowModal <> mrOK then Exit;
    FReader.Graph.BandwidthRB := StrToIntDef(EditBw.Text, 110);
    FReader.Graph.SampleRate := StrToIntDef(EditSampleRate.Text, 30720000);
    FReader.PlayInterval := StrToIntDef(EditPlayInterval.Text, 30);

    FReader.Graph.FrameType := TFrameType(RadioFrameType.ItemIndex);
    FReader.Graph.CPMode    := TCPMode(RadioCPType.ItemIndex);
    FReader.Graph.SubframeAlloc := ComboFrameAlloc.ItemIndex;
    FReader.Graph.SpecialSubframeConfig := ComboSpecial.ItemIndex;
  end;
end;

procedure TSuTimingForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TSuTimingForm.PaintBox1MouseEnter(Sender: TObject);
begin

end;

procedure TSuTimingForm.PaintBox1MouseLeave(Sender: TObject);
begin

end;

procedure TSuTimingForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TSuTimingForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TSuTimingForm.PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TSuTimingForm.Button1Click(Sender: TObject);
begin

end;

end.

