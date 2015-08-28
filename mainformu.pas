unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, ComCtrls, ex_logparser_su, LteGraph, types,
  ex_logparser_tds, ex_sleep_check;

type

  { TTimingForm }

  TTimingForm = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    FLteReader: TLteTimingReaderSu;
    FTdsReader: TTdsTimingReader;
    FSleepReader: TSleepTimingReader;
  public
    { public declarations }
  end;

var
  TimingForm: TTimingForm;

implementation

uses
  helpformu, settingsu, timingloadingformu, quickloadformu;

{$R *.lfm}

const
  PAGE_LTE = 0;
  PAGE_TDS = 1;
  PAGE_MS = 2;

{ TTimingForm }

procedure TTimingForm.FormCreate(Sender: TObject);
begin
  FLteReader := TLteTimingReaderSu.Create;
  FLteReader.FrameList := ListBox1;
  FLteReader.PaintBox := PaintBox1;
  //FLteReader.Graph.ShowEventLabel := False;

  FTdsReader := TTdsTimingReader.Create;
  with FTdsReader do
  begin
    FrameList := ListBox2;
    PaintBox := PaintBox2;
  end;

  FSleepReader := TSleepTimingReader.Create;
  with FSleepReader do
  begin
    FrameList := ListBox3;
    PaintBox := PaintBox3;
  end;
  // something is strange
 // PaintBox1.OnResize(PaintBox1);    PaintBox1.OnResize(PaintBox1);
end;

procedure TTimingForm.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Caption := PageControl1.ActivePage.Caption + ' Timing - ' + OpenDialog1.FileName;
    Application.Title := Caption;
    case PageControl1.PageIndex of
      PAGE_LTE: TimingLoadingForm.LoadFile(OpenDialog1.FileName, FLteReader);
      PAGE_TDS: TimingLoadingForm.LoadFile(OpenDialog1.FileName, FTdsReader);
      PAGE_MS:  TimingLoadingForm.LoadFile(OpenDialog1.FileName, FSleepReader);
    end;
  end;
end;

procedure TTimingForm.MenuItem4Click(Sender: TObject);
begin
  case PageControl1.PageIndex of
    PAGE_LTE:
      begin
        FLteReader.Playing := not FLteReader.Playing;
        FTdsReader.Playing := False;
        FSleepReader.Playing := False;
      end;
    PAGE_TDS:
      begin
        FLteReader.Playing := False;
        FTdsReader.Playing := not FTdsReader.Playing;
        FSleepReader.Playing := False;
      end;
    PAGE_MS:
      begin
        FLteReader.Playing := False;
        FTdsReader.Playing := False;
        FSleepReader.Playing := not FTdsReader.Playing;
      end;
  end;
end;

procedure TTimingForm.MenuItem6Click(Sender: TObject);
begin
  case PageControl1.PageIndex of
    PAGE_LTE:
      begin
        FLteReader.Legend := SuTimingHelpForm.TreeView1;
        SuTimingHelpForm.Show;
      end;
    PAGE_TDS:
      begin
        ;
      end;
    PAGE_MS:
      begin
        ;
      end;
  end;
end;

procedure TTimingForm.MenuItem7Click(Sender: TObject);
begin
  if PageControl1.PageIndex <> PAGE_LTE then Exit;

  with SuTmingOptionForm do
  begin
    RadioFrameType.ItemIndex := Ord(FLteReader.Graph.FrameType);
    RadioCPType.ItemIndex := Ord(FLteReader.Graph.CPMode);
    ComboFrameAlloc.ItemIndex := FLteReader.Graph.SubframeAlloc;
    ComboSpecial.ItemIndex    := FLteReader.Graph.SpecialSubframeConfig;
    EditBw.Text := IntToStr(FLteReader.Graph.BandwidthRB);
    EditSampleRate.Text := IntToStr(FLteReader.Graph.SampleRate);
    EditPlayInterval.Text := IntToStr(FLteReader.PlayInterval);
    if ShowModal <> mrOK then Exit;
    FLteReader.Graph.BandwidthRB := StrToIntDef(EditBw.Text, 110);
    FLteReader.Graph.SampleRate := StrToIntDef(EditSampleRate.Text, 30720000);
    FLteReader.PlayInterval := StrToIntDef(EditPlayInterval.Text, 30);

    FLteReader.Graph.FrameType := TFrameType(RadioFrameType.ItemIndex);
    FLteReader.Graph.CPMode    := TCPMode(RadioCPType.ItemIndex);
    FLteReader.Graph.SubframeAlloc := ComboFrameAlloc.ItemIndex;
    FLteReader.Graph.SpecialSubframeConfig := ComboSpecial.ItemIndex;
  end;
end;

procedure TTimingForm.MenuItem8Click(Sender: TObject);
begin
  case PageControl1.PageIndex of
    PAGE_LTE: QuickLoadForm.QuickLoad(FLteReader);
    PAGE_TDS: QuickLoadForm.QuickLoad(FTdsReader);
    PAGE_MS: QuickLoadForm.QuickLoad(FSleepReader);
  end;
end;

end.

