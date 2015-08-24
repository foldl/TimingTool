unit settingsu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  { TSuTmingOptionForm }

  TSuTmingOptionForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ComboFrameAlloc: TComboBox;
    ComboSpecial: TComboBox;
    EditBw: TLabeledEdit;
    EditSampleRate: TLabeledEdit;
    EditPlayInterval: TLabeledEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    RadioFrameType: TRadioGroup;
    RadioCPType: TRadioGroup;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SuTmingOptionForm: TSuTmingOptionForm;

implementation

{$R *.lfm}

{ TSuTmingOptionForm }

procedure TSuTmingOptionForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSuTmingOptionForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

