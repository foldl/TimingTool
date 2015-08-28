unit quickloadformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TimingReader;

type

  { TQuickLoadForm }

  TQuickLoadForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FReader: TTimingReader;
  public
    function QuickLoad(Reader: TTimingReader): Boolean;
  end;

var
  QuickLoadForm: TQuickLoadForm;

implementation

{$R *.lfm}

{ TQuickLoadForm }

procedure TQuickLoadForm.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Memo1.Lines.Count - 1 do
    FReader.DoLoadLine(Self, Memo1.Lines[I], I + 1);
  ModalResult := mrOK;
end;

procedure TQuickLoadForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FReader) then
  begin
    FReader.AfterRead;
    FReader := nil;
  end;
end;

function TQuickLoadForm.QuickLoad(Reader: TTimingReader): Boolean;
begin
  FReader := Reader;
  FReader.BeforeRead;
  ShowModal;
  Result := True;
end;

end.

