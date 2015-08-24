unit helpformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls;

type

  { TSuTimingHelpForm }

  TSuTimingHelpForm = class(TForm)
    ImageList1: TImageList;
    Memo1: TMemo;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SuTimingHelpForm: TSuTimingHelpForm;

implementation

{$R *.lfm}

{ TSuTimingHelpForm }

end.

