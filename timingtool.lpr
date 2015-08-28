program timingtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainformu, ltegraph, tachartlazaruspkg, lazcontrols, timingreader,
  mousetool, helpformu, settingsu, superobject, timingloadingformu,
  ex_logparser_su, KLib, timinggraph, tdsgraph, ex_logparser_tds,
  quickloadformu, ex_sleep_check;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TTimingForm, TimingForm);
  Application.CreateForm(TSuTimingHelpForm, SuTimingHelpForm);
  Application.CreateForm(TSuTmingOptionForm, SuTmingOptionForm);
  Application.CreateForm(TTimingLoadingForm, TimingLoadingForm);
  Application.CreateForm(TQuickLoadForm, QuickLoadForm);
  Application.Run;
end.

