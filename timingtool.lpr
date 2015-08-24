program timingtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainformu, ltegraph, tachartlazaruspkg, lazcontrols, LteTimingReader,
mousetool, helpformu, settingsu, superobject, sutimingloadingformu, 
logparser_su, KLib, timinggraph, tdsgraph
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TSuTimingForm, SuTimingForm);
  Application.CreateForm(TSuTimingHelpForm, SuTimingHelpForm);
  Application.CreateForm(TSuTmingOptionForm, SuTmingOptionForm);
  Application.CreateForm(TSuTimingLoadingForm, SuTimingLoadingForm);
  Application.Run;
end.

