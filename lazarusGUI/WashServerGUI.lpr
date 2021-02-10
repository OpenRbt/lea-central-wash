program WashServerGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, Source, superobject, manager, collection, clientAPI,
  managerRelays, settingsKasse, cardreadercfg;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tclient, Client);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TManageForm, ManageForm);
  Application.CreateForm(TMoneyCollectionForm, MoneyCollectionForm);
  Application.CreateForm(TManagePrograms, ManagePrograms);
  Application.CreateForm(TsettingsKasse, settingKasse);
  Application.CreateForm(TCardReaderConfigForm, CardReaderConfigForm);
  Application.Run;
end.

