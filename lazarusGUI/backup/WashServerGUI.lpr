program WashServerGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, Source, superobject, manager, collection, clientAPI;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.Create(Client);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TManageForm, ManageForm);
  Application.CreateForm(TMoneyCollectionForm, MoneyCollectionForm);
  Application.Run;
end.

