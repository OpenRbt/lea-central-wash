program QuazarManagerGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, login, Base, users, main, stations, programs, dosatrons, settings,
  statistics, user_edit_base, user_add, user_edit, user_edit_password,
  setting_edit, station_balance
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TBaseForm, BaseForm);
  Application.CreateForm(TUsersForm, UsersForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStationsForm, StationsForm);
  Application.CreateForm(TProgramsForm, ProgramsForm);
  Application.CreateForm(TDosatronsForm, DosatronsForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TStatisticsForm, StatisticsForm);
  Application.CreateForm(TUserEditBaseForm, UserEditBaseForm);
  Application.CreateForm(TUserAddForm, UserAddForm);
  Application.CreateForm(TUserEditForm, UserEditForm);
  Application.CreateForm(TUserEditPasswordForm, UserEditPasswordForm);
  Application.CreateForm(TSettingEditForm, SettingEditForm);
  Application.CreateForm(TStationBalanceForm, StationBalanceForm);
  Application.Run;
end.

