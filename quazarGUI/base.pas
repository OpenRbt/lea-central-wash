unit Base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls;

type

  { TBaseForm }

  TBaseForm = class(TForm)
    Main: TPanel;
    NotAuthorized: TStaticText;
    Stations: TPanel;
    Programs: TPanel;
    Dosatrons: TPanel;
    Settings: TPanel;
    Logout: TPanel;
    Statistics: TPanel;
    Users: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;

    procedure MainClick(Sender: TObject); virtual;
    procedure MainMouseEnter(Sender: TObject); virtual;
    procedure MainMouseLeave(Sender: TObject); virtual;
    procedure StationsClick(Sender: TObject); virtual;
    procedure StationsMouseEnter(Sender: TObject); virtual;
    procedure StationsMouseLeave(Sender: TObject); virtual;
    procedure ProgramsClick(Sender: TObject); virtual;
    procedure ProgramsMouseEnter(Sender: TObject); virtual;
    procedure ProgramsMouseLeave(Sender: TObject); virtual;
    procedure DosatronsClick(Sender: TObject); virtual;
    procedure DosatronsMouseEnter(Sender: TObject); virtual;
    procedure DosatronsMouseLeave(Sender: TObject); virtual;
    procedure SettingsClick(Sender: TObject); virtual;
    procedure SettingsMouseEnter(Sender: TObject); virtual;
    procedure SettingsMouseLeave(Sender: TObject); virtual;
    procedure UsersClick(Sender: TObject); virtual;
    procedure UsersMouseEnter(Sender: TObject); virtual;
    procedure UsersMouseLeave(Sender: TObject); virtual;
    procedure StatisticsClick(Sender: TObject); virtual;
    procedure StatisticsMouseEnter(Sender: TObject); virtual;
    procedure StatisticsMouseLeave(Sender: TObject); virtual;
    procedure LogoutClick(Sender: TObject); virtual;
    procedure LogoutMouseEnter(Sender: TObject);
    procedure LogoutMouseLeave(Sender: TObject);

    procedure SetPinCode(code: string);
    function GetPinCode(): string;
    procedure SetServerEndpoint(endpoint: string);
    function GetServerEndpoint(): string;
    function GetHoverColor(): TColor;

  private
    pinCode: string;
    serverEndpoint: string;
  public

  end;

var
  BaseForm: TBaseForm;

const
  hoverColor: TColor = clFuchsia;

implementation
  uses
    Login, Main, Stations, Programs, Dosatrons, Settings, Users, Statistics;
{$R *.lfm}

procedure TBaseForm.FormCreate(Sender: TObject);
begin

end;

procedure TBaseForm.SetPinCode(code: string);
begin
  BaseForm.pinCode := code;
end;

function TBaseForm.GetPinCode(): string;
begin
  Result := BaseForm.pinCode;
end;

procedure TBaseForm.SetServerEndpoint(endpoint: string);
begin
  BaseForm.serverEndpoint := endpoint;
end;

function TBaseForm.GetServerEndpoint(): string;
begin
  Result := BaseForm.serverEndpoint;
end;

function TBaseForm.GetHoverColor(): TColor;
begin
  Result := hoverColor;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin

end;

procedure TBaseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  LoginForm.Show;
end;

procedure TBaseForm.MainClick(Sender: TObject);
begin
  MainForm.Show;
end;

procedure TBaseForm.StationsClick(Sender: TObject);
begin
  StationsForm.Show;
end;

procedure TBaseForm.ProgramsClick(Sender: TObject);
begin
  ProgramsForm.Show;
end;

procedure TBaseForm.DosatronsClick(Sender: TObject);
begin
  DosatronsForm.Show;
end;

procedure TBaseForm.SettingsClick(Sender: TObject);
begin
  SettingsForm.Show;
end;

procedure TBaseForm.UsersClick(Sender: TObject);
begin
  UsersForm.Show;
end;

procedure TBaseForm.StatisticsClick(Sender: TObject);
begin
  StatisticsForm.Show;
end;

procedure TBaseForm.LogoutClick(Sender: TObject);
begin
  LoginForm.Show;
end;

procedure TBaseForm.LogoutMouseEnter(Sender: TObject);
begin
  Logout.Font.Color := hoverColor;
end;

procedure TBaseForm.LogoutMouseLeave(Sender: TObject);
begin
  Logout.Font.Color := clDefault;
end;

procedure TBaseForm.MainMouseEnter(Sender: TObject);
begin
  Main.Font.Color := hoverColor;
end;

procedure TBaseForm.MainMouseLeave(Sender: TObject);
begin
  Main.Font.Color := clDefault;
end;

procedure TBaseForm.ProgramsMouseEnter(Sender: TObject);
begin
  Programs.Font.Color := hoverColor;
end;

procedure TBaseForm.ProgramsMouseLeave(Sender: TObject);
begin
  Programs.Font.Color := clDefault;
end;

procedure TBaseForm.SettingsMouseEnter(Sender: TObject);
begin
  Settings.Font.Color := hoverColor;
end;

procedure TBaseForm.SettingsMouseLeave(Sender: TObject);
begin
  Settings.Font.Color := clDefault;
end;

procedure TBaseForm.StationsMouseEnter(Sender: TObject);
begin
  Stations.Font.Color := hoverColor;
end;

procedure TBaseForm.StationsMouseLeave(Sender: TObject);
begin
  Stations.Font.Color := clDefault;
end;

procedure TBaseForm.StatisticsMouseEnter(Sender: TObject);
begin
  Statistics.Font.Color := hoverColor;
end;

procedure TBaseForm.StatisticsMouseLeave(Sender: TObject);
begin
  Statistics.Font.Color := clDefault;
end;

procedure TBaseForm.UsersMouseEnter(Sender: TObject);
begin
  Users.Font.Color := hoverColor;
end;

procedure TBaseForm.UsersMouseLeave(Sender: TObject);
begin
  Users.Font.Color := clDefault;
end;

procedure TBaseForm.DosatronsMouseEnter(Sender: TObject);
begin
  Dosatrons.Font.Color := hoverColor;
end;

procedure TBaseForm.DosatronsMouseLeave(Sender: TObject);
begin
  Dosatrons.Font.Color := clDefault;
end;

end.

