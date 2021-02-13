unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TSettingsForm }

  TSettingsForm = class(TBaseForm)
    Placeholder: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;

    procedure SettingsMouseEnter(Sender: TObject); override;
    procedure SettingsMouseLeave(Sender: TObject); override;

  private

  public

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.FormCreate(Sender: TObject);
begin

end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  SettingsForm.Settings.Font.Color := SettingsForm.GetHoverColor();
end;

procedure TSettingsForm.MainClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TSettingsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
end;

procedure TSettingsForm.SettingsMouseEnter(Sender: TObject);
begin

end;

procedure TSettingsForm.SettingsMouseLeave(Sender: TObject);
begin

end;

end.

