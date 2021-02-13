unit stations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TStationsForm }

  TStationsForm = class(TBaseForm)
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

    procedure StationsMouseEnter(Sender: TObject); override;
    procedure StationsMouseLeave(Sender: TObject); override;
  private

  public

  end;

var
  StationsForm: TStationsForm;

implementation

{$R *.lfm}

procedure TStationsForm.FormCreate(Sender: TObject);
begin

end;

procedure TStationsForm.FormShow(Sender: TObject);
begin
  StationsForm.Stations.Font.Color := StationsForm.GetHoverColor();
end;

procedure TStationsForm.MainClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.StationsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TStationsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  StationsForm.Hide;
end;

procedure TStationsForm.StationsMouseEnter(Sender: TObject);
begin

end;

procedure TStationsForm.StationsMouseLeave(Sender: TObject);
begin

end;

end.

