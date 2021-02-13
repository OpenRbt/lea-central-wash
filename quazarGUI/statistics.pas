unit statistics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TStatisticsForm }

  TStatisticsForm = class(TBaseForm)
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

    procedure StatisticsMouseEnter(Sender: TObject); override;
    procedure StatisticsMouseLeave(Sender: TObject); override;

  private

  public

  end;

var
  StatisticsForm: TStatisticsForm;

implementation

{$R *.lfm}

procedure TStatisticsForm.FormCreate(Sender: TObject);
begin

end;

procedure TStatisticsForm.FormShow(Sender: TObject);
begin
  StatisticsForm.Statistics.Font.Color := StatisticsForm.GetHoverColor();
end;

procedure TStatisticsForm.MainClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TStatisticsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  StatisticsForm.Hide;
end;

procedure TStatisticsForm.StatisticsMouseEnter(Sender: TObject);
begin

end;

procedure TStatisticsForm.StatisticsMouseLeave(Sender: TObject);
begin

end;

end.

