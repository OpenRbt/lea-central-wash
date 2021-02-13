unit programs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TProgramsForm }

  TProgramsForm = class(TBaseForm)
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

    procedure ProgramsMouseEnter(Sender: TObject); override;
    procedure ProgramsMouseLeave(Sender: TObject); override;

  private

  public

  end;

var
  ProgramsForm: TProgramsForm;

implementation

{$R *.lfm}

procedure TProgramsForm.FormCreate(Sender: TObject);
begin

end;

procedure TProgramsForm.FormShow(Sender: TObject);
begin
  ProgramsForm.Programs.Font.Color := ProgramsForm.GetHoverColor();
end;

procedure TProgramsForm.MainClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TProgramsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.ProgramsMouseEnter(Sender: TObject);
begin

end;

procedure TProgramsForm.ProgramsMouseLeave(Sender: TObject);
begin

end;

end.

