unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TMainForm }

  TMainForm = class(TBaseForm)
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

    procedure MainMouseEnter(Sender: TObject); override;
    procedure MainMouseLeave(Sender: TObject); override;

  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  MainForm.Main.Font.Color := MainForm.GetHoverColor();
end;

procedure TMainForm.MainClick(Sender: TObject);
begin
  Inherited;
end;

procedure TMainForm.StationsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.UsersClick(Sender: TObject);
begin
  Inherited;
  MainForm.Close;
end;

procedure TMainForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
end;

procedure TMainForm.MainMouseEnter(Sender: TObject);
begin

end;

procedure TMainForm.MainMouseLeave(Sender: TObject);
begin

end;

end.

