unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TMainForm }

  TMainForm = class(TBaseForm)
    Station4Pause: TPanel;
    Station3Pause: TPanel;
    Station2Pause: TPanel;
    Station4Dry: TPanel;
    Station3Dry: TPanel;
    Station2Dry: TPanel;
    Station4Wax: TPanel;
    Station3Wax: TPanel;
    Station2Wax: TPanel;
    Station4Rinse: TPanel;
    Station3Rinse: TPanel;
    Station2Rinse: TPanel;
    Station1Shampoo1: TPanel;
    Station3Shampoo: TPanel;
    Station2Shampoo: TPanel;
    Station4Foam: TPanel;
    Station3Foam: TPanel;
    Station2Foam: TPanel;
    Station2: TPanel;
    Station3: TPanel;
    Station4: TPanel;
    Station6Pause: TPanel;
    Station8Dry: TPanel;
    Station7Dry: TPanel;
    Station6Dry: TPanel;
    Station8Wax: TPanel;
    Station7Wax: TPanel;
    Station6Wax: TPanel;
    Station8Rinse: TPanel;
    Station7Rinse: TPanel;
    Station6Rinse: TPanel;
    Station6Shampoo: TPanel;
    Station8Shampoo: TPanel;
    Station7Shampoo: TPanel;
    Station8Foam: TPanel;
    Station7Foam: TPanel;
    Station6Foam: TPanel;
    Station5Pause: TPanel;
    Station5Dry: TPanel;
    Station1Foam: TPanel;
    Station1: TPanel;
    Station5Wax: TPanel;
    Station5Shampoo: TPanel;
    Station5Rinse: TPanel;
    Station5Foam: TPanel;
    Station1Shampoo: TPanel;
    Station1Rinse: TPanel;
    Station1Wax: TPanel;
    Station1Dry: TPanel;
    Station1Pause: TPanel;
    Station5: TPanel;
    Station6: TPanel;
    Station7: TPanel;
    Station8: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
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
  MainForm.Hide;
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

