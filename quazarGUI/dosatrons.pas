unit dosatrons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base;

type

  { TDosatronsForm }

  TDosatronsForm = class(TBaseForm)
    Placeholder: TStaticText;
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

    procedure DosatronsMouseEnter(Sender: TObject); override;
    procedure DosatronsMouseLeave(Sender: TObject); override;

  private

  public

  end;

var
  DosatronsForm: TDosatronsForm;

implementation

{$R *.lfm}

procedure TDosatronsForm.FormCreate(Sender: TObject);
begin

end;

procedure TDosatronsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  inherited;
end;

procedure TDosatronsForm.FormShow(Sender: TObject);
begin
  DosatronsForm.Dosatrons.Font.Color := DosatronsForm.GetHoverColor();
end;

procedure TDosatronsForm.MainClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TDosatronsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.DosatronsMouseEnter(Sender: TObject);
begin

end;

procedure TDosatronsForm.DosatronsMouseLeave(Sender: TObject);
begin

end;

end.

