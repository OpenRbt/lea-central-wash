unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser, Types;

type
  THashes = array of string;

  { TSettingsForm }

  TSettingsForm = class(TBaseForm)
    StationsGrid: TStringGrid;
    UpdateTimer: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure StationsGridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;

    procedure SettingsMouseEnter(Sender: TObject); override;
    procedure SettingsMouseLeave(Sender: TObject); override;
    procedure StationsGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
    procedure UpdateCall(Sender: TObject);

  private

  public
    PLACEHOLDER : string;
  end;

var
  SettingsForm: TSettingsForm;

const
  MAX_NUM_STATIONS : integer = 12;
  NO_ID       : integer = 0;
  NAME_COL    : integer = 0;
  ADDRESS_COL : integer = 1;
  STATUS_COL  : integer = 2;
  EDIT_COL    : integer = 3;
  EDIT_STRING : string = '...';
  PADDING     : string = '  ';

implementation
  uses setting_edit;

{$R *.lfm}

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  PLACEHOLDER := '';
  UpdateTimer.Interval := 1000;
end;

procedure TSettingsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  inherited;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.UpdateCall(Sender: TObject);
begin
  UpdateTimer.Enabled := false;
  SettingsForm.FormShow(Sender);
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  SettingsForm.Settings.Font.Color := GetHoverColor();
  SettingsForm.NotAuthorized.Visible:=False;
  SettingsForm.StationsGrid.Visible:=True;

  UpdateStations();
  SettingsForm.StationsGrid.RowCount := SettingsForm.StationsGrid.FixedRows + GetNumStations();// - freeHashID;
  SettingsForm.StationsGrid.Rows[SettingsForm.StationsGrid.FixedRows].Clear;
  for i := 1 to GetNumStations() do
  begin
    //if GetStationHashByID(i) <> PLACEHOLDER then
    //begin
      SettingsForm.StationsGrid.Cells[ADDRESS_COL, i] := PADDING + GetStationHashByID(i);
      SettingsForm.StationsGrid.Cells[NAME_COL,    i] := PADDING + GetStationNameByID(i);
      SettingsForm.StationsGrid.Cells[STATUS_COL,  i] := GetStationStatusByID(i);
      SettingsForm.StationsGrid.Cells[EDIT_COL,    i] := EDIT_STRING;
    //end;
  end;
  UpdateTimer.Enabled := true;
end;

procedure TSettingsForm.MainClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TSettingsForm.StationsGridButtonClick(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateTimer.Enabled := false;
  SettingEditForm.Init(ResponseStations.id[aRow-1], ResponseStations.name[aRow-1], ResponseStations.hash[aRow-1]);
  SettingEditForm.ShowModal;
  UpdateTimer.Enabled := true;
end;

procedure TSettingsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  SettingsForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TSettingsForm.SettingsMouseEnter(Sender: TObject);
begin

end;

procedure TSettingsForm.SettingsMouseLeave(Sender: TObject);
begin

end;

procedure TSettingsForm.StationsGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  status: string;
begin
  // We draw colored cells in status column only
  if (aCol = STATUS_COL) and (aRow <> 0) then
  begin
    status := StationsGrid.Cells[STATUS_COL,aRow];
    if status = ON_LINE then
    begin
      StationsGrid.Canvas.Brush.Color := clLime;
      StationsGrid.Canvas.FillRect(aRect);
      StationsGrid.Canvas.TextRect(aRect,
        aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, ON_LINE);
    end;
  end;
end;

end.

