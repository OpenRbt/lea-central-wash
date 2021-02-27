unit stations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser;

type

  { TStationsForm }

  TStationsForm = class(TBaseForm)
    StationsGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure StationsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;

    procedure StationsMouseEnter(Sender: TObject); override;
    procedure StationsMouseLeave(Sender: TObject); override;

    procedure UpdatePreflight();
    procedure ShowStations();

  private

  public
    PLACEHOLDER : string;
  end;

var
  StationsForm: TStationsForm;
  ResponseStations: StationsInfo;
  freeHashVals : array of string;
  freeHashID : integer;

const
  NAME_COL            : integer = 0;
  PREFLIGHT_LABEL_COL : integer = 1;
  PREFLIGHT_DEC       : integer = 2;
  PREFLIGHT_VAL_COL   : integer = 3;
  PREFLIGHT_INC       : integer = 4;
  ENABLED_LABEL_COL   : integer = 5;
  ENABLED_CHECKBOX_COL: integer = 6;

  PREFLIGHT_STR: string = 'ПРОКАЧКА';
  ENABLED_STR  : string = 'Активный';
  PADDING      : string = '  ';

implementation

{$R *.lfm}

procedure TStationsForm.FormCreate(Sender: TObject);
begin
  PLACEHOLDER := '';
end;

procedure TStationsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  inherited;
end;

procedure TStationsForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  StationsForm.Stations.Font.Color := StationsForm.GetHoverColor();

  NotAuthorized.Visible:=False;
  StationsGrid.Visible:=True;

  for i:=0 to StationsGrid.RowCount-1 do
  begin
    StationsGrid.Cells[ENABLED_CHECKBOX_COL, i] := '0';
  end;

  UpdateStations();
  UpdatePreflight();
  ShowStations();
end;

procedure TStationsForm.UpdatePreflight();
var
  id : integer;
begin
  for id:=1 to GetNumStations() do
  begin
    if GetStationHashByID(id) <> PLACEHOLDER then
    begin
      UpdateStationPreflightSec(id);
    end;
  end;
end;

procedure TStationsForm.ShowStations();
var
  id : integer;
begin
  for id:=1 to GetNumStations() do
  begin
    if GetStationHashByID(id) <> PLACEHOLDER then
    begin
      StationsGrid.Cells[NAME_COL, id-1] := PADDING + GetStationNameByID(id);
      StationsGrid.Cells[PREFLIGHT_LABEL_COL, id-1] := PREFLIGHT_STR;
      StationsGrid.Cells[PREFLIGHT_DEC, id-1] := '-';
      StationsGrid.Cells[PREFLIGHT_VAL_COL, id-1] := IntToStr(GetStationPreflightSec(id));
      StationsGrid.Cells[PREFLIGHT_INC, id-1] := '+';
      StationsGrid.Cells[ENABLED_LABEL_COL, id-1] := ENABLED_STR;
      StationsGrid.Cells[ENABLED_CHECKBOX_COL, id-1] := '1';
    end;
  end;
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

procedure TStationsForm.StationsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  id: integer;
  preflightSec : integer;
begin
  if (ResponseStations.Count > 0) and (ResponseStations.hash[aRow] = PLACEHOLDER) then
  begin
    Exit;
  end;
  if (aCol = ENABLED_CHECKBOX_COL) or (aCol = ENABLED_LABEL_COL) then
  begin
    if StationsGrid.Cells[ENABLED_CHECKBOX_COL, aRow] = '0' then
    begin
      //ProgramsInfo.Enabled[aRow] := true;
      StationsGrid.Cells[ENABLED_CHECKBOX_COL, aRow] := '1';
    end
    else
    begin
      //ProgramsInfo.Enabled[aRow] := false;
      StationsGrid.Cells[ENABLED_CHECKBOX_COL, aRow] := '0';
    end;
  end
  else if (aCol = PREFLIGHT_DEC) then
  begin
    id := aRow+1;
    preflightSec := GetStationPreflightSec(id);
    if preflightSec < 1 then
    begin
      Exit;
    end;
    SetStationPreflightSec(id, preflightSec - 1);
    StationsGrid.Cells[PREFLIGHT_VAL_COL, aRow] := IntToStr(GetStationPreflightSec(id));
  end
  else if (aCol = PREFLIGHT_INC) then
  begin
    id := aRow+1;
    preflightSec := GetStationPreflightSec(id);
    SetStationPreflightSec(id, preflightSec + 1);
    StationsGrid.Cells[PREFLIGHT_VAL_COL, aRow] := IntToStr(GetStationPreflightSec(id));
  end;
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

