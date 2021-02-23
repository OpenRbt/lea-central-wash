unit stations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser;

type

  StationsInfo = packed record
    Count: integer;
    id: array of integer;
    name: array of string;
    hash: array of string;
    status: array of string;
    info: array of string;
    currentBalance: array of integer;
    currentProgram: array of integer;
    end;
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
  RequestAnswer: string;
  stationsJson: TJsonArray;
  i: integer;
  path: TJSONdata;
  hasID : boolean;
  id : integer;

begin
  StationsForm.Stations.Font.Color := StationsForm.GetHoverColor();

  NotAuthorized.Visible:=False;
  StationsGrid.Visible:=True;

  for i:=0 to StationsGrid.RowCount-1 do
  begin
    StationsGrid.Cells[ENABLED_CHECKBOX_COL, i] := '0';
  end;

  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());
        RequestAnswer := Get(GetServerEndpoint() + 'status');

        stationsJson := GetJson(RequestAnswer).GetPath('stations') as TJsonArray;

        setlength(ResponseStations.id, stationsJson.Count);
        setlength(ResponseStations.name, stationsJson.Count);
        setlength(ResponseStations.hash, stationsJson.Count);
        setlength(ResponseStations.status, stationsJson.Count);
        setlength(ResponseStations.info, stationsJson.Count);
        setlength(ResponseStations.currentBalance, stationsJson.Count);
        setlength(ResponseStations.currentProgram, stationsJson.Count);

        setlength(freeHashVals, stationsJson.Count);

        ResponseStations.Count := stationsJson.Count;

        freeHashID := 0;
        id := NO_ID;
        for i := 0 to stationsJson.Count - 1 do
        begin
          with stationsJson.items[i] do
          begin
            path := FindPath('id');
            if path <> nil then
            begin
              id := path.AsInteger-1;
              ResponseStations.id[id] := path.AsInteger;
              hasID := true;
            end
            else
            begin
              ResponseStations.id[stationsJson.Count-1-freeHashID] := NO_ID;
              hasID := false;
            end;

            path := FindPath('hash');
            if path <> nil then
            begin
              if hasID then
              begin
                ResponseStations.hash[id] := path.AsString;
              end
              else
              begin
                freeHashVals[freeHashID] := path.AsString;
                freeHashID := freeHashID + 1;
              end;
            end
            else
            begin
              //if hasID then
              //begin
                ResponseStations.hash[id] := PLACEHOLDER;
              //end;
            end;

            path := FindPath('name');
            if path <> nil then
            begin
              if ResponseStations.hash[id] <> PLACEHOLDER then
              begin
                ResponseStations.name[id] := path.AsString;
                StationsGrid.Cells[NAME_COL, id] := PADDING + ResponseStations.name[id];
                StationsGrid.Cells[PREFLIGHT_LABEL_COL, id] := PREFLIGHT_STR;
                StationsGrid.Cells[PREFLIGHT_DEC, id] := '-';
                StationsGrid.Cells[PREFLIGHT_VAL_COL, id] := '0';
                StationsGrid.Cells[PREFLIGHT_INC, id] := '+';
                StationsGrid.Cells[ENABLED_LABEL_COL, id] := ENABLED_STR;
                StationsGrid.Cells[ENABLED_CHECKBOX_COL, id] := '1';
              end;
            end;
          end;
        end;

      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          401, 403: // do nothing
            ;
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        setlength(ResponseStations.id, 0);
        setlength(ResponseStations.name, 0);
        setlength(ResponseStations.hash, 0);
        setlength(ResponseStations.status, 0);
        setlength(ResponseStations.info, 0);
        setlength(ResponseStations.currentBalance, 0);
        setlength(ResponseStations.currentProgram, 0);
        setlength(freeHashVals, 0);

        ResponseStations.Count := 0;
      end;
    finally
      Free;
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

  end
  else if (aCol = PREFLIGHT_INC) then
  begin

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

