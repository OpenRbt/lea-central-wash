unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser, Types;

type
  THashes = array of string;

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
    function GetFreeHashes() : THashes;

  private

  public
    PLACEHOLDER : string;
  end;

var
  SettingsForm: TSettingsForm;
  ResponseStations: StationsInfo;
  freeHashVals : array of string;
  freeHashID : integer;

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

function TSettingsForm.GetFreeHashes() : THashes;
var
  hashes : THashes;
  i : integer;
begin
  setlength(hashes, freeHashID);
  for i := 0 to freeHashID-1 do
  begin
    hashes[i] := freeHashVals[i];
  end;
  Result := hashes;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  RequestAnswer: string;
  stationsJson: TJsonArray;
  i: integer;
  path: TJSONdata;
  hasID : boolean;
  id : integer;
  //successful: boolean = True;

begin
  SettingsForm.Settings.Font.Color := GetHoverColor();
  SettingsForm.NotAuthorized.Visible:=False;
  SettingsForm.StationsGrid.Visible:=True;

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
              end
              else
              begin
                ResponseStations.name[id] := PLACEHOLDER;
              end;
            end;

            path := FindPath('status');
            if path <> nil then
            begin
              if ResponseStations.hash[id] <> PLACEHOLDER then
              begin
                ResponseStations.status[id] := path.AsString;
              end
              else
              begin
                ResponseStations.status[id] := PLACEHOLDER;
              end;
            end;

            path := FindPath('info');
            if path <> nil then
            begin
              if hasID then
              begin
                ResponseStations.info[id] := path.AsString;
              end;
            end;

            path := FindPath('currentBalance');
            if path <> nil then
            begin
              if hasID then
              begin
                ResponseStations.currentBalance[id] := path.AsInteger;
              end;
            end;

            path := FindPath('currentProgram');
            if path <> nil then
            begin
              if hasID then
              begin
                ResponseStations.currentProgram[id] := path.AsInteger;
              end;
            end;
          end;
        end;

        SettingsForm.StationsGrid.RowCount := SettingsForm.StationsGrid.FixedRows + ResponseStations.Count - freeHashID;
        SettingsForm.StationsGrid.Rows[SettingsForm.StationsGrid.FixedRows].Clear;
        for i := 0 to stationsJson.Count - 1 do
        begin
          if ResponseStations.id[i] <> NO_ID then
          begin
            SettingsForm.StationsGrid.Cells[ADDRESS_COL, ResponseStations.id[i]] := PADDING + ResponseStations.hash[i];
            SettingsForm.StationsGrid.Cells[NAME_COL, ResponseStations.id[i]] := PADDING + ResponseStations.name[i];
            SettingsForm.StationsGrid.Cells[STATUS_COL, ResponseStations.id[i]] := ResponseStations.status[i];
            SettingsForm.StationsGrid.Cells[EDIT_COL, ResponseStations.id[i]] := EDIT_STRING;
          end;
        end;
        UpdateTimer.Enabled := true;

      except
        //SettingsForm.NotAuthorized.Visible:=True;
        //SettingsForm.StationsGrid.Visible:=False;
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

