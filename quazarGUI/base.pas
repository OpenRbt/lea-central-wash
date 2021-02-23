unit Base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, fphttpclient, Fpjson, jsonparser;

type

  RelayConfig = packed record
    Count           : integer;
    ID              : array of integer;
    TimeON          : array of integer;
    TimeOFF         : array of integer;
    end;

  ProgramConfig = packed record
    Count           : integer;
    ID              : array of integer;
    Name            : array of string;
    Price           : array of integer;
    PreflightEnabled: array of boolean;
    Relays          : array of array of integer;
    PreflightRelays : array of array of integer;
    end;

  { TBaseForm }

  TBaseForm = class(TForm)
    Main: TPanel;
    NotAuthorized: TStaticText;
    Stations: TPanel;
    Programs: TPanel;
    Dosatrons: TPanel;
    Settings: TPanel;
    Logout: TPanel;
    Statistics: TPanel;
    Users: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;

    procedure MainClick(Sender: TObject); virtual;
    procedure MainMouseEnter(Sender: TObject); virtual;
    procedure MainMouseLeave(Sender: TObject); virtual;
    procedure StationsClick(Sender: TObject); virtual;
    procedure StationsMouseEnter(Sender: TObject); virtual;
    procedure StationsMouseLeave(Sender: TObject); virtual;
    procedure ProgramsClick(Sender: TObject); virtual;
    procedure ProgramsMouseEnter(Sender: TObject); virtual;
    procedure ProgramsMouseLeave(Sender: TObject); virtual;
    procedure DosatronsClick(Sender: TObject); virtual;
    procedure DosatronsMouseEnter(Sender: TObject); virtual;
    procedure DosatronsMouseLeave(Sender: TObject); virtual;
    procedure SettingsClick(Sender: TObject); virtual;
    procedure SettingsMouseEnter(Sender: TObject); virtual;
    procedure SettingsMouseLeave(Sender: TObject); virtual;
    procedure UsersClick(Sender: TObject); virtual;
    procedure UsersMouseEnter(Sender: TObject); virtual;
    procedure UsersMouseLeave(Sender: TObject); virtual;
    procedure StatisticsClick(Sender: TObject); virtual;
    procedure StatisticsMouseEnter(Sender: TObject); virtual;
    procedure StatisticsMouseLeave(Sender: TObject); virtual;
    procedure LogoutClick(Sender: TObject); virtual;
    procedure LogoutMouseEnter(Sender: TObject);
    procedure LogoutMouseLeave(Sender: TObject);

    procedure SetPinCode(code: string);
    function GetPinCode(): string;
    procedure SetServerEndpoint(endpoint: string);
    function GetServerEndpoint(): string;
    function GetHoverColor(): TColor;

    function CheckProgram(id: integer) : boolean;
    procedure SetProgram(id: integer);
    procedure SetRelayTimeOn(relayID, timeON : integer);
    function GetRelayTimeOn(relayID : integer) : integer;
    procedure UpdatePrograms();
    procedure CheckPrograms();
    function GetProgramName(id: integer): string;
    function GetProgramPrice(id: integer): integer;
    procedure SetProgramPrice(id, price : integer);

  private
    pinCode: string;
    serverEndpoint: string;
  public

  end;

var
  BaseForm      : TBaseForm;
  ProgramsConfig: ProgramConfig;
  RelaysConfig  : RelayConfig;

const
  hoverColor: TColor = clFuchsia;
  NO_ID     : integer = 0;

  NUM_PROGRAMS          : integer = 6;
  NUM_PREFLIGHT_PROGRAMS: integer = 4;

  FOAM_RELAY_ID      : integer = 6;
  SHAMPOO_RELAY_ID   : integer = 7;
  WAX_RELAY_ID       : integer = 8;
  POLYMER_RELAY_ID   : integer = 9;
  COLD_WATER_RELAY_ID: integer = 1;
  HOT_WATER_RELAY_ID : integer = 2;
  OSM_WATER_RELAY_ID : integer = 3;
  LIGHT_RELAY_ID     : integer = 4;

  FOAM_PROGRAM_ID    : integer = 1;
  SHAMPOO_PROGRAM_ID : integer = 2;
  RINSE_PROGRAM_ID   : integer = 3;
  WAX_PROGRAM_ID     : integer = 4;
  DRY_PROGRAM_ID     : integer = 5;
  PAUSE_PROGRAM_ID   : integer = 6;
  FOAM_PREFLIGHT_PROGRAM_ID   : integer =  7;
  SHAMPOO_PREFLIGHT_PROGRAM_ID: integer =  8;
  WAX_PREFLIGHT_PROGRAM_ID    : integer =  9;
  POLYMER_PREFLIGHT_PROGRAM_ID: integer = 10;

  FOAM_STR    : string = 'ПЕНА';
  SHAMPOO_STR : string = 'ВОДА + ШАМПУНЬ';
  RINSE_STR   : string = 'ОПОЛАСКИВАНИЕ';
  WAX_STR     : string = 'ВОСК';
  DRY_STR     : string = 'СУШКА И БЛЕСК';
  PAUSE_STR   : string = 'ПАУЗА';

  TOTAL_TIME  : integer = 1000;

implementation
  uses
    Login, Main, Stations, Programs, Dosatrons, Settings, Users, Statistics;
{$R *.lfm}

procedure TBaseForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  RelaysConfig.Count := 9;
  setlength(RelaysConfig.ID,      RelaysConfig.Count);
  setlength(RelaysConfig.TimeON,  RelaysConfig.Count);
  setlength(RelaysConfig.TimeOFF, RelaysConfig.Count);

  RelaysConfig.ID[FOAM_RELAY_ID       - 1] := FOAM_RELAY_ID;
  RelaysConfig.ID[SHAMPOO_RELAY_ID    - 1] := SHAMPOO_RELAY_ID;
  RelaysConfig.ID[WAX_RELAY_ID        - 1] := WAX_RELAY_ID;
  RelaysConfig.ID[POLYMER_RELAY_ID    - 1] := POLYMER_RELAY_ID;
  RelaysConfig.ID[COLD_WATER_RELAY_ID - 1] := COLD_WATER_RELAY_ID;
  RelaysConfig.ID[HOT_WATER_RELAY_ID  - 1] := HOT_WATER_RELAY_ID;
  RelaysConfig.ID[OSM_WATER_RELAY_ID  - 1] := OSM_WATER_RELAY_ID;
  RelaysConfig.ID[LIGHT_RELAY_ID      - 1] := LIGHT_RELAY_ID;

  RelaysConfig.TimeON[FOAM_RELAY_ID       - 1] := 500;
  RelaysConfig.TimeON[SHAMPOO_RELAY_ID    - 1] := 500;
  RelaysConfig.TimeON[WAX_RELAY_ID        - 1] := 500;
  RelaysConfig.TimeON[POLYMER_RELAY_ID    - 1] := 500;
  RelaysConfig.TimeON[COLD_WATER_RELAY_ID - 1] := 1000;
  RelaysConfig.TimeON[HOT_WATER_RELAY_ID  - 1] := 1000;
  RelaysConfig.TimeON[OSM_WATER_RELAY_ID  - 1] := 1000;
  RelaysConfig.TimeON[LIGHT_RELAY_ID      - 1] := 1000;

  RelaysConfig.TimeOFF[FOAM_RELAY_ID       - 1] := TOTAL_TIME - RelaysConfig.TimeON[FOAM_RELAY_ID       - 1];
  RelaysConfig.TimeOFF[SHAMPOO_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[SHAMPOO_RELAY_ID    - 1];
  RelaysConfig.TimeOFF[WAX_RELAY_ID        - 1] := TOTAL_TIME - RelaysConfig.TimeON[WAX_RELAY_ID        - 1];
  RelaysConfig.TimeOFF[POLYMER_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[POLYMER_RELAY_ID    - 1];
  RelaysConfig.TimeOFF[COLD_WATER_RELAY_ID - 1] := TOTAL_TIME - RelaysConfig.TimeON[COLD_WATER_RELAY_ID - 1];
  RelaysConfig.TimeOFF[HOT_WATER_RELAY_ID  - 1] := TOTAL_TIME - RelaysConfig.TimeON[HOT_WATER_RELAY_ID  - 1];
  RelaysConfig.TimeOFF[OSM_WATER_RELAY_ID  - 1] := TOTAL_TIME - RelaysConfig.TimeON[OSM_WATER_RELAY_ID  - 1];
  RelaysConfig.TimeOFF[LIGHT_RELAY_ID      - 1] := TOTAL_TIME - RelaysConfig.TimeON[LIGHT_RELAY_ID      - 1];

  ProgramsConfig.Count := NUM_PROGRAMS + NUM_PREFLIGHT_PROGRAMS;
  setlength(ProgramsConfig.ID,               ProgramsConfig.Count);
  setlength(ProgramsConfig.Name,             ProgramsConfig.Count);
  setlength(ProgramsConfig.Price,            ProgramsConfig.Count);
  setlength(ProgramsConfig.PreflightEnabled, ProgramsConfig.Count);
  setlength(ProgramsConfig.Relays,           ProgramsConfig.Count);
  setlength(ProgramsConfig.PreflightRelays,  ProgramsConfig.Count);

  ProgramsConfig.ID[FOAM_PROGRAM_ID              - 1] := FOAM_PROGRAM_ID;
  ProgramsConfig.ID[SHAMPOO_PROGRAM_ID           - 1] := SHAMPOO_PROGRAM_ID;
  ProgramsConfig.ID[RINSE_PROGRAM_ID             - 1] := RINSE_PROGRAM_ID;
  ProgramsConfig.ID[WAX_PROGRAM_ID               - 1] := WAX_PROGRAM_ID;
  ProgramsConfig.ID[DRY_PROGRAM_ID               - 1] := DRY_PROGRAM_ID;
  ProgramsConfig.ID[PAUSE_PROGRAM_ID             - 1] := PAUSE_PROGRAM_ID;
  ProgramsConfig.ID[FOAM_PREFLIGHT_PROGRAM_ID    - 1] := FOAM_PREFLIGHT_PROGRAM_ID;
  ProgramsConfig.ID[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1] := SHAMPOO_PREFLIGHT_PROGRAM_ID;
  ProgramsConfig.ID[WAX_PREFLIGHT_PROGRAM_ID     - 1] := WAX_PREFLIGHT_PROGRAM_ID;
  ProgramsConfig.ID[POLYMER_PREFLIGHT_PROGRAM_ID - 1] := POLYMER_PREFLIGHT_PROGRAM_ID;

  ProgramsConfig.Name[FOAM_PROGRAM_ID              - 1] := FOAM_STR;
  ProgramsConfig.Name[SHAMPOO_PROGRAM_ID           - 1] := SHAMPOO_STR;
  ProgramsConfig.Name[RINSE_PROGRAM_ID             - 1] := RINSE_STR;
  ProgramsConfig.Name[WAX_PROGRAM_ID               - 1] := WAX_STR;
  ProgramsConfig.Name[DRY_PROGRAM_ID               - 1] := DRY_STR;
  ProgramsConfig.Name[PAUSE_PROGRAM_ID             - 1] := PAUSE_STR;
  ProgramsConfig.Name[FOAM_PREFLIGHT_PROGRAM_ID    - 1] := FOAM_STR + ' ПРОКАЧКА';
  ProgramsConfig.Name[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1] := SHAMPOO_STR + ' ПРОКАЧКА';
  ProgramsConfig.Name[WAX_PREFLIGHT_PROGRAM_ID     - 1] := WAX_STR + ' ПРОКАЧКА';
  ProgramsConfig.Name[POLYMER_PREFLIGHT_PROGRAM_ID - 1] := 'ПОЛИМЕР ПРОКАЧКА';

  ProgramsConfig.Price[FOAM_PROGRAM_ID              - 1] := 15;
  ProgramsConfig.Price[SHAMPOO_PROGRAM_ID           - 1] := 15;
  ProgramsConfig.Price[RINSE_PROGRAM_ID             - 1] := 15;
  ProgramsConfig.Price[WAX_PROGRAM_ID               - 1] := 15;
  ProgramsConfig.Price[DRY_PROGRAM_ID               - 1] := 15;
  ProgramsConfig.Price[PAUSE_PROGRAM_ID             - 1] := 15;
  ProgramsConfig.Price[FOAM_PREFLIGHT_PROGRAM_ID    - 1] :=  0;
  ProgramsConfig.Price[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1] :=  0;
  ProgramsConfig.Price[WAX_PREFLIGHT_PROGRAM_ID     - 1] :=  0;
  ProgramsConfig.Price[POLYMER_PREFLIGHT_PROGRAM_ID - 1] :=  0;

  setlength(ProgramsConfig.Relays[FOAM_PROGRAM_ID - 1], 3);
  ProgramsConfig.Relays[FOAM_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.Relays[FOAM_PROGRAM_ID - 1][1] := FOAM_RELAY_ID;
  ProgramsConfig.Relays[FOAM_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[SHAMPOO_PROGRAM_ID - 1], 3);
  ProgramsConfig.Relays[SHAMPOO_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.Relays[SHAMPOO_PROGRAM_ID - 1][1] := SHAMPOO_RELAY_ID;
  ProgramsConfig.Relays[SHAMPOO_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[RINSE_PROGRAM_ID - 1], 2);
  ProgramsConfig.Relays[RINSE_PROGRAM_ID - 1][0] := COLD_WATER_RELAY_ID;
  ProgramsConfig.Relays[RINSE_PROGRAM_ID - 1][1] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[WAX_PROGRAM_ID - 1], 3);
  ProgramsConfig.Relays[WAX_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.Relays[WAX_PROGRAM_ID - 1][1] := WAX_RELAY_ID;
  ProgramsConfig.Relays[WAX_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[DRY_PROGRAM_ID - 1], 3);
  ProgramsConfig.Relays[DRY_PROGRAM_ID - 1][0] := OSM_WATER_RELAY_ID;
  ProgramsConfig.Relays[DRY_PROGRAM_ID - 1][1] := POLYMER_RELAY_ID;
  ProgramsConfig.Relays[DRY_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[PAUSE_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[PAUSE_PROGRAM_ID - 1][0] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.Relays[FOAM_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[FOAM_PREFLIGHT_PROGRAM_ID - 1][0] := FOAM_RELAY_ID;

  setlength(ProgramsConfig.Relays[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1][0] := SHAMPOO_RELAY_ID;

  setlength(ProgramsConfig.Relays[WAX_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[WAX_PREFLIGHT_PROGRAM_ID - 1][0] := WAX_RELAY_ID;

  setlength(ProgramsConfig.Relays[POLYMER_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[POLYMER_PREFLIGHT_PROGRAM_ID - 1][0] := POLYMER_RELAY_ID;

  serverEndpoint := 'http://localhost:8020/';

  for i:=1 to NUM_PROGRAMS+NUM_PREFLIGHT_PROGRAMS do
  begin
    CheckProgram(i);
  end;

end;

procedure TBaseForm.UpdatePrograms();
var
  i: integer;
begin
  for i:=1 to NUM_PROGRAMS+NUM_PREFLIGHT_PROGRAMS do
  begin
    SetProgram(i);
  end;
end;

procedure TBaseForm.SetRelayTimeOn(relayID, timeON : integer);
begin
  RelaysConfig.TimeON[relayID-1] := timeON;
  RelaysConfig.TimeOFF[relayID-1] := TOTAL_TIME - RelaysConfig.TimeON[relayID-1];
end;

function TBaseForm.GetRelayTimeOn(relayID : integer) : integer;
begin
  Result := RelaysConfig.TimeON[relayID-1];
end;

function TBaseForm.GetProgramName(id: integer): string;
begin
  Result := ProgramsConfig.Name[id-1];
end;

function TBaseForm.GetProgramPrice(id: integer): integer;
begin
  Result := ProgramsConfig.Price[id-1];
end;

procedure TBaseForm.SetProgramPrice(id, price : integer);
begin
  ProgramsConfig.Price[id-1] := price;
end;

procedure TBaseForm.CheckPrograms();
var
  i: integer;
begin
  for i:=1 to NUM_PROGRAMS+NUM_PREFLIGHT_PROGRAMS do
  begin
    CheckProgram(i);
  end;
end;

function TBaseForm.CheckProgram(id: integer) : boolean;
var
  RequestAnswer: string;
  programRequestJson : TJSONObject;
  programsJson: TJsonArray;
  i: integer;
  path: TJSONdata;
  programID: integer;
  relaysJson: TJsonArray;
  relayID: integer;
begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        programRequestJson := TJSONObject.Create;
        programRequestJson.Add('programID', id);

        RequestBody := TStringStream.Create(programRequestJson.AsJSON);
        RequestAnswer := Post(GetServerEndpoint() + 'programs');

        programsJson := GetJson(RequestAnswer) as TJsonArray;

        if programsJson.Count = 0 then
        begin
          SetProgram(id);
        end
        else
        begin
          programID := NO_ID;
          for i:=0 to programsJson.Count-1 do
          begin
            with programsJson.Items[i] do
            begin
              programID := FindPath('id').AsInteger;

              path := FindPath('name');
              if path <> nil then
              begin
                ProgramsConfig.Name[programID-1] := path.AsString;
              end;
              path := FindPath('price');
              if path <> nil then
              begin
                ProgramsConfig.Price[programID-1] := path.AsInteger;
              end;
              relaysJson := GetPath('relays') as TJsonArray;
              if relaysJson <> nil then
              begin
                with relaysJson.items[i] do
                begin
                  relayID := FindPath('id').AsInteger;

                  path := FindPath('timeon');
                  if path <> nil then
                  begin
                    RelaysConfig.TimeON[relayID-1] := path.AsInteger;
                    RelaysConfig.TimeOFF[relayID-1] := TOTAL_TIME - RelaysConfig.TimeON[relayID-1];
                  end;
                end;
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
      end;
    finally
      Free;
    end;
end;

procedure TBaseForm.SetProgram(id: integer);
var
  RequestAnswer: string;
  programJson : TJSONObject;
  relays:  TJsonArray;
  relay : TJSONObject;
  i: integer;

begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        relays := TJsonArray.Create;
        for i:=0 to Length(ProgramsConfig.Relays[id-1])-1 do
        begin
          relay := TJSONObject.Create;
          relay.Add('id',      RelaysConfig.ID[ProgramsConfig.Relays[id-1][i]-1]);
          relay.Add('timeon',  RelaysConfig.TimeON[ProgramsConfig.Relays[id-1][i]-1]);
          relay.Add('timeoff', RelaysConfig.TimeOFF[ProgramsConfig.Relays[id-1][i]-1]);
          relays.Add(relay);
        end;

        programJson := TJSONObject.Create;
        programJson.Add('id', id);
        programJson.Add('name', ProgramsConfig.Name[id-1]);
        programJson.Add('price', ProgramsConfig.Price[id-1]);
        programJson.Add('relays', relays);

        RequestBody := TStringStream.Create(programJson.AsJSON);
        RequestAnswer := Post(GetServerEndpoint() + 'set-program');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

      except
        writeln(RequestAnswer);
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          401, 403: // do nothing
            ;
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TBaseForm.SetPinCode(code: string);
begin
  BaseForm.pinCode := code;
end;

function TBaseForm.GetPinCode(): string;
begin
  Result := BaseForm.pinCode;
end;

procedure TBaseForm.SetServerEndpoint(endpoint: string);
begin
  BaseForm.serverEndpoint := endpoint;
end;

function TBaseForm.GetServerEndpoint(): string;
begin
  Result := BaseForm.serverEndpoint;
end;

function TBaseForm.GetHoverColor(): TColor;
begin
  Result := hoverColor;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin

end;

procedure TBaseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  LoginForm.Show;
end;

procedure TBaseForm.MainClick(Sender: TObject);
begin
  MainForm.Show;
end;

procedure TBaseForm.StationsClick(Sender: TObject);
begin
  StationsForm.Show;
end;

procedure TBaseForm.ProgramsClick(Sender: TObject);
begin
  ProgramsForm.Show;
end;

procedure TBaseForm.DosatronsClick(Sender: TObject);
begin
  DosatronsForm.Show;
end;

procedure TBaseForm.SettingsClick(Sender: TObject);
begin
  SettingsForm.Show;
end;

procedure TBaseForm.UsersClick(Sender: TObject);
begin
  UsersForm.Show;
end;

procedure TBaseForm.StatisticsClick(Sender: TObject);
begin
  StatisticsForm.Show;
end;

procedure TBaseForm.LogoutClick(Sender: TObject);
begin
  LoginForm.Show;
end;

procedure TBaseForm.LogoutMouseEnter(Sender: TObject);
begin
  Logout.Font.Color := hoverColor;
end;

procedure TBaseForm.LogoutMouseLeave(Sender: TObject);
begin
  Logout.Font.Color := clDefault;
end;

procedure TBaseForm.MainMouseEnter(Sender: TObject);
begin
  Main.Font.Color := hoverColor;
end;

procedure TBaseForm.MainMouseLeave(Sender: TObject);
begin
  Main.Font.Color := clDefault;
end;

procedure TBaseForm.ProgramsMouseEnter(Sender: TObject);
begin
  Programs.Font.Color := hoverColor;
end;

procedure TBaseForm.ProgramsMouseLeave(Sender: TObject);
begin
  Programs.Font.Color := clDefault;
end;

procedure TBaseForm.SettingsMouseEnter(Sender: TObject);
begin
  Settings.Font.Color := hoverColor;
end;

procedure TBaseForm.SettingsMouseLeave(Sender: TObject);
begin
  Settings.Font.Color := clDefault;
end;

procedure TBaseForm.StationsMouseEnter(Sender: TObject);
begin
  Stations.Font.Color := hoverColor;
end;

procedure TBaseForm.StationsMouseLeave(Sender: TObject);
begin
  Stations.Font.Color := clDefault;
end;

procedure TBaseForm.StatisticsMouseEnter(Sender: TObject);
begin
  Statistics.Font.Color := hoverColor;
end;

procedure TBaseForm.StatisticsMouseLeave(Sender: TObject);
begin
  Statistics.Font.Color := clDefault;
end;

procedure TBaseForm.UsersMouseEnter(Sender: TObject);
begin
  Users.Font.Color := hoverColor;
end;

procedure TBaseForm.UsersMouseLeave(Sender: TObject);
begin
  Users.Font.Color := clDefault;
end;

procedure TBaseForm.DosatronsMouseEnter(Sender: TObject);
begin
  Dosatrons.Font.Color := hoverColor;
end;

procedure TBaseForm.DosatronsMouseLeave(Sender: TObject);
begin
  Dosatrons.Font.Color := clDefault;
end;

end.

