unit Base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, myfphttpclient, Fpjson, jsonparser, Process;

type
  THashes = array of string;

  RelayConfig = packed record
    Count           : integer;
    ID              : array of integer;
    TimeON          : array of integer;
    TimeOFF         : array of integer;
    end;

  ProgramConfig = packed record
    Count            : integer;
    ID               : array of integer;
    Name             : array of string;
    Price            : array of integer;
    PreflightEnabled : array of boolean;
    MotorSpeedPercent: array of integer;
    PreflightMotorSpeedPercent: array of integer;
    Relays           : array of array of integer;
    PreflightRelays  : array of array of integer;
    end;

  StationsInfo = packed record
    Count: integer;
    id    : array of integer;
    name  : array of string;
    hash  : array of string;
    status: array of string;
    info  : array of string;
    currentBalance: array of integer;
    currentProgram: array of integer;
    preflightSec  : array of integer;
    relayBoard    : array of string;
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

    procedure CheckProgram(id: integer);
    procedure SetProgram(id: integer);
    procedure SetRelayTimeOn(relayID, timeON : integer);
    function GetRelayTimeOn(relayID : integer) : integer;
    procedure UpdatePrograms();
    procedure CheckPrograms();
    function GetProgramName(id: integer): string;
    function GetProgramPrice(id: integer): integer;
    procedure SetProgramPrice(id, price : integer);
    function GetNumPrograms(): integer;

    function UpdateStations() : boolean;
    function GetStationNameByID(id : integer) : string;
    function GetStationHashByID(id : integer) : string;
    function GetStationCurrentBalanceByID(id : integer) : integer;
    function GetStationCurrentProgramByID(id : integer) : integer;
    function GetStationStatusByID(id : integer) : string;
    function GetStationPreflightSec(id: integer): integer;
    procedure UpdateStationPreflightSec(id: integer);
    procedure SetStationPreflightSec(id, preflightSec: integer);
    function GetNumStations(): integer;
    function GetFreeHashes() : THashes;

    function GetFoamProgramID(): integer;
    function GetShampooProgramID(): integer;
    function GetRinseProgramID(): integer;
    function GetWaxProgramID(): integer;
    function GetDryProgramID(): integer;
    function GetPauseProgramID(): integer;
    function GetFoamPreflightProgramID(): integer;
    function GetShampooPreflightProgramID(): integer;
    function GetWaxPreflightProgramID(): integer;
    function GetPolymerPreflightProgramID(): integer;
    function GetOpenDoorProgramID(): integer;

    procedure SetStationCurrentProgramByID(programID, stationID: integer; isPreflight: boolean);

    function GetIpAddrList(): string;

  private
    pinCode: string;
    serverEndpoint: string;
    _isStandBy : boolean;
  public

  end;

var
  BaseForm      : TBaseForm;
  ProgramsConfig: ProgramConfig;
  RelaysConfig  : RelayConfig;
  ResponseStations: StationsInfo;
  freeHashVals : array of string;
  freeHashID : integer;

const
  hoverColor: TColor = clFuchsia;
  NO_ID   : integer = -1;
  ON_LINE : string = 'online';
  PLACEHOLDER : string = '';

  NUM_PROGRAMS          : integer = 6;
  NUM_PREFLIGHT_PROGRAMS: integer = 5;
  NUM_RELAYS            : integer = 13;

  FOAM_RELAY_ID      : integer = 6;
  SHAMPOO_RELAY_ID   : integer = 7;
  WAX_RELAY_ID       : integer = 8;
  POLYMER_RELAY_ID   : integer = 9;
  COLD_WATER_RELAY_ID: integer = 1;
  HOT_WATER_RELAY_ID : integer = 2;
  OSM_WATER_RELAY_ID : integer = 3;
  LIGHT_RELAY_ID     : integer = 4;
  OPEN_DOOR_RELAY_ID : integer = 5;

  PREFLIGHT_FOAM_RELAY_ID: integer    = 10;
  PREFLIGHT_SHAMPOO_RELAY_ID: integer = 11;
  PREFLIGHT_WAX_RELAY_ID: integer     = 12;
  PREFLIGHT_POLYMER_RELAY_ID: integer = 13;

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
  OPEN_DOOR_PROGRAM_ID        : integer = 11;

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

function TBaseForm.GetNumPrograms(): integer;
begin
  Result := NUM_PROGRAMS+NUM_PREFLIGHT_PROGRAMS;
end;

function TBaseForm.GetFoamProgramID(): integer;
begin
  Result := FOAM_PROGRAM_ID;
end;

function TBaseForm.GetShampooProgramID(): integer;
begin
  Result := SHAMPOO_PROGRAM_ID;
end;

function TBaseForm.GetRinseProgramID(): integer;
begin
  Result := RINSE_PROGRAM_ID;
end;

function TBaseForm.GetWaxProgramID(): integer;
begin
  Result := WAX_PROGRAM_ID;
end;

function TBaseForm.GetDryProgramID(): integer;
begin
  Result := DRY_PROGRAM_ID;
end;

function TBaseForm.GetPauseProgramID(): integer;
begin
  Result := PAUSE_PROGRAM_ID;
end;

function TBaseForm.GetFoamPreflightProgramID(): integer;
begin
  Result := FOAM_PREFLIGHT_PROGRAM_ID;
end;

function TBaseForm.GetShampooPreflightProgramID(): integer;
begin
  Result := SHAMPOO_PREFLIGHT_PROGRAM_ID;
end;

function TBaseForm.GetWaxPreflightProgramID(): integer;
begin
  Result := WAX_PREFLIGHT_PROGRAM_ID;
end;

function TBaseForm.GetPolymerPreflightProgramID(): integer;
begin
  Result := POLYMER_PREFLIGHT_PROGRAM_ID;
end;

function TBaseForm.GetOpenDoorProgramID(): integer;
begin
  Result := OPEN_DOOR_PROGRAM_ID;
end;

function TBaseForm.GetFreeHashes() : THashes;
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

procedure TBaseForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  _isStandBy  := false;

  RelaysConfig.Count := NUM_RELAYS;
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
  RelaysConfig.ID[OPEN_DOOR_RELAY_ID  - 1] := OPEN_DOOR_RELAY_ID;
  RelaysConfig.ID[PREFLIGHT_FOAM_RELAY_ID       - 1] := FOAM_RELAY_ID;
  RelaysConfig.ID[PREFLIGHT_SHAMPOO_RELAY_ID    - 1] := SHAMPOO_RELAY_ID;
  RelaysConfig.ID[PREFLIGHT_WAX_RELAY_ID        - 1] := WAX_RELAY_ID;
  RelaysConfig.ID[PREFLIGHT_POLYMER_RELAY_ID    - 1] := POLYMER_RELAY_ID;

  RelaysConfig.TimeON[FOAM_RELAY_ID       - 1] := 500;
  RelaysConfig.TimeON[SHAMPOO_RELAY_ID    - 1] := 500;
  RelaysConfig.TimeON[WAX_RELAY_ID        - 1] := 500;
  RelaysConfig.TimeON[POLYMER_RELAY_ID    - 1] := 500;
  RelaysConfig.TimeON[COLD_WATER_RELAY_ID - 1] := 1000;
  RelaysConfig.TimeON[HOT_WATER_RELAY_ID  - 1] := 1000;
  RelaysConfig.TimeON[OSM_WATER_RELAY_ID  - 1] := 1000;
  RelaysConfig.TimeON[LIGHT_RELAY_ID      - 1] := 1000;
  RelaysConfig.TimeON[OPEN_DOOR_RELAY_ID  - 1] := 1000;
  RelaysConfig.TimeON[PREFLIGHT_FOAM_RELAY_ID       - 1] := 1000;
  RelaysConfig.TimeON[PREFLIGHT_SHAMPOO_RELAY_ID    - 1] := 1000;
  RelaysConfig.TimeON[PREFLIGHT_WAX_RELAY_ID        - 1] := 1000;
  RelaysConfig.TimeON[PREFLIGHT_POLYMER_RELAY_ID    - 1] := 1000;

  RelaysConfig.TimeOFF[FOAM_RELAY_ID       - 1] := TOTAL_TIME - RelaysConfig.TimeON[FOAM_RELAY_ID       - 1];
  RelaysConfig.TimeOFF[SHAMPOO_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[SHAMPOO_RELAY_ID    - 1];
  RelaysConfig.TimeOFF[WAX_RELAY_ID        - 1] := TOTAL_TIME - RelaysConfig.TimeON[WAX_RELAY_ID        - 1];
  RelaysConfig.TimeOFF[POLYMER_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[POLYMER_RELAY_ID    - 1];
  RelaysConfig.TimeOFF[COLD_WATER_RELAY_ID - 1] := TOTAL_TIME - RelaysConfig.TimeON[COLD_WATER_RELAY_ID - 1];
  RelaysConfig.TimeOFF[HOT_WATER_RELAY_ID  - 1] := TOTAL_TIME - RelaysConfig.TimeON[HOT_WATER_RELAY_ID  - 1];
  RelaysConfig.TimeOFF[OSM_WATER_RELAY_ID  - 1] := TOTAL_TIME - RelaysConfig.TimeON[OSM_WATER_RELAY_ID  - 1];
  RelaysConfig.TimeOFF[LIGHT_RELAY_ID      - 1] := TOTAL_TIME - RelaysConfig.TimeON[LIGHT_RELAY_ID      - 1];
  RelaysConfig.TimeOFF[OPEN_DOOR_RELAY_ID  - 1] := TOTAL_TIME - RelaysConfig.TimeON[OPEN_DOOR_RELAY_ID  - 1];
  RelaysConfig.TimeOFF[PREFLIGHT_FOAM_RELAY_ID       - 1] := TOTAL_TIME - RelaysConfig.TimeON[PREFLIGHT_FOAM_RELAY_ID       - 1];
  RelaysConfig.TimeOFF[PREFLIGHT_SHAMPOO_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[PREFLIGHT_SHAMPOO_RELAY_ID    - 1];
  RelaysConfig.TimeOFF[PREFLIGHT_WAX_RELAY_ID        - 1] := TOTAL_TIME - RelaysConfig.TimeON[PREFLIGHT_WAX_RELAY_ID        - 1];
  RelaysConfig.TimeOFF[PREFLIGHT_POLYMER_RELAY_ID    - 1] := TOTAL_TIME - RelaysConfig.TimeON[PREFLIGHT_POLYMER_RELAY_ID    - 1];

  ProgramsConfig.Count := NUM_PROGRAMS + NUM_PREFLIGHT_PROGRAMS;
  setlength(ProgramsConfig.ID,               ProgramsConfig.Count);
  setlength(ProgramsConfig.Name,             ProgramsConfig.Count);
  setlength(ProgramsConfig.Price,            ProgramsConfig.Count);
  setlength(ProgramsConfig.PreflightEnabled, ProgramsConfig.Count);
  setlength(ProgramsConfig.Relays,           ProgramsConfig.Count);
  setlength(ProgramsConfig.PreflightRelays,  ProgramsConfig.Count);
  setlength(ProgramsConfig.MotorSpeedPercent,           ProgramsConfig.Count);
  setlength(ProgramsConfig.PreflightMotorSpeedPercent,  ProgramsConfig.Count);

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
  ProgramsConfig.ID[OPEN_DOOR_PROGRAM_ID - 1]         := OPEN_DOOR_PROGRAM_ID;

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
  ProgramsConfig.Name[OPEN_DOOR_PROGRAM_ID - 1]         := 'OPEN';

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
  ProgramsConfig.Price[OPEN_DOOR_PROGRAM_ID - 1]         :=  0;

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

  setlength(ProgramsConfig.Relays[OPEN_DOOR_PROGRAM_ID - 1], 1);
  ProgramsConfig.Relays[OPEN_DOOR_PROGRAM_ID - 1][0] := OPEN_DOOR_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[FOAM_PROGRAM_ID - 1], 3);
  ProgramsConfig.PreflightRelays[FOAM_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.PreflightRelays[FOAM_PROGRAM_ID - 1][1] := FOAM_RELAY_ID;
  ProgramsConfig.PreflightRelays[FOAM_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[SHAMPOO_PROGRAM_ID - 1], 3);
  ProgramsConfig.PreflightRelays[SHAMPOO_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.PreflightRelays[SHAMPOO_PROGRAM_ID - 1][1] := SHAMPOO_RELAY_ID;
  ProgramsConfig.PreflightRelays[SHAMPOO_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[RINSE_PROGRAM_ID - 1], 2);
  ProgramsConfig.PreflightRelays[RINSE_PROGRAM_ID - 1][0] := COLD_WATER_RELAY_ID;
  ProgramsConfig.PreflightRelays[RINSE_PROGRAM_ID - 1][1] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[WAX_PROGRAM_ID - 1], 3);
  ProgramsConfig.PreflightRelays[WAX_PROGRAM_ID - 1][0] := HOT_WATER_RELAY_ID;
  ProgramsConfig.PreflightRelays[WAX_PROGRAM_ID - 1][1] := WAX_RELAY_ID;
  ProgramsConfig.PreflightRelays[WAX_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[DRY_PROGRAM_ID - 1], 3);
  ProgramsConfig.PreflightRelays[DRY_PROGRAM_ID - 1][0] := OSM_WATER_RELAY_ID;
  ProgramsConfig.PreflightRelays[DRY_PROGRAM_ID - 1][1] := POLYMER_RELAY_ID;
  ProgramsConfig.PreflightRelays[DRY_PROGRAM_ID - 1][2] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[PAUSE_PROGRAM_ID - 1], 1);
  ProgramsConfig.PreflightRelays[PAUSE_PROGRAM_ID - 1][0] := LIGHT_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[FOAM_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.PreflightRelays[FOAM_PREFLIGHT_PROGRAM_ID - 1][0] := PREFLIGHT_FOAM_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.PreflightRelays[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1][0] := PREFLIGHT_SHAMPOO_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[WAX_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.PreflightRelays[WAX_PREFLIGHT_PROGRAM_ID - 1][0] := PREFLIGHT_WAX_RELAY_ID;

  setlength(ProgramsConfig.PreflightRelays[POLYMER_PREFLIGHT_PROGRAM_ID - 1], 1);
  ProgramsConfig.PreflightRelays[POLYMER_PREFLIGHT_PROGRAM_ID - 1][0] := PREFLIGHT_POLYMER_RELAY_ID;

  ProgramsConfig.MotorSpeedPercent[FOAM_PROGRAM_ID              - 1] :=  15;
  ProgramsConfig.MotorSpeedPercent[SHAMPOO_PROGRAM_ID           - 1] :=  50;
  ProgramsConfig.MotorSpeedPercent[RINSE_PROGRAM_ID             - 1] := 100;
  ProgramsConfig.MotorSpeedPercent[WAX_PROGRAM_ID               - 1] :=  50;
  ProgramsConfig.MotorSpeedPercent[DRY_PROGRAM_ID               - 1] := 100;
  ProgramsConfig.MotorSpeedPercent[PAUSE_PROGRAM_ID             - 1] :=   0;
  ProgramsConfig.MotorSpeedPercent[FOAM_PREFLIGHT_PROGRAM_ID    - 1] :=   0;
  ProgramsConfig.MotorSpeedPercent[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1] :=   0;
  ProgramsConfig.MotorSpeedPercent[WAX_PREFLIGHT_PROGRAM_ID     - 1] :=   0;
  ProgramsConfig.MotorSpeedPercent[POLYMER_PREFLIGHT_PROGRAM_ID - 1] :=   0;
  ProgramsConfig.MotorSpeedPercent[OPEN_DOOR_PROGRAM_ID - 1]         :=   0;

  ProgramsConfig.PreflightMotorSpeedPercent[FOAM_PROGRAM_ID              - 1] := 100;
  ProgramsConfig.PreflightMotorSpeedPercent[SHAMPOO_PROGRAM_ID           - 1] := 100;
  ProgramsConfig.PreflightMotorSpeedPercent[RINSE_PROGRAM_ID             - 1] := 100;
  ProgramsConfig.PreflightMotorSpeedPercent[WAX_PROGRAM_ID               - 1] := 100;
  ProgramsConfig.PreflightMotorSpeedPercent[DRY_PROGRAM_ID               - 1] := 100;
  ProgramsConfig.PreflightMotorSpeedPercent[PAUSE_PROGRAM_ID             - 1] :=   0;
  ProgramsConfig.PreflightMotorSpeedPercent[FOAM_PREFLIGHT_PROGRAM_ID    - 1] :=   0;
  ProgramsConfig.PreflightMotorSpeedPercent[SHAMPOO_PREFLIGHT_PROGRAM_ID - 1] :=   0;
  ProgramsConfig.PreflightMotorSpeedPercent[WAX_PREFLIGHT_PROGRAM_ID     - 1] :=   0;
  ProgramsConfig.PreflightMotorSpeedPercent[POLYMER_PREFLIGHT_PROGRAM_ID - 1] :=   0;
  ProgramsConfig.PreflightMotorSpeedPercent[OPEN_DOOR_PROGRAM_ID - 1]         :=   0;

  serverEndpoint := GetIpAddrList();

  for i:=1 to NUM_PROGRAMS+NUM_PREFLIGHT_PROGRAMS do
  begin
    CheckProgram(i);
  end;

end;

function TBaseForm.GetIpAddrList(): string;
var
  AProcess: TProcess;
  s, addr: string;
  sl: TStringList;
  i, n, j: integer;
  successful: boolean;
begin
  successful := false;
  Result:='';
  sl:=TStringList.Create();
  {$IFDEF WINDOWS}
  AProcess:=TProcess.Create(nil);
  AProcess.Executable := 'ipconfig.exe';
  AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole];
  try
    AProcess.Execute();
    Sleep(500); // poWaitOnExit don't work as expected
    sl.LoadFromStream(AProcess.Output);
  finally
    AProcess.Free();
  end;

  with TFPHttpClient.Create(nil) do
    try
      //AddHeader('Content-Type', 'application/json');
      for i:=0 to sl.Count-1 do
        begin
          if (Pos('IPv4', sl[i])=0) and (Pos('IP-', sl[i])=0) and (Pos('IP Address', sl[i])=0) then Continue;
          s:=sl[i];
          s:=Trim(Copy(s, Pos(':', s)+1, 999));
          if Pos(':', s)>0 then Continue; // IPv6
          n := LastDelimiter('.', s);
          s := Copy(s, 1, n);
          for j:=1 to 255 do
          begin
            try
              addr := 'http://' + s + IntToStr(j) + ':8020/';
              writeln('Trying ' + addr);
              Get(addr + 'ping');
              successful := True;
            except

            end;
            if successful then
            begin
              writeln('FOUND SERVER ON ' + addr);
              Result:= addr;
              Exit;
            end;
          end;
        end;

    finally
      Free
    end;
  {$ENDIF}
  {$IFDEF UNIX}
  AProcess:=TProcess.Create(nil);
  AProcess.Executable := '/sbin/ifconfig';
  AProcess.Options := AProcess.Options + [poUsePipes, poWaitOnExit];
  try
    AProcess.Execute();
    sl.LoadFromStream(AProcess.Output);
  finally
    AProcess.Free();
  end;

  with TFPHttpClient.Create(nil) do
    try
      //AddHeader('Content-Type', 'application/json');
      for i:=0 to sl.Count-1 do
        begin
          n:=Pos('inet ', sl[i]);
          if n=0 then Continue;
          s:=sl[i];
          s:=Copy(s, n+Length('inet '), 999);
          s := Trim(Copy(s, 1, Pos(' ', s)));
          n := LastDelimiter('.', s);
          s := Copy(s, 1, n);
          for j:=1 to 255 do
          begin
            try
              addr := 'http://' + s + IntToStr(j) + ':8020/';
              writeln('Trying ' + addr);
              Get(addr + 'ping');
              successful := True;
            except

            end;
            if successful then
            begin
              writeln('FOUND SERVER ON ' + addr);
              Result:= addr;
              Exit;
            end;
          end;
        end;

    finally
      Free
    end;
  {$ENDIF}
  sl.Free();
end;

procedure TBaseForm.SetStationCurrentProgramByID(programID, stationID: integer; isPreflight: boolean);
var
  settingJson: TJSONObject;
begin
  if GetStationHashByID(stationID) = PLACEHOLDER then
  begin
    Exit;
  end;
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', BaseForm.GetPinCode());

        settingJson := TJSONObject.Create;

        settingJson.Add('buttonID', programID);
        settingJson.Add('hash', BaseForm.GetStationHashByID(stationID));

        RequestBody := TStringStream.Create(settingJson.AsJSON);
        Post(BaseForm.GetServerEndpoint() + '/press-button');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          401: ShowMessage('Not authorized');
          403, 404: ShowMessage('Forbidden');
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

function TBaseForm.UpdateStations() : boolean;
var
  RequestAnswer: string;
  stationsJson: TJsonArray;
  i: integer;
  path: TJSONdata;
  hasID : boolean;
  id : integer;

begin
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
        setlength(ResponseStations.preflightSec, stationsJson.Count);
        setlength(ResponseStations.relayBoard, stationsJson.Count);

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
              ResponseStations.hash[id] := PLACEHOLDER;
              ResponseStations.name[id] := PLACEHOLDER;
              ResponseStations.status[id] := PLACEHOLDER;
              ResponseStations.info[id] := PLACEHOLDER;
              ResponseStations.currentBalance[id] := NO_ID;
              ResponseStations.currentProgram[id] := NO_ID;
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
            end;

            path := FindPath('name');
            if path <> nil then
            begin
              if ResponseStations.hash[id] <> PLACEHOLDER then
              begin
                ResponseStations.name[id] := path.AsString;
              end;
            end;

            path := FindPath('status');
            if path <> nil then
            begin
              if ResponseStations.hash[id] <> PLACEHOLDER then
              begin
                ResponseStations.status[id] := path.AsString;
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
      Result := true;
      _isStandBy := false;
      except
        Result := false;
        if not _isStandBy then
        begin
          _isStandBy := true;
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
        setlength(ResponseStations.id, 0);
        setlength(ResponseStations.name, 0);
        setlength(ResponseStations.hash, 0);
        setlength(ResponseStations.status, 0);
        setlength(ResponseStations.info, 0);
        setlength(ResponseStations.currentBalance, 0);
        setlength(ResponseStations.currentProgram, 0);

        ResponseStations.Count := 0;
      end;
    finally
      Free;
    end;
end;

procedure TBaseForm.UpdateStationPreflightSec(id: integer);
var
  RequestAnswer: string;
  requestJson: TJSONObject;
  stationJson: TJsonData;
  path: TJSONdata;
begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        requestJson := TJSONObject.Create;
        requestJson.Add('id', id);
        RequestBody := TStringStream.Create(requestJson.AsJSON);
        RequestAnswer := Post(GetServerEndpoint() + 'station');

        stationJson := GetJson(RequestAnswer);
        path := stationJson.FindPath('preflightSec');
        if path <> nil then
        begin
          ResponseStations.preflightSec[id-1] := path.AsInteger;
        end
        else
        begin
          ResponseStations.preflightSec[id-1] := 0;
        end;

        path := stationJson.FindPath('relayBoard');
        if path <> nil then
        begin
          ResponseStations.relayBoard[id-1] := path.AsString;
        end

      except
          case ResponseStatusCode of
            0: ShowMessage('Can`t connect to server');
            401, 403, 404: // do nothing
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

procedure TBaseForm.SetStationPreflightSec(id, preflightSec: integer);
var
  settingJson: TJSONObject;
begin
  if GetStationHashByID(id) = PLACEHOLDER then
  begin
    Exit;
  end;
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        settingJson := TJSONObject.Create;

        settingJson.Add('id', id);
        settingJson.Add('name', ResponseStations.name[id-1]);
        settingJson.Add('hash', ResponseStations.hash[id-1]);
        settingJson.Add('preflightSec', preflightSec);
        settingJson.Add('relayBoard', ResponseStations.relayBoard[id-1]);

        RequestBody := TStringStream.Create(settingJson.AsJSON);
        Post(GetServerEndpoint() + '/set-station');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        ResponseStations.preflightSec[id-1] := preflightSec;

      except
        case ResponseStatusCode of
          0: begin ModalResult := 0; ShowMessage('Can`t connect to server'); end;
          401: begin ModalResult := 0; ShowMessage('Not authorized'); end;
          403, 404: begin ModalResult := 0; ShowMessage('Forbidden'); end;
          500: begin ModalResult := 0; ShowMessage('Server Error: 500'); end;
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
      end;

    finally
      Free;
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

function TBaseForm.GetStationNameByID(id : integer) : string;
begin
  if id > 0 then
  begin
    Result := ResponseStations.name[id-1];
  end;
end;

function TBaseForm.GetStationHashByID(id : integer) : string;
begin
  if id > 0 then
  begin
    Result := ResponseStations.hash[id-1];
  end;
end;

function TBaseForm.GetStationCurrentBalanceByID(id : integer) : integer;
begin
  if id > 0 then
  begin
    Result := ResponseStations.currentBalance[id-1];
  end;
end;

function TBaseForm.GetStationStatusByID(id : integer) : string;
begin
  if id > 0 then
  begin
    Result := ResponseStations.status[id-1];
  end;
end;

function TBaseForm.GetStationCurrentProgramByID(id : integer) : integer;
begin
  if id > 0 then
  begin
    Result := ResponseStations.currentProgram[id-1];
  end;
end;

function TBaseForm.GetStationPreflightSec(id: integer): integer;
begin
  Result := ResponseStations.preflightSec[id-1];
end;

function TBaseForm.GetNumStations(): integer;
begin
  Result := ResponseStations.Count;
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

procedure TBaseForm.CheckProgram(id: integer);
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
              path := FindPath('motorSpeedPercent');
              if path <> nil then
              begin
                ProgramsConfig.MotorSpeedPercent[programID-1] := path.AsInteger;
              end;
              path := FindPath('preflightMotorSpeedPercent');
              if path <> nil then
              begin
                ProgramsConfig.PreflightMotorSpeedPercent[programID-1] := path.AsInteger;
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
  preflightRelays:  TJsonArray;
  relay : TJSONObject;
  i: integer;

begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        programJson := TJSONObject.Create;
        programJson.Add('id', id);
        programJson.Add('name', ProgramsConfig.Name[id-1]);
        programJson.Add('price', ProgramsConfig.Price[id-1]);
        programJson.Add('motorSpeedPercent', ProgramsConfig.MotorSpeedPercent[id-1]);
        programJson.Add('preflightMotorSpeedPercent', ProgramsConfig.PreflightMotorSpeedPercent[id-1]);

        relays := TJsonArray.Create;
        for i:=0 to Length(ProgramsConfig.Relays[id-1])-1 do
        begin
          relay := TJSONObject.Create;
          relay.Add('id',      RelaysConfig.ID[ProgramsConfig.Relays[id-1][i]-1]);
          relay.Add('timeon',  RelaysConfig.TimeON[ProgramsConfig.Relays[id-1][i]-1]);
          relay.Add('timeoff', RelaysConfig.TimeOFF[ProgramsConfig.Relays[id-1][i]-1]);
          relays.Add(relay);
        end;
        programJson.Add('relays', relays);

        preflightRelays := TJsonArray.Create;
        for i:=0 to Length(ProgramsConfig.PreflightRelays[id-1])-1 do
        begin
          relay := TJSONObject.Create;
          relay.Add('id',      RelaysConfig.ID[ProgramsConfig.PreflightRelays[id-1][i]-1]);
          relay.Add('timeon',  RelaysConfig.TimeON[ProgramsConfig.PreflightRelays[id-1][i]-1]);
          relay.Add('timeoff', RelaysConfig.TimeOFF[ProgramsConfig.PreflightRelays[id-1][i]-1]);
          preflightRelays.Add(relay);
        end;
        programJson.Add('preflightRelays', preflightRelays);

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

