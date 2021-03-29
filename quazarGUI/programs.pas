unit programs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser;

type

  ProgramInfo = packed record
    Count  : integer;
    ProgramID: array of integer;

  end;

  { TProgramsForm }

  TProgramsForm = class(TBaseForm)
    Cancel: TButton;
    Save: TButton;
    ProgramsGrid: TStringGrid;
    procedure CancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure ProgramsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SaveClick(Sender: TObject);
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
  ProgramsInfo: ProgramInfo;
  stationHashes : array of string;
  freeHashID : integer;

const
  PROGRAM_NAME_COL    : integer = 0;
  PROGRAM_PRICE_STR   : integer = 1;
  PROGRAM_PRICE_DEC   : integer = 2;
  PROGRAM_PRICE_VAL   : integer = 3;
  PROGRAM_PRICE_INC   : integer = 4;
  PROGRAM_PRICE_CURR  : integer = 5;
  PROGRAM_ENABLED_STR : integer = 6;
  PROGRAM_ENABLED_CHK : integer = 7;

  FOAM        : integer = 0;
  SHAMPOO     : integer = 1;
  RINSE       : integer = 2;
  WAX         : integer = 3;
  DRY         : integer = 4;
  PAUSE       : integer = 5;

  PRICE_STR   : string = 'Цена';
  CURRENCY_STR: string = 'руб.';
  ENABLED_STR : string = 'Активный';

  PADDING     : string = '  ';

implementation

{$R *.lfm}

procedure TProgramsForm.FormCreate(Sender: TObject);
begin
  ProgramsInfo.Count := NUM_PROGRAMS;

  setlength(ProgramsInfo.ProgramID, ProgramsInfo.Count);

  ProgramsInfo.ProgramID[FOAM] := FOAM_PROGRAM_ID;
  ProgramsInfo.ProgramID[SHAMPOO] := SHAMPOO_PROGRAM_ID;
  ProgramsInfo.ProgramID[RINSE] := RINSE_PROGRAM_ID;
  ProgramsInfo.ProgramID[WAX] := WAX_PROGRAM_ID;
  ProgramsInfo.ProgramID[DRY] := DRY_PROGRAM_ID;
  ProgramsInfo.ProgramID[PAUSE] := PAUSE_PROGRAM_ID;
end;

procedure TProgramsForm.CancelClick(Sender: TObject);
begin
  FormShow(Sender);
end;

procedure TProgramsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  inherited;
end;

procedure TProgramsForm.FormShow(Sender: TObject);
var
  i: integer;

begin
  Programs.Font.Color := ProgramsForm.GetHoverColor();
  NotAuthorized.Visible:=False;
  ProgramsGrid.Visible:=True;
  ProgramsGrid.Enabled:=True;
  Save.Enabled:=True;
  Cancel.Enabled:=True;

  CheckPrograms();
  for i:=1 to NUM_PROGRAMS do
  begin
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_NAME_COL, i-1] := PADDING + GetProgramName(i);
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_STR, i-1] := PRICE_STR;
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_INC, i-1] := '+';
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_VAL, i-1] := IntToStr(GetProgramPrice(i));
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_DEC, i-1] := '-';
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_CURR, i-1] := CURRENCY_STR;
  end;
end;

procedure TProgramsForm.MainClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.ProgramsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  if (aCol = PROGRAM_PRICE_DEC) and (GetProgramPrice(ProgramsInfo.ProgramID[aRow]) > 1) then
  begin
    SetProgramPrice(ProgramsInfo.ProgramID[aRow], GetProgramPrice(ProgramsInfo.ProgramID[aRow]) - 1);
    ProgramsGrid.Cells[PROGRAM_PRICE_VAL, aRow] := IntToStr(GetProgramPrice(ProgramsInfo.ProgramID[aRow]));
  end
  else if (aCol = PROGRAM_PRICE_INC) then
  begin
    SetProgramPrice(ProgramsInfo.ProgramID[aRow], GetProgramPrice(ProgramsInfo.ProgramID[aRow]) + 1);
    ProgramsGrid.Cells[PROGRAM_PRICE_VAL, aRow] := IntToStr(GetProgramPrice(ProgramsInfo.ProgramID[aRow]));
  end;
end;

procedure TProgramsForm.SaveClick(Sender: TObject);
var
  RequestAnswer: string;
  stationsJson: TJsonArray;
  i: integer;
  path: TJSONdata;

begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());
        RequestAnswer := Get(GetServerEndpoint() + 'status');

        stationsJson := GetJson(RequestAnswer).GetPath('stations') as TJsonArray;
        setlength(stationHashes, stationsJson.Count);
        freeHashID := 0;

        for i := 0 to stationsJson.Count - 1 do
        begin
          with stationsJson.items[i] do
          begin
            path := FindPath('hash');
            if path <> nil then
            begin
                stationHashes[freeHashID] := path.AsString;
                freeHashID := freeHashID + 1;
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
  UpdatePrograms();
  FormShow(Sender);
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

