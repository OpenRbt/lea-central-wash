unit programs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser;

type

  ProgramInfo = packed record
    Count  : integer;
    Name   : array of string;
    Key    : array of string;
    Price  : array of integer;
    Enabled: array of boolean;

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

    procedure UpdateStations();

  private

  public

  end;

var
  ProgramsForm: TProgramsForm;
  ProgramsInfo: ProgramInfo;
  stationHashes : array of string;
  freeHashID : integer;

const
  DEFAULT_PRICE : integer = 27;
  DEFAULT_ENABLED : boolean = true;

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
  NUM_PROGRAMS: integer = 6;

  FOAM_KEY    : string = 'price1';
  SHAMPOO_KEY : string = 'price2';
  RINSE_KEY   : string = 'price3';
  WAX_KEY     : string = 'price4';
  DRY_KEY     : string = 'price5';
  PAUSE_KEY   : string = 'price6';

  FOAM_STR    : string = 'ПЕНА';
  SHAMPOO_STR : string = 'ВОДА + ШАМПУНЬ';
  RINSE_STR   : string = 'ОПОЛАСКИВАНИЕ';
  WAX_STR     : string = 'ВОСК';
  DRY_STR     : string = 'СУШКА И БЛЕСК';
  PAUSE_STR   : string = 'ПАУЗА';
  PRICE_STR   : string = 'Цена';
  CURRENCY_STR: string = 'руб.';
  ENABLED_STR : string = 'Активный';

  PADDING     : string = '  ';

implementation

{$R *.lfm}

procedure TProgramsForm.FormCreate(Sender: TObject);
begin
  ProgramsInfo.Count := NUM_PROGRAMS;
  setlength(ProgramsInfo.Name,    ProgramsInfo.Count);
  setlength(ProgramsInfo.Key,     ProgramsInfo.Count);
  setlength(ProgramsInfo.Price,   ProgramsInfo.Count);
  setlength(ProgramsInfo.Enabled, ProgramsInfo.Count);

  ProgramsInfo.Name[FOAM]    := FOAM_STR;
  ProgramsInfo.Name[SHAMPOO] := SHAMPOO_STR;
  ProgramsInfo.Name[RINSE]   := RINSE_STR;
  ProgramsInfo.Name[WAX]     := WAX_STR;
  ProgramsInfo.Name[DRY]     := DRY_STR;
  ProgramsInfo.Name[PAUSE]   := PAUSE_STR;

  ProgramsInfo.Key[FOAM]     := FOAM_KEY;
  ProgramsInfo.Key[SHAMPOO]  := SHAMPOO_KEY;
  ProgramsInfo.Key[RINSE]    := RINSE_KEY;
  ProgramsInfo.Key[WAX]      := WAX_KEY;
  ProgramsInfo.Key[DRY]      := DRY_KEY;
  ProgramsInfo.Key[PAUSE]    := PAUSE_KEY;

  ProgramsInfo.Price[FOAM]    := DEFAULT_PRICE;
  ProgramsInfo.Price[SHAMPOO] := DEFAULT_PRICE;
  ProgramsInfo.Price[RINSE]   := DEFAULT_PRICE;
  ProgramsInfo.Price[WAX]     := DEFAULT_PRICE;
  ProgramsInfo.Price[DRY]     := DEFAULT_PRICE;
  ProgramsInfo.Price[PAUSE]   := DEFAULT_PRICE;

  ProgramsInfo.Enabled[FOAM]    := DEFAULT_ENABLED;
  ProgramsInfo.Enabled[SHAMPOO] := DEFAULT_ENABLED;
  ProgramsInfo.Enabled[RINSE]   := DEFAULT_ENABLED;
  ProgramsInfo.Enabled[WAX]     := DEFAULT_ENABLED;
  ProgramsInfo.Enabled[DRY]     := DEFAULT_ENABLED;
  ProgramsInfo.Enabled[PAUSE]   := DEFAULT_ENABLED;
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
  RequestAnswer: string;
  programsJson: TJsonArray;
  i: integer;
  path: TJSONdata;
  key: string;
  val: integer;

begin
  Programs.Font.Color := ProgramsForm.GetHoverColor();
  NotAuthorized.Visible:=False;
  ProgramsGrid.Visible:=True;
  ProgramsGrid.Enabled:=True;
  Save.Enabled:=True;
  Cancel.Enabled:=True;

  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());
        RequestAnswer := Post(GetServerEndpoint() + 'stations-variables');

        programsJson := GetJson(RequestAnswer) as TJsonArray;

        if programsJson.Count > 0 then
        begin
          with programsJson.items[0] do
            begin
              path := FindPath('keyPairs');
              if path <> nil then
              begin
                for i:=0 to path.Count - 1 do
                begin
                  with path.Items[i] do
                  begin
                    key   := FindPath('key').AsString;
                    val := FindPath('value').AsInteger;
                      if key = FOAM_KEY then
                      begin
                        ProgramsInfo.Price[FOAM] := val;
                      end
                      else if key = SHAMPOO_KEY then
                      begin
                        ProgramsInfo.Price[SHAMPOO] := val;
                      end
                      else if key = RINSE_KEY then
                      begin
                        ProgramsInfo.Price[RINSE] := val;
                      end
                      else if key = WAX_KEY then
                      begin
                        ProgramsInfo.Price[WAX] := val;
                      end
                      else if key = DRY_KEY then
                      begin
                        ProgramsInfo.Price[DRY] := val;
                      end
                      else if key = PAUSE_KEY then
                      begin
                        ProgramsInfo.Price[PAUSE] := val;
                      end;
                  end;
                end;
              end;
            end;

          for i:=0 to NUM_PROGRAMS - 1 do
          begin
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_NAME_COL, i] := PADDING + ProgramsInfo.Name[i];
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_STR, i] := PRICE_STR;
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_INC, i] := '+';
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_VAL, i] := IntToStr(ProgramsInfo.Price[i]);
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_DEC, i] := '-';
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_CURR, i] := CURRENCY_STR;
            ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_STR, i] := ENABLED_STR;
            if ProgramsInfo.Enabled[i] then
            begin
              ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_CHK, i] := '1';
            end
            else
            begin
              ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_CHK, i] := '0';
            end;
          end;
        end
        else
        begin
          ProgramsGrid.Enabled:=false;
          Save.Enabled:=false;
          Cancel.Enabled:=false;
        end;

      except
        //NotAuthorized.Visible:=True;
        //ProgramsGrid.Visible:=False;
        //Save.Visible:=false;
        //Cancel.Visible:=false;
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

procedure TProgramsForm.MainClick(Sender: TObject);
begin
  Inherited;
  ProgramsForm.Hide;
end;

procedure TProgramsForm.ProgramsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  if (aCol = PROGRAM_ENABLED_CHK) or (aCol = PROGRAM_ENABLED_STR) then
  begin
    if ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_CHK, aRow] = '0' then
    begin
      ProgramsInfo.Enabled[aRow] := true;
      ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_CHK, aRow] := '1';
    end
    else
    begin
      ProgramsInfo.Enabled[aRow] := false;
      ProgramsForm.ProgramsGrid.Cells[PROGRAM_ENABLED_CHK, aRow] := '0';
    end;
  end
  else if (aCol = PROGRAM_PRICE_DEC) and ProgramsInfo.Enabled[aRow] then
  begin
    ProgramsInfo.Price[aRow] := ProgramsInfo.Price[aRow] - 1;
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_VAL, aRow] := IntToStr(ProgramsInfo.Price[aRow]);
  end
  else if (aCol = PROGRAM_PRICE_INC) and ProgramsInfo.Enabled[aRow] then
  begin
    ProgramsInfo.Price[aRow] := ProgramsInfo.Price[aRow] + 1;
    ProgramsForm.ProgramsGrid.Cells[PROGRAM_PRICE_VAL, aRow] := IntToStr(ProgramsInfo.Price[aRow]);
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

  UpdateStations();
  FormShow(Sender);
end;

procedure TProgramsForm.UpdateStations();
var
  programJson: TJSONObject;
  keyPair: TJSONObject;
  station: integer;
  key: integer;

begin
  with TFPHttpClient.Create(nil) do
  try
     AddHeader('Content-Type', 'application/json');
     AddHeader('Pin', GetPinCode());

     for station:=0 to freeHashID-1 do
     begin
       for key:=0 to ProgramsInfo.Count-1 do
       begin
         try
            keyPair := TJSONObject.Create;
            keyPair.Add('key', ProgramsInfo.Key[key]);
            keyPair.Add('value', IntToStr(ProgramsInfo.Price[key]));
            programJson := TJSONObject.Create;
            programJson.Add('hash', stationHashes[station]);
            programJson.Add('keyPair', keyPair);

            RequestBody := TStringStream.Create(programJson.AsJSON);
            Post(GetServerEndpoint() + 'save');

            if ResponseStatusCode <> 204 then
            begin
              raise Exception.Create(IntToStr(ResponseStatusCode));
            end;

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
       end;
     end;
    finally
      Free;
    end;
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

