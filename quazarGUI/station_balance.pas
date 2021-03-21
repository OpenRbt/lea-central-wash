unit station_balance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fphttpclient, Fpjson, jsonparser;

type

  { TStationBalanceForm }

  TStationBalanceForm = class(TForm)
    BalanceAmountLabel: TLabel;
    FoamCheckBox: TCheckBox;
    CurrencyLabel: TLabel;
    CollectionBtn: TButton;
    BackBtn: TButton;
    DryCheckBox: TCheckBox;
    FoamPanel: TPanel;
    DryPanel: TPanel;
    PausePanel: TPanel;
    WaxPanel: TPanel;
    RinsePanel: TPanel;
    ShampooPanel: TPanel;
    PauseCheckBox: TCheckBox;
    UpdateTimer: TTimer;
    WaxCheckBox: TCheckBox;
    RinseCheckBox: TCheckBox;
    ShampooCheckBox: TCheckBox;
    OpenBtn: TButton;
    IncBtn: TButton;
    CurrentAmount: TPanel;
    DecBtn: TButton;
    IncrementAmount: TPanel;
    StationLabel: TLabel;
    BalanceLabel: TLabel;
    procedure BackBtnClick(Sender: TObject);
    procedure CollectionBtnClick(Sender: TObject);
    procedure DecBtnClick(Sender: TObject);
    procedure DryPanelClick(Sender: TObject);
    procedure FoamPanelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IncBtnClick(Sender: TObject);
    procedure Init(id: integer);
    procedure OpenBtnClick(Sender: TObject);
    procedure PausePanelClick(Sender: TObject);
    procedure RinsePanelClick(Sender: TObject);
    procedure ShampooPanelClick(Sender: TObject);
    procedure UpdateCall(Sender: TObject);

    function GetCurrentMoney() : integer;
    procedure RunProgram();
    procedure WaxPanelClick(Sender: TObject);
  private
    _id : integer;
    _isStandBy : boolean;
    _currProgram : integer;
  public

  end;

var
  StationBalanceForm: TStationBalanceForm;

const
  NO_RESPONSE : integer = -1;
  DEFAULT_SERVICE_INCREMENT : integer = 10;

implementation
  uses base;
{$R *.lfm}

{ TStationBalanceForm }

procedure TStationBalanceForm.BackBtnClick(Sender: TObject);
begin
  StationBalanceForm.Close;
end;

procedure TStationBalanceForm.CollectionBtnClick(Sender: TObject);
var
  requestJson : TJSONObject;

begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', BaseForm.GetPinCode());

        requestJson := TJSONObject.Create;
        requestJson.Add('id', _id);
        RequestBody := TStringStream.Create(requestJson.AsJSON);

        Post(BaseForm.GetServerEndpoint() + 'save-collection');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
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

procedure TStationBalanceForm.DecBtnClick(Sender: TObject);
begin

end;

procedure TStationBalanceForm.RunProgram();
begin
  if BaseForm.GetStationCurrentBalanceByID(_id) < 1 then
  begin
    if _currProgram <> BaseForm.GetOpenDoorProgramID() then
    begin
      Exit;
    end;
  end;
  BaseForm.SetStationCurrentProgramByID(_currProgram, _id, false);
  if _currProgram = BaseForm.GetOpenDoorProgramID() then
  begin
    sleep(5000);
    _currProgram := NO_ID;
    BaseForm.SetStationCurrentProgramByID(_currProgram, _id, false);
  end;
end;

procedure TStationBalanceForm.DryPanelClick(Sender: TObject);
begin
  if _currProgram = NO_ID then
  begin
    _currProgram := 1;
    RunProgram();
    sleep(2000);
  end;
  _currProgram := BaseForm.GetDryProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.FoamPanelClick(Sender: TObject);
begin
  if _currProgram = NO_ID then
  begin
    _currProgram := 1;
    RunProgram();
    sleep(2000);
  end;
  _currProgram := BaseForm.GetFoamProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  UpdateTimer.Enabled := false;
end;

procedure TStationBalanceForm.FormCreate(Sender: TObject);
begin
  _isStandBy := false;
  UpdateTimer.Interval := 1000;
end;

procedure TStationBalanceForm.FormShow(Sender: TObject);
var
  currentMoney : integer;
  currentBalance : integer;
  currentProgram : integer;
begin
  UpdateTimer.Enabled := false;
  if BaseForm.UpdateStations() then
  begin
    if BaseForm.GetStationStatusByID(_id) = ON_LINE then
    begin
      StationLabel.Caption:=BaseForm.GetStationNameByID(_id);

      currentBalance := BaseForm.GetStationCurrentBalanceByID(_id);
      if currentBalance < 0 then
      begin
        currentBalance := 0;
      end;
      CurrentAmount.Caption := IntToStr(currentBalance);

      currentMoney := GetCurrentMoney();
      if currentMoney <> NO_RESPONSE then
      begin
        BalanceAmountLabel.Caption := IntToStr(currentMoney);
      end
      else
      begin
        BalanceAmountLabel.Caption := '0';
      end;

      FoamPanel.Color:=clDefault;
      ShampooPanel.Color:=clDefault;
      RinsePanel.Color:=clDefault;
      WaxPanel.Color:=clDefault;
      DryPanel.Color:=clDefault;
      PausePanel.Color:=clDefault;

      currentProgram := BaseForm.GetStationCurrentProgramByID(_id);

      case currentProgram of
        1: FoamPanel.Color:=clLime;
        2: ShampooPanel.Color:=clLime;
        3: RinsePanel.Color:=clLime;
        4: WaxPanel.Color:=clLime;
        5: DryPanel.Color:=clLime;
        6: PausePanel.Color:=clLime;
      end;
    end
    else
    begin
      ShowMessage('Station is offline');
      StationBalanceForm.Close;
    end;
  end
  else
  begin
    ShowMessage('Can`t connect to server');
    StationBalanceForm.Close;
  end;
  UpdateTimer.Enabled := true;
end;

procedure TStationBalanceForm.IncBtnClick(Sender: TObject);
var
  hash : string;
  requestJson : TJSONObject;

begin
  hash := BaseForm.GetStationHashByID(_id);
  if hash = PLACEHOLDER then
  begin
    Exit;
  end;
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', BaseForm.GetPinCode());

        requestJson := TJSONObject.Create;
        requestJson.Add('hash', BaseForm.GetStationHashByID(_id));
        requestJson.Add('amount', DEFAULT_SERVICE_INCREMENT);
        RequestBody := TStringStream.Create(requestJson.AsJSON);

        Post(BaseForm.GetServerEndpoint() + 'add-service-amount');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
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

procedure TStationBalanceForm.Init(id: integer);
begin
  _id := id;
  _currProgram := NO_ID;
end;

procedure TStationBalanceForm.OpenBtnClick(Sender: TObject);
begin
  _currProgram := BaseForm.GetOpenDoorProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.PausePanelClick(Sender: TObject);
begin
  _currProgram := BaseForm.GetPauseProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.RinsePanelClick(Sender: TObject);
begin
  if _currProgram = NO_ID then
  begin
    _currProgram := 1;
    RunProgram();
    sleep(2000);
  end;
  _currProgram := BaseForm.GetRinseProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.ShampooPanelClick(Sender: TObject);
begin
  if _currProgram = NO_ID then
  begin
    _currProgram := 1;
    RunProgram();
    sleep(2000);
  end;
  _currProgram := BaseForm.GetShampooProgramID();
  RunProgram();
end;

procedure TStationBalanceForm.UpdateCall(Sender: TObject);
begin
  StationBalanceForm.FormShow(Sender);
end;

function TStationBalanceForm.GetCurrentMoney() : integer;
var
  RequestAnswer: string;
  requestJson : TJSONObject;
  stationsJson: TJSONObject;
  path: TJSONdata;
  banknotes : integer;
  coins : integer;

begin
  with TFPHttpClient.Create(nil) do
  try
     try
        Result := NO_RESPONSE;
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', BaseForm.GetPinCode());

        requestJson := TJSONObject.Create;
        requestJson.Add('id', _id);
        RequestBody := TStringStream.Create(requestJson.AsJSON);

        RequestAnswer := Post(BaseForm.GetServerEndpoint() + 'station-report-current-money');

        if ResponseStatusCode = 200 then
        begin
          stationsJson := GetJson(RequestAnswer).GetPath('moneyReport') as TJSONObject;

          with stationsJson do
          begin
            path := FindPath('banknotes');
            if path <> nil then
            begin
              banknotes := path.AsInteger;
              Result := banknotes;
            end;
            path := FindPath('coins');
            if path <> nil then
            begin
              coins := path.AsInteger;
              Result := Result + coins;
            end;
          end;

          _isStandBy := false;
        end;
      except
        if not _isStandBy then
        begin
          _isStandBy := true;
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
        Result := NO_RESPONSE;
      end;
    finally
      Free;
    end;
end;

procedure TStationBalanceForm.WaxPanelClick(Sender: TObject);
begin
  if _currProgram = NO_ID then
  begin
    _currProgram := 1;
    RunProgram();
    sleep(2000);
  end;
  _currProgram := BaseForm.GetWaxProgramID();
  RunProgram();
end;

end.

