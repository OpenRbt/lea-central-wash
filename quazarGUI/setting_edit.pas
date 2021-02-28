unit setting_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fphttpclient, Fpjson, jsonparser;

type

  { TSettingEditForm }

  TSettingEditForm = class(TForm)
    AddressLabel: TLabel;
    AddressEdit: TComboBox;
    SaveBtn: TButton;
    NameEdit: TEdit;
    NameLabel: TLabel;
    CancelBtn: TButton;
    procedure AddressEditChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);

    procedure SetStationButtons();

    procedure Init(stationID : integer; stationName, hash : string);
  private
    _id : integer;
    _name : string;
    _hash : string;

  public

  end;

var
  SettingEditForm: TSettingEditForm;

const
  DEFAULT_PREFLIGHT_SEC : integer = 0;
  DEFAULT_RELAY_BOARD   : string = 'localGPIO';

implementation
  uses settings;

{$R *.lfm}

{ TSettingEditForm }

procedure TSettingEditForm.FormCreate(Sender: TObject);
begin

end;

procedure TSettingEditForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := 0;
  SettingEditForm.Close;
end;

procedure TSettingEditForm.AddressEditChange(Sender: TObject);
begin
  _hash := AddressEdit.Items[AddressEdit.ItemIndex];
end;

procedure TSettingEditForm.FormShow(Sender: TObject);
var
  hashes : THashes;
  i : integer;
begin
  NameEdit.Text := _name;
  hashes := SettingsForm.GetFreeHashes();
  AddressEdit.Clear;
  for i:=0 to Length(hashes)-1 do
  begin
    AddressEdit.Items.Add(hashes[i]);
  end;
  AddressEdit.Items.Add(_hash);
  AddressEdit.ItemIndex := AddressEdit.Items.IndexOf(_hash);
end;

procedure TSettingEditForm.SaveBtnClick(Sender: TObject);
var
  settingJson: TJSONObject;
begin
  if _hash = SettingsForm.PLACEHOLDER then
  begin
    Exit;
  end;
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', SettingsForm.GetPinCode());

        settingJson := TJSONObject.Create;

        settingJson.Add('id', _id);
        settingJson.Add('name', NameEdit.Text);
        settingJson.Add('hash', AddressEdit.items[AddressEdit.ItemIndex]);
        settingJson.Add('preflightSec', DEFAULT_PREFLIGHT_SEC);
        settingJson.Add('relayBoard', DEFAULT_RELAY_BOARD);

        RequestBody := TStringStream.Create(settingJson.AsJSON);
        Post(SettingsForm.GetServerEndpoint() + '/set-station');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        ModalResult := 1;

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
  SetStationButtons();
  if ModalResult = 1 then
  begin
    SettingEditForm.Close;
  end;
end;

procedure TSettingEditForm.SetStationButtons();
var
  settingJson: TJSONObject;
  buttons    :  TJsonArray;
  button     : TJSONObject;
  i: integer;
begin
  if _hash = SettingsForm.PLACEHOLDER then
  begin
    Exit;
  end;
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', SettingsForm.GetPinCode());

        settingJson := TJSONObject.Create;

        settingJson.Add('stationID', _id);

        buttons := TJsonArray.Create;
        for i:=1 to SettingsForm.GetNumPrograms() do
        begin
          button := TJSONObject.Create;
          button.Add('buttonID', i);
          button.Add('programID', i);
          buttons.Add(button);
        end;
        settingJson.Add('buttons', buttons);

        RequestBody := TStringStream.Create(settingJson.AsJSON);
        Post(SettingsForm.GetServerEndpoint() + '/set-station-button');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        ModalResult := 1;

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

procedure TSettingEditForm.Init(stationID : integer; stationName, hash : string);
begin
  _id   := stationID;
  _name := stationName;
  _hash := hash;
end;

end.

