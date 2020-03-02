unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, fphttpclient, Fpjson, jsonparser;

type

  { TManageForm }

  TManageForm = class(TForm)
    btnOK: TButton;
    btnSendData: TButton;
    btnSendMoney: TButton;
    GroupBox1: TGroupBox;
    editHash: TLabeledEdit;
    editMoney: TLabeledEdit;
    editName: TLabeledEdit;
    editID: TLabeledEdit;
    Panel1: TPanel;
    procedure btnSendMoneyClick(Sender: TObject);
    procedure btnSendDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetHash(input: String);
    procedure SetName(input: String);
    procedure SetID(input: String);
  private

  public

  end;

var
  ManageForm: TManageForm;
  StationHash: String;
  StationName: String;
  StationID: String;

implementation

{$R *.lfm}

{ TManageForm }

procedure TManageForm.SetHash(input: String);
begin
     StationHash := input;
end;

procedure TManageForm.SetName(input: String);
begin
     StationName := input;
end;

procedure TManageForm.SetID(input: String);
begin
     StationID := input;
end;

procedure TManageForm.btnSendMoneyClick(Sender: TObject);
var
  postJson: TJSONObject;
  hashToSend: String;
  moneyToSend: Integer;

begin
  if TryStrToInt(editMoney.Text, Longint(moneyToSend)) = True then begin
     hashToSend := editHash.Text;

     if (moneyToSend > 0) and (moneyToSend <= 999) then begin
        postJson := TJSONObject.Create;
        postJson.Add('hash', TJSONString.Create(hashToSend));
        postJson.Add('amount', moneyToSend);
        editMoney.Text := '0';

        With TFPHttpClient.Create(Nil) do
        try
           try
              AddHeader('Content-Type', 'application/json');
              RequestBody := TStringStream.Create(postJson.AsJSON);
              Post('http://localhost:8020/add-service-amount');
           except
           end;

        finally
            Free;
        end;
     end;
  end;
end;

procedure TManageForm.btnSendDataClick(Sender: TObject);
var
  postJson: TJSONObject;
  idToSend: Integer;
  nameToSend: String;
  hashToSend: String;

begin
  idToSend := -1;
  TryStrToInt(editID.Text, Longint(idToSend));

  nameToSend := editName.Text;
  hashToSend := editHash.Text;

  postJson := TJSONObject.Create;

  if idToSend <> -1 then
     postJson.Add('id', idToSend);

  postJson.Add('name', TJSONString.Create(nameToSend));
  postJson.Add('hash', TJSONString.Create(hashToSend));
  With TFPHttpClient.Create(Nil) do
  try
     AddHeader('Content-Type', 'application/json');
     RequestBody := TStringStream.Create(postJson.AsJSON);
     Post('http://localhost:8020/set-station');
  finally
     Free;
  end;

end;


procedure TManageForm.FormShow(Sender: TObject);
begin
     editHash.Text := TCaption(StationHash);
     editID.Text := TCaption(StationID);
     editName.Text := TCaption(StationName);
end;

end.

