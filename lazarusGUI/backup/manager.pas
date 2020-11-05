unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Buttons, fphttpclient, Fpjson, jsonparser;

type

  { TManageForm }

  TManageForm = class(TForm)
    btnOK: TButton;
    btnSendMoney: TButton;
    btnChangeHash: TButton;
    btnRemoveHash: TButton;
    btnChangeName: TButton;
    btnOpenStation: TButton;
    cbHash: TComboBox;
    edName: TEdit;
    editMoney: TLabeledEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    PricesData: TStringGrid;
    procedure btnChangeHashClick(Sender: TObject);
    procedure btnChangeNameClick(Sender: TObject);
    procedure btnRemoveHashClick(Sender: TObject);
    procedure btnSendMoneyClick(Sender: TObject);
    procedure btnOpenStationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PricesDataEditingDone(Sender: TObject);
    procedure SetHash(input: String);
    procedure SetName(input: String);
    procedure SetID(input: integer);
    procedure SetAvailableHashes(input: TStringList);
    procedure PairIdAndHash(Hash, StationName: String; ID: integer);
  private

  public

  end;

var
  ManageForm: TManageForm;
  StationHash: String;
  StationName: String;
  StationID: integer;
  RequestAnswer: String;
  AvailableHashes: TStringList;

implementation

{$R *.lfm}

{ TManageForm }

procedure TManageForm.SetAvailableHashes(input: TStringList);
begin
     AvailableHashes := input;
end;

procedure TManageForm.SetHash(input: String);
begin
     StationHash := input;
end;

procedure TManageForm.SetName(input: String);
begin
     StationName := input;
end;

procedure TManageForm.SetID(input: integer);
begin
     StationID := input;
end;

procedure TManageForm.btnSendMoneyClick(Sender: TObject);
var
  postJson: TJSONObject;
  moneyToSend: Integer;

begin
  if TryStrToInt(editMoney.Text, Longint(moneyToSend)) = True then begin

     if (moneyToSend > 0) and (moneyToSend <= 999) then begin
        postJson := TJSONObject.Create;
        postJson.Add('hash', TJSONString.Create(StationHash));
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

procedure TManageForm.btnOpenStationClick(Sender: TObject);
begin
        postJson := TJSONObject.Create;
        postJson.Add('stationID', StationID);

        With TFPHttpClient.Create(Nil) do
        try
           try
              AddHeader('Content-Type', 'application/json');
              RequestBody := TStringStream.Create(postJson.AsJSON);
              Post('http://localhost:8020/open-station');
           except
           end;

        finally
            Free;
        end;
end;

procedure TManageForm.btnChangeHashClick(Sender: TObject);
begin
  if cbHash.ItemIndex >= 0 then begin
    PairIdAndHash(cbHash.Items[cbHash.ItemIndex], StationName, StationID);
    close;
  end else ShowMessage('Choose a hash');
end;

procedure TManageForm.btnChangeNameClick(Sender: TObject);
begin
  PairIdAndHash(StationHash, edName.Text, StationID);
  close;
end;

procedure TManageForm.btnRemoveHashClick(Sender: TObject);
begin
  PairIdAndHash('', StationName, StationID);
end;

procedure TManageForm.FormCreate(Sender: TObject);
begin

end;

procedure TManageForm.FormShow(Sender: TObject);
var
    postJson: TJSONObject;
    i: Integer;
    Key: String;
    Value: String;

begin
  cbHash.Text:='';
  cbHash.Items.Clear;
    PricesData.Enabled:=false;
    editMoney.Enabled:=false;
    btnSendMoney.Enabled:=false;
    btnRemoveHash.Enabled:=false;
    btnChangeName.Enabled:=false;
    btnOpenStation.Enabled:=false;
    edName.ReadOnly:=true;
    edName.Text:=StationName;
  for i := 0 to AvailableHashes.Count - 1 do
       cbHash.Items.Add(AvailableHashes.ValueFromIndex[i]);

    cbHash.ItemIndex := cbHash.Items.IndexOf(StationHash);
    if StationHash <> '' then begin
    PricesData.Enabled:=true;
    editMoney.Enabled:=true;
    btnSendMoney.Enabled:=true;
    btnRemoveHash.Enabled:=true;
    btnChangeName.Enabled:=true;
    btnOpenStation.Enabled:=true;
    edName.ReadOnly:=false;

    for i := 1 to 6 do begin
        Key := 'price' + IntToStr(i);
        postJson := TJSONObject.Create;
        postJson.Add('hash', TJSONString.Create(StationHash));
        postJson.Add('key', TJSONString.Create(Key));

        With TFPHttpClient.Create(Nil) do
        try
           AddHeader('Content-Type', 'application/json');
           RequestBody := TStringStream.Create(postJson.AsJSON);
           RequestAnswer := Post('http://localhost:8020/load');
           Value := RequestAnswer.Substring(1,RequestAnswer.Length-3);
           PricesData.Cells[i, 1] := Value;
        finally
            Free;
        end;
    end;
    end;
end;

procedure TManageForm.PricesDataEditingDone(Sender: TObject);
var
   valueInsideText: Integer;
   postJson: TJSONObject;
   keyPairJson: TJSONObject;
   i: Integer;
   valueFromGrid: Integer;
   Key: String;

begin
   valueInsideText := -1;
   if TryStrToInt(PricesData.Cells[PricesData.Col, PricesData.Row], Longint(valueInsideText)) then begin
      if valueInsideText > 99 then
         PricesData.Cells[PricesData.Col, PricesData.Row] := IntToStr(99);
      if valueInsideText < 10 then
         PricesData.Cells[PricesData.Col, PricesData.Row] := IntToStr(10);
   end
   else
   begin
       PricesData.Cells[PricesData.Col, PricesData.Row] := IntToStr(10);
   end;

   i := PricesData.Col;

   Key := 'price' + IntToStr(i);

   valueFromGrid := -1;

   // Numeric value in grid cell check
   if TryStrToInt(PricesData.Cells[i, 1], Longint(valueFromGrid)) then begin
      postJson := TJSONObject.Create;
      keyPairJson := TJSONObject.Create;

      postJson.Add('hash', TJSONString.Create(StationHash));
      keyPairJson.Add('key', TJSONString.Create(Key));
      keyPairJson.Add('value', TJSONString.Create(PricesData.Cells[i, 1]));
      postJson.Add('KeyPair', keyPairJson);

      With TFPHttpClient.Create(Nil) do
      try
         AddHeader('Content-Type', 'application/json');
         RequestBody := TStringStream.Create(postJson.AsJSON);
         Post('http://localhost:8020/save');
      finally
         Free;
      end;
   end;


end;

procedure TManageForm.PairIdAndHash(Hash, StationName: String; ID: integer);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('id', ID);
  postJson.Add('hash', TJSONString.Create(Hash));
  postJson.Add('name', TJSONString.Create(StationName));
  With TFPHttpClient.Create(Nil) do
  try
   try
     AddHeader('Content-Type', 'application/json');
     RequestBody := TStringStream.Create(postJson.AsJSON);
     Post('http://localhost:8020/set-station');
   except
        On E: Exception do
      end;
  finally
     Free;
  end;

end;


end.

