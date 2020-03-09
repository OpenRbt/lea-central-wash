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
    editMoney: TLabeledEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    PricesData: TStringGrid;
    procedure btnSendMoneyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PricesDataEditingDone(Sender: TObject);
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
  RequestAnswer: String;

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

procedure TManageForm.FormShow(Sender: TObject);
var
    postJson: TJSONObject;
    i: Integer;
    Key: String;
    Value: String;

begin
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

end.

