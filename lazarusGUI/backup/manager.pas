unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Buttons, ClientAPI;

type

  { TManageForm }

  TManageForm = class(TForm)
    btnOK: TButton;
    btnSendMoney: TButton;
    btnChangeHash: TButton;
    btnRemoveHash: TButton;
    btnChangeName: TButton;
    btnOpenStation: TButton;
    Button1: TButton;
    cbHash: TComboBox;
    edName: TEdit;
    editMoney: TLabeledEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    PricesData: TStringGrid;
    procedure btnChangeHashClick(Sender: TObject);
    procedure btnChangeNameClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRemoveHashClick(Sender: TObject);
    procedure btnSendMoneyClick(Sender: TObject);
    procedure btnOpenStationClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetHash(input: String);
    procedure SetName(input: String);
    procedure SetID(input: integer);
    procedure SetAvailableHashes(input: TStringList);
  private
    oldPrice: array of string;
  public

  end;

const programCount = 199;
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
  moneyToSend: Integer;

begin
  if TryStrToInt(editMoney.Text, Longint(moneyToSend)) = True then begin

     if (moneyToSend > 0) and (moneyToSend <= 999) then begin
        client.SendMoney(StationHash, moneyToSend);
        editMoney.Text := '0';
     end;
  end;
end;

procedure TManageForm.btnOpenStationClick(Sender: TObject);
var
  Reply, BoxStyle: Integer;
  btnYesNo, yesAnswer: Integer;
  icoQuestion: Integer;

begin
// taken from constants as lazarus and freepascal are QUITE BUGGY still
// and I already got an issue using these constants original names
// so decided to hardcode for now
  btnYesNo := $00000004;
  icoQuestion := $00000020;
  yesAnswer := 6;
  BoxStyle := icoQuestion + btnYesNo;
  Reply := Application.MessageBox('REALLY OPEN STATION BOX DOOR?', 'WARNING', BoxStyle);
  if Reply <> yesAnswer then exit;
  client.OpenStation(StationID);
end;

procedure TManageForm.Button1Click(Sender: TObject);
var
  Reply, BoxStyle: Integer;
  btnYesNo, yesAnswer: Integer;
  icoQuestion: Integer;
  i: Integer;
  Value: Integer;

begin
  btnYesNo := $00000004;
  icoQuestion := $00000020;
  yesAnswer := 6;
  BoxStyle := icoQuestion + btnYesNo;
  Reply := Application.MessageBox('Save price?', 'WARNING', BoxStyle);
  if Reply <> yesAnswer then exit;

  for i:=1 to programCount do begin;
    if PricesData.Cells[i, 1] <> oldPrice[i] then begin
       // Numeric value in grid cell check
       if TryStrToInt(PricesData.Cells[i, 1], Longint(Value)) then begin
          if Value > 10000 then value := 10000;
          if Value < 10 then  value := 10;
          client.SetPriceByID(StationHash,i , Value);
       end;
    end;
  end;
  close;
end;

procedure TManageForm.btnChangeHashClick(Sender: TObject);
begin
  if cbHash.ItemIndex >= 0 then begin
    client.PairIdAndHash(cbHash.Items[cbHash.ItemIndex], StationName, StationID);
    close;
  end else ShowMessage('Choose a hash');
end;

procedure TManageForm.btnChangeNameClick(Sender: TObject);
begin
  client.PairIdAndHash(StationHash, edName.Text, StationID);
  close;
end;

procedure TManageForm.btnOKClick(Sender: TObject);
begin

end;

procedure TManageForm.btnRemoveHashClick(Sender: TObject);
begin
  client.PairIdAndHash('', StationName, StationID);
end;

procedure TManageForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
   SetLength(oldPrice, programCount+1);
   PricesData.Columns.Clear;
   for i := 1 to programCount do begin
       PricesData.Columns.Add();
       PricesData.Columns[i-1].Title.Caption := IntToStr(i);
       PricesData.Columns[i-1].Width:=50;
   end;
end;

procedure TManageForm.FormShow(Sender: TObject);
var
    i: Integer;
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

    for i := 1 to programCount do begin
        Value:= client.PriceByID(StationHash, i);
        PricesData.Cells[i, 1] := Value;
        oldPrice[i]:= Value;
    end;
    end;
end;

end.

