unit settingsKasse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, ClientAPI;

type

  { TsettingsKasse }

  TsettingsKasse = class(TForm)
    btnClose: TButton;
    btnSave: TButton;
    comboBoxTAX: TComboBox;
    labelCashier: TLabel;
    labelCashierINN: TLabel;
    textCashierINN: TMaskEdit;
    textReceiptItem: TEdit;
    labelTAX: TLabel;
    labelReceiptItem: TLabel;
    textCashier: TEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadSettings();
    procedure CheckChanges();
    function SaveSettings():boolean;
  private

  public

  end;

var
  settingKasse: TsettingsKasse;

implementation

{$R *.lfm}

{ TsettingsKasse }

procedure TsettingsKasse.LoadSettings();
var
  Kasse: KasseInfo;
  status: boolean;
  i: integer;
begin
  status := client.GetKasse(Kasse);
  if status then
  begin
    for i := 0 to comboBoxTax.Items.Count do
    begin
      if comboBoxTax.items[i] = Kasse.Tax then
      begin
        comboBoxTax.ItemIndex := i;
        break;
      end;
    end;
    textReceiptItem.Text := Kasse.receiptItemName;
    textCashier.Text := Kasse.cashier;
    textCashierINN.Text := Kasse.cashierINN;
  end
  else
  begin
    close;
  end;
end;

function TsettingsKasse.SaveSettings():boolean;
var
  Kasse: KasseInfo;
  status: boolean;
begin
  Kasse.cashier := textCashier.Text;
  Kasse.cashierINN := textCashierINN.Text;
  kasse.Tax := comboBoxTax.Items[comboboxTax.ItemIndex];
  kasse.receiptItemName := textReceiptItem.Text;
  if (Kasse.cashier <> '') and (length(Kasse.cashierINN)<>12) then begin
    ShowMessage('not correct cashiers inn');
    Result:= false;
    exit;
  end;
  client.SetKasse(kasse, status);
  Result:=status;
end;

procedure TsettingsKasse.btnCloseClick(Sender: TObject);
begin
  CheckChanges();
  Close();
end;

procedure TsettingsKasse.btnSaveClick(Sender: TObject);
var
  status:boolean;
begin
  status:=SaveSettings();
  if status then close;
end;

procedure TsettingsKasse.CheckChanges();
var
  oldKasse: kasseinfo;
  status: boolean;
  changes: boolean;
begin
  status := client.GetKasse(oldKasse);
  changes := False;

  if status then
  begin
    if textReceiptItem.Text <> oldKasse.receiptItemName then
    begin
      changes := True;
    end
    else
    if textCashier.Text <> oldKasse.cashier then
    begin
      changes := True;
    end
    else
    if textCashierINN.Text <> oldKasse.cashierINN then
    begin
      changes := True;
    end
    else
    if comboBoxTax.Items[comboBoxTax.ItemIndex] <> oldKasse.Tax then
    begin
      changes := True;
    end;


    //comboBoxTax.ItemIndex := 1;
    if changes then
    begin
      case QuestionDLG('Save changes?', 'Save new kasse configuration?',
          mtCustom, [mrYes, 'Save', mrNo, 'Don`t Save'], '') of
        mrYes: SaveSettings();
      end;
    end;

  end
  else
  begin
    btnCloseClick(self);
  end;

end;

procedure TsettingsKasse.FormShow(Sender: TObject);
begin
  LoadSettings();
  Show;
end;

end.
