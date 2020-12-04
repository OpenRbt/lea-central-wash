unit settingsKasse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ClientAPI;

type

  { TsettingsKasse }

  TsettingsKasse = class(TForm)
    btnClose: TButton;
    btnSave: TButton;
    comboBoxTAX: TComboBox;
    labelCashier: TLabel;
    labelCashierINN: TLabel;
    textReceiptItem: TEdit;
    labelTAX: TLabel;
    labelReceiptItem: TLabel;
    textCashier: TEdit;
    textCashierINN: TEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadSettings();
    procedure CheckChanges();
    procedure SaveSettings();
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
    btnCloseClick(self);
  end;
end;

procedure TsettingsKasse.SaveSettings();
var
  Kasse: KasseInfo;
  status: boolean;
begin
  Kasse.cashier := textCashier.Text;
  Kasse.cashierINN := textCashierINN.Text;
  kasse.Tax := comboBoxTax.Items[comboboxTax.ItemIndex];
  kasse.receiptItemName := textReceiptItem.Text;

  client.SetKasse(kasse, status);
end;

procedure TsettingsKasse.btnCloseClick(Sender: TObject);
begin
  CheckChanges();
  Close();
end;

procedure TsettingsKasse.btnSaveClick(Sender: TObject);
begin
  SaveSettings();
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
