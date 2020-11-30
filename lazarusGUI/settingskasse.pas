unit settingsKasse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TsettingsKasse }

  TsettingsKasse = class(TForm)
    btnClose: TButton;
    btnTAX: TButton;
    btnTitle: TButton;
    comboBoxTAX: TComboBox;
    textTitle: TEdit;
    labelTAX: TLabel;
    labelTitle: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnTAXClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadSettings();
    procedure SaveTax();
    procedure SaveTitle();
    procedure CheckChanges();
  private

  public

  end;

var
  settingKasse: TsettingsKasse;

implementation

{$R *.lfm}

{ TsettingsKasse }

procedure TsettingsKasse.LoadSettings();
begin
  //Get current tax from server
  comboBoxTax.ItemIndex := 1;
  //Get title from server
  textTitle.Text := 'Car Washing';
end;

procedure TsettingsKasse.SaveTax();
begin

end;

procedure TsettingsKasse.SaveTitle();
begin

end;

procedure TsettingsKasse.btnTAXClick(Sender: TObject);
begin
  SaveTax();
end;

procedure TsettingsKasse.btnCloseClick(Sender: TObject);
begin
  CheckChanges();
  Close();
end;

procedure TsettingsKasse.btnTitleClick(Sender: TObject);
begin
  SaveTitle();
end;

procedure TsettingsKasse.CheckChanges();
begin
  SaveTitle();
end;

procedure TsettingsKasse.FormShow(Sender: TObject);
begin
  LoadSettings();
  Show;
end;

end.
