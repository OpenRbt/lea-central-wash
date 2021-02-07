unit cardreaderconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ClientAPI;

type

  { TCardReaderConfigForm }

  TCardReaderConfigForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbType: TComboBox;
    edHost: TEdit;
    edPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LoadConfig();
  private

  public
    StationID: integer;

  end;

var
  CardReaderConfigForm: TCardReaderConfigForm;

implementation

{$R *.lfm}

{ TCardReaderConfigForm }

procedure TCardReaderConfigForm.LoadConfig();
var
  cfg: CardReaderConfig;
  status: boolean;
  i: integer;
begin
  status := client.GetCardReaderConfig(StationID, CardReaderConfig);
  if status then
  begin
    for i := 0 to cbType.Items.Count do
    begin
      if cbType.items[i] = cfg.CardReaderType then
      begin
        cbType.ItemIndex := i;
        break;
      end;
    end;
    edHost.Text := cfg.Host;
    edPort.Text := cfg.Port;
  end
  else
  begin
    close;
  end;
end;

procedure TCardReaderConfigForm.FormShow(Sender: TObject);
begin
  LoadConfig();
end;

end.

