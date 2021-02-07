unit cardreadercfg;

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadConfig();
    function SaveConfig():boolean;
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
  status := client.GetCardReaderConfig(StationID, cfg);
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

function TCardReaderConfigForm.SaveConfig():boolean;
var
  cfg: CardReaderConfig;
  port: longint;
begin
  cfg.Host := edHost.Text;
  cfg.Port := edPort.Text;
  cfg.CardReaderType := cbType.Items[cbType.ItemIndex];
  cfg.StationID:=StationID;
  if (cfg.Port <> '') and (not TryStrToInt(cfg.Port, port)) then begin
    ShowMessage('port must be numeric');
    Result:= false;
    exit;
  end;
    cfg.CardReaderType := cbType.Items[cbType.ItemIndex];
  if (cfg.CardReaderType = 'VENDOTEK') and ((cfg.Host = '') or (cfg.Port = '')) then begin
    ShowMessage('for the Vendotek card reader, the host and port are required');
    Result:= false;
    exit;
  end;

  Result:=client.SetCardReaderConfig(cfg);
end;

procedure TCardReaderConfigForm.FormShow(Sender: TObject);
begin
  LoadConfig();
end;

procedure TCardReaderConfigForm.Button2Click(Sender: TObject);
begin
  close;
end;

procedure TCardReaderConfigForm.Button1Click(Sender: TObject);
var
  status:boolean;
begin
  status:=SaveConfig();
  if status then close;
end;

end.

