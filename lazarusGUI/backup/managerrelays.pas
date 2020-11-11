unit managerRelays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TManageRelays }

  TManageRelays = class(TForm)
    btnOk: TButton;
    RelayPanel: TPanel;
    RelayBoxTMPLT: TGroupBox;
    relayActive: TCheckBox;
    relayON: TEdit;
    relayOff: TEdit;
    relayMSEC: TEdit;
    ProgramList: TListBox;
    procedure ProgramListClick(Sender: TObject);
    procedure RelayBoxTMPLTClick(Sender: TObject);
  private

  public

  end;

  RelayConfig = packed record
    RelayBox: TGroupBox;
    RelayTrigger: TCheckBox;
    RelayOnTime: TEdit;
    RelayOffTime: TEdit;
    RelayMsec: TEdit;
  end;

var
  ManageRelays: TManageRelays;
  RelaysCount: integer;


implementation

{$R *.lfm}

{ TManageRelays }

procedure PrepareRelaysConfig(RelayPanel: TPanel);
var
  configs: array of RelayConfig;
  i: integer;
begin
  RelaysCount := 17;
  SetLength(configs, RelaysCount);
  for i := 0 to 16 do
  begin
    configs[i].RelayBox := TGroupBox.Create(nil);

    with configs[i].RelayBox do
    begin
      Caption := 'Relay ' + IntToStr(i);
      Parent := RelayPanel;
      Left := 0;
      Top := 60 * i;
      Height := 60;
    end;

  end;
end;

procedure TManageRelays.ProgramListClick(Sender: TObject);
var
  i: integer;
begin
  //PrepareRelaysConfig(RelayPanel);
  for i := 0 to 16 do
  begin

  end;
  //     ShowMessage('Clicked on ' + IntToStr(ProgramList.ItemIndex));
end;

procedure TManageRelays.RelayBoxTMPLTClick(Sender: TObject);
begin

end;

end.
