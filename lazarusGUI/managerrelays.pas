unit managerRelays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TManagePrograms }

  TManagePrograms = class(TForm)
    btnSaveProgramName: TButton;
    btnCancelProgramName: TButton;
    btnOk: TButton;
    btnSaveRelaysConfig: TButton;
    btnRevertRelayConfig: TButton;
    relayLabel: TLabel;
    labelRelayActive: TLabel;
    labelTimeOn: TLabel;
    labelTimeOff: TLabel;
    labelTimeMsec: TLabel;
    programNameEdit: TEdit;
    GroupBox1: TGroupBox;
    ProgramList: TListBox;
    relayActive: TCheckBox;
    RelayBoxTMPLT: TPanel;
    RelayListBox: TScrollBox;
    relayMSEC: TEdit;
    relayOff: TEdit;
    relayON: TEdit;
    procedure btnCancelProgramNameClick(Sender: TObject);
    procedure btnRevertRelayConfigClick(Sender: TObject);
    procedure btnSaveProgramNameClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSaveRelaysConfigClick(Sender: TObject);
    procedure ProgramListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

  RelayConfig = packed record
    RelayPanel: TPanel;
    RelayLabel: TLabel;
    RelayTrigger: TCheckBox;
    RelayOnTime: TEdit;
    RelayOffTime: TEdit;
    RelayMsec: TEdit;
  end;

var
  ManagePrograms: TManagePrograms;
  RelaysCount: integer;
  configs: array of RelayConfig;


implementation

{$R *.lfm}

{ TManagePrograms }
procedure PrepareProgramList(ProgramList: TListBox);
var
  i: integer;
  programsCount: integer;
begin
  ProgramList.Items.Clear;
  programsCount := 9;

  for i := 0 to programsCount - 1 do
  begin
    ProgramList.Items.Add('ProgramName ' + IntToStr(i + 1));
  end;
end;

procedure PrepareRelaysConfig(RelayListBox: TScrollBox);
var
  i: integer;
begin
  //Get Relays count request
  if Assigned(configs) then
  begin
    if Length(configs) > 0 then
    begin
      for i := Length(configs) - 1 to 0 do
      begin
        with configs[i] do
        begin
          RelayPanel.Free;
          RelayLabel.Free;
          RelayTrigger.Free;
          RelayOnTime.Free;
          RelayOffTime.Free;
          RelayMsec.Free;
        end;
      end;
      Finalize(configs);
    end;
  end;


  RelaysCount := 17;
  SetLength(configs, RelaysCount);
  for i := 0 to 16 do
  begin
    configs[i].RelayPanel := TPanel.Create(nil);
    configs[i].RelayTrigger := TCheckBox.Create(nil);
    configs[i].RelayOnTime := TEdit.Create(nil);
    configs[i].RelayOffTime := TEdit.Create(nil);
    configs[i].RelayMsec := TEdit.Create(nil);
    configs[i].RelayLabel := TLabel.Create(nil);
    //Relay Config Creation

    with configs[i].RelayPanel do
    begin
      Parent := RelayListBox;
      Left := 0;
      Width := 380;
      Top := 35 * i;
      Height := 35;
    end;

    with configs[i].RelayLabel do
    begin
      Caption := 'Relay ' + IntToStr(i + 1);
      Parent := configs[i].RelayPanel;
      Width := 60;
      Height := 30;
      Left := 10;
      Top := 8;
    end;

    with configs[i].RelayTrigger do
    begin
      Parent := configs[i].RelayPanel;
      Width := 40;
      Height := 33;
      Left := 80;
      Top := 5;
    end;

    with configs[i].RelayOnTime do
    begin
      Parent := configs[i].RelayPanel;
      Width := 80;
      Height := 33;
      Left := 110;
      Top := 1;
    end;

    with configs[i].RelayOffTime do
    begin
      Parent := configs[i].RelayPanel;
      Width := 80;
      Height := 33;
      Left := 200;
      Top := 1;
    end;

    with configs[i].RelayMsec do
    begin
      Parent := configs[i].RelayPanel;
      Width := 80;
      Height := 33;
      Left := 290;
      Top := 1;
    end;
  end;
  //ShowMessage('RelayPrepared');
end;

procedure LoadRelaysConfig();
var
  i: integer;
begin
  //Get program I relays request
  for i := 0 to RelaysCount - 1 do
  begin
    with configs[i] do
    begin
      RelayTrigger.Checked := False;
      RelayOnTime.Caption := '0';
      RelayOffTime.Caption := '10';
      RelayMsec.Caption := '100';
    end;
  end;
  //ShowMessage('ProgramLoaded');
end;

procedure SaveRelaysConfig();
var
  i: integer;
begin
  for i := 0 to RelaysCount - 1 do
  begin
    with configs[i] do
    begin

    end;
  end;
  ShowMessage('Saved Relays Configuration');
  //Send program I relays request

end;

procedure TManagePrograms.ProgramListClick(Sender: TObject);
begin
  LoadRelaysConfig();

  if ProgramList.ItemIndex <> -1 then
  begin
    programNameEdit.Text := ProgramList.Items[ProgramList.ItemIndex];
  end;

end;

procedure TManagePrograms.btnCancelProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    programNameEdit.Text := ProgramList.Items[ProgramList.ItemIndex];
  end
  else
  begin
    programNameEdit.Text := '';
  end;

end;

procedure TManagePrograms.btnRevertRelayConfigClick(Sender: TObject);
begin
  LoadRelaysConfig();
  ShowMessage('Revert Relays Configuration');
end;

procedure TManagePrograms.btnSaveProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    ProgramList.Items[ProgramList.ItemIndex] := programNameEdit.Text;
  end;
  //Save programName request
end;

procedure TManagePrograms.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TManagePrograms.btnSaveRelaysConfigClick(Sender: TObject);
begin
  SaveRelaysConfig();
end;


procedure TManagePrograms.FormShow(Sender: TObject);
begin
  Show;
  PrepareProgramList(ProgramList);
  PrepareRelaysConfig(RelayListBox);
  btnCancelProgramNameClick(self);
end;


end.
