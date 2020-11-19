unit managerRelays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ClientAPI;

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
    procedure LoadRelaysConfig();
    procedure FormShow(Sender: TObject; ID: integer);
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
  RelaysCount, StationID, ProgramID: integer;
  configs: array of RelayConfig;



implementation

{$R *.lfm}

{ TManagePrograms }
procedure PrepareProgramList(ProgramList: TListBox);
var
  i: integer;  
  programs: ProgramsInfo;
begin
  ProgramList.Items.Clear

  programs := client.GetPrograms(StationID);

  for i := 0 to 9 do
  begin
    ProgramList.Items.Add('Program ' + IntToStr(i + 1));
  end;

  for i := 0 to programs.Count - 1 do
  begin
    ProgramList.Items[programs.programID[i] - 1] := programs.programName[i];
  end;
end;

procedure PrepareRelaysConfig(RelayListBox: TScrollBox);
var
  i: integer;
begin
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
  for i := 0 to RelaysCount - 1 do
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
      NumbersOnly := True;
      Width := 80;
      Height := 33;
      Left := 110;
      Top := 1;
    end;

    with configs[i].RelayOffTime do
    begin
      Parent := configs[i].RelayPanel;
      NumbersOnly := True;
      Width := 80;
      Height := 33;
      Left := 200;
      Top := 1;
    end;

    with configs[i].RelayMsec do
    begin
      Parent := configs[i].RelayPanel;
      NumbersOnly := True;
      Width := 80;
      Height := 33;
      Left := 290;
      Top := 1;
    end;
  end;
end;

procedure TManagePrograms.LoadRelaysConfig();
var
  i: integer;
  relays: RelaysInfo;

begin
  relays := client.GetProgramRelays(StationID, ProgramID);

  //Setting Default values
  for i := 0 to RelaysCount - 1 do
  begin
    with configs[i] do
    begin
      RelayTrigger.Checked := False;
      RelayOnTime.Text := '0';
      RelayOffTime.Text := '0';
      RelayMsec.Text := '0';
    end;
  end;

  //Setting Relays
  for i := 0 to relays.Count - 1 do
  begin
    with configs[relays.realyID[i] - 1] do
    begin
      RelayTrigger.Checked := True;
      RelayOnTime.Text := IntToStr(relays.timeON[i]);
      RelayOffTime.Text := IntToStr(relays.timeOFF[i]);
      RelayMsec.Text := IntToStr(relays.preflight[i]);
    end;
  end;
end;

procedure SaveRelaysConfig();
var
  i, j: integer;
  relays: RelaysInfo;
  sendCount: integer;
begin
  sendCount := 0;
  for i := 0 to RelaysCount - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        sendCount := sendCount + 1;
      end;
    end;
  end;
  setlength(relays.realyID, sendCount);
  setlength(relays.timeON, sendCount);
  setlength(relays.timeOFF, sendCount);
  setlength(relays.preflight, sendCount);
  relays.Count := sendCount;
  j := 0;
  for i := 0 to RelaysCount - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        relays.realyID[j] := i + 1;
        try
          relays.timeON[j] := StrToInt(RelayOnTime.Text);
          relays.timeOFF[j] := StrToInt(RelayOffTime.Text);
          relays.preflight[j] := StrToInt(RelayMsec.Text);

          if (relays.timeON[j] < 0) or (relays.timeOFF[j] < 0) or
            (relays.preflight[j] < 0) then
            raise Exception.Create('');
        except
          ShowMessage('Incorrect values for relay ' + IntToStr(i + 1));
          exit;
        end;
        j := j + 1;
      end;
    end;
  end;

  client.SetProgramRelays(StationID, ProgramID, relays);
end;

procedure TManagePrograms.ProgramListClick(Sender: TObject);
begin

  if ProgramList.ItemIndex <> -1 then
  begin
    ProgramID := programList.ItemIndex + 1;
    LoadRelaysConfig();
    programNameEdit.Text := programList.Items[programList.ItemIndex];
  end;

end;

procedure TManagePrograms.btnCancelProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    programNameEdit.Text := programList.Items[ProgramList.ItemIndex];
  end
  else
  begin
    programNameEdit.Text := '';
  end;

end;

procedure TManagePrograms.btnRevertRelayConfigClick(Sender: TObject);
begin
  LoadRelaysConfig();
end;

procedure TManagePrograms.btnSaveProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    ProgramList.Items[ProgramList.ItemIndex] := programNameEdit.Text;

    client.SetProgramName(StationID, ProgramList.ItemIndex +1,
      programNameEdit.Text);
  end;
end;

procedure TManagePrograms.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TManagePrograms.btnSaveRelaysConfigClick(Sender: TObject);
begin
  SaveRelaysConfig();
end;


procedure TManagePrograms.FormShow(Sender: TObject; ID: integer);
begin
  Show;
  StationID := ID;
  PrepareRelaysConfig(RelayListBox);
  PrepareProgramList(ProgramList);
  btnCancelProgramNameClick(self);
end;


end.
