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
    StationInfo: TLabel;
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
    procedure FormShow(Sender: TObject; ID: integer; stationName: string);
    procedure PrepareProgramList();
    procedure PrepareRelaysConfig();
    procedure CheckRelaysChanges();
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


const
  MAX_PROGRAMS = 9;

const
  MAX_RELAYS = 17;

var
  ManagePrograms: TManagePrograms;
  StationID: integer = -1;
  ProgramID: integer = -1;
  configs: array of RelayConfig;



implementation

{$R *.lfm}

{ TManagePrograms }
procedure TManagePrograms.PrepareProgramList();
var
  i: integer;
  programs: ProgramsInfo;
  status: boolean;
begin
  ProgramList.Items.Clear;

  status := client.GetPrograms(StationID, programs);

  if status = False then
  begin
    Close;
  end;

  for i := 0 to MAX_PROGRAMS - 1 do
  begin
    ProgramList.Items.Add('Program ' + IntToStr(i + 1));
  end;

  for i := 0 to programs.Count - 1 do
  begin
    if programs.programName[i] <> '' then
      ProgramList.Items[programs.programID[i] - 1] := programs.programName[i];
  end;
end;

procedure TManagePrograms.PrepareRelaysConfig();
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

  SetLength(configs, MAX_RELAYS);
  for i := 0 to MAX_RELAYS - 1 do
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
  status: boolean;

begin
  status := client.GetProgramRelays(StationID, ProgramID, relays);

  if status = False then
  begin
    btnSaveRelaysConfig.Enabled := False;
  end
  else
  begin
    btnSaveRelaysConfig.Enabled := True;
  end;

  //Setting Default values
  for i := 0 to MAX_RELAYS - 1 do
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
    with configs[relays.relayID[i] - 1] do
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
  status: boolean;

begin
  sendCount := 0;
  for i := 0 to MAX_RELAYS - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        sendCount := sendCount + 1;
      end;
    end;
  end;
  setlength(relays.relayID, sendCount);
  setlength(relays.timeON, sendCount);
  setlength(relays.timeOFF, sendCount);
  setlength(relays.preflight, sendCount);
  relays.Count := sendCount;
  j := 0;
  for i := 0 to MAX_RELAYS - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        relays.relayID[j] := i + 1;
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

  client.SetProgramRelays(StationID, ProgramID, relays, status);

  if status = False then
  begin
    //close;
  end;
end;

procedure TManagePrograms.ProgramListClick(Sender: TObject);
begin

  if ProgramList.ItemIndex <> -1 then
  begin
    if ProgramID <> -1 then
      CheckRelaysChanges();
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
var
  status: boolean;
begin
  if ProgramList.ItemIndex <> -1 then
  begin

    client.SetProgramName(StationID, ProgramList.ItemIndex + 1,
      programNameEdit.Text, status);

    if status = True then
    begin
      ProgramList.Items[ProgramList.ItemIndex] := programNameEdit.Text;
    end;
  end;
end;

procedure TManagePrograms.btnOkClick(Sender: TObject);
begin
  StationID := -1;
  ProgramID := -1;
  Close;
end;

procedure TManagePrograms.btnSaveRelaysConfigClick(Sender: TObject);
begin
  SaveRelaysConfig();
end;

procedure TManagePrograms.CheckRelaysChanges();
var
  oldRelays, newRelays: RelaysInfo;
  relayCount, i, j: integer;
  status, changes: boolean;
begin

  relayCount := 0;
  for i := 0 to MAX_RELAYS - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        relayCount := relayCount + 1;
      end;
    end;
  end;
  setlength(newRelays.relayID, relayCount);
  setlength(newRelays.timeON, relayCount);
  setlength(newRelays.timeOFF, relayCount);
  setlength(newRelays.preflight, relayCount);
  newRelays.Count := relayCount;
  j := 0;
  for i := 0 to MAX_RELAYS - 1 do
  begin
    with configs[i] do
    begin
      if RelayTrigger.Checked then
      begin
        newRelays.relayID[j] := i + 1;
        newRelays.timeON[j] := StrToInt(RelayOnTime.Text);
        newRelays.timeOFF[j] := StrToInt(RelayOffTime.Text);
        newRelays.preflight[j] := StrToInt(RelayMsec.Text);
        j := j + 1;
      end;
    end;
  end;

  status := client.GetProgramRelays(StationID, ProgramID, oldRelays);
  changes := False;
  if status then
  begin
    if oldRelays.Count <> newRelays.Count then
    begin
      changes := True;
    end
    else
    begin
      for i := 0 to newRelays.Count - 1 do
      begin
        if oldRelays.relayID[i] <> newRelays.relayID[i] then
        begin
          changes := True;
          break;
        end;
      end;
    end;

    if changes then
    begin
      case QuestionDLG('Save changes?', 'Program ' + IntToStr(ProgramID) + ' changed' +
          sLineBreak + 'Save new program?', mtCustom,
          [mrYes, 'Save', mrNo, 'Don`t Save'], '') of
        mrYes: SaveRelaysConfig();
      end;
    end;
  end;

end;

procedure TManagePrograms.FormShow(Sender: TObject; ID: integer; stationName: string);
begin
  Show;
  StationInfo.Caption := 'Station ID: ' + IntToStr(ID) + ' - ' + stationName;
  StationID := ID;
  PrepareRelaysConfig();
  PrepareProgramList();
  btnCancelProgramNameClick(self);
end;


end.
