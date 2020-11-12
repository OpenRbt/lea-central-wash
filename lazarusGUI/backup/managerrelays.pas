unit managerRelays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TManagePrograms }

  TManagePrograms = class(TForm)
    btnOk: TButton;
    btnSaveProgramName: TButton;
    btnCancelProgramName: TButton;
    programNameEdit: TEdit;
    GroupBox1: TGroupBox;
    RelayBoxTMPLT: TGroupBox;
    relayActive: TCheckBox;
    relayON: TEdit;
    relayOff: TEdit;
    relayMSEC: TEdit;
    ProgramList: TListBox;
    RelayListBox: TScrollBox;
    procedure btnCancelProgramNameClick(Sender: TObject);
    procedure btnSaveProgramNameClick(Sender: TObject);
    procedure ProgramListClick(Sender: TObject);
  private

  public

  end;

  RelayConfigHeader = packed record
    ActiveLabel: TLabel;
    TimeOnLabel: TLabel;
    TimeOffLabel: TLabel;
    TimeMsecLabel: TLabel;
  end;

  RelayConfig = packed record
    RelayBox: TGroupBox;
    RelayTrigger: TCheckBox;
    RelayOnTime: TEdit;
    RelayOffTime: TEdit;
    RelayMsec: TEdit;
  end;

var
  ManagePrograms: TManagePrograms;
  RelaysCount: integer;
  configs: array of RelayConfig;
  Header: RelayConfigHeader;


implementation

{$R *.lfm}

{ TManagePrograms }

procedure PrepareRelaysConfig(RelayListBox: TScrollBox);
var
  i: integer;
begin
  //Get Relays count request
  if Assigned(configs) then
  begin
    Header.ActiveLabel.Free;
    Header.TimeOnLabel.Free;
    Header.TimeOffLabel.Free;
    Header.TimeMsecLabel.Free;
    if Length(configs) > 0 then
    begin
      for i := Length(configs) - 1 to 0 do
      begin
        with configs[i] do
        begin
          RelayBox.Free;
          RelayTrigger.Free;
          RelayOnTime.Free;
          RelayOffTime.Free;
          RelayMsec.Free;
        end;
      end;
      Finalize(configs);
      //Dispose(configs);
    end;
  end;

  Header.ActiveLabel := TLabel.Create(nil);
  with Header.ActiveLabel do
  begin
    Caption := 'Active';
    Parent := RelayListBox;
    Width := 80;
    Height := 30;
    Top := 0;
    Left := 0;
  end;
  Header.TimeOnLabel := TLabel.Create(nil);
  with Header.TimeOnLabel do
  begin
    Caption := 'TimeOn';
    Parent := RelayListBox;
    Width := 80;
    Height := 30;
    Top := 0;
    Left := 80;
  end;
  Header.TimeOffLabel := TLabel.Create(nil);
  with Header.TimeOffLabel do
  begin
    Caption := 'Time Off';
    Parent := RelayListBox;
    Width := 80;
    Height := 30;
    Top := 0;
    Left := 160;
  end;
  Header.TimeMsecLabel := TLabel.Create(nil);
  with Header.TimeMsecLabel do
  begin
    Caption := 'TimeMsec';
    Parent := RelayListBox;
    Width := 80;
    Height := 30;
    Top := 0;
    Left := 240;
  end;

  RelaysCount := 17;
  SetLength(configs, RelaysCount);
  for i := 0 to 16 do
  begin
    configs[i].RelayBox := TGroupBox.Create(nil);
    configs[i].RelayTrigger := TCheckBox.Create(nil);
    configs[i].RelayOnTime := TEdit.Create(nil);
    configs[i].RelayOffTime := TEdit.Create(nil);
    configs[i].RelayMsec := TEdit.Create(nil);
    //Relay Config Creation
    with configs[i].RelayBox do
    begin
      Caption := 'Relay ' + IntToStr(i);
      Parent := RelayListBox;
      Left := 0;
      Width := 390;
      Top := 20 + 60 * i;
      Height := 60;
    end;

    with configs[i].RelayTrigger do
    begin
      Parent := configs[i].RelayBox;
      Width := 40;
      Height := 30;
      Left := 20;
      Top := 5;
    end;

    with configs[i].RelayOnTime do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 80;
      Top := 0;

    end;

    with configs[i].RelayOffTime do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 160;
      Top := 0;

    end;

    with configs[i].RelayMsec do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 240;
      Top := 0;

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
  //Send program I relays request

end;

procedure TManagePrograms.ProgramListClick(Sender: TObject);
var
  i: integer;
begin
  PrepareRelaysConfig(RelayListBox);
  LoadRelaysConfig();

  if ProgramList.ItemIndex <> -1 then
  begin
    programNameEdit.Caption := ProgramList.Items[ProgramList.ItemIndex];
  end;

end;

procedure TManagePrograms.btnCancelProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    programNameEdit.Caption := ProgramList.Items[ProgramList.ItemIndex];
  end;

end;

procedure TManagePrograms.btnSaveProgramNameClick(Sender: TObject);
begin
  if ProgramList.ItemIndex <> -1 then
  begin
    ProgramList.Items[ProgramList.ItemIndex] := programNameEdit.Caption;
  end;
  //Save programName request
end;



end.
