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
    labelRelayActive: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
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
    procedure btnRevertRelayConfigClick(Sender: TObject);
    procedure btnSaveProgramNameClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSaveRelaysConfigClick(Sender: TObject);
    procedure ProgramListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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


implementation

{$R *.lfm}

{ TManagePrograms }
procedure PrepareProgramList(ProgramList : TListBox);
var
  i : integer;
  programsCount : integer;
  begin
    ProgramList.Items.Clear;
    programsCount := 9;

    for i:= 0 to programsCount-1 do
    begin
      ProgramList.Items.Add('ProgramName ' + IntToStr(i));
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
      Top := 0 + 60 * i;
      Height := 60;
    end;

    with configs[i].RelayTrigger do
    begin
      Parent := configs[i].RelayBox;
      Width := 40;
      Height := 30;
      Left := 10;
      Top := 5;
    end;

    with configs[i].RelayOnTime do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 45;
      Top := 0;

    end;

    with configs[i].RelayOffTime do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 135;
      Top := 0;

    end;

    with configs[i].RelayMsec do
    begin
      Parent := configs[i].RelayBox;
      Width := 80;
      Height := 30;
      Left := 225;
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
  ShowMessage('Saved Relays Configuration');
  //Send program I relays request

end;

procedure TManagePrograms.ProgramListClick(Sender: TObject);
var
  i: integer;
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


