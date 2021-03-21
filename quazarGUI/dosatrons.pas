unit dosatrons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, Types, fphttpclient, Fpjson, jsonparser;

type

  DosatronInfo = packed record
    Count    : integer;
    Name     : array of string;
    RelayID  : array of integer;
    ProgramID: array of integer;
    Preflight: array of boolean;

  end;

  { TDosatronsForm }

  TDosatronsForm = class(TBaseForm)
    Cancel: TButton;
    DosatronsGrid: TStringGrid;
    Save: TButton;
    UpdateTimer: TTimer;
    procedure CancelClick(Sender: TObject);
    procedure DosatronsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DosatronsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure SaveClick(Sender: TObject);
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure UpdateCall(Sender: TObject);
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;

    procedure DosatronsMouseEnter(Sender: TObject); override;
    procedure DosatronsMouseLeave(Sender: TObject); override;

    procedure ShowDosatrons();
    procedure RunProgram();

  private
    _currProgram: integer;

  public

  end;

var
  DosatronsForm: TDosatronsForm;
  DosatronParams: DosatronInfo;

const
  NUM_DOSATRONS         : integer = 4;
  DOSATRON_TIMEON_CHANGE: integer = 5;

  DOSATRON_NAME_COL     : integer = 0;
  DOSATRON_DEC          : integer = 1;
  DOSATRON_VAL_COL      : integer = 2;
  DOSATRON_VAL_LABEL_COL: integer = 3;
  DOSATRON_INC          : integer = 4;
  DOSATRON_SPACER_COL   : integer = 5;
  DOSATRON_PREFLIGHT_COL: integer = 6;

  PREFLIGHT_STR : string = 'ПРОКАЧКА';
  PADDING     : string = '  ';

  STATION_ID : integer = 2;

implementation

{$R *.lfm}

procedure TDosatronsForm.FormCreate(Sender: TObject);
begin
  DosatronParams.Count := NUM_DOSATRONS;
  setlength(DosatronParams.Name, DosatronParams.Count);
  DosatronParams.Name[0] := 'АКТИВНАЯ ПЕНА';
  DosatronParams.Name[1] := 'ШАМПУНЬ';
  DosatronParams.Name[2] := 'ВОСК';
  DosatronParams.Name[3] := 'ПОЛИМЕР';

  setlength(DosatronParams.RelayID, DosatronParams.Count);
  DosatronParams.RelayID[0] := FOAM_RELAY_ID;
  DosatronParams.RelayID[1] := SHAMPOO_RELAY_ID;
  DosatronParams.RelayID[2] := WAX_RELAY_ID;
  DosatronParams.RelayID[3] := POLYMER_RELAY_ID;

  setlength(DosatronParams.ProgramID, DosatronParams.Count);
  DosatronParams.ProgramID[0] := FOAM_PREFLIGHT_PROGRAM_ID;
  DosatronParams.ProgramID[1] := SHAMPOO_PREFLIGHT_PROGRAM_ID;
  DosatronParams.ProgramID[2] := WAX_PREFLIGHT_PROGRAM_ID;
  DosatronParams.ProgramID[3] := POLYMER_PREFLIGHT_PROGRAM_ID;

  setlength(DosatronParams.Preflight, DosatronParams.Count);
  DosatronParams.Preflight[0] := false;
  DosatronParams.Preflight[1] := false;
  DosatronParams.Preflight[2] := false;
  DosatronParams.Preflight[3] := false;

  _currProgram := NO_ID;

  UpdateTimer.Interval := 1000;
  UpdateTimer.Enabled  := false;
end;

procedure TDosatronsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  inherited;
end;

procedure TDosatronsForm.CancelClick(Sender: TObject);
begin
  FormShow(Sender);
end;

procedure TDosatronsForm.DosatronsGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = DOSATRON_PREFLIGHT_COL) then
  begin
    DosatronsGrid.Canvas.Pen.Width:=2;
    DosatronsGrid.Canvas.Pen.Color:=clHighlight;
    DosatronsGrid.Canvas.Font.Color := clHighlight;
    DosatronsGrid.Canvas.Rectangle(aRect.TopLeft.x+1, aRect.TopLeft.y + 10, aRect.BottomRight.x-10, aRect.BottomRight.y - 10);
    if DosatronParams.Preflight[aRow] then
    begin
      DosatronsGrid.Canvas.Brush.Color := clHighlight;
      DosatronsGrid.Canvas.Font.Color := clWindow;
      DosatronsGrid.Canvas.FillRect(aRect.TopLeft.x+1, aRect.TopLeft.y + 10, aRect.BottomRight.x-10, aRect.BottomRight.y - 10);
    end;
    DosatronsGrid.Canvas.TextRect(aRect, aRect.Left+25, aRect.Top + 2, PREFLIGHT_STR);
  end;
end;

procedure TDosatronsForm.DosatronsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  clickedProgram: integer;
  i: integer;
begin
  if (aCol = DOSATRON_PREFLIGHT_COL) then
  begin
    for i := 0 to DosatronsGrid.RowCount - 1 do
    begin
      if i <> aRow then
      begin
        DosatronParams.Preflight[i] := false;
      end;
    end;
    DosatronParams.Preflight[aRow] := not DosatronParams.Preflight[aRow];
    clickedProgram := DosatronParams.ProgramID[aRow];
    if clickedProgram = _currProgram then
    begin
      _currProgram := NO_ID;
      UpdateTimer.Enabled := false;
      RunProgram();
    end
    else
    begin
      _currProgram := clickedProgram;
      UpdateTimer.Enabled := false;//true;
      RunProgram();
    end;
  end
  else if (aCol = DOSATRON_DEC) then
  begin
    SetRelayTimeOn(DosatronParams.RelayID[aRow], GetRelayTimeOn(DosatronParams.RelayID[aRow]) - DOSATRON_TIMEON_CHANGE * TOTAL_TIME div 100);
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_VAL_COL, aRow] := IntToStr(100 * GetRelayTimeOn(DosatronParams.RelayID[aRow]) div TOTAL_TIME);
  end
  else if (aCol = DOSATRON_INC) then
  begin
    SetRelayTimeOn(DosatronParams.RelayID[aRow], GetRelayTimeOn(DosatronParams.RelayID[aRow]) + DOSATRON_TIMEON_CHANGE * TOTAL_TIME div 100);
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_VAL_COL, aRow] := IntToStr(100 * GetRelayTimeOn(DosatronParams.RelayID[aRow]) div TOTAL_TIME);
  end;
end;

procedure TDosatronsForm.FormShow(Sender: TObject);
begin
  DosatronsForm.Dosatrons.Font.Color := DosatronsForm.GetHoverColor();
  NotAuthorized.Visible:=False;
  DosatronsGrid.Visible:=True;
  DosatronsGrid.Enabled:=True;
  Save.Enabled:=True;
  Cancel.Enabled:=True;

  ShowDosatrons();
end;

procedure TDosatronsForm.ShowDosatrons();
var
  i: integer;
begin
  CheckPrograms();
  for i:=0 to NUM_DOSATRONS-1 do
  begin
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_NAME_COL, i] := PADDING + DosatronParams.Name[i];
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_INC, i] := '+';
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_VAL_COL, i] := IntToStr(100 * GetRelayTimeOn(DosatronParams.RelayID[i]) div TOTAL_TIME);
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_DEC, i] := '-';
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_VAL_LABEL_COL, i] := '%';
    DosatronsForm.DosatronsGrid.Cells[DOSATRON_PREFLIGHT_COL, i] := PADDING + PREFLIGHT_STR;
  end;
end;

procedure TDosatronsForm.MainClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.SaveClick(Sender: TObject);
begin
  UpdatePrograms();
  ShowDosatrons();
end;

procedure TDosatronsForm.StationsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
end;

procedure TDosatronsForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.RunProgram();
begin
  SetStationCurrentProgramByID(_currProgram, STATION_ID, false);
end;

procedure TDosatronsForm.UpdateCall(Sender: TObject);
begin
  RunProgram();
end;

procedure TDosatronsForm.UsersClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  DosatronsForm.Hide;
end;

procedure TDosatronsForm.DosatronsMouseEnter(Sender: TObject);
begin

end;

procedure TDosatronsForm.DosatronsMouseLeave(Sender: TObject);
begin

end;

end.

