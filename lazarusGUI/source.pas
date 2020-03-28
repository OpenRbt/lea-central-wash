unit Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, EditBtn, ComboEx, DateTimePicker,
  fphttpclient, Fpjson, jsonparser, superobject, Types, manager;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnManage: TButton;
    btnDay: TButton;
    btnWeek: TButton;
    btnMonth: TButton;
    btnYear: TButton;
    cbHash1: TCheckComboBox;
    cbHash10: TCheckComboBox;
    cbHash11: TCheckComboBox;
    cbHash12: TCheckComboBox;
    cbHash2: TCheckComboBox;
    cbHash3: TCheckComboBox;
    cbHash4: TCheckComboBox;
    cbHash5: TCheckComboBox;
    cbHash6: TCheckComboBox;
    cbHash7: TCheckComboBox;
    cbHash8: TCheckComboBox;
    cbHash9: TCheckComboBox;
    dtFrom: TDateTimePicker;
    dtTo: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    Logo: TImage;
    Memo1: TMemo;
    StationsData: TStringGrid;
    MoneyData: TStringGrid;
    UpdateTimer: TTimer;
    procedure btnDayClick(Sender: TObject);
    procedure btnManageClick(Sender: TObject);
    procedure btnMonthClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnWeekClick(Sender: TObject);
    procedure btnYearClick(Sender: TObject);
    procedure dtFromEditingDone(Sender: TObject);
    procedure dtToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StationsDataDblClick(Sender: TObject);
    procedure StationsDataSelection(Sender: TObject; aCol, aRow: Integer);
    procedure UpdateStations(Sender: TObject);
    procedure StationsDataDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure UpdateCall(Sender: TObject);


  private

  public
    property GetStationsData : TStringGrid read StationsData;

  end;

var
  MainForm: TMainForm;
  RequestAnswer: String;

  // This array stores color ID-s of fixed column
  // 0 - None, 1 - Green, 2 - Red
  CellColor: array [1..12] of Integer;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.UpdateStations(Sender: TObject);
var
    Data: ISuperObject;
    Station: ISuperObject;
    Stations: TSuperArray;
    i, pos: Integer;

begin
    With TFPHttpClient.Create(Nil) do
    try
    try
      MainForm.Cursor := crHourGlass;
      StationsData.Cursor := crHourGlass;
      btnManage.Cursor := crHourGlass;

      RequestAnswer := Get('http://localhost:8020/status');
      Memo1.Text := RequestAnswer;
      Data := SO(UTF8Decode(RequestAnswer));

      btnManage.Enabled := False;

      // Check the null data in array stations
      if Data.S['stations'] <> '' then begin
         btnManage.Enabled := True;

         Stations := Data.A['stations'];

         for i := 0 to 12 do
             CellColor[i] := 0;

         // Iterate over all incoming stations from server
         for i := 1 to Stations.Length do begin
             Station := Stations.O[i-1];

             // We can show data from pre-created stations only
             if Station.AsObject.Exists('id') then begin
                pos := StrToInt(Station.s['id']);

                StationsData.Cells[1,pos] := Station.s['hash'];

                if Station.s['status'] = 'offline' then begin
                   CellColor[pos] := 2;
                end;
                if Station.s['status'] = 'online' then begin
                   CellColor[pos] := 1;
                end;

                StationsData.Cells[2,pos] := Station.s['status'];

                if Station.AsObject.Exists('name') then begin
                   StationsData.Cells[4,i] := Station.s['name'];
                end;
             end;
         end;
      end;
     except
    On E: Exception Do
            End;
    finally
      Free;
      MainForm.Cursor := crDefault;
      StationsData.Cursor := crDefault;
      btnManage.Cursor := crDefault;
    end;

    MainForm.Cursor := crDefault;
    StationsData.Cursor := crDefault;
    btnManage.Cursor := crDefault;
end;

procedure TMainForm.btnUpdateClick(Sender: TObject);
begin
    UpdateStations(Sender);

    if UpdateTimer.Enabled = False then
       UpdateTimer.Enabled := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
    s: TTextStyle;
    i: Integer;
begin
  //Sleep(10000);
  s := StationsData.DefaultTextStyle;
  s.Alignment := taCenter;
  StationsData.DefaultTextStyle := s;

  s := MoneyData.DefaultTextStyle;
  s.Alignment := taCenter;
  MoneyData.DefaultTextStyle := s;

  for i := 0 to 12 do
      CellColor[i] := 0;

  cbHash1.Color := clHighLight;

  btnManage.Enabled := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    UpdateStations(Sender);

    if UpdateTimer.Enabled = False then
       UpdateTimer.Enabled := True;
end;

procedure TMainForm.StationsDataDblClick(Sender: TObject);
var
    selectedRow: Integer;
    selectedHash: String;
    selectedID: String;
    selectedName: String;
begin
    selectedRow := StationsData.Row;
    selectedHash := StationsData.Cells[1, selectedRow];
    selectedID := StationsData.Cells[3, selectedRow];
    selectedName := StationsData.Cells[4, selectedRow];

    if selectedHash <> '' then begin
       ManageForm.SetHash(selectedHash);
       ManageForm.SetID(selectedID);
       ManageForm.SetName(selectedName);

       ManageForm.ShowModal;
    end;
end;

procedure TMainForm.StationsDataSelection(Sender: TObject; aCol, aRow: Integer);
begin
  // This is real shit, need to replace that
  cbHash1.Color:=clDefault;
  cbHash2.Color:=clDefault;
  cbHash3.Color:=clDefault;
  cbHash4.Color:=clDefault;
  cbHash5.Color:=clDefault;
  cbHash6.Color:=clDefault;
  cbHash7.Color:=clDefault;
  cbHash8.Color:=clDefault;
  cbHash9.Color:=clDefault;
  cbHash10.Color:=clDefault;
  cbHash11.Color:=clDefault;
  cbHash12.Color:=clDefault;

  case StationsData.Row of
  1: cbHash1.Color:=clHighLight;
  2: cbHash2.Color:=clHighLight;
  3: cbHash3.Color:=clHighLight;
  4: cbHash4.Color:=clHighLight;
  5: cbHash5.Color:=clHighLight;
  6: cbHash6.Color:=clHighLight;
  7: cbHash7.Color:=clHighLight;
  8: cbHash8.Color:=clHighLight;
  9: cbHash9.Color:=clHighLight;
  10:cbHash10.Color:=clHighLight;
  11:cbHash11.Color:=clHighLight;
  12:cbHash12.Color:=clHighLight;
  end;
end;

procedure TMainForm.StationsDataDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
    currentColor: Integer;
begin
   // We draw colored cells in status column only
   if (aCol = 2) and (aRow <> 0) then begin
       currentColor := CellColor[aRow];
       if currentColor = 1 then begin
          StationsData.Canvas.Brush.Color := clGreen;
          StationsData.Canvas.FillRect(aRect);
          StationsData.Canvas.TextRect(aRect,
          aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, 'online');
       end;
       if currentColor = 2 then begin
          StationsData.Canvas.Brush.Color := clRed;
          StationsData.Canvas.FillRect(aRect);
          StationsData.Canvas.TextRect(aRect,
          aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, 'offline');
       end;
   end;
end;

procedure TMainForm.UpdateCall(Sender: TObject);
begin
     UpdateStations(Sender);
end;

procedure TMainForm.btnManageClick(Sender: TObject);
var
    selectedRow: Integer;
    selectedHash: String;
    selectedID: String;
    selectedName: String;
begin
    selectedRow := StationsData.Row;
    selectedHash := StationsData.Cells[1, selectedRow];
    selectedID := StationsData.Cells[3, selectedRow];
    selectedName := StationsData.Cells[4, selectedRow];

    if selectedHash <> '' then begin
       ManageForm.SetHash(selectedHash);
       ManageForm.SetID(selectedID);
       ManageForm.SetName(selectedName);

       ManageForm.ShowModal;
    end;
end;

procedure TMainForm.btnMonthClick(Sender: TObject);
var
   currentYear, currentMonth, currentDay: Word;
begin
     DecodeDate(Now(), currentYear, currentMonth, currentDay);
     dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth, 1, 0, 0, 0, 0);
     dtTo.DateTime := Now();
end;

procedure TMainForm.btnDayClick(Sender: TObject);
var
   currentYear, currentMonth, currentDay: Word;
begin
     DecodeDate(Now(), currentYear, currentMonth, currentDay);
     dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth, currentDay, 0, 0, 0, 0);
     dtTo.DateTime := Now();
end;

procedure TMainForm.btnWeekClick(Sender: TObject);
var
   currentYear, currentMonth, currentDay, currentNameOfTheDay: Word;
   tmpTime: TDateTime;
begin
     tmpTime := Now();

     currentNameOfTheDay := DayOfTheWeek(tmpTime);

     while currentNameOfTheDay <> 1 do begin
           tmpTime := IncDay(tmpTime, -1);
           currentNameOfTheDay := DayOfTheWeek(tmpTime);
     end;

     DecodeDate(tmpTime, currentYear, currentMonth, currentDay);
     dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth, currentDay, 0, 0, 0, 0);
     dtTo.DateTime := Now();
end;

procedure TMainForm.btnYearClick(Sender: TObject);
var
   currentYear, currentMonth, currentDay: Word;
begin
     DecodeDate(Now(), currentYear, currentMonth, currentDay);
     dtFrom.DateTime := EncodeDateTime(currentYear, 1, 1, 0, 0, 0, 0);
     dtTo.DateTime := Now();
end;

procedure TMainForm.dtFromEditingDone(Sender: TObject);
begin
   if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then begin
      dtFrom.DateTime := dtTo.DateTime;
   end;
end;

procedure TMainForm.dtToEditingDone(Sender: TObject);
begin
    if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then begin
      dtTo.DateTime := dtFrom.DateTime;
   end;
end;

end.

