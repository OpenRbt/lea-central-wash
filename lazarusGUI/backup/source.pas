unit Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, EditBtn, ComboEx, DateTimePicker,
  fphttpclient, Fpjson, jsonparser, superobject, Types, manager, collection;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnManage: TButton;
    btnDay: TButton;
    btnMoneyCollection: TButton;
    btnWeek: TButton;
    btnMonth: TButton;
    btnYear: TButton;
    cbIsBlocked: TCheckBox;
    cbMoneyRealTime: TCheckBox;
    dtFrom: TDateTimePicker;
    dtTo: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    StationsData: TStringGrid;
    MoneyData: TStringGrid;
    UpdateTimer: TTimer;
    procedure btnDayClick(Sender: TObject);
    procedure btnManageClick(Sender: TObject);
    procedure btnMoneyCollectionClick(Sender: TObject);
    procedure btnMonthClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnWeekClick(Sender: TObject);
    procedure btnYearClick(Sender: TObject);
    procedure cbIsBlockedChange(Sender: TObject);
    procedure dtFromEditingDone(Sender: TObject);
    procedure dtToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateStations(Sender: TObject);
    procedure StationsDataDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure UpdateCall(Sender: TObject);

    procedure PairIdAndHash(Hash, StationName: String; ID: integer; Sender: TObject);
    procedure LoadMoney(ID: integer; Sender: TObject);

  private

  public
    property GetStationsData: TStringGrid read StationsData;

  end;

var
  MainForm: TMainForm;
  RequestAnswer: string;

  // This array stores color ID-s of fixed column
  // 0 - None, 1 - Green, 2 - Red
  CellColor: array [1..12] of integer;

  AvailableHashes: TStringList;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadMoney(ID: integer; Sender: TObject);
var
  Data: ISuperObject;
  Report: ISuperObject;
  postJson: TJSONObject;
  unixFrom: Longint;
  unixTo:   Longint;
  totalMoney: integer;

const
  UnixStartDate: TDateTime = 25569.0;

begin
  postJson := TJSONObject.Create;
  postJson.Add('id', ID);

  unixFrom := Round((dtFrom.DateTime - UnixStartDate) * 86400);

  if cbMoneyRealTime.Checked then
  begin
     unixTo := Round((Now() - UnixStartDate) * 86400);
  end
  else
  begin
     unixTo := Round((dtTo.DateTime - UnixStartDate) * 86400);
  end;

  postJson.Add('startDate', unixFrom);
  postJson.Add('endDate', unixTo);

  With TFPHttpClient.Create(Nil) do
  try
    try
     AddHeader('Content-Type', 'application/json');
     RequestBody := TStringStream.Create(postJson.AsJSON);
     RequestAnswer := Post('http://localhost:8020/station-report');
     Data := SO(UTF8Decode(RequestAnswer));

     if Data.S['moneyReport'] <> '' then
     begin
          Report := Data.O['moneyReport'];

          totalMoney := 0;
          if Report.AsObject.Exists('coins') then
          begin
               totalMoney := totalMoney + Report.I['coins'];
               MoneyData.Cells[2, ID] := IntToStr(Report.I['coins']);
          end
          else
          begin
               MoneyData.Cells[2, ID] := '0';
          end;

          if Report.AsObject.Exists('carsTotal') then
          begin
               MoneyData.Cells[4, ID] := IntToStr(Report.I['carsTotal']);
          end
          else
          begin
               MoneyData.Cells[4, ID] := '0';
          end;

          if Report.AsObject.Exists('service') then
          begin
               MoneyData.Cells[5, ID] := IntToStr(Report.I['service']);
          end
          else
          begin
               MoneyData.Cells[5, ID] := '0';
          end;

          if Report.AsObject.Exists('banknotes') then
          begin
               totalMoney := totalMoney + Report.I['banknotes'];
               MoneyData.Cells[3, ID] := IntToStr(Report.I['banknotes']);
          end
          else
          begin
               MoneyData.Cells[3, ID] := '0';
          end;

          if Report.AsObject.Exists('electronical') then
          begin
               totalMoney := totalMoney + Report.I['electronical'];
               MoneyData.Cells[1, ID] := IntToStr(Report.I['electronical']);
          end
          else
          begin
               MoneyData.Cells[1, ID] := '0';
          end;
          MoneyData.Cells[0, ID] := IntToStr(totalMoney);
     end
     else
     begin
         MoneyData.Cells[0, ID] := '0';
         MoneyData.Cells[1, ID] := '0';
         MoneyData.Cells[2, ID] := '0';
         MoneyData.Cells[3, ID] := '0';
         MoneyData.Cells[4, ID] := '0';
         MoneyData.Cells[5, ID] := '0';
     end;
    except
        On E: Exception do
      end;
  finally
     Free;
  end;

end;

procedure TMainForm.PairIdAndHash(Hash, StationName: String; ID: integer; Sender: TObject);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('id', ID);
  postJson.Add('hash', TJSONString.Create(Hash));
  postJson.Add('name', TJSONString.Create(StationName));
  With TFPHttpClient.Create(Nil) do
  try
   try
     AddHeader('Content-Type', 'application/json');
     RequestBody := TStringStream.Create(postJson.AsJSON);
     Post('http://localhost:8020/set-station');
   except
        On E: Exception do
      end;
  finally
     Free;
  end;

end;


procedure TMainForm.UpdateStations(Sender: TObject);
var
  Data: ISuperObject;
  Station: ISuperObject;
  Stations: TSuperArray;
  i, pos, findRes: integer;

begin
  with TFPHttpClient.Create(nil) do
    try
      try
        MainForm.Cursor := crHourGlass;
        StationsData.Cursor := crHourGlass;
        MoneyData.Cursor := crHourGlass;
        btnManage.Cursor := crHourGlass;
        btnMoneyCollection.Cursor := crHourGlass;

        // Disable controls in case of failure
        btnManage.Enabled := False;
        btnMoneyCollection.Enabled := False;

        RequestAnswer := Get('http://localhost:8020/status');
        Data := SO(UTF8Decode(RequestAnswer));

        // Check the null data in stations array
        if Data.S['stations'] <> '' then
        begin
          Stations := Data.A['stations'];

          for i := 0 to 12 do
            CellColor[i] := 0;

          // Iterate over all incoming stations from server
          for i := 1 to Stations.Length do
          begin
            Station := Stations.O[i - 1];

            // If the station has ID
            if Station.AsObject.Exists('id') then
            begin
              pos := StrToInt(Station.s['id']);

              LoadMoney(pos, Sender);

              // GENERAL DATA
              // Paint the cell, depending on status message
              if Station.s['status'] = 'offline' then
              begin
                   CellColor[pos] := 2;
              end;
              if Station.s['status'] = 'online' then
              begin
                 CellColor[pos] := 1;
              end;

              StationsData.Cells[2, pos] := Station.s['status'];

              if Station.AsObject.Exists('name') then
              begin
                 StationsData.Cells[3, pos] := Station.s['name'];
              end;
              // END OF GENERAL DATA

              btnManage.Enabled := True;
              btnMoneyCollection.Enabled := True;

              // ID and Hash both exist => station is already paired
              // We need to add it's ID to list and choose the Hash by default
              if Station.AsObject.Exists('hash') then
              begin
                   findRes := AvailableHashes.IndexOf(Station.s['hash']);

                   // Check that this hash is new in the list
                   if findRes < 0 then begin
                      AvailableHashes.Add(Station.s['hash']);
                   end;
              end;
            end
            else
            begin
              // If only Hash exists - add this data to the list
              if Station.AsObject.Exists('hash') then
              begin
                   findRes := AvailableHashes.IndexOf(Station.s['hash']);

                   // Check that this hash is new in the list
                   if findRes < 0 then begin
                      AvailableHashes.Add(Station.s['hash']);
                      RefreshHashData(Sender);
                   end;
              end;
            end;
            // END OF ADDITIONAL DATA
          end;
        end;
        // END OF ARRAY PARSE
        RefreshHashData(Sender);

      except
        On E: Exception do
      end;
    finally
      Free;
      MainForm.Cursor := crDefault;
      StationsData.Cursor := crDefault;
      MoneyData.Cursor := crDefault;
      btnManage.Cursor := crDefault;
      btnMoneyCollection.Cursor := crDefault;
    end;

  MainForm.Cursor := crDefault;
  StationsData.Cursor := crDefault;
  MoneyData.Cursor := crDefault;
  btnManage.Cursor := crDefault;
  btnMoneyCollection.Cursor := crDefault;
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
  i: integer;
begin
  Sleep(1000);
  s := StationsData.DefaultTextStyle;
  s.Alignment := taCenter;
  StationsData.DefaultTextStyle := s;

  s := MoneyData.DefaultTextStyle;
  s.Alignment := taCenter;
  MoneyData.DefaultTextStyle := s;

  for i := 0 to 12 do
    CellColor[i] := 0;

  AvailableHashes := TStringList.Create;

  btnManage.Enabled := False;
  btnMoneyCollection.Enabled := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateStations(Sender);

  if UpdateTimer.Enabled = False then
    UpdateTimer.Enabled := True;
end;

procedure TMainForm.StationsDataDblClick(Sender: TObject);
var
  selectedRow: integer;
  selectedHash: string;
  selectedID: string;
  selectedName: string;
begin
  selectedRow := StationsData.Row;
  selectedHash := StationsData.Cells[1, selectedRow];
  selectedID := IntToStr(StationsData.Row);
  selectedName := StationsData.Cells[3, selectedRow];

  if selectedHash <> '' then
  begin
    ManageForm.SetHash(selectedHash);
    ManageForm.SetID(selectedID);
    ManageForm.SetName(selectedName);

    ManageForm.ShowModal;
  end;
end;

procedure TMainForm.StationsDataDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  currentColor: integer;
begin
  // We draw colored cells in status column only
  if (aCol = 2) and (aRow <> 0) then
  begin
    currentColor := CellColor[aRow];
    if currentColor = 1 then
    begin
      StationsData.Canvas.Brush.Color := clGreen;
      StationsData.Canvas.FillRect(aRect);
      StationsData.Canvas.TextRect(aRect,
        aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, 'online');
    end;
    if currentColor = 2 then
    begin
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
  selectedRow: integer;
  selectedHash: string;
  selectedID: string;
  selectedName: string;
begin
  selectedRow := StationsData.Row;
  selectedHash := StationsData.Cells[1, selectedRow];
  selectedID := IntToStr(StationsData.Row);
  selectedName := StationsData.Cells[3, selectedRow];

  if selectedHash <> '' then
  begin
    ManageForm.SetHash(selectedHash);
    ManageForm.SetID(selectedID);
    ManageForm.SetName(selectedName);

    ManageForm.ShowModal;
  end;
end;

procedure TMainForm.btnMoneyCollectionClick(Sender: TObject);
begin
    MoneyCollectionForm.ShowModal;
end;

procedure TMainForm.btnMonthClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  cbMoneyRealTime.Checked := False;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth, 1, 0, 0, 0, 0);
  dtTo.DateTime := Now();
end;

procedure TMainForm.btnDayClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  cbMoneyRealTime.Checked := False;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth,
    currentDay, 0, 0, 0, 0);
  dtTo.DateTime := Now();
end;

procedure TMainForm.btnWeekClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay, currentNameOfTheDay: word;
  tmpTime: TDateTime;
begin
  cbMoneyRealTime.Checked := False;
  tmpTime := Now();

  currentNameOfTheDay := DayOfTheWeek(tmpTime);

  while currentNameOfTheDay <> 1 do
  begin
    tmpTime := IncDay(tmpTime, -1);
    currentNameOfTheDay := DayOfTheWeek(tmpTime);
  end;

  DecodeDate(tmpTime, currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth,
    currentDay, 0, 0, 0, 0);
  dtTo.DateTime := Now();
end;

procedure TMainForm.btnYearClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  cbMoneyRealTime.Checked := False;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, 1, 1, 0, 0, 0, 0);
  dtTo.DateTime := Now();
end;

procedure TMainForm.cbIsBlockedChange(Sender: TObject);
begin
     if cbIsBlocked.Checked then
     begin
          DisableAllItems(Sender);
     end
     else
     begin
          EnableAllItems(Sender);
     end;
end;

procedure TMainForm.dtFromEditingDone(Sender: TObject);
begin
  cbMoneyRealTime.Checked := False;
  if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then
  begin
    dtFrom.DateTime := dtTo.DateTime;
  end;
end;

procedure TMainForm.dtToEditingDone(Sender: TObject);
begin
  cbMoneyRealTime.Checked := False;
  if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then
  begin
    dtTo.DateTime := dtFrom.DateTime;
  end;
end;

end.
