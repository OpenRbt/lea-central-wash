unit Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, EditBtn, ComboEx, DateTimePicker,
  fphttpclient, Fpjson, jsonparser, superobject, Types, manager, collection, clientAPI,settingsKasse;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnManage: TButton;
    btnDay: TButton;
    btnMoneyCollection: TButton;
    btnWeek: TButton;
    btnMonth: TButton;
    btnYear: TButton;
    btnKasseSetting: TButton;
    dtFrom: TDateTimePicker;
    dtTo: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    lbStatus: TLabel;
    rgReportType: TRadioGroup;
    StationsData: TStringGrid;
    MoneyData: TStringGrid;
    UpdateTimer: TTimer;
    procedure btnDayClick(Sender: TObject);
    procedure btnKasseSettingClick(Sender: TObject);
    procedure btnManageClick(Sender: TObject);
    procedure btnMoneyCollectionClick(Sender: TObject);
    procedure btnMonthClick(Sender: TObject);
    procedure btnWeekClick(Sender: TObject);
    procedure btnYearClick(Sender: TObject);
    procedure dtFromEditingDone(Sender: TObject);
    procedure dtToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StationsDataDblClick(Sender: TObject);
    procedure UpdateStations(Sender: TObject);
    procedure StationsDataDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure UpdateCall(Sender: TObject);

    procedure LoadMoney(ID: integer; Sender: TObject);

  private

  public
    property GetStationsData: TStringGrid read StationsData;

  end;

var
  MainForm: TMainForm;
  RequestAnswer: string;
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
  url: string;

const
  UnixStartDate: TDateTime = 25569.0;

begin
  postJson := TJSONObject.Create;
  postJson.Add('id', ID);

  unixFrom := Round((dtFrom.DateTime - UnixStartDate) * 86400);
  unixTo := Round((dtTo.DateTime - UnixStartDate) * 86400);

  // API requires UTC time; UTC = LocalTime + GetLocalTimeOffset
  postJson.Add('startDate', unixFrom + GetLocalTimeOffset() * 60);
  postJson.Add('endDate',   unixTo   + GetLocalTimeOffset() * 60);
  if rgReportType.ItemIndex = 1 then url:= 'http://localhost:8020/station-report-dates'
                  else url:= 'http://localhost:8020/station-report-current-money';
  With TFPHttpClient.Create(Nil) do
  try
    try
     AddHeader('Content-Type', 'application/json');
     RequestBody := TStringStream.Create(postJson.AsJSON);
     RequestAnswer := Post(url);
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

          // Iterate over all incoming stations from server
          for i := 1 to Stations.Length do
          begin
            Station := Stations.O[i - 1];

            // If the station has ID
            if Station.AsObject.Exists('id') then
            begin
              pos := StrToInt(Station.s['id']);

              LoadMoney(pos, Sender);

              StationsData.Cells[2, pos] := Station.s['status'];
              if Station.AsObject.Exists('name') then
              begin
                 StationsData.Cells[3, pos] := Station.s['name'];
              end;
              if Station.AsObject.Exists('hash') then
                 StationsData.Cells[1, pos] := Station.s['hash']
                 else StationsData.Cells[1, pos] := ' ';

              // END OF GENERAL DATA

              // Make controls in the specified line active
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
                   end;
              end;
            end;
            // END OF ADDITIONAL DATA
          end;
        end;
        // END OF ARRAY PARSE

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

procedure TMainForm.FormCreate(Sender: TObject);
var
  s: TTextStyle;
begin
  Sleep(1000);
  s := StationsData.DefaultTextStyle;
  s.Alignment := taCenter;
  StationsData.DefaultTextStyle := s;

  s := MoneyData.DefaultTextStyle;
  s.Alignment := taCenter;
  MoneyData.DefaultTextStyle := s;

  AvailableHashes := TStringList.Create;

  btnManage.Enabled := False;
  btnMoneyCollection.Enabled := True;
  btnDayClick(Sender);
  rgReportType.ItemIndex:=0;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateCall(Sender);
  UpdateTimer.Enabled := True;
end;

procedure TMainForm.StationsDataDblClick(Sender: TObject);
begin
  if btnManage.Enabled then btnManageClick(Sender);
end;

procedure TMainForm.StationsDataDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  status: string;
begin
  // We draw colored cells in status column only
  if (aCol = 2) and (aRow <> 0) then
  begin
    status := StationsData.Cells[2,aRow];
    if status = 'online' then
    begin
      StationsData.Canvas.Brush.Color := clGreen;
      StationsData.Canvas.FillRect(aRect);
      StationsData.Canvas.TextRect(aRect,
        aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, 'online');
    end;
    if status = 'offline' then
    begin
      StationsData.Canvas.Brush.Color := clRed;
      StationsData.Canvas.FillRect(aRect);
      StationsData.Canvas.TextRect(aRect,
       aRect.Left + (aRect.Right - aRect.Left) div 2, aRect.Top + 2, 'offline');
    end;
  end;
end;

procedure TMainForm.UpdateCall(Sender: TObject);
var
  info: String;
begin
  UpdateTimer.Enabled := False;
  info := client.Info();
  if info <> '' then begin
    lbStatus.Caption:= 'Connected: ' + info;
    lbStatus.Font.Color:=clGreen;
  end else begin
    lbStatus.Caption:= 'Disconnected';
    lbStatus.Font.Color:=clRed;
    btnManage.Enabled:= false;
    btnMoneyCollection.Enabled:= false;
    UpdateTimer.Enabled := True;
    exit;
  end;
  UpdateStations(Sender);
  UpdateTimer.Enabled := True;
end;

procedure TMainForm.btnManageClick(Sender: TObject);
var
  selectedRow: integer;
  selectedHash: string;
  selectedName: string;
begin
  UpdateTimer.Enabled:=false;
  selectedRow := StationsData.Row;
  selectedHash := trim(StationsData.Cells[1, selectedRow]);
  selectedName := StationsData.Cells[3, selectedRow];

    ManageForm.SetHash(selectedHash);
    ManageForm.SetID(StationsData.Row);
    ManageForm.SetName(selectedName);
    ManageForm.SetAvailableHashes(AvailableHashes);
    ManageForm.ShowModal;
  UpdateStations(Sender);
  UpdateTimer.Enabled:=true;

end;

procedure TMainForm.btnMoneyCollectionClick(Sender: TObject);
begin
    MoneyCollectionForm.ShowModal;
end;

procedure TMainForm.btnMonthClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  rgReportType.ItemIndex:=1;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth, 1, 0, 0, 0, 0);
  dtTo.DateTime := IncMonth(dtFrom.DateTime, 1)-0.000001;
end;

procedure TMainForm.btnDayClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  rgReportType.ItemIndex:=1;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, currentMonth,
    currentDay, 0, 0, 0, 0);
  dtTo.DateTime := EncodeDateTime(currentYear, currentMonth,
    currentDay, 23, 59, 59, 999);
end;

procedure TMainForm.btnKasseSettingClick(Sender: TObject);
begin
  settingKasse.FormShow(self);
end;

procedure TMainForm.btnWeekClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay, currentNameOfTheDay: word;
  tmpTime: TDateTime;
begin
  rgReportType.ItemIndex:=1;
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
  dtTo.DateTime := dtFrom.DateTime+7-0.000001;
end;

procedure TMainForm.btnYearClick(Sender: TObject);
var
  currentYear, currentMonth, currentDay: word;
begin
  rgReportType.ItemIndex:=1;
  DecodeDate(Now(), currentYear, currentMonth, currentDay);
  dtFrom.DateTime := EncodeDateTime(currentYear, 1, 1, 0, 0, 0, 0);
    dtTo.DateTime := IncMonth(dtFrom.DateTime, 12)-0.000001;
end;

procedure TMainForm.dtFromEditingDone(Sender: TObject);
begin
  rgReportType.ItemIndex:=1;
  if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then
  begin
    dtFrom.DateTime := dtTo.DateTime;
  end;
end;

procedure TMainForm.dtToEditingDone(Sender: TObject);
begin
  rgReportType.ItemIndex:=1;
  if CompareDateTime(dtFrom.DateTime, dtTo.DateTime) > 0 then
  begin
    dtTo.DateTime := dtFrom.DateTime;
  end;
end;

end.
