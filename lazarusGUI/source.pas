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
    cbHash1: TComboBox;
    cbHash10: TComboBox;
    cbHash11: TComboBox;
    cbHash12: TComboBox;
    cbHash2: TComboBox;
    cbHash3: TComboBox;
    cbHash4: TComboBox;
    cbHash5: TComboBox;
    cbHash6: TComboBox;
    cbHash7: TComboBox;
    cbHash8: TComboBox;
    cbHash9: TComboBox;
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
    procedure cbHash10EditingDone(Sender: TObject);
    procedure cbHash11EditingDone(Sender: TObject);
    procedure cbHash12EditingDone(Sender: TObject);
    procedure cbHash1EditingDone(Sender: TObject);
    procedure cbHash2EditingDone(Sender: TObject);
    procedure cbHash3EditingDone(Sender: TObject);
    procedure cbHash4EditingDone(Sender: TObject);
    procedure cbHash5EditingDone(Sender: TObject);
    procedure cbHash6EditingDone(Sender: TObject);
    procedure cbHash7EditingDone(Sender: TObject);
    procedure cbHash8EditingDone(Sender: TObject);
    procedure cbHash9EditingDone(Sender: TObject);
    procedure cbIsBlockedChange(Sender: TObject);
    procedure dtFromEditingDone(Sender: TObject);
    procedure dtToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StationsDataDblClick(Sender: TObject);
    procedure StationsDataSelection(Sender: TObject; aCol, aRow: integer);
    procedure UpdateStations(Sender: TObject);
    procedure StationsDataDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure UpdateCall(Sender: TObject);

    procedure DisableAllItems(Sender: TObject);
    procedure EnableAllItems(Sender: TObject);
    procedure EnableItemOnPos(Pos: integer; Sender: TObject);
    procedure SetHashOnPos(Pos: integer; Hash: String; Sender: TObject);
    procedure RefreshHashData(Sender: TObject);
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

procedure TMainForm.DisableAllItems(Sender: TObject);
begin
     cbHash1.Enabled := False;
     cbHash2.Enabled := False;
     cbHash3.Enabled := False;
     cbHash4.Enabled := False;
     cbHash5.Enabled := False;
     cbHash6.Enabled := False;
     cbHash7.Enabled := False;
     cbHash8.Enabled := False;
     cbHash9.Enabled := False;
     cbHash10.Enabled := False;
     cbHash11.Enabled := False;
     cbHash12.Enabled := False;
end;

procedure TMainForm.EnableAllItems(Sender: TObject);
begin
     cbHash1.Enabled := True;
     cbHash2.Enabled := True;
     cbHash3.Enabled := True;
     cbHash4.Enabled := True;
     cbHash5.Enabled := True;
     cbHash6.Enabled := True;
     cbHash7.Enabled := True;
     cbHash8.Enabled := True;
     cbHash9.Enabled := True;
     cbHash10.Enabled := True;
     cbHash11.Enabled := True;
     cbHash12.Enabled := True;
end;

procedure TMainForm.EnableItemOnPos(Pos: Integer; Sender: TObject);
begin
     case Pos of
     1: cbHash1.Enabled := True;
     2: cbHash2.Enabled := True;
     3: cbHash3.Enabled := True;
     4: cbHash4.Enabled := True;
     5: cbHash5.Enabled := True;
     6: cbHash6.Enabled := True;
     7: cbHash7.Enabled := True;
     8: cbHash8.Enabled := True;
     9: cbHash9.Enabled := True;
     10: cbHash10.Enabled := True;
     11: cbHash11.Enabled := True;
     12: cbHash12.Enabled := True;
     end;
end;

procedure TMainForm.SetHashOnPos(Pos: Integer; Hash: String; Sender: TObject);
begin
     StationsData.Cells[1, pos] := Hash;

     case Pos of
     1: cbHash1.ItemIndex := cbHash1.Items.IndexOf(Hash);
     2: cbHash2.ItemIndex := cbHash2.Items.IndexOf(Hash);
     3: cbHash3.ItemIndex := cbHash3.Items.IndexOf(Hash);
     4: cbHash4.ItemIndex := cbHash4.Items.IndexOf(Hash);
     5: cbHash5.ItemIndex := cbHash5.Items.IndexOf(Hash);
     6: cbHash6.ItemIndex := cbHash6.Items.IndexOf(Hash);
     7: cbHash7.ItemIndex := cbHash7.Items.IndexOf(Hash);
     8: cbHash8.ItemIndex := cbHash8.Items.IndexOf(Hash);
     9: cbHash9.ItemIndex := cbHash9.Items.IndexOf(Hash);
     10: cbHash10.ItemIndex := cbHash10.Items.IndexOf(Hash);
     11: cbHash11.ItemIndex := cbHash11.Items.IndexOf(Hash);
     12: cbHash12.ItemIndex := cbHash12.Items.IndexOf(Hash);
     end;
end;

procedure TMainForm.RefreshHashData(Sender: TObject);
var
  i, findRes: Integer;
  currentHash: String;
begin
     if cbHash1.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash1.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash1.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash2.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash2.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash2.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash3.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash3.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash3.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash4.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash4.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash4.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash5.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash5.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash5.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash6.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash6.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash6.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash7.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash7.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash7.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash8.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash8.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash8.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash9.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash9.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash9.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash10.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash10.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash10.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash11.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash11.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash11.Items.Add(currentHash);
               end;
          end;
     end;
     if cbHash12.Enabled then
     begin
          for i := 0 to AvailableHashes.Count - 1 do
          begin
               currentHash := AvailableHashes.ValueFromIndex[i];
               findRes := cbHash12.Items.IndexOf(currentHash);
               if findRes < 0 then begin
                  cbHash12.Items.Add(currentHash);
               end;
          end;
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
        DisableAllItems(Sender);

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

              // Make controls in the specified line active
              if not cbIsBlocked.Checked then
                 EnableItemOnPos(pos, Sender);
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
                      RefreshHashData(Sender);
                      SetHashOnPos(pos, Station.s['hash'], Sender);
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

  cbHash1.Color := clHighLight;

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

procedure TMainForm.StationsDataSelection(Sender: TObject; aCol, aRow: integer);
begin
  // This is real shit, need to replace that
  cbHash1.Color := clDefault;
  cbHash2.Color := clDefault;
  cbHash3.Color := clDefault;
  cbHash4.Color := clDefault;
  cbHash5.Color := clDefault;
  cbHash6.Color := clDefault;
  cbHash7.Color := clDefault;
  cbHash8.Color := clDefault;
  cbHash9.Color := clDefault;
  cbHash10.Color := clDefault;
  cbHash11.Color := clDefault;
  cbHash12.Color := clDefault;

  case StationsData.Row of
    1: cbHash1.Color := clHighLight;
    2: cbHash2.Color := clHighLight;
    3: cbHash3.Color := clHighLight;
    4: cbHash4.Color := clHighLight;
    5: cbHash5.Color := clHighLight;
    6: cbHash6.Color := clHighLight;
    7: cbHash7.Color := clHighLight;
    8: cbHash8.Color := clHighLight;
    9: cbHash9.Color := clHighLight;
    10: cbHash10.Color := clHighLight;
    11: cbHash11.Color := clHighLight;
    12: cbHash12.Color := clHighLight;
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

procedure TMainForm.cbHash10EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 10] := cbHash10.Text;
     PairIdAndHash(cbHash10.Text, StationsData.Cells[3, 10], 10, Sender);
end;

procedure TMainForm.cbHash11EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 11] := cbHash11.Text;
     PairIdAndHash(cbHash11.Text, StationsData.Cells[3, 11], 11, Sender);
end;

procedure TMainForm.cbHash12EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 12] := cbHash12.Text;
     PairIdAndHash(cbHash12.Text, StationsData.Cells[3, 12], 12, Sender);
end;

procedure TMainForm.cbHash1EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 1] := cbHash1.Text;
     PairIdAndHash(cbHash1.Text, StationsData.Cells[3, 1], 1, Sender);
end;

procedure TMainForm.cbHash2EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 2] := cbHash2.Text;
     PairIdAndHash(cbHash2.Text, StationsData.Cells[3, 2], 2, Sender);
end;

procedure TMainForm.cbHash3EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 3] := cbHash3.Text;
     PairIdAndHash(cbHash3.Text, StationsData.Cells[3, 3], 3, Sender);
end;

procedure TMainForm.cbHash4EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 4] := cbHash4.Text;
     PairIdAndHash(cbHash4.Text, StationsData.Cells[3, 4], 4, Sender);
end;

procedure TMainForm.cbHash5EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 5] := cbHash5.Text;
     PairIdAndHash(cbHash5.Text, StationsData.Cells[3, 5], 5, Sender);
end;

procedure TMainForm.cbHash6EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 6] := cbHash6.Text;
     PairIdAndHash(cbHash6.Text, StationsData.Cells[3, 6], 6, Sender);
end;

procedure TMainForm.cbHash7EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 7] := cbHash7.Text;
     PairIdAndHash(cbHash7.Text, StationsData.Cells[3, 7], 7, Sender);
end;

procedure TMainForm.cbHash8EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 8] := cbHash8.Text;
     PairIdAndHash(cbHash8.Text, StationsData.Cells[3, 8], 8, Sender);
end;

procedure TMainForm.cbHash9EditingDone(Sender: TObject);
begin
     StationsData.Cells[1, 9] := cbHash9.Text;
     PairIdAndHash(cbHash9.Text, StationsData.Cells[3, 9], 9, Sender);
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
