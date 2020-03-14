unit Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, fphttpclient, Fpjson, jsonparser, superobject, Types, manager;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnUpdate: TButton;
    btnManage: TButton;
    Logo: TImage;
    StationsData: TStringGrid;
    UpdateTimer: TTimer;
    procedure btnManageClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StationsDataDblClick(Sender: TObject);
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
    i: Integer;

begin
    With TFPHttpClient.Create(Nil) do
    try
    try
      MainForm.Cursor := crHourGlass;
      StationsData.Cursor := crHourGlass;
      btnUpdate.Cursor := crHourGlass;
      btnManage.Cursor := crHourGlass;

      RequestAnswer := Get('http://localhost:8020/status');
      Data := SO(UTF8Decode(RequestAnswer));

      btnManage.Enabled := False;

      // Check the null data in array stations
      if Data.S['stations'] <> '' then begin
         btnManage.Enabled := True;

         Stations := Data.A['stations'];

         for i := 0 to 12 do
             CellColor[i] := 0;

         for i := 1 to Stations.Length do begin
             Station := Stations.O[i-1];
             StationsData.Cells[1,i] := Station.s['hash'];

             if Station.s['status'] = 'offline' then begin
                CellColor[1] := 2;
             end;
             if Station.s['status'] = 'online' then begin
                CellColor[1] := 1;
             end;

             StationsData.Cells[2,i] := Station.s['status'];

             if Station.AsObject.Exists('name') then begin
                StationsData.Cells[4,i] := Station.s['name'];
             end;

             if Station.AsObject.Exists('id') then begin
                StationsData.Cells[3,i] := Station.s['id'];
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
      btnUpdate.Cursor := crDefault;
      btnManage.Cursor := crDefault;
    end;

    MainForm.Cursor := crDefault;
    StationsData.Cursor := crDefault;
    btnUpdate.Cursor := crDefault;
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
  Sleep(10000);
  s := StationsData.DefaultTextStyle;
  s.Alignment := taCenter;
  StationsData.DefaultTextStyle := s;

  for i := 0 to 12 do
      CellColor[i] := 0;

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

end.

