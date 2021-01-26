unit collection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  fphttpclient, Fpjson, jsonparser, superobject, Types, DateUtils;

type

  { TMoneyCollectionForm }

  TMoneyCollectionForm = class(TForm)
    BtnCollect: TButton;
    CollectionData: TStringGrid;
    procedure BtnCollectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  MoneyCollectionForm: TMoneyCollectionForm;

implementation

{$R *.lfm}

{ TMoneyCollectionForm }

procedure TMoneyCollectionForm.BtnCollectClick(Sender: TObject);
var
  buttonSelected: Integer;
  postJson: TJSONObject;

begin
  buttonSelected := MessageDlg('Collect money from station ' + IntToStr(CollectionData.Row) + '?', mtConfirmation, mbOKCancel, 0);

  if buttonSelected = mrOK then
  begin
    postJson := TJSONObject.Create;
    postJson.Add('id', CollectionData.Row);

    With TFPHttpClient.Create(Nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post('http://localhost:8020/save-collection');

        CollectionData.Cells[2, CollectionData.Row] := DateTimeToStr(Now());
        CollectionData.Cells[1, CollectionData.Row] := '0';
      except
        On E: Exception do
      end;
    finally
       Free;
    end;
  end;
end;

procedure TMoneyCollectionForm.FormCreate(Sender: TObject);
begin

end;

procedure TMoneyCollectionForm.FormShow(Sender: TObject);
var
  s: TTextStyle;
  Data: ISuperObject;
  Station: ISuperObject;
  Stations: TSuperArray;
  RequestAnswer: string;
  ConvertedTime: TDateTime;
  i, id, timeUnix: integer;
begin
  s := CollectionData.DefaultTextStyle;
  s.Alignment := taCenter;
  CollectionData.DefaultTextStyle := s;

  with TFPHttpClient.Create(nil) do
  try
    try
      RequestAnswer := Get('http://localhost:8020/status-collection');
      Data := SO(UTF8Decode(RequestAnswer));

      if Data.S['stations'] <> '' then
      begin
        Stations := Data.A['stations'];

        for i := 1 to Stations.Length do
        begin
          Station := Stations.O[i - 1];

          if Station.AsObject.Exists('id') then
          begin
            id := Station.I['id'];

            if Station.AsObject.Exists('electronical') then
            begin
               CollectionData.Cells[1, id] := IntToStr(Station.I['electronical']);
            end
            else
            begin
               CollectionData.Cells[1, id] := '0';
            end;

            if Station.AsObject.Exists('coins') then
            begin
               CollectionData.Cells[2, id] := IntToStr(Station.I['coins']);
            end
            else
            begin
               CollectionData.Cells[2, id] := '0';
            end;

            if Station.AsObject.Exists('banknotes') then
            begin
               CollectionData.Cells[3, id] := IntToStr(Station.I['banknotes']);
            end
            else
            begin
               CollectionData.Cells[3, id] := '0';
            end;

            if Station.AsObject.Exists('carsTotal') then
            begin
               CollectionData.Cells[4, id] := IntToStr(Station.I['carsTotal']);
            end
            else
            begin
               CollectionData.Cells[4, id] := '0';
            end;

            if Station.AsObject.Exists('service') then
            begin
               CollectionData.Cells[5, id] := IntToStr(Station.I['service']);
            end
            else
            begin
               CollectionData.Cells[5, id] := '0';
            end;

            if Station.AsObject.Exists('ctime') then
            begin
               timeUnix := Station.I['ctime'];
               if timeUnix>0 then begin
               // API returns UTC time; LocalTime = UTC - GetLocalTimeOffset
               convertedTime := UnixToDateTime(timeUnix-GetLocalTimeOffset()*60);
               CollectionData.Cells[6, id] := DateTimeToStr(convertedTime);
               end else CollectionData.Cells[6, id] := '-----';
            end
            else
            begin
               CollectionData.Cells[6, id] := '-----';
            end;
          end;
        end;
      end;

    except
      On E: Exception do
    end;
  finally
    Free;
  end;
end;
end.

