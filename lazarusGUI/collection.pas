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
    postJson.Add('money', StrToInt(CollectionData.Cells[1, CollectionData.Row]));

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

            if Station.AsObject.Exists('money') then
            begin
               CollectionData.Cells[1, id] := IntToStr(Station.I['money']);
            end
            else
            begin
               CollectionData.Cells[1, id] := '0';
            end;

            if Station.AsObject.Exists('ctime') then
            begin
               timeUnix := Station.I['ctime'];
               convertedTime := UnixToDateTime(timeUnix);
               CollectionData.Cells[2, id] := DateTimeToStr(convertedTime);
            end
            else
            begin
               CollectionData.Cells[2, id] := '------';
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

