unit Source;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, fphttpclient, Fpjson, jsonparser, superobject;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnUpdate: TButton;
    btnAdd: TButton;
    btnSendService: TButton;
    StationsData: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnSendServiceClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;
  RequestAnswer: String;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnUpdateClick(Sender: TObject);
var
    Data: ISuperObject;
    Station: ISuperObject;
    Stations:TSuperArray;

begin
    With TFPHttpClient.Create(Nil) do
    try
      RequestAnswer := Get('http://localhost:8020/status');
      Data := SO(RequestAnswer);
      if Data.S['stations'] <> '' then begin
         Stations := Data.A['stations'];
         Station := Stations.O[0];
         StationsData.Cells[1,1] := Station.s['hash'];
         StationsData.Cells[2,1] := Station.s['status'];

         if Station.AsObject.Exists('name') then begin
            StationsData.Cells[4,1] := Station.s['name'];
         end;

         if Station.AsObject.Exists('id') then begin
            StationsData.Cells[3,1] := Station.s['id'];
         end;

      end;

    finally
      Free;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StationsData.Options:=StationsData.Options+[goEditing];
end;

procedure TMainForm.btnAddClick(Sender: TObject);
var
  postJson: TJSONObject;
  //responseData: String;
begin
  if (StrToInt(StationsData.Cells[3,1]) <> 0) and (StationsData.Cells[4,1] <> '') then begin
     postJson := TJSONObject.Create;
     postJson.Add('id', StrToInt(StationsData.Cells[3,1]));
     postJson.Add('name',TJSONString.Create(StationsData.Cells[4,1]));
     postJson.Add('hash',TJSONString.Create(StationsData.Cells[1,1]));
     With TFPHttpClient.Create(Nil) do
     try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post('http://localhost:8020/set-station');
     finally
        Free;
     end;
  end;
end;

procedure TMainForm.btnSendServiceClick(Sender: TObject);
var
  postJson: TJSONObject;
  //responseData: String;
begin
  if (StrToInt(StationsData.Cells[5,1]) <> 0) and (StationsData.Cells[1,1] <> '') then begin
     postJson := TJSONObject.Create;
     postJson.Add('hash',TJSONString.Create(StationsData.Cells[1,1]));
     postJson.Add('amount', StrToInt(StationsData.Cells[5,1]));
     StationsData.Cells[5,1] := '0';
     With TFPHttpClient.Create(Nil) do
     try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post('http://localhost:8020/add-service-amount');
     finally
        Free;
     end;
  end;

end;

end.

