unit clientAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Fpjson, jsonparser;

type

  { TClient }

  TClient = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    function PriceByID(hash: string; id: integer):string;
    procedure SetPriceByID(hash: string; id, value: integer);
    procedure PairIdAndHash(Hash, StationName: String; ID: integer);
    procedure OpenStation(id: Integer);
    procedure SendMoney(hash: string; moneyToSend: Integer);
    function Info():string;

  private
  serverEndpoint: string;
  public

  end;

var
  Client: TClient;

implementation

{$R *.lfm}

{ TClient }

procedure TClient.DataModuleCreate(Sender: TObject);
begin
  serverEndpoint := 'http://localhost:8020/';
end;

function TClient.PriceByID(hash: string; id: integer):string;
var
  key: string;
  postJson: TJSONObject;
  RequestAnswer: String;

begin
Key := 'price' + IntToStr(id);
postJson := TJSONObject.Create;
postJson.Add('hash', TJSONString.Create(hash));
postJson.Add('key', TJSONString.Create(Key));

With TFPHttpClient.Create(Nil) do
try
   AddHeader('Content-Type', 'application/json');
   RequestBody := TStringStream.Create(postJson.AsJSON);
   RequestAnswer := Post(serverEndpoint + 'load');
   Result := RequestAnswer.Substring(1,RequestAnswer.Length-3);
finally
    Free;
end;
end;

procedure TClient.SetPriceByID(hash: string; id, value: integer);
var
  key: string;
  keyPairJson, postJson: TJSONObject;
  RequestAnswer: String;

begin
Key := 'price' + IntToStr(id);
postJson := TJSONObject.Create;
keyPairJson := TJSONObject.Create;

postJson.Add('hash', TJSONString.Create(hash));
keyPairJson.Add('key', TJSONString.Create(Key));
keyPairJson.Add('value', TJSONString.Create(IntToStr(value)));
postJson.Add('KeyPair', keyPairJson);

With TFPHttpClient.Create(Nil) do
try
   AddHeader('Content-Type', 'application/json');
   RequestBody := TStringStream.Create(postJson.AsJSON);
   Post(serverEndpoint + 'save');
finally
   Free;
end;
end;

procedure TClient.PairIdAndHash(Hash, StationName: String; ID: integer);
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
     Post(serverEndpoint + 'set-station');
   except
        On E: Exception do
      end;
  finally
     Free;
  end;

end;

procedure TClient.OpenStation(id: Integer);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('stationID', id);

  With TFPHttpClient.Create(Nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post(serverEndpoint + 'open-station');
     except
     end;

  finally
      Free;
  end;

end;

procedure TClient.SendMoney(hash: string; moneyToSend: Integer);
var
  postJson: TJSONObject;

begin
        postJson := TJSONObject.Create;
        postJson.Add('hash', TJSONString.Create(hash));
        postJson.Add('amount', moneyToSend);

        With TFPHttpClient.Create(Nil) do
        try
           try
              AddHeader('Content-Type', 'application/json');
              RequestBody := TStringStream.Create(postJson.AsJSON);
              Post('http://localhost:8020/add-service-amount');
           except
           end;

        finally
            Free;
        end;
  end;

function TClient.Info():string;
begin
Result :='';
try
With TFPHttpClient.Create(Nil) do
try
   Result := Get(serverEndpoint + 'info');
finally
    Free;
end;
except
end;

end.

