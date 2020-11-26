unit clientAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Fpjson, jsonparser, Dialogs;

type

  ProgramsInfo = packed record
    Count: integer;
    programID: array of integer;
    programName: array of string;
  end;

  RelaysInfo = packed record
    Count: integer;
    realyID: array of integer;
    timeON: array of integer;
    timeOFF: array of integer;
    preflight: array of integer;
  end;

  { TClient }

  TClient = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    function PriceByID(hash: string; id: integer): string;
    procedure SetPriceByID(hash: string; id, Value: integer);
    procedure PairIdAndHash(Hash, StationName: string; ID: integer);
    procedure OpenStation(id: integer);
    procedure SendMoney(hash: string; moneyToSend: integer);
    function Info(): string;

    function GetPrograms(StationID: integer; out successful: boolean): ProgramsInfo;
    procedure SetProgramName(StationID: integer; ProgramID: integer;
      programName: string; out successful: boolean);
    function GetProgramRelays(StationID: integer; ProgramID: integer;
      out successful: boolean): RelaysInfo;
    procedure SetProgramRelays(StationID: integer; ProgramID: integer;
      relays: RelaysInfo; out successful: boolean);

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

function TClient.PriceByID(hash: string; id: integer): string;
var
  key: string;
  postJson: TJSONObject;
  RequestAnswer: string;

begin
  Key := 'price' + IntToStr(id);
  postJson := TJSONObject.Create;
  postJson.Add('hash', TJSONString.Create(hash));
  postJson.Add('key', TJSONString.Create(Key));

  with TFPHttpClient.Create(nil) do
    try
      AddHeader('Content-Type', 'application/json');
      RequestBody := TStringStream.Create(postJson.AsJSON);
      RequestAnswer := Post(serverEndpoint + 'load');
      Result := RequestAnswer.Substring(1, RequestAnswer.Length - 3);
    finally
      Free;
    end;
end;

procedure TClient.SetPriceByID(hash: string; id, Value: integer);
var
  key: string;
  keyPairJson, postJson: TJSONObject;
  RequestAnswer: string;

begin
  Key := 'price' + IntToStr(id);
  postJson := TJSONObject.Create;
  keyPairJson := TJSONObject.Create;

  postJson.Add('hash', TJSONString.Create(hash));
  keyPairJson.Add('key', TJSONString.Create(Key));
  keyPairJson.Add('value', TJSONString.Create(IntToStr(Value)));
  postJson.Add('KeyPair', keyPairJson);

  with TFPHttpClient.Create(nil) do
    try
      AddHeader('Content-Type', 'application/json');
      RequestBody := TStringStream.Create(postJson.AsJSON);
      Post(serverEndpoint + 'save');
    finally
      Free;
    end;
end;

procedure TClient.PairIdAndHash(Hash, StationName: string; ID: integer);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('id', ID);
  postJson.Add('hash', TJSONString.Create(Hash));
  postJson.Add('name', TJSONString.Create(StationName));
  with TFPHttpClient.Create(nil) do
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

procedure TClient.OpenStation(id: integer);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('stationID', id);

  with TFPHttpClient.Create(nil) do
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

procedure TClient.SendMoney(hash: string; moneyToSend: integer);
var
  postJson: TJSONObject;

begin
  postJson := TJSONObject.Create;
  postJson.Add('hash', TJSONString.Create(hash));
  postJson.Add('amount', moneyToSend);

  with TFPHttpClient.Create(nil) do
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

function TClient.Info(): string;
begin
  Result := '';
  try
    with TFPHttpClient.Create(nil) do
      try
        Result := Get(serverEndpoint + 'info');
      finally
        Free;
      end;
  except
  end;
end;

function TClient.GetPrograms(StationID: integer; out successful: boolean): ProgramsInfo;
var
  postJson: TJSONObject;
  programsJson: TJsonArray;
  RequestAnswer: string;
  i: integer;
  tmp: TJSONData;
begin
  successful := True;
  postJson := TJSONObject.Create;
  postJson.Add('stationID', stationID);
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        RequestAnswer := Post(serverEndpoint + 'programs');

        if ResponseStatusCode <> 200 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        programsJson := GetJson(RequestAnswer) as TJsonArray;

        setlength(Result.programID, programsJson.Count);
        setlength(Result.programName, programsJson.Count);
        Result.Count := programsJson.Count;

        for i := 0 to programsJson.Count - 1 do
        begin
          with programsJson.items[i] do
          begin
            Result.programID[i] := GetPath('id').AsInteger;
            tmp := FindPath('name');
            if tmp <> nil then
              Result.programName[i] := GetPath('name').AsString;
          end;
        end;



      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        setlength(Result.programID, 0);
        setlength(Result.programName, 0);
        Result.Count := 0;
        successful := False;

      end;
    finally
      Free
    end;
end;

procedure TClient.SetProgramName(StationID: integer; ProgramID: integer;
  programName: string; out successful: boolean);
var
  postJson: TJSONObject;
begin

  successful := True;
  postJson := TJSONObject.Create;
  postJson.Add('stationID', StationID);
  postJson.Add('programID', ProgramID);
  postJson.Add('name', programName);
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post(serverEndpoint + 'set-program-name');

        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        successful := False;
      end;


    finally
      Free
    end;
end;

function TClient.GetProgramRelays(StationID: integer; ProgramID: integer;
  out successful: boolean): RelaysInfo;
var
  postJson: TJSONObject;
  relaysJson: TJsonArray;
  RequestAnswer: string;
  i: integer;
  tmp: TJsonData;
begin
  successful := True;
  postJson := TJSONObject.Create;
  postJson.Add('stationID', stationID);
  postJson.Add('programID', ProgramID);
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        RequestAnswer := Post(serverEndpoint + 'program-relays');

        if ResponseStatusCode <> 200 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;
              
        writeln(RequestAnswer);

        relaysJson := GetJson(RequestAnswer).GetPath('relays') as TJsonArray;

        writeln(relaysJson.AsJSON);

        setlength(Result.realyID, relaysJson.Count);
        setlength(Result.timeON, relaysJson.Count);
        setlength(Result.timeOFF, relaysJson.Count);
        setlength(Result.preflight, relaysJson.Count);

        Result.Count := relaysJson.Count;
        for i := 0 to relaysJson.Count - 1 do
        begin
          with relaysJson.items[i] do
          begin
            Result.realyID[i] := FindPath('id').AsInteger;

            tmp := FindPath('timeon');
            if tmp <> nil then
            begin
              Result.timeON[i] := tmp.AsInteger;
            end
            else
            begin
              Result.timeON[i] := 0;
            end;

            tmp := FindPath('timeoff');
            if tmp <> nil then
            begin
              Result.timeOFF[i] := tmp.AsInteger;
            end
            else
            begin
              Result.timeOFF[i] := 0;
            end;

            tmp := FindPath('prfelight');
            if tmp <> nil then
            begin
              Result.preflight[i] := tmp.AsInteger;
            end
            else
            begin
              Result.preflight[i] := 0;
            end;
          end;
        end;


      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        setlength(Result.realyID, 0);
        setlength(Result.timeON, 0);
        setlength(Result.timeOFF, 0);
        setlength(Result.preflight, 0);

        Result.Count := 0;
        successful := False;
      end;
    finally
      Free
    end;
end;

procedure TClient.SetProgramRelays(StationID: integer; ProgramID: integer;
  relays: RelaysInfo; out successful: boolean);
var
  postJson: TJSONObject;
  relaysJson: TJsonArray;
  i: integer;
  tmp: TJSONObject;
begin
  successful := True;
  postJson := TJSONObject.Create;
  postJson.Add('stationID', StationID);
  postJson.Add('programID', ProgramID);
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');

        relaysJson := TJsonArray.Create;
        for i := 0 to relays.Count - 1 do
        begin
          tmp := TJSONObject.Create;
          tmp.Add('id', relays.realyID[i]);

          if relays.timeON[i] <> 0 then
          begin
            tmp.Add('timeon', relays.timeON[i]);
          end;

          if relays.timeOFF[i] <> 0 then
          begin
            tmp.Add('timeoff', relays.timeOFF[i]);
          end;

          if relays.preflight[i] <> 0 then
          begin
            tmp.Add('prfelight', relays.preflight[i]);
          end;

          relaysJson.Add(tmp);
        end;

        postJson.Add('relays', relaysJson);
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post(serverEndpoint + 'set-program-relays');


        if ResponseStatusCode <> 204 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
            successful := False;
        end;
      end;

    finally
      Free
    end;
end;

end.
