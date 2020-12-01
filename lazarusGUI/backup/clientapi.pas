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
    relayID: array of integer;
    timeON: array of integer;
    timeOFF: array of integer;
    preflight: array of integer;
  end;

  KasseInfo = packed record
    Tax: string;
    receiptItemNae: string;
    cashier: string;
    cashierINN: string;
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

    function GetPrograms(StationID: integer; out programs: ProgramsInfo): boolean;
    procedure SetProgramName(StationID: integer; ProgramID: integer;
      programName: string; out successful: boolean);
    function GetProgramRelays(StationID: integer; ProgramID: integer;
      out Relays: RelaysInfo): boolean;
    procedure SetProgramRelays(StationID: integer; ProgramID: integer;
      relays: RelaysInfo; out successful: boolean);
    function GetKasse(out Kasse: KasseInfo): boolean;
    procedure SetKasse(Kasse: KasseInfo; out successful: boolean);

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

function TClient.GetPrograms(StationID: integer; out programs: ProgramsInfo): boolean;
var
  postJson: TJSONObject;
  programsJson: TJsonArray;
  RequestAnswer: string;
  i: integer;
  tmp: TJSONData;
begin
  Result := True;
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

        setlength(programs.programID, programsJson.Count);
        setlength(programs.programName, programsJson.Count);
        programs.Count := programsJson.Count;

        for i := 0 to programsJson.Count - 1 do
        begin
          with programsJson.items[i] do
          begin
            programs.programID[i] := GetPath('id').AsInteger;
            tmp := FindPath('name');
            if tmp <> nil then
              programs.programName[i] := GetPath('name').AsString;
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
        setlength(programs.programID, 0);
        setlength(programs.programName, 0);
        programs.Count := 0;
        Result := False;

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
  out Relays: RelaysInfo): boolean;
var
  postJson: TJSONObject;
  relaysJson: TJsonArray;
  RequestAnswer: string;
  i: integer;
  tmp: TJsonData;
begin
  Result := True;
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

        relaysJson := GetJson(RequestAnswer).GetPath('relays') as TJsonArray;

        setlength(Relays.relayID, relaysJson.Count);
        setlength(Relays.timeON, relaysJson.Count);
        setlength(Relays.timeOFF, relaysJson.Count);
        setlength(Relays.preflight, relaysJson.Count);

        Relays.Count := relaysJson.Count;
        for i := 0 to relaysJson.Count - 1 do
        begin
          with relaysJson.items[i] do
          begin
            Relays.relayID[i] := FindPath('id').AsInteger;

            tmp := FindPath('timeon');
            if tmp <> nil then
            begin
              Relays.timeON[i] := tmp.AsInteger;
            end
            else
            begin
              Relays.timeON[i] := 0;
            end;

            tmp := FindPath('timeoff');
            if tmp <> nil then
            begin
              Relays.timeOFF[i] := tmp.AsInteger;
            end
            else
            begin
              Relays.timeOFF[i] := 0;
            end;

            tmp := FindPath('prfelight');
            if tmp <> nil then
            begin
              Relays.preflight[i] := tmp.AsInteger;
            end
            else
            begin
              Relays.preflight[i] := 0;
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
        setlength(Relays.relayID, 0);
        setlength(Relays.timeON, 0);
        setlength(Relays.timeOFF, 0);
        setlength(Relays.preflight, 0);

        Relays.Count := 0;
        Result := False;
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
          tmp.Add('id', relays.relayID[i]);

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

function TClient.GetKasse(out Kasse: KasseInfo): boolean;
var
  postJson: TJSONObject;
  RequestAnswer: string;
  AnswerJson: TJsonData;
  tmp: TJsonData;
begin
  Result := True;
  postJson := TJSONObject.Create;
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        RequestAnswer := Post(serverEndpoint + 'kasse');

        if ResponseStatusCode <> 200 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        AnswerJson := GetJson(RequestAnswer);

        Kasse.Tax := AnswerJson.GetPath('tax').AsString;
        Kasse.receiptItemNae := AnswerJson.GetPath('receiptItemName').AsString;
        Kasse.cashier := AnswerJson.GetPath('cashier').AsString;
        Kasse.cashierINN := AnswerJson.GetPath('cashierINN').AsString;
      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          404: ShowMessage('Server Error: 404');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        Result := False;

      end;
    finally
      Free
    end;
end;

procedure TClient.SetKasse(Kasse: KasseInfo; out successful: boolean);
var
  postJson: TJSONObject;
begin
  successful := True;
  postJson := TJSONObject.Create;
  postJson.Add('tax', Kasse.Tax);
  postJson.Add('receiptItemName', Kasse.receiptItemNae);
  postJson.Add('cashier', Kasse.cashier);
  postJson.Add('cashierINN', Kasse.cashierINN);
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        RequestBody := TStringStream.Create(postJson.AsJSON);
        Post(serverEndpoint + 'set-kasse');

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

end.
