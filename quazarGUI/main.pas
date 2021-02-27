unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Base, fphttpclient, Fpjson, jsonparser;

type
  THashes = array of string;

  { TMainForm }

  TMainForm = class(TBaseForm)
    Dry1: TPanel;
    Dry2: TPanel;
    Dry3: TPanel;
    Dry4: TPanel;
    Dry6: TPanel;
    Dry7: TPanel;
    Dry8: TPanel;
    Foam2: TPanel;
    Foam3: TPanel;
    Foam4: TPanel;
    Foam5: TPanel;
    Foam1: TPanel;
    Foam6: TPanel;
    Foam7: TPanel;
    Foam8: TPanel;
    Money1: TPanel;
    Money2: TPanel;
    Money3: TPanel;
    Money4: TPanel;
    Money6: TPanel;
    Money7: TPanel;
    Money8: TPanel;
    Pause1: TPanel;
    Pause2: TPanel;
    Pause3: TPanel;
    Pause4: TPanel;
    Pause6: TPanel;
    Pause7: TPanel;
    Pause8: TPanel;
    Rinse2: TPanel;
    Rinse3: TPanel;
    Rinse4: TPanel;
    Rinse5: TPanel;
    Rinse1: TPanel;
    Rinse6: TPanel;
    Rinse7: TPanel;
    Rinse8: TPanel;
    Shampoo2: TPanel;
    Shampoo3: TPanel;
    Shampoo4: TPanel;
    Shampoo5: TPanel;
    Dry5: TPanel;
    Pause5: TPanel;
    Shampoo1: TPanel;
    Shampoo6: TPanel;
    Shampoo7: TPanel;
    Shampoo8: TPanel;
    Station1: TPanel;
    Station2: TPanel;
    Station3: TPanel;
    Station4: TPanel;
    Station6: TPanel;
    Station7: TPanel;
    Station8: TPanel;
    UpdateTimer: TTimer;
    Wax2: TPanel;
    Wax3: TPanel;
    Wax4: TPanel;
    Wax5: TPanel;
    Station5: TPanel;
    Money5: TPanel;
    Wax1: TPanel;
    Wax6: TPanel;
    Wax7: TPanel;
    Wax8: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure Money1Click(Sender: TObject);
    procedure Money2Click(Sender: TObject);
    procedure Money3Click(Sender: TObject);
    procedure Money4Click(Sender: TObject);
    procedure Money5Click(Sender: TObject);
    procedure Money6Click(Sender: TObject);
    procedure Money7Click(Sender: TObject);
    procedure Money8Click(Sender: TObject);
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure UpdateCall(Sender: TObject);
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;

    procedure MainMouseEnter(Sender: TObject); override;
    procedure MainMouseLeave(Sender: TObject); override;

    procedure ShowStation(id : integer);
    procedure ShowStations();

  private

  public
    PLACEHOLDER : string;
  end;

var
  MainForm: TMainForm;

implementation
  uses station_balance;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateTimer.Interval := 1000;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  MainForm.Main.Font.Color := MainForm.GetHoverColor();
  UpdateTimer.Enabled := false;
  UpdateStations();
  ShowStations();
  UpdateTimer.Enabled := true;
end;

procedure TMainForm.ShowStations();
var
  i : integer;
begin
  for i := 0 to ResponseStations.Count - 1 do
  begin
    if ResponseStations.id[i] <> NO_ID then
      begin
        case ResponseStations.id[i] of
          1:
            begin
              Foam1.Color:=clDefault;
              Shampoo1.Color:=clDefault;
              Rinse1.Color:=clDefault;
              Wax1.Color:=clDefault;
              Dry1.Color:=clDefault;
              Pause1.Color:=clDefault;
              Money1.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money1.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam1.Color:=clLime;
                  2: Shampoo1.Color:=clLime;
                  3: Rinse1.Color:=clLime;
                  4: Wax1.Color:=clLime;
                  5: Dry1.Color:=clLime;
                  6: Pause1.Color:=clLime;
                end;
              end;
            end;
          2:
            begin
              Foam2.Color:=clDefault;
              Shampoo2.Color:=clDefault;
              Rinse2.Color:=clDefault;
              Wax2.Color:=clDefault;
              Dry2.Color:=clDefault;
              Pause2.Color:=clDefault;
              Money2.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money2.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam2.Color:=clLime;
                  2: Shampoo2.Color:=clLime;
                  3: Rinse2.Color:=clLime;
                  4: Wax2.Color:=clLime;
                  5: Dry2.Color:=clLime;
                  6: Pause2.Color:=clLime;
                end;
              end;
            end;
          3:
            begin
              Foam3.Color:=clDefault;
              Shampoo3.Color:=clDefault;
              Rinse3.Color:=clDefault;
              Wax3.Color:=clDefault;
              Dry3.Color:=clDefault;
              Pause3.Color:=clDefault;
              Money3.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money3.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam3.Color:=clLime;
                  2: Shampoo3.Color:=clLime;
                  3: Rinse3.Color:=clLime;
                  4: Wax3.Color:=clLime;
                  5: Dry3.Color:=clLime;
                  6: Pause3.Color:=clLime;
                end;
              end;
            end;
          4:
            begin
              Foam4.Color:=clDefault;
              Shampoo4.Color:=clDefault;
              Rinse4.Color:=clDefault;
              Wax4.Color:=clDefault;
              Dry4.Color:=clDefault;
              Pause4.Color:=clDefault;
              Money4.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money4.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam4.Color:=clLime;
                  2: Shampoo4.Color:=clLime;
                  3: Rinse4.Color:=clLime;
                  4: Wax4.Color:=clLime;
                  5: Dry4.Color:=clLime;
                  6: Pause4.Color:=clLime;
                end;
              end;
            end;
          5:
            begin
              Foam5.Color:=clDefault;
              Shampoo5.Color:=clDefault;
              Rinse5.Color:=clDefault;
              Wax5.Color:=clDefault;
              Dry5.Color:=clDefault;
              Pause5.Color:=clDefault;
              Money5.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money5.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam5.Color:=clLime;
                  2: Shampoo5.Color:=clLime;
                  3: Rinse5.Color:=clLime;
                  4: Wax5.Color:=clLime;
                  5: Dry5.Color:=clLime;
                  6: Pause5.Color:=clLime;
                end;
              end;
            end;
          6:
            begin
              Foam6.Color:=clDefault;
              Shampoo6.Color:=clDefault;
              Rinse6.Color:=clDefault;
              Wax6.Color:=clDefault;
              Dry6.Color:=clDefault;
              Pause6.Color:=clDefault;
              Money6.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money6.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam6.Color:=clLime;
                  2: Shampoo6.Color:=clLime;
                  3: Rinse6.Color:=clLime;
                  4: Wax6.Color:=clLime;
                  5: Dry6.Color:=clLime;
                  6: Pause6.Color:=clLime;
                end;
              end;
            end;
          7:
            begin
              Foam7.Color:=clDefault;
              Shampoo7.Color:=clDefault;
              Rinse7.Color:=clDefault;
              Wax7.Color:=clDefault;
              Dry7.Color:=clDefault;
              Pause7.Color:=clDefault;
              Money7.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money7.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam7.Color:=clLime;
                  2: Shampoo7.Color:=clLime;
                  3: Rinse7.Color:=clLime;
                  4: Wax7.Color:=clLime;
                  5: Dry7.Color:=clLime;
                  6: Pause7.Color:=clLime;
                end;
              end;
            end;
          8:
            begin
              Foam8.Color:=clDefault;
              Shampoo8.Color:=clDefault;
              Rinse8.Color:=clDefault;
              Wax8.Color:=clDefault;
              Dry8.Color:=clDefault;
              Pause8.Color:=clDefault;
              Money8.Caption := '';
              if ResponseStations.status[i] = ON_LINE then
              begin
                if ResponseStations.currentBalance[i] <> NO_ID then
                begin
                  Money8.Caption := IntToStr(ResponseStations.currentBalance[i]);
                end;
                case ResponseStations.currentProgram[i] of
                  1: Foam8.Color:=clLime;
                  2: Shampoo8.Color:=clLime;
                  3: Rinse8.Color:=clLime;
                  4: Wax8.Color:=clLime;
                  5: Dry8.Color:=clLime;
                  6: Pause8.Color:=clLime;
                end;
              end;
            end;
          end;
      end;
  end;
end;

procedure TMainForm.MainClick(Sender: TObject);
begin
  Inherited;
end;

procedure TMainForm.ShowStation(id : integer);
begin
  if ResponseStations.Count = 0 then
  begin
    ShowMessage('Can`t connect to server');
  end
  else
  begin
    if ResponseStations.status[id-1] = ON_LINE then
    begin
      UpdateTimer.Enabled := false;
      StationBalanceForm.Init(id);
      StationBalanceForm.ShowModal;
      UpdateTimer.Enabled := true;
    end
    else
    begin
      ShowMessage('Station is offline');
    end;
  end;
end;

procedure TMainForm.Money1Click(Sender: TObject);
begin
  ShowStation(1);
end;

procedure TMainForm.Money2Click(Sender: TObject);
begin
  ShowStation(2);
end;

procedure TMainForm.Money3Click(Sender: TObject);
begin
  ShowStation(3);
end;

procedure TMainForm.Money4Click(Sender: TObject);
begin
  ShowStation(4);
end;

procedure TMainForm.Money5Click(Sender: TObject);
begin
  ShowStation(5);
end;

procedure TMainForm.Money6Click(Sender: TObject);
begin
  ShowStation(6);
end;

procedure TMainForm.Money7Click(Sender: TObject);
begin
  ShowStation(7);
end;

procedure TMainForm.Money8Click(Sender: TObject);
begin
  ShowStation(8);
end;

procedure TMainForm.StationsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.UpdateCall(Sender: TObject);
begin
  MainForm.FormShow(Sender);
end;

procedure TMainForm.UsersClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  MainForm.Hide;
  UpdateTimer.Enabled := false;
end;

procedure TMainForm.MainMouseEnter(Sender: TObject);
begin

end;

procedure TMainForm.MainMouseLeave(Sender: TObject);
begin

end;

end.

