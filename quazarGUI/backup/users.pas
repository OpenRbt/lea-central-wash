unit users;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Base, fphttpclient, Fpjson, jsonparser;

type
  TRoles = array of string;

  UsersInfo = packed record
    Count: integer;
    login: array of string;
    firstName: array of string;
    middleName: array of string;
    lastName: array of string;
    isAdmin: array of boolean;
    isEngineer: array of boolean;
    isOperator: array of boolean;

  end;

  { TUsersForm }

  TUsersForm = class(TBaseForm)
    UsersGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure MainClick(Sender: TObject); override;
    procedure StationsClick(Sender: TObject); override;
    procedure ProgramsClick(Sender: TObject); override;
    procedure DosatronsClick(Sender: TObject); override;
    procedure SettingsClick(Sender: TObject); override;
    procedure UsersClick(Sender: TObject); override;
    procedure StatisticsClick(Sender: TObject); override;
    procedure LogoutClick(Sender: TObject); override;
    procedure UsersGridButtonClick(Sender: TObject; aCol, aRow: Integer);

    procedure UsersMouseEnter(Sender: TObject); override;
    procedure UsersMouseLeave(Sender: TObject); override;

    function GetRoles() : TRoles;
    function GetAdminRole() : integer;
    function GetEngineerRole() : integer;
    function GetOperatorRole() : integer;
    function GetNoRole() : integer;
  private
    roles: TRoles;
    ADMIN_ROLE : integer;
    ENGINEER_ROLE : integer;
    OPERATOR_ROLE : integer;
    NO_ROLE : integer;
  public

  end;

var
  UsersForm: TUsersForm;
  ResponseUsers: UsersInfo;

const
  LAST_NAME   : integer = 0;
  FIRST_NAME  : integer = 1;
  MIDDLE_NAME : integer = 2;
  ROLE        : integer = 3;
  EDIT        : integer = 4;
  NUM_COLS    : integer = 5;
  ADMIN_STRING    : string = 'АДМИН';
  ENGINEER_STRING : string = 'ИНЖЕНЕР';
  OPERATOR_STRING : string = 'ОПЕРАТОР';
  DISABLED_STRING : string = '------------------';
  EDIT_STRING     : string = '...';
  ADD_STRING      : string = '+';
  PLACEHOLDER     : string = '';
  PADDING         : string = '  ';

implementation
  uses User_edit, User_add;
{$R *.lfm}

procedure TUsersForm.FormCreate(Sender: TObject);
begin;
  setlength(roles, 4);
  ADMIN_ROLE := 0;
  ENGINEER_ROLE := 1;
  OPERATOR_ROLE := 2;
  NO_ROLE := 3;
  roles[ADMIN_ROLE] := ADMIN_STRING;
  roles[ENGINEER_ROLE] := ENGINEER_STRING;
  roles[OPERATOR_ROLE] := OPERATOR_STRING;
  roles[NO_ROLE] := DISABLED_STRING;
end;

function TUsersForm.GetRoles() : TRoles;
begin
  Result := roles;
end;

procedure TUsersForm.FormShow(Sender: TObject);
var
  RequestAnswer: string;
  usersJson: TJsonArray;
  i: integer;
  userData : array of string;
  //successful: boolean = True;

begin
  UsersForm.Users.Font.Color := UsersForm.GetHoverColor();
  UsersForm.NotAuthorized.Visible:=False;
  UsersForm.UsersGrid.Visible:=True;

  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', UsersForm.GetPinCode());
        RequestAnswer := Get(UsersForm.GetServerEndpoint() + 'users');

        usersJson := GetJson(RequestAnswer).GetPath('users') as TJsonArray;

        setlength(ResponseUsers.login, usersJson.Count);
        setlength(ResponseUsers.firstName, usersJson.Count);
        setlength(ResponseUsers.middleName, usersJson.Count);
        setlength(ResponseUsers.lastName, usersJson.Count);
        setlength(ResponseUsers.isAdmin, usersJson.Count);
        setlength(ResponseUsers.isEngineer, usersJson.Count);
        setlength(ResponseUsers.isOperator, usersJson.Count);
        setlength(userData, NUM_COLS);

        ResponseUsers.Count := usersJson.Count;

        UsersForm.UsersGrid.RowCount := UsersForm.UsersGrid.FixedRows + 1;
        UsersForm.UsersGrid.Rows[UsersForm.UsersGrid.FixedRows].Clear;
        for i := 0 to usersJson.Count - 1 do
        begin
          with usersJson.items[i] do
          begin
            ResponseUsers.login[i]      := FindPath('login').AsString;
            ResponseUsers.firstName[i]  := FindPath('firstName').AsString;
            ResponseUsers.middleName[i] := FindPath('middleName').AsString;
            ResponseUsers.lastName[i]   := FindPath('lastName').AsString;
            ResponseUsers.isAdmin[i]    := FindPath('isAdmin').AsBoolean;
            ResponseUsers.isOperator[i] := FindPath('isOperator').AsBoolean;
            ResponseUsers.isEngineer[i] := FindPath('isEngineer').AsBoolean;

            userData[FIRST_NAME] := PADDING + ResponseUsers.firstName[i];
            userData[MIDDLE_NAME] := PADDING + ResponseUsers.middleName[i];
            userData[LAST_NAME] := PADDING + ResponseUsers.lastName[i];
            if ResponseUsers.isAdmin[i] then
            begin
              userData[ROLE] := PADDING + ADMIN_STRING;
            end
            else if ResponseUsers.isEngineer[i] then
            begin
              userData[ROLE] := PADDING + ENGINEER_STRING;
            end
            else if ResponseUsers.isOperator[i] then
            begin
              userData[ROLE] := PADDING + OPERATOR_STRING;
            end
            else
            begin
              userData[ROLE] := PADDING + DISABLED_STRING;
            end;
            userData[EDIT] := EDIT_STRING;
          end;
          UsersForm.UsersGrid.InsertRowWithValues(i+1, userData);
        end;
        userData[FIRST_NAME] := PLACEHOLDER;
        userData[MIDDLE_NAME] := PLACEHOLDER;
        userData[LAST_NAME] := PLACEHOLDER;
        userData[ROLE] := PLACEHOLDER;
        userData[EDIT] := ADD_STRING;
        for i := usersJson.Count to 9 do
        begin
          UsersForm.UsersGrid.InsertRowWithValues(i+1, userData);
        end;

      except
        UsersForm.NotAuthorized.Visible:=True;
        UsersForm.UsersGrid.Visible:=False;
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          401, 403: // do nothing
            ;
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
        setlength(ResponseUsers.login, 0);
        setlength(ResponseUsers.firstName, 0);
        setlength(ResponseUsers.middleName, 0);
        setlength(ResponseUsers.lastName, 0);
        setlength(ResponseUsers.isAdmin, 0);
        setlength(ResponseUsers.isEngineer, 0);
        setlength(ResponseUsers.isOperator, 0);

        ResponseUsers.Count := 0;

      end;
    finally
      Free;
    end;

end;

procedure TUsersForm.MainClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.StationsClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.ProgramsClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.DosatronsClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.SettingsClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.UsersClick(Sender: TObject);
begin
  Inherited;
end;

procedure TUsersForm.StatisticsClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.LogoutClick(Sender: TObject);
begin
  Inherited;
  UsersForm.Hide;
end;

procedure TUsersForm.UsersGridButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  response: integer;
begin
  if aRow > ResponseUsers.Count then
  begin
    UserAddForm.Init('', '', '', '', false, false, false);
    response := UserAddForm.ShowModal;
  end
  else
  begin
    UserEditForm.Init(ResponseUsers.login[aRow-1], ResponseUsers.firstName[aRow-1],
      ResponseUsers.middleName[aRow-1], ResponseUsers.lastName[aRow-1],
      ResponseUsers.isAdmin[aRow-1], ResponseUsers.isEngineer[aRow-1],
      ResponseUsers.isOperator[aRow-1]);
    response := UserEditForm.ShowModal;
  end;
  if response > 0 then
    begin
      UsersForm.FormShow(Self);
    end;
end;

procedure TUsersForm.UsersMouseEnter(Sender: TObject);
begin

end;

procedure TUsersForm.UsersMouseLeave(Sender: TObject);
begin

end;

end.

