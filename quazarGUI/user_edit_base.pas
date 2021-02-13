unit user_edit_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TUserEditBaseForm }

  TUserEditBaseForm = class(TForm)
    Cancel: TButton;
    Save: TButton;
    StatusEdit: TComboBox;
    LastNameEdit: TEdit;
    FirstNameEdit: TEdit;
    MiddleNameEdit: TEdit;
    FirstName: TLabel;
    Status: TLabel;
    MiddleName: TLabel;
    LastName: TLabel;
    procedure CancelClick(Sender: TObject); virtual;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Init(login, first, middle, last: string; isAdmin, isEngineer, isOperator: boolean);

    procedure SetPinCode(newCode: string);
    function GetPinCode(): string;
    function GetServerEndpoint(): string;

  protected
    _login: string;
    _firstName: string;
    _middleName: string;
    _lastName: string;
    _isAdmin: boolean;
    _isEngineer: boolean;
    _isOperator: boolean;

    ADMIN_ROLE : integer;
    ENGINEER_ROLE : integer;
    OPERATOR_ROLE : integer;
    NO_ROLE : integer;

  public

  end;

var
  UserEditBaseForm: TUserEditBaseForm;

implementation
  uses Users;
{$R *.lfm}

{ TUserEditBaseForm }

procedure TUserEditBaseForm.FormCreate(Sender: TObject);
var
  roles: TRoles;
  i: integer;
begin
  StatusEdit.Items.Clear;             //Delete all existing choices
  roles := UsersForm.GetRoles();
  for i := 0 to Length(roles)-1 do
  begin
    StatusEdit.Items.Add(roles[i]);        //Add an choice
  end;
  ADMIN_ROLE := UsersForm.GetAdminRole();
  ENGINEER_ROLE := UsersForm.GetEngineerRole();
  OPERATOR_ROLE := UsersForm.GetOperatorRole();
  NO_ROLE := UsersForm.GetNoRole();
end;

procedure TUserEditBaseForm.CancelClick(Sender: TObject);
begin
  ModalResult := 0;
  UserEditBaseForm.Close;
end;

procedure TUserEditBaseForm.FormShow(Sender: TObject);
begin
  FirstNameEdit.Text := _firstName;
  MiddleNameEdit.Text := _middleName;
  LastNameEdit.Text := _lastName;
  if _isAdmin then
  begin
    StatusEdit.ItemIndex := ADMIN_ROLE;
  end
  else if _isEngineer then
  begin
    StatusEdit.ItemIndex := ENGINEER_ROLE;
  end
  else if _isOperator then
  begin
    StatusEdit.ItemIndex := OPERATOR_ROLE;
  end
  else
  begin
    StatusEdit.ItemIndex := NO_ROLE;
  end;
end;

procedure TUserEditBaseForm.Init(login, first, middle, last: string;
      isAdmin, isEngineer, isOperator: boolean);
begin
  _login := login;
  _firstName := first;
  _middleName := middle;
  _lastName := last;
  _isAdmin := isAdmin;
  _isEngineer := isEngineer;
  _isOperator := isOperator;
end;

procedure TUserEditBaseForm.SetPinCode(newCode: string);
begin
  UsersForm.SetPinCode(newCode);
end;

function TUserEditBaseForm.GetPinCode(): string;
begin
  Result := UsersForm.GetPinCode();
end;

function TUserEditBaseForm.GetServerEndpoint(): string;
begin
  Result := UsersForm.GetServerEndpoint()
end;

end.

