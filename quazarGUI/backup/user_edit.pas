unit user_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  user_edit_base, fphttpclient, Fpjson, jsonparser;

type

  { TUserEditForm }

  TUserEditForm = class(TUserEditBaseForm)
    DeleteBtn: TButton;
    Password: TButton;
    procedure CancelClick(Sender: TObject); override;
    procedure DeleteBtnClick(Sender: TObject);
    procedure PasswordClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private

  public

  end;

var
  UserEditForm: TUserEditForm;

implementation
  uses User_edit_password;
{$R *.lfm}

procedure TUserEditForm.CancelClick(Sender: TObject);
begin
  Inherited;
  ModalResult := 0;
  UserEditForm.Close;
end;

procedure TUserEditForm.DeleteBtnClick(Sender: TObject);
var
  userJson: TJSONObject;
begin
  with TFPHttpClient.Create(nil) do
    try
       try
          AddHeader('Content-Type', 'application/json');
          AddHeader('Pin', GetPinCode());

          userJson := TJSONObject.Create;

          userJson.Add('login', UserEditForm._login);

          RequestBody := TStringStream.Create(userJson.AsJSON);
          Delete(GetServerEndpoint() + 'user');

          if ResponseStatusCode <> 204 then
          begin
            raise Exception.Create(IntToStr(ResponseStatusCode));
          end;
          ModalResult := 1;

        except
          case ResponseStatusCode of
            0: begin ModalResult := 0; ShowMessage('Can`t connect to server'); end;
            401, 403, 409: begin ModalResult := 0; ShowMessage('40X code'); end;
            500: begin ModalResult := 0; ShowMessage('Server Error: 500'); end;
            else
              begin
                ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
                  sLineBreak + ResponseStatusText);
                ModalResult := 0;
              end;
          end;
        end;

      finally
        Free;
      end;
    UserEditForm.Close;
end;

procedure TUserEditForm.PasswordClick(Sender: TObject);
begin
  UserEditPasswordForm.Init(_login, _firstName, _middleName, _lastName, _isAdmin, _isEngineer, _isOperator);
  ModalResult := UserEditPasswordForm.ShowModal;
  UserEditForm.Close;
end;

procedure TUserEditForm.SaveClick(Sender: TObject);
var
  userJson: TJSONObject;
begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        userJson := TJSONObject.Create;

        userJson.Add('login', UserEditForm._login);
        if Length(UserEditForm.FirstNameEdit.Text) > 0 then
        begin
          userJson.Add('firstName', UserEditForm.FirstNameEdit.Text);
        end;
        if Length(UserEditForm.MiddleNameEdit.Text) > 0 then
        begin
          userJson.Add('middleName', UserEditForm.MiddleNameEdit.Text);
        end;
        if Length(UserEditForm.LastNameEdit.Text) > 0 then
        begin
          userJson.Add('lastName', UserEditForm.LastNameEdit.Text);
        end;
        if UserEditForm.StatusEdit.ItemIndex = UsersForm.ADMIN_ROLE then
        begin
            userJson.Add('isAdmin', true);
            userJson.Add('isEngineer', false);
            userJson.Add('isOperator', false);
        end
        else if UserEditForm.StatusEdit.ItemIndex = UsersForm.ENGINEER_ROLE then
        begin
            userJson.Add('isAdmin', false);
            userJson.Add('isEngineer', true);
            userJson.Add('isOperator', false);
        end
        else if UserEditForm.StatusEdit.ItemIndex = UsersForm.OPERATOR_ROLE then
        begin
            userJson.Add('isAdmin', false);
            userJson.Add('isEngineer', false);
            userJson.Add('isOperator', true);
        end
        else
        begin
            userJson.Add('isAdmin', false);
            userJson.Add('isEngineer', false);
            userJson.Add('isOperator', false);
        end;

        RequestBody := TStringStream.Create(userJson.AsJSON);
        Put(GetServerEndpoint() + 'user');

        if ResponseStatusCode <> 201 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;
        ModalResult := 1;

      except
        case ResponseStatusCode of
          0: begin ModalResult := 0; ShowMessage('Can`t connect to server'); end;
          401, 403, 404: begin ModalResult := 0; ShowMessage('40X code'); end;
          500: begin ModalResult := 0; ShowMessage('Server Error: 500'); end;
          else
            begin
              ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
                sLineBreak + ResponseStatusText);
              ModalResult := 0;
            end;
        end;
      end;

    finally
      Free;
    end;
  UserEditForm.Close;
end;

end.

