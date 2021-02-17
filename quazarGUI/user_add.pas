unit user_add;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  user_edit_base, fphttpclient, Fpjson, jsonparser;

type

  { TUserAddForm }

  TUserAddForm = class(TUserEditBaseForm)
    PasswordEdit: TEdit;
    Password: TLabel;
    procedure CancelClick(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PasswordEditChange(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private

  public

  end;

var
  UserAddForm: TUserAddForm;
  new_val: integer = 0;
  old_val: integer = 0;
  str_val: string = '';
implementation

{$R *.lfm}

procedure TUserAddForm.CancelClick(Sender: TObject);
begin
  Inherited;
  ModalResult := 0;
  UserAddForm.Close;
end;

procedure TUserAddForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  PasswordEdit.Text := '';
  str_val := '';
end;

procedure TUserAddForm.PasswordEditChange(Sender: TObject);
begin
  if not TryStrToInt(PasswordEdit.Text, new_val) then begin // not a number
     if not Length(str_val) = 0 then begin   // not the first symbol
       str(old_val, str_val);
     end;
     if Length(PasswordEdit.Text) = 0 then begin  // empty line after delete
       str_val := '';
     end else begin
       PasswordEdit.Text := str_val;              // default value
       PasswordEdit.SelStart := Length(PasswordEdit.Text);
     end;
  end else begin
     old_val := new_val;
     str_val := PasswordEdit.Text;
  end;
end;

procedure TUserAddForm.SaveClick(Sender: TObject);
var
  userJson: TJSONObject;
  message: string;
  randomLogin: string;
begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        userJson := TJSONObject.Create;

        Randomize;
        randomLogin := IntToHex(Random(Int64($7fffffffffffffff)), 16);
        userJson.Add('login', randomLogin);

        if Length(UserAddForm.PasswordEdit.Text) > 3 then
        begin
          userJson.Add('password', UserAddForm.PasswordEdit.Text);
        end
        else
        begin
          message := 'Password must be at least 4 digits long';
        end;
        if Length(UserAddForm.FirstNameEdit.Text) > 0 then
        begin
          userJson.Add('firstName', UserAddForm.FirstNameEdit.Text);
        end;
        if Length(UserAddForm.MiddleNameEdit.Text) > 0 then
        begin
          userJson.Add('middleName', UserAddForm.MiddleNameEdit.Text);
        end;
        if Length(UserAddForm.LastNameEdit.Text) > 0 then
        begin
          userJson.Add('lastName', UserAddForm.LastNameEdit.Text);
        end;
        if UserAddForm.StatusEdit.ItemIndex = ADMIN_ROLE then
        begin
            userJson.Add('isAdmin', true);
        end
        else if UserAddForm.StatusEdit.ItemIndex = ENGINEER_ROLE then
        begin
            userJson.Add('isEngineer', true);
        end
        else if UserAddForm.StatusEdit.ItemIndex = OPERATOR_ROLE then
        begin
            userJson.Add('isOperator', true);
        end;

        RequestBody := TStringStream.Create(userJson.AsJSON);
        Post(GetServerEndpoint() + 'user');

        if ResponseStatusCode <> 201 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;
        ModalResult := 1;

      except
        case ResponseStatusCode of
          0: begin ModalResult := 0; ShowMessage('Can`t connect to server'); end;
          401: begin ModalResult := 0; ShowMessage('Not authorized'); end;
          403: begin ModalResult := 0; ShowMessage('Password might be already in use'); end;
          409: begin ModalResult := 0; ShowMessage('Login is already in use. Try again.'); end;
          500: begin ModalResult := 0; ShowMessage('Server Error: 500'); end;
          else
            begin
              ShowMessage(message);
              ModalResult := 0;
            end;
        end;
      end;

    finally
      Free;
    end;
  if ModalResult = 1 then
  begin
    UserAddForm.Close;
  end;
end;

end.

