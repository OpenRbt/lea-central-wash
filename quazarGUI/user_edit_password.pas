unit user_edit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  user_edit_base, fphttpclient, Fpjson, jsonparser;

type

  { TUserEditPasswordForm }

  TUserEditPasswordForm = class(TUserEditBaseForm)
    NewPasswordEdit: TEdit;
    OldPasswordEdit: TEdit;
    OldPasswordLabel: TLabel;
    NewPasswordLabel: TLabel;
    procedure CancelClick(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure NewPasswordEditChange(Sender: TObject);
    procedure OldPasswordEditChange(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private

  public

  end;

var
  UserEditPasswordForm: TUserEditPasswordForm;
  new_pass_new_val: integer = 0;
  new_pass_old_val: integer = 0;
  new_pass_str_val: string = '';
  old_pass_new_val: integer = 0;
  old_pass_old_val: integer = 0;
  old_pass_str_val: string = '';

implementation

{$R *.lfm}

{ TUserEditPasswordForm }

procedure TUserEditPasswordForm.SaveClick(Sender: TObject);
var
  userJson: TJSONObject;
  message : string;
begin
  with TFPHttpClient.Create(nil) do
  try
     try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', GetPinCode());

        userJson := TJSONObject.Create;

        userJson.Add('login', UserEditPasswordForm._login);
        if Length(UserEditPasswordForm.OldPasswordEdit.Text) > 3 then
        begin
          userJson.Add('oldPassword', UserEditPasswordForm.OldPasswordEdit.Text);
        end
        else
        begin
          message := 'Password must be at least 4 digits long';
        end;
        if Length(UserEditPasswordForm.NewPasswordEdit.Text) > 3 then
        begin
          userJson.Add('newPassword', UserEditPasswordForm.NewPasswordEdit.Text);
        end
        else
        begin
          message := 'Password must be at least 4 digits long';
        end;

        RequestBody := TStringStream.Create(userJson.AsJSON);
        Post(GetServerEndpoint() + 'user-password');

        if ResponseStatusCode <> 201 then
        begin
          raise Exception.Create(IntToStr(ResponseStatusCode));
        end;

        ModalResult := 1;
        if UserEditPasswordForm.OldPasswordEdit.Text = GetPinCode() then
        begin
          SetPinCode(UserEditPasswordForm.NewPasswordEdit.Text);
        end;

      except
        case ResponseStatusCode of
          0: begin ModalResult := 0; ShowMessage('Can`t connect to server'); end;
          401: begin ModalResult := 0; ShowMessage('Not authorized'); end;
          403: begin ModalResult := 0; ShowMessage('Password might be already in use'); end;
          404: begin ModalResult := 0; ShowMessage('Password did not match.'); end;
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
    UserEditPasswordForm.Close;
  end;
end;

procedure TUserEditPasswordForm.CancelClick(Sender: TObject);
begin
  ModalResult := 0;
  UserEditPasswordForm.Close;
end;

procedure TUserEditPasswordForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  NewPasswordEdit.Text := '';
  new_pass_str_val := '';
  OldPasswordEdit.Text := '';
  old_pass_str_val := '';
end;

procedure TUserEditPasswordForm.NewPasswordEditChange(Sender: TObject);
begin
  if not TryStrToInt(NewPasswordEdit.Text, new_pass_new_val) then begin // not a number
     if not Length(new_pass_str_val) = 0 then begin   // not the first symbol
       str(new_pass_old_val, new_pass_str_val);
     end;
     if Length(NewPasswordEdit.Text) = 0 then begin  // empty line after delete
       new_pass_str_val := '';
     end else begin
       NewPasswordEdit.Text := new_pass_str_val;              // default value
       NewPasswordEdit.SelStart := Length(NewPasswordEdit.Text);
     end;
  end else begin
     new_pass_old_val := new_pass_new_val;
     new_pass_str_val := NewPasswordEdit.Text;
  end;
end;

procedure TUserEditPasswordForm.OldPasswordEditChange(Sender: TObject);
begin
  if not TryStrToInt(OldPasswordEdit.Text, old_pass_new_val) then begin // not a number
     if not Length(old_pass_str_val) = 0 then begin   // not the first symbol
       str(old_pass_old_val, old_pass_str_val);
     end;
     if Length(OldPasswordEdit.Text) = 0 then begin  // empty line after delete
       old_pass_str_val := '';
     end else begin
       OldPasswordEdit.Text := old_pass_str_val;              // default value
       OldPasswordEdit.SelStart := Length(OldPasswordEdit.Text);
     end;
  end else begin
     old_pass_old_val := old_pass_new_val;
     old_pass_str_val := OldPasswordEdit.Text;
  end;
end;

end.

