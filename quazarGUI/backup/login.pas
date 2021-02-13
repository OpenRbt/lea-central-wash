unit login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit, fphttpclient;

type

  { TLoginForm }

  TLoginForm = class(TForm)
    LognBtn: TButton;
    PINLabel: TLabel;
    PINEdit: TEdit;
    procedure PINEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LognBtnClick(Sender: TObject);
  private
    new_val: integer = 0;
    old_val: integer = 0;
    str_val: string = '';
  public

  end;

var
  LoginForm: TLoginForm;

const
  serverEndpoint : string = 'http://localhost:8020/';

implementation

uses
  Base, Main;
{$R *.lfm}

{ TLoginForm }

procedure TLoginForm.PINEditChange(Sender: TObject);
begin
  if not TryStrToInt(PINEdit.Text, new_val) then begin // not a number
     if not Length(str_val) = 0 then begin   // not the first symbol
       str(old_val, str_val);
     end;
     if Length(PINEdit.Text) = 0 then begin  // empty line after delete
       str_val := '';
     end else begin
       PINEdit.Text := str_val;              // default value
       PINEdit.SelStart := Length(PINEdit.Text);
     end;
  end else begin
     old_val := new_val;
     str_val := PINEdit.Text;
  end;
end;

procedure TLoginForm.FormCreate(Sender: TObject);
begin

end;

procedure TLoginForm.LognBtnClick(Sender: TObject);
var
  successful: boolean = False;
begin
  with TFPHttpClient.Create(nil) do
    try
      try
        AddHeader('Content-Type', 'application/json');
        AddHeader('Pin', PINEdit.Text);
        Get(serverEndpoint + 'user');
        successful := True;

      except
        case ResponseStatusCode of
          0: ShowMessage('Can`t connect to server');
          401: ShowMessage('Unauthorized');
          500: ShowMessage('Server Error: 500');
          else
            ShowMessage('Unexpected Error: ' + IntToStr(ResponseStatusCode) +
              sLineBreak + ResponseStatusText);
        end;
      end;

    finally
      Free
    end;

    if successful then
    begin
      LoginForm.Hide;
      BaseForm.SetServerEndpoint(serverEndpoint);
      BaseForm.SetPinCode(PINEdit.Text);
      MainForm.Show;
    end;

    PINEdit.Text := '';
    str_val := '';
end;

end.

