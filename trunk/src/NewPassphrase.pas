unit NewPassphrase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScaledForm;

type
  TNewPassphraseForm = class(TScaledForm)
    Label1: TLabel;
    PassphraseEdit: TEdit;
    OKButton: TButton;
    Button2: TButton;
    Label2: TLabel;
    ConfirmEdit: TEdit;
    Label3: TLabel;
    procedure OKButtonClick(Sender: TObject);
  private
  public
    function Display(passphrase: PChar): boolean;
  end;

implementation

{$R *.dfm}

{ TNewPassphraseForm }

function TNewPassphraseForm.Display(passphrase: PChar): boolean;
var
  blank: string;
  i: integer;
begin
  PassphraseEdit.MaxLength := 64;
  ConfirmEdit.MaxLength := 64;
  PassphraseEdit.Text := '';
  ConfirmEdit.Text := '';
  
  try
    result := false;
    if ShowModal <> mrOK then Exit;
    StrLCopy(passphrase, PChar(PassphraseEdit.Text), 63);
    result := true;
  finally
    blank := '';
    for i := 1 to Length(PassphraseEdit.Text) do blank := blank + ' ';
    PassphraseEdit.Text := blank;
    PassphraseEdit.Text := '';

    blank := '';
    for i := 1 to Length(ConfirmEdit.Text) do blank := blank + ' ';
    ConfirmEdit.Text := blank;
    ConfirmEdit.Text := '';
  end;
end;

procedure TNewPassphraseForm.OKButtonClick(Sender: TObject);
begin
  if Length(PassphraseEdit.Text) = 0 then
  begin
    MessageDlg('You must enter a passphrase.', mtError, [mbOK], 0);
    exit;
  end;

  if PassphraseEdit.Text <> ConfirmEdit.Text then
  begin
    MessageDlg('The two passphrase fields do not match.  Please re-enter both fields.', mtError, [mbOK], 0);
    exit;
  end;

  modalResult := mrOK;
end;

end.
