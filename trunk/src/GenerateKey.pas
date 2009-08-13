unit GenerateKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ScaledForm;

type
  TGenerateKeyForm = class(TScaledForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NameEdit: TEdit;
    AddressEdit: TEdit;
    CommentEdit: TEdit;
    ExpiryPicker: TDateTimePicker;
    ExpiresCheck: TCheckBox;
    PassphraseEdit: TEdit;
    ConfirmEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    GenerateButton: TButton;
    CancelButton: TButton;
    Label4: TLabel;
    Label7: TLabel;
    DSACombo: TComboBox;
    ElGamalCombo: TComboBox;
    StatusLabel: TLabel;
    Label8: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ExpiresCheckClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
  private
    FDots: integer;
    procedure CommandCallback(command: string; lineNumber: integer; isFragment: boolean; var write_stdin: THandle; var terminateProgram: boolean);
  public
    { Public declarations }
  end;

var
  GenerateKeyForm: TGenerateKeyForm;

implementation

{$R *.dfm}

uses DateUtils, GPGWrapper, GPGOps, Config, DisplayData, Resources;

procedure TGenerateKeyForm.FormShow(Sender: TObject);
begin
  FDots := 0;

  StatusLabel.Caption := '';
  NameEdit.Text := '';
  AddressEdit.Text := '';
  CommentEdit.Text := '';
  PassphraseEdit.Text := '';
  ConfirmEdit.Text := '';

  ExpiresCheck.Checked := false;
  ExpiryPicker.Visible := false;
  ExpiryPicker.DateTime := IncYear(Now);

  DSACombo.Clear;
  DSACombo.Items.Add('512');
  DSACombo.Items.Add('1024');
  DSACombo.ItemIndex := 1;

  ElGamalCombo.Clear;
  ElGamalCombo.Items.Add('512');
  ElGamalCombo.Items.Add('1024');
  ElGamalCombo.Items.Add('2048');
  ElGamalCombo.ItemIndex := 1;

  NameEdit.SetFocus;
end;

procedure TGenerateKeyForm.ExpiresCheckClick(Sender: TObject);
begin
  ExpiryPicker.Visible := ExpiresCheck.Checked;
end;

procedure TGenerateKeyForm.GenerateButtonClick(Sender: TObject);
var
  ops: TGPGOps;
  dt: TDateTime;
  ddf: TDisplayDataForm;
begin
  if Length(NameEdit.Text) < 1 then
  begin
    MessageDlg(RGenKeyMustSpecifyName, mtError, [mbOK], 0);
    Exit;
  end;

  if Length(PassphraseEdit.Text) < 1 then
  begin
    MessageDlg(RGenKeyMustSpecifyPassphrase, mtError, [mbOK], 0);
    Exit;
  end;

  if PassphraseEdit.Text <> ConfirmEdit.Text then
  begin
    MessageDlg(RGenKeyPhrasesMustMatch, mtError, [mbOK], 0);
    Exit;
  end;

  if ExpiresCheck.Checked and (ExpiryPicker.DateTime < Now) then
  begin
    MessageDlg(RGenKeyDateInPast, mtError, [mbOK], 0);
    Exit;
  end;

  dt := 0;
  if ExpiresCheck.Checked then dt := ExpiryPicker.DateTime;

  ops := TGPGOps.Create;
  try
    Enabled := false;
    GenerateButton.Enabled := false;
    CancelButton.Enabled := false;
    Screen.Cursor := crHourglass;
    Application.ProcessMessages;

    ops.DataReceivedCallback := CommandCallback;
    try
      ops.GenerateKey(
        gpgDSA, StrToInt(DSACombo.Items[DSACombo.ItemIndex]),
        gpgElGamal, StrToInt(ElGamalCombo.Items[ElGamalCombo.ItemIndex]),
        NameEdit.Text, CommentEdit.Text, AddressEdit.Text,
        dt, PChar(PassphraseEdit.Text)
      );

      MessageDlg(RGenKeySuccess, mtInformation, [mbOK], 0);
      ModalResult := mrOK;
    except
      on oe: GPGOpsException do
      begin
        ddf := TDisplayDataForm.Create(self);
        ddf.DisplayData(Format(RUnhelpfulError, ['GK', oe.RawData]));
        ddf.Free;
      end;
      on e: Exception do MessageDlg('Unable to generate key: ' + e.Message, mtError, [mbOK], 0);
    end;

  finally
    Enabled := true;
    GenerateButton.Enabled := true;
    CancelButton.Enabled := true;
    Screen.Cursor := crDefault;
    ops.Free;
  end;
end;

procedure TGenerateKeyForm.CommandCallback(command: string;
  lineNumber: integer; isFragment: boolean; var write_stdin: THandle;
  var terminateProgram: boolean);
var
  i: integer;
begin
  if isFragment then exit;
  
  if GConfig.DebugMode then
    StatusLabel.Caption := command
  else
  begin
    StatusLabel.Caption := RGenKeyStatusMessage;
    for i := 0 to FDots do StatusLabel.Caption := StatusLabel.Caption + '.';
    Inc(FDots);
    if FDots = 20 then FDots := 1;
  end;
  Application.ProcessMessages;
end;

end.
