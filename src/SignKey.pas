unit SignKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs, GPGWrapper, ScaledForm;

type
  TSignKeyForm = class(TScaledForm)
    Label1: TLabel;
    KeyLabel: TLabel;
    LongIDLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FingerprintLabel: TLabel;
    Label4: TLabel;
    KeyCombo: TComboBox;
    Button1: TButton;
    Button2: TButton;
    MatchCheck: TCheckBox;
    SelectedKeyInfoLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeyComboChange(Sender: TObject);
  private
  public
    function Display(secretKeys: TObjectList; keyToSign: TGPGKey; var signWithKeyLongID: string): integer;
  end;

implementation

{$R *.dfm}

uses Resources, Config, Utils;

{ TSignKeyForm }

function TSignKeyForm.Display(secretKeys: TObjectList; keyToSign: TGPGKey; var signWithKeyLongID: string): integer;
var
  i, j, n, defaultItem: integer;
  key, sig: TGPGKey;
  valid: integer;
  alreadySigned: boolean;
begin
  result := mrCancel;
  MatchCheck.Checked := false;

  if not keyToSign.IsValid then
  begin
    // Key must be valid to sign it.
    MessageDlg(RSignKeyMustBeValid, mtError, [mbOK], 0);
    Exit;
  end;

  valid := 0;
  defaultItem := 0;
  KeyCombo.Clear;
  for i := 0 to secretKeys.Count - 1 do
  begin
    key := secretKeys[i] as TGPGKey;
    if not key.IsValid then continue;
    Inc(valid);

    // Check whether this secret key has already signed the selected key.
    alreadySigned := false;
    for j := 0 to keyToSign.Signatures.Count - 1 do
    begin
      sig := keyToSign.Signatures[j] as TGPGKey;
      if sig.LongID = key.LongID then alreadySigned := true;
    end;
    if alreadySigned then continue;

    n := KeyCombo.Items.AddObject(key.UserID, key);
    if GConfig.DefaultSecretKey = key.LongID then defaultItem := n;
  end;

  if valid = 0 then
  begin
    // No valid secret keys.
    MessageDlg(RSignKeyNeedValidSecretKey, mtError, [mbOK], 0);
    Exit;
  end;

  if KeyCombo.Items.Count = 0 then
  begin
    // Already signed it.
    MessageDlg(RSignKeyAlreadySigned, mtError, [mbOK], 0);
    Exit;
  end;

  KeyCombo.ItemIndex := defaultItem;
  KeyComboChange(KeyCombo);

  KeyLabel.Caption := keyToSign.UserID;
  LongIDLabel.Caption := RID + ' ' + keyToSign.GetKeyID(GConfig.Show16DigitIDs);
  FingerprintLabel.Caption := RFingerprint + ' ' + keyToSign.GetFingerprint;

  result := ShowModal;

  if result = mrOK then
  begin
    signWithKeyLongID := TGPGKey(KeyCombo.Items.Objects[KeyCombo.ItemIndex]).LongID;
  end;
end;

procedure TSignKeyForm.Button1Click(Sender: TObject);
begin
  if not MatchCheck.Checked then
  begin
    MessageDlg(RSignKeyMustCheckFingerprint, mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TSignKeyForm.FormShow(Sender: TObject);
begin
  KeyCombo.SetFocus;
end;

procedure TSignKeyForm.KeyComboChange(Sender: TObject);
var
  key: TGPGKey;
begin
  if (KeyCombo.ItemIndex < 0) then exit;
  key := TGPGKey(KeyCombo.Items.Objects[KeyCombo.ItemIndex]);
  if not Assigned(key) then exit;
  
  SelectedKeyInfoLabel.Caption := 'ID ' + key.LongID + ' (created ' + DateTimeToYMD(key.CreatedDate) + ')';
end;

end.
