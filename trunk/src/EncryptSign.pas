unit EncryptSign;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Contnrs, ScaledForm,
  GPGOps, Crypto;

type
  TEncryptSignForm = class(TScaledForm)
    Label1: TLabel;
    Label2: TLabel;
    OutFileChangeButton: TButton;
    EncryptGroup: TGroupBox;
    EncryptCheck: TCheckBox;
    ListView: TListView;
    SignGroup: TGroupBox;
    SignCheck: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox: TComboBox;
    ArmourOutputCheck: TCheckBox;
    ProcessButton: TButton;
    Button6: TButton;
    InNameLabel: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    AllowUntrustedCheck: TCheckBox;
    DataSigRadio: TRadioButton;
    SigRadio: TRadioButton;
    PlainSigRadio: TRadioButton;
    OutNameLabel: TLabel;
    Label6: TLabel;
    FilterEdit: TEdit;
    SymmetricCheck: TCheckBox;
    SecretIDLabel: TLabel;
    SecretCreatedLabel: TLabel;
    SecretExpiresLabel: TLabel;
    procedure EncryptCheckClick(Sender: TObject);
    procedure SignCheckClick(Sender: TObject);
    procedure ProcessButtonClick(Sender: TObject);
    procedure OutFileChangeButtonClick(Sender: TObject);
    procedure ArmourOutputCheckClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DataSigRadioClick(Sender: TObject);
    procedure PlainSigRadioClick(Sender: TObject);
    procedure SigRadioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ComboBoxChange(Sender: TObject);
  private
    FPublicKeys: TList;
    FProcessListUpdates: boolean;
    FOutputFileChanged: boolean;
    FOutputFileSpecified: boolean;
    FSelectedKeys: TStringList;
    procedure UpdateControls;
  public
    function Display(crypto: TCrypto; var inFilename, outFilename: string; encryptTicked, signTicked: boolean; const filter: string; out eso: TGPGEncryptSignOpts; recipients: TStrings; out signKeyLongID: string): integer;
  end;

var
  EncryptSignForm: TEncryptSignForm;

implementation

uses Config, Utils, GPGWrapper, Resources;

{$R *.dfm}

procedure TEncryptSignForm.FormCreate(Sender: TObject);
begin
  FSelectedKeys := TStringList.Create;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height - ListView.Height + 50;
end;

procedure TEncryptSignForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelectedKeys);
end;

function TEncryptSignForm.Display(crypto: TCrypto; var inFilename, outFilename: string; encryptTicked, signTicked: boolean; const filter: string; out eso: TGPGEncryptSignOpts; recipients: TStrings; out signKeyLongID: string): integer;
var
  i: integer;
  key, pkey: TGPGKey;
begin
  FPublicKeys := crypto.PublicKeys;
  FSelectedKeys.Clear;
  result := mrCancel;

  FOutputFileChanged := false;
  FOutputFileSpecified := false;
  SymmetricCheck.Checked := false;
  EncryptCheck.Checked := encryptTicked;
  SignCheck.Checked := signTicked;
  AllowUntrustedCheck.Checked := false;
  ArmourOutputCheck.Checked := false;
  DataSigRadio.Checked := true;

  if inFilename = '' then
  begin
    OpenDialog.Title := 'Select file to encrypt/sign';
    if not OpenDialog.Execute then Exit;
    inFilename := OpenDialog.FileName;
  end;

  InNameLabel.Caption := inFilename;

  if outFilename = '' then
    OutNameLabel.Caption := inFilename + '.' + CDefaultExt[GConfig.DefaultExt]
  else
  begin
    FOutputFileChanged := true;
    FOutputFileSpecified := true;
    OutNameLabel.Caption := outFilename;
    ArmourOutputCheck.Checked := true;
    OutFileChangeButton.Visible := false;
  end;

  FProcessListUpdates := false;
  FilterEdit.Text := filter;
  FProcessListUpdates := true;
  FilterEditChange(nil);

  ComboBox.Items.Clear;
  ComboBox.Sorted := false;
  for i := 0 to crypto.SecretKeys.Count - 1 do
  begin
    key := crypto.SecretKeys[i] as TGPGKey;
    if not key.IsValid then continue;
    // Get the corresponding public key and check it.
    pkey := crypto.GetUserKey(key.LongID);
    if Assigned(pkey) and not pkey.IsValid then continue;
    ComboBox.Items.AddObject(key.UserID, key);
  end;
  if (ComboBox.ItemIndex = -1) and (ComboBox.Items.Count > 0) then ComboBox.ItemIndex := 0;
  ComboBox.Sorted := true;

  for i := 0 to ComboBox.Items.Count - 1 do
  begin
    key := ComboBox.Items.Objects[i] as TGPGKey;
    if GConfig.DefaultSecretKey = key.LongID then ComboBox.ItemIndex := i;
  end;

  UpdateControls;
  result := ShowModal;

  if result = mrOK then
  begin
    outFilename := OutNameLabel.Caption;

    eso := [];
    if EncryptCheck.Checked then eso := eso + [gpgEncrypt];
    if SymmetricCheck.Checked then eso := eso + [gpgEncryptSym];
    if SignCheck.Checked then
    begin
      signKeyLongID := TGPGKey(ComboBox.Items.Objects[ComboBox.ItemIndex]).LongID;
      if DataSigRadio.Checked then eso := eso + [gpgSign];
      if PlainSigRadio.Checked then eso := eso + [gpgSignPlain];
      if SigRadio.Checked then eso := eso + [gpgSignDetached];
    end;
    if AllowUntrustedCheck.Checked then eso := eso + [gpgAlwaysTrust];
    if ArmourOutputCheck.Checked then eso := eso + [gpgASCIIOutput];

    recipients.Clear;
    for i := 0 to ListView.Items.Count - 1 do
    begin
      if ListView.Items[i].Checked then
      begin
        recipients.Add(TGPGKey(ListView.Items[i].Data).LongID);
      end;
    end;
  end;
end;

procedure TEncryptSignForm.FormShow(Sender: TObject);
begin
  if EncryptCheck.Checked then
  begin
    FilterEdit.SetFocus;
    FilterEdit.SelStart := 0;
    FilterEdit.SelLength := Length(FilterEdit.Text);
  end
  else
    SignCheck.SetFocus;
end;

procedure TEncryptSignForm.EncryptCheckClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.SignCheckClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.ProcessButtonClick(Sender: TObject);
var
  i: integer;
  found: boolean;
begin
  if not EncryptCheck.Checked and not SignCheck.Checked and not SymmetricCheck.Checked then
  begin
    MessageDlg('You must choose to at least encrypt or sign your data.', mtError, [mbOK], 0);
    Exit;
  end;

  if EncryptCheck.Checked then
  begin
    found := false;
    for i := 0 to ListView.Items.Count - 1 do
    begin
      if ListView.Items[i].Checked then
      begin
        found := true;
        break;
      end;
    end;

    if not found then
    begin
      MessageDlg('At least one recipient must be specified if you wish to encrypt the data.', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if AllowUntrustedCheck.Checked then
  begin
    if MessageDlg(REncryptAlwaysTrustWarning, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  ModalResult := mrOK;
end;

procedure TEncryptSignForm.OutFileChangeButtonClick(Sender: TObject);
begin
  SaveDialog.FileName := OutNameLabel.Caption;
  if SaveDialog.Execute then
  begin
    FOutputFileChanged := true;
    OutNameLabel.Caption := SaveDialog.FileName;
  end;
end;

procedure TEncryptSignForm.UpdateControls;
var
  key: TGPGKey;
begin
  ListView.Enabled := EncryptCheck.Checked;
  AllowUntrustedCheck.Enabled := EncryptCheck.Checked;
  DataSigRadio.Enabled := SignCheck.Checked;
  PlainSigRadio.Enabled := not EncryptCheck.Checked and SignCheck.Checked;
  SigRadio.Enabled := not EncryptCheck.Checked and SignCheck.Checked;
  if EncryptCheck.Checked then DataSigRadio.Checked := true;
  ComboBox.Enabled := SignCheck.Checked;
  ArmourOutputCheck.Enabled := not PlainSigRadio.Checked and not FOutputFileSpecified;

  if (ComboBox.ItemIndex >= 0) and SignCheck.Checked then
  begin
    key := TGPGKey(ComboBox.Items.Objects[ComboBox.ItemIndex]);
    SecretIDLabel.Caption := key.GetKeyID(GConfig.Show16DigitIDs);
    SecretCreatedLabel.Caption := 'created ' + DateTimeToYMD(key.CreatedDate);
    if key.ExpiryDate > 0 then
      SecretExpiresLabel.Caption := 'expires ' + DateTimeToYMD(key.ExpiryDate)
    else
      SecretExpiresLabel.Caption := '';
  end
  else
  begin
    SecretIDLabel.Caption := '';
    SecretCreatedLabel.Caption := '';
    SecretExpiresLabel.Caption := '';
  end;

  if not FOutputFileChanged then
  begin
    if SigRadio.Checked then
      OutNameLabel.Caption := InNameLabel.Caption + '.sig'
    else if ArmourOutputCheck.Checked or PlainSigRadio.Checked then
      OutNameLabel.Caption := InNameLabel.Caption + '.asc'
    else
      OutNameLabel.Caption := InNameLabel.Caption + '.' + CDefaultExt[GConfig.DefaultExt];
  end;
end;

procedure TEncryptSignForm.ArmourOutputCheckClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.DataSigRadioClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.PlainSigRadioClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.SigRadioClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TEncryptSignForm.FilterEditChange(Sender: TObject);
var
  i: integer;
  key: TGPGKey;
  li: TListItem;
  f: string;
begin
  FProcessListUpdates := false;

  f := UpperCase(FilterEdit.Text);
  ListView.Items.Clear;
  for i := 0 to FPublicKeys.Count - 1 do
  begin
    key := TGPGKey(FPublicKeys[i]);
    if not key.IsValid then continue;
    if (f <> '') and
       (Pos(f, UpperCase(key.UserID)) = 0) and
       (Pos(f, UpperCase(key.LongID)) = 0) then continue;

    li := ListView.Items.Add;
    li.Data := key;
    li.Caption := key.UserID;
    li.SubItems.Add(key.GetKeyID(GConfig.Show16DigitIDs));
    li.SubItems.Add(DateTimeToYMD(key.CreatedDate));
    if key.ExpiryDate > 1 then
      li.SubItems.Add(DateTimeToYMD(key.ExpiryDate))
    else
      li.SubItems.Add('');
    li.Checked := FSelectedKeys.IndexOf(key.LongID) > -1;
  end;

  if ListView.Items.Count = 1 then ListView.Items[0].Checked := true;

  FProcessListUpdates := true;
end;

procedure TEncryptSignForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  i: integer;
begin
  // Check for FSelectedKeys being assigned due to a weird bug where this
  // procedure gets called AFTER FormDestroy is called.  
  if not Assigned(Item.Data) or
     not FProcessListUpdates or
     not Assigned(FSelectedKeys) then Exit;

  if Item.Checked then
  begin
    FSelectedKeys.Add(TGPGKey(Item.Data).LongID);
  end
  else
  begin
    repeat
      i := FSelectedKeys.IndexOf(TGPGKey(Item.Data).LongID);
      if i >= 0 then FSelectedKeys.Delete(i);
    until i = -1;
  end;
end;

procedure TEncryptSignForm.ComboBoxChange(Sender: TObject);
begin
  UpdateControls;
end;

end.

