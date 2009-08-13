unit KeyProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScaledForm,
  GPGWrapper, ComCtrls;

type
  TKeyPropertiesForm = class(TScaledForm)
    Button2: TButton;
    Label2: TLabel;
    FingerprintLabel: TLabel;
    UserLabel: TLabel;
    LongLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CreatedLabel: TLabel;
    ExpiresLabel: TLabel;
    Label6: TLabel;
    KeyTypeLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    UserTrustLabel: TLabel;
    CalcTrustLabel: TLabel;
    ListView: TListView;
    Label9: TLabel;
    SubKeysLV: TListView;
    Label1: TLabel;
    Label10: TLabel;
    KeyAlgorithmLabel: TLabel;
    Label11: TLabel;
    KeyUsesLabel: TLabel;
    Label12: TLabel;
    UIDListView: TListView;
    procedure SubKeysLVDblClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UIDListViewDblClick(Sender: TObject);
  private
    FKey: TGPGKey;
    FKeyList: TList;
    FInstance: integer;
  public
    function Display(key: TGPGKey; keyList: TList; instance: integer = 0): integer;
  end;

var
  KeyPropertiesForm: TKeyPropertiesForm;

implementation

{$R *.dfm}

uses Utils, Config;

{ TKeyPropertiesForm }

function TKeyPropertiesForm.Display(key: TGPGKey; keyList: TList; instance: integer): integer;
var
  i, n: integer;
  item: TListItem;
  skey: TGPGKey;
begin
  FKey := key;
  FKeyList := keyList;
  FInstance := instance;
  
  n := Pos(' (', key.UserID);
  if n = 0 then n := Pos(' <', key.UserID);
  if n > 0 then
    Caption := Copy(key.UserID, 1, n - 1)
  else if key.UserID <> '' then
    Caption := key.UserID
  else
    Caption := 'Key Properties';

  UserLabel.Caption := key.UserID;
  LongLabel.Caption := key.GetKeyID(GConfig.Show16DigitIDs);
  FingerprintLabel.Caption := key.GetFingerprint;
  CreatedLabel.Caption := DateTimeToYMD(key.CreatedDate);
  if key.ExpiryDate > 0 then
    ExpiresLabel.Caption := DateTimeToYMD(key.ExpiryDate)
  else
    ExpiresLabel.Caption := '(no expiry)';

  KeyTypeLabel.Caption := CGPGKeyType[key.KeyType];
  KeyAlgorithmLabel.Caption := IntToStr(key.KeyLength) + ' bit ' + CGPGAlgorithm[key.Algorithm];
  if key.IsSigning and key.IsEncrypt then
    KeyUsesLabel.Caption := 'Encryption and Signing'
  else if key.IsSigning then
    KeyUsesLabel.Caption := 'Signing'
  else if key.IsEncrypt then
    KeyUsesLabel.Caption := 'Encryption'
  else
    KeyUsesLabel.Caption := '';

  UserTrustLabel.Caption := CGPGTrust[key.UserTrust];
  CalcTrustLabel.Caption := CGPGTrust[key.CalcTrust];

  ListView.Items.Clear;
  for i := 0 to key.Signatures.Count - 1 do
  begin
    item := ListView.Items.Add;
    skey := TGPGKey(key.Signatures[i]);
    item.Data := skey;
    item.Caption := skey.UserIDName;
    item.SubItems.Add(skey.UserIDEmail);
    item.SubItems.Add(skey.GetKeyID(GConfig.Show16DigitIDs));
  end;

  SubKeysLV.Items.Clear;
  for i := 0 to key.SubKeys.Count - 1 do
  begin
    item := SubKeysLV.Items.Add;
    skey := TGPGKey(key.SubKeys[i]);
    item.Data := skey;
    item.Caption := CGPGKeyType[skey.KeyType];
    item.SubItems.Add(IntToStr(skey.KeyLength) + ' bit ' + CGPGAlgorithm[skey.Algorithm]);
    item.SubItems.Add(skey.GetKeyID(GConfig.Show16DigitIDs));
  end;

  UIDListView.Items.Clear;
  for i := 0 to key.UserIDs.Count - 1 do
  begin
    item := UIDListView.Items.Add;
    skey := TGPGKey(key.UserIDs[i]);
    item.Data := skey;
    item.Caption := skey.UserID;
    item.SubItems.Add(DateTimeToYMD(skey.CreatedDate));
    item.SubItems.Add(skey.GetKeyID(GConfig.Show16DigitIDs));
  end;

  result := ShowModal;
end;

procedure TKeyPropertiesForm.SubKeysLVDblClick(Sender: TObject);
var
  kp: TKeyPropertiesForm;
begin
  if not Assigned(SubKeysLV.Selected) then Exit;
  if not Assigned(SubKeysLV.Selected.Data) then Exit;

  kp := TKeyPropertiesForm.Create(self);
  kp.Display(SubKeysLV.Selected.Data, FKeyList, FInstance + 1);
  kp.Free;
end;

procedure TKeyPropertiesForm.ListViewDblClick(Sender: TObject);
var
  kp: TKeyPropertiesForm;
  key: TGPGKey;
  i: integer;
begin
  if not Assigned(ListView.Selected) then Exit;
  if not Assigned(ListView.Selected.Data) then Exit;

  key := TGPGKey(ListView.Selected.Data);
  if Assigned(FKeyList) then
  begin
    for i := 0 to FKeyList.Count - 1 do
    begin
      if TGPGKey(FKeyList[i]).LongID = key.LongID then
      begin
        key := TGPGKey(FKeyList[i]);
        break;
      end;
    end;
  end;

  kp := TKeyPropertiesForm.Create(self);
  kp.Display(key, FKeyList, FInstance + 1);
  kp.Free;
end;

procedure TKeyPropertiesForm.FormShow(Sender: TObject);
begin
  Top := Top + FInstance * 30;
  Left := Left + FInstance * 30;
end;

procedure TKeyPropertiesForm.UIDListViewDblClick(Sender: TObject);
var
  kp: TKeyPropertiesForm;
begin
  if not Assigned(UIDListView.Selected) then exit;
  if not Assigned(UIDListView.Selected.Data) then exit;

  kp := TKeyPropertiesForm.Create(self);
  kp.Display(UIDListView.Selected.Data, FKeyList, FInstance + 1);
  kp.Free;
end;

end.
