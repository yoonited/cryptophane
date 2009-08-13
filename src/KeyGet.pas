unit KeyGet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScaledForm;

type
  TKeyGetForm = class(TScaledForm)
    Label1: TLabel;
    Label2: TLabel;
    KeyserverCombo: TComboBox;
    KeyEdit: TEdit;
    GetKeyButton: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetKeyButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KeyGetForm: TKeyGetForm;

implementation

{$R *.dfm}

uses GPGOps, Config, Resources, RegExpr;

procedure TKeyGetForm.FormCreate(Sender: TObject);
begin
  KeyserverCombo.Items.Clear;
  KeyserverCombo.Items.Assign(GConfig.KeyServers);
  if KeyserverCombo.Items.Count > 0 then KeyserverCombo.ItemIndex := 0;

  KeyEdit.Text := '';
end;

procedure TKeyGetForm.FormShow(Sender: TObject);
begin
  KeyEdit.SetFocus;
end;

procedure TKeyGetForm.GetKeyButtonClick(Sender: TObject);
var
  ops: TGPGOps;
  sl: TStringList;
  r: TRegExpr;
  imported: cardinal;
begin
  r := TRegExpr.Create;
  try
    r.Expression := '^[0-9a-f]{8,16}$';
    r.ModifierI := true;
    if not r.Exec(KeyEdit.Text) then
    begin
      MessageDlg(RKeyGetInvalidKey, mtError, [mbOK], 0);
      Exit;
    end;
  finally
    r.Free;
  end;

  ops := TGPGOps.Create;
  sl := TStringList.Create;
  try
    sl.Add(KeyEdit.Text);
    try
      ops.RecvKeys(KeyServerCombo.Text, GConfig.ProxySetting, GConfig.ProxyHost, GConfig.ProxyPort, sl, imported, nil);
      MessageDlg(RKeyGetSuccess, mtInformation, [mbOK], 0);
      modalResult := mrOK;
    except
      on ce: GPGConnectException do
        MessageDlg(ce.Message, mtError, [mbOK], 0)
      else
        MessageDlg(RKeyGetKeyNotFound, mtError, [mbOK], 0);
    end;
  finally
    sl.Free;
    ops.Free;
  end;
end;

end.
