unit KeySend;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ScaledForm;

type
  TKeySendForm = class(TScaledForm)
    Label1: TLabel;
    KeyserverCombo: TComboBox;
    ListView: TListView;
    SendButton: TButton;
    Button2: TButton;
    procedure SendButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    function Display(keys: TList; selected: string): integer;
  end;

implementation

{$R *.dfm}

uses GPGOps, GPGWrapper, Config;

{ TKeySendForm }

function TKeySendForm.Display(keys: TList; selected: string): integer;
var
  i: integer;
  key: TGPGKey;
  li: TListItem;
begin
  KeyserverCombo.Items.Clear;
  KeyserverCombo.Items.Assign(GConfig.KeyServers);
  if KeyserverCombo.Items.Count > 0 then KeyserverCombo.ItemIndex := 0;

  ListView.Items.Clear;
  for i := 0 to keys.Count - 1 do
  begin
    key := TGPGKey(keys[i]);
    li := ListView.Items.Add;
    li.Caption := key.UserID;
    li.SubItems.Add(key.GetKeyID(GConfig.Show16DigitIDs));
    if key.LongID = selected then li.Checked := true;
  end;
  ListView.AlphaSort;

  result := ShowModal;
end;

procedure TKeySendForm.SendButtonClick(Sender: TObject);
var
  ops: TGPGOps;
  sl: TStringList;
  i: integer;
begin
  ops := TGPGOps.Create;
  sl := TStringList.Create;
  Screen.Cursor := crHourglass;
  Application.ProcessMessages;
  try
    for i := 0 to ListView.Items.Count - 1 do
    begin
      if ListView.Items[i].Checked then sl.Add(ListView.Items[i].SubItems[0]);
    end;

    if sl.Count = 0 then
    begin
      MessageDlg('You must first check one or more keys to send to the keyserver.', mtError, [mbOK], 0);
      Exit;
    end;

    Screen.Cursor := crHourGlass;
    try
      ops.SendKeys(KeyServerCombo.Text, GConfig.ProxySetting, GConfig.ProxyHost, GConfig.ProxyPort, sl);
      Screen.Cursor := crDefault;
      MessageDlg('Key(s) successfully sent to keyserver.', mtInformation, [mbOK], 0);
      ModalResult := mrOK;
    except
      on ce: GPGConnectException do
      begin
        Screen.Cursor := crDefault;
        MessageDlg(ce.Message, mtError, [mbOK], 0);
      end
      else
      begin
        Screen.Cursor := crDefault;
        MessageDlg('An error occurred while attempting to send the selected keys to the keyserver.', mtError, [mbOK], 0);
      end;
    end;

  finally
    sl.Free;
    ops.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TKeySendForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ListView.Items.Count - 1 do
  begin
    if ListView.Items[i].Checked then ListView.Items[i].MakeVisible(false);
  end;
end;

end.
