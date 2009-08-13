unit EditTrust;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScaledForm,
  GPGWrapper;

type
  TEditTrustForm = class(TScaledForm)
    Label1: TLabel;
    Label2: TLabel;
    UnknownRadio: TRadioButton;
    NoRadio: TRadioButton;
    MarginalRadio: TRadioButton;
    Label3: TLabel;
    FullyRadio: TRadioButton;
    Label4: TLabel;
    UltimatelyRadio: TRadioButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    UserLabel: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function Display(key: TGPGKey): integer;
  end;

var
  EditTrustForm: TEditTrustForm;

implementation

{$R *.dfm}

uses GPGOps;

{ TEditTrustForm }

function TEditTrustForm.Display(key: TGPGKey): integer;
var
  ops: TGPGOps;
  trust: TGPGTrust;
begin
  UserLabel.Caption := key.UserID;
  case key.UserTrust of
    trustNone: NoRadio.Checked := true;
    trustMarginal: MarginalRadio.Checked := true;
    trustFull: FullyRadio.Checked := true;
    trustUltimate: UltimatelyRadio.Checked := true;
    else UnknownRadio.Checked := true;
  end;

  result := ShowModal;
  if result = mrOK then
  begin
    trust := trustUnknown;
    if NoRadio.Checked then trust := trustNone;
    if MarginalRadio.Checked then trust := trustMarginal;
    if FullyRadio.Checked then trust := trustFull;
    if UltimatelyRadio.Checked then trust := trustUltimate;
    
    ops := TGPGOps.Create;
    try
      try
        ops.EditTrust(key, trust);
        MessageDlg('Trust updated.', mtInformation, [mbOK], 0);
      except
        on e: Exception do MessageDlg('Unhelpful error: ' + e.Message, mtError, [mbOK], 0);
      end;
    finally
      ops.Free;
    end;
  end;
end;

procedure TEditTrustForm.Button1Click(Sender: TObject);
begin
  if UltimatelyRadio.Checked then
  begin
    if MessageDlg(
      'You should only use ultimate trust for keys that you are positive that you created yourself.'#13#10#10 +
      'For any other user that you trust unreservedly, use the Trust Fully option instead.'#13#10#10 +
      'Are you sure you want to grant ultimate trust to this user?',
      mtConfirmation, [mbYes, mbNo], 0
    ) <> mrYes then Exit;
  end;

  modalResult := mrOK;
end;

end.
