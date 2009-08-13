unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ScaledForm;

type
  TAboutForm = class(TScaledForm)
    TitleLabel: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    UsesLabel: TLabel;
    EXEPathLabel: TLabel;
    Label1: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    HomeDirLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses Utils, GPGOps, GPGWrapper;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  ops: TGPGOps;
begin
  TitleLabel.Caption := TitleLabel.Caption + ' ' + GetFileVer(Application.ExeName, true);
  ops := TGPGOps.Create;
  UsesLabel.Caption := 'Using GnuPG version ' + ops.GetVersion + ' found at:';
  EXEPathLabel.Caption := GGPGLocation.EXEPath;
  HomeDirLabel.Caption := GGPGLocation.HomeDir;
  ops.Free;
end;

end.
