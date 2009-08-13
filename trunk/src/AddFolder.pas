unit AddFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScaledForm;

type
  TAddFolderForm = class(TScaledForm)
    Label1: TLabel;
    FolderEdit: TEdit;
    OKButton: TButton;
    Button2: TButton;
    procedure OKButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddFolderForm: TAddFolderForm;

implementation

{$R *.dfm}

procedure TAddFolderForm.OKButtonClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  FolderEdit.Text := Trim(FolderEdit.Text);
  s := FolderEdit.Text;
  if s = '' then
  begin
    MessageDlg('You must specify a folder name.', mtError, [mbOK], 0);
    Exit;
  end;

  for i := 1 to Length(s) do
  begin
    if (Ord(s[i]) < 32) or (Ord(s[i]) > 126) or (s[i] in ['/']) then
    begin
      MessageDlg('Invalid character in new folder name.', mtError, [mbOK], 0);
      Exit;
    end; 
  end;

  ModalResult := mrOK;
end;

end.
