unit ChangePassphrase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TChangePassphraseForm = class(TForm)
  private
    { Private declarations }
  public
    function Display(passphrase: string): boolean;
    { Public declarations }
  end;

var
  ChangePassphraseForm: TChangePassphraseForm;

implementation

{$R *.dfm}

{ TChangePassphraseForm }

function TChangePassphraseForm.Display(passphrase: string): boolean;
begin
  result := false;
end;

end.
