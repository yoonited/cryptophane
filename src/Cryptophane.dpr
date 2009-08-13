program Cryptophane;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  GPGWrapper in 'GPGWrapper.pas',
  Utils in 'Utils.pas',
  About in 'About.pas' {AboutForm},
  Passphrase in 'Passphrase.pas' {PassphraseForm},
  EncryptSign in 'EncryptSign.pas' {EncryptSignForm},
  SignKey in 'SignKey.pas' {SignKeyForm},
  KeyProperties in 'KeyProperties.pas' {KeyPropertiesForm},
  KeySearch in 'KeySearch.pas' {KeySearchForm},
  KeySend in 'KeySend.pas' {KeySendForm},
  GenerateKey in 'GenerateKey.pas' {GenerateKeyForm},
  EditTrust in 'EditTrust.pas' {EditTrustForm},
  KeyGet in 'KeyGet.pas' {KeyGetForm},
  Config in 'Config.pas',
  Resources in 'Resources.pas',
  ShellSetup in 'ShellSetup.pas',
  Setup in 'Setup.pas' {SetupForm},
  GetMessage in 'GetMessage.pas' {GetMessageForm},
  DisplayData in 'DisplayData.pas' {DisplayDataForm},
  KeyExport in 'KeyExport.pas' {KeyExportForm},
  AddFolder in 'AddFolder.pas' {AddFolderForm},
  ScaledForm in 'ScaledForm.pas',
  Crypto in 'Crypto.pas',
  NewPassphrase in 'NewPassphrase.pas' {NewPassphraseForm},
  SymPassphrase in 'SymPassphrase.pas' {SymPassphraseForm},
  Call in 'Call.pas',
  GPGOps in 'GPGOps.pas',
  ChangePassphrase in 'ChangePassphrase.pas' {ChangePassphraseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Cryptophane';
  Application.HelpFile := 'Cryptophane.chm';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
