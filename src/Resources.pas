unit Resources;

interface

resourcestring
  RID = 'ID';
  RFingerPrint = 'Fingerprint';

  RTheRecipient = 'The recipient';
  RSignatureBy = 'Signature by:';
  RIsNotValid ='is not valid.';
  RTheSecretKey = 'The secret key';
  ROneSecretKey = 'One of the following secret keys';
  RThePublicKey = 'The public key';
  RRequiredToDecryptNotFound = 'is required to decrypt this data, but was not found.';
  RUnknownKey = 'unknown key';
  RUnknownError = 'An error occurred while performing the requested operation.';
  RUnhelpfulError = 'Unhelpful error message %s.  Please report the data below to the author.'#13#10'%s';
  RFileNotFound = 'File "%s" does not exist.';
  RCannotWriteFile = 'Cannot write to file "%s".';
  RDebugInfoTitle = 'Debug information';


  RGPGNotInstalled = 'GnuPG does not appear to be installed.'#13#10#10'Please install GPG in C:\Program Files\GnuPG\ and re-run this program.  Alternatively, read the documentation to learn how to specify a GnuPG path to Cryptophane.';
  RGPGInvalidVersion = 'This program requires GnuPG version 1.2.3 or above to run.'#13#10#10'Please install a new version and then re-run this program.';

  REncryptSuccess = 'Data successfully encrypted.';
  RSignSuccess = 'Data successfully signed.';
  REncryptSignSuccess = 'Data successfully encrypted and signed.';

  RMainCannotGetKeyLists = 'Cannot retrieve public/secret key lists.'#13#10#10'This generally indicates that Cryptophane is having trouble talking with GPG.  Unfortunately the program will not function correctly without the key lists loaded.';
  RMainSuccessDecryptAndVerify = 'Successfully decrypted and verified.';
  RMainSuccessVerify = 'Successfully verified.';
  RMainSuccessKeySign = 'Key successfully signed.';
  RMainSuccessSigsVerified = 'The following signatures were successfully verified:';
  RMainWarningSignatures = 'The following signatures were NOT valid:'#13#10#10'%s'#13#10'Either you do not have the public key to verify the signature, or the file has been tampered with.';
  RMainWarningSignaturesButOK = 'The file has been successfully decrypted but one or more signatures were not valid.';
  RMainInvalidRecipient = 'An invalid recipient was specified.';
  RMainNoData = 'You specified a file that does not contain valid GPG/PGP/OpenPGP data.'#13#10#10'Please check the file and retry.';
  RMainPublicAlreadySigned = 'That public key has already been signed by:';
  RMainDecryptionFailed = 'Decryption failed.';
  RMainFailKeySignExpired = 'You cannot sign a key that has expired.';
  RMainSuccessKeysReceived = 'All keys received from keyserver successfully.';
  RMainFailKeysReceived = 'Unable to receive keys from keyserver.';
  RMainSuccessImportPublic = 'Successfully imported the following public keys:';
  RMainSuccessImportSecret = '%d secret key(s) were imported.';
  RMainSuccessImportRevoked = '%d key(s) were revoked.';
  RMainImportOtherPublicFail = '%d public key(s) were not imported because they are unchanged.';
  RMainImportOtherSecret = '%d secret key(s) were also imported.';
  RMainImportOtherSecretFail = '%d secret key(s) were not imported because they already exist.';
  RMainImportFail = 'Unable to import any keys from the specified file.';
  RMainExportFail = 'Unable to export keys.';
  RMainExportSecretConfirm = 'If any other person gets access to your secret keys, they may be able to decrypt your messages or impersonate you.  Be sure you keep your exported keys safe.'#13#10#10'Are you sure you want to export your secret keys?';
  RMainDeleteConfirm = 'Are you sure you want to delete the key'#13#10#10'  %s'#13#10#10'from your Cryptophane keyring?';
  RMainDeleteSecretConfirm = 'You are about to delete a secret key.  This will mean all messages encrypted to this key will never be able to be read.'#13#10#10'Are you SURE you want to delete this secret key?';
  RMainDeleteError = 'There was an error deleting the key from the keyring.';
  RMainReqToValidate = 'was required to validate this key, but was not found.';
  RMainDecryptionSuccessful = 'However, the decryption/extraction of this file was successful.  Please note you are not guaranteed that the file came from the indicated person until you obtain the user''s public key and decrypt this file again.';  
  RMainWantToDownloadPrompt = 'Would you like to try to download this key from the keyserver?';
  RMainNoKeyservers = 'At least one key server must be configured.';
  RMainNoTempPath = 'Couldn''t get temp path.';
  RMainNoTempFile = 'Couldn''t create temporary file.';
  RMainOneKeyInRing = 'There is 1 key in your Cryptophane keyring.'; 
  RMainKeysInRing = 'There are %s keys in your Cryptophane keyring.';
  RMainNoKeySelected = 'No key is selected.'#13#10#10'Please select a key from your keyring by clicking on it, then try again.';
  RMainFolderNotEmpty = 'You cannot delete a folder that is not empty.'#13#10#10'Remove the keys from the folder and then try again.';
  RMainCannotDeleteRoot = 'You cannot delete the root folder.';
  RMainCannotEditSecretKeyTrust = 'You cannot edit the trust of a secret key when you do not have its corresponding private key also in your keyring.';
  RMainRevokeFail = 'Cannot create a revocation certificate for this key.'#13#10#10'Make sure you have the secret key available on your system.';
  RMainSuccessCreateRevoke = 'Revocation certificate successfully created.'#13#10#10'To revoke your key at a later date, import the revocation certificate using the File/Import Keys option and then send it to a keyserver so that no-one else can use it.';

  RInvalidRecipientHelp = 'Cryptophane is not sure that the key on your keyring belongs to the person you are encrypting this file to.'#13#10#10 + 'To fix this, right click on the recipient''s key in the Cryptophane window and select "Sign Key", then follow the instructions on that window.';

  RHint1Title = 'Cryptophane hint: Starting out using Cryptophane';
  RHint1Label1 = 'If you''ve never used cryptography products before, start by';
  RHint1Link1 = 'Creating a Secret Key for yourself.';
  RHint1Index1 = 'creating a new key pair';
  RHint1Label2 = 'If you''ve previously made a private key that you''d like to import into Cryptophane, learn about';
  RHint1Link2 = 'Importing Secret Keys.';
  RHint1Index2 = 'import keys';

  RHint2Title = 'Cryptophane hint: Adding other people''s keys to your keyring';
  RHint2Label1 = 'Learn how to search for other people''s keys using the';
  RHint2Link1 = 'Search Keyserver Function.';
  RHint2Index1 = 'search keyserver';
  RHint2Label2 = 'If someone has given you their key in a file, find out about';
  RHint2Link2 = 'Importing Public Keys.';
  RHint2Index2 = 'importing keys';

  RHint3Title = 'Cryptophane hint: Signing keys on your keyring';
  RHint3Label1 = 'Before you can encrypt or decrypt files from other people, you have to make sure their key really belongs to them.';
  RHint3Link1 = '';
  RHint3Index1 = '';
  RHint3Label2 = 'You do this by checking and then signing their key.  Find out more about';
  RHint3Link2 = 'Signing Keys.';
  RHint3Index2 = 'signing a key';

  RHint4Title = 'Cryptophane hint: Assigning trust to other users';
  RHint4Label1 = 'Once you''ve signed a person''s key, you may decide you want to automatically trust the people that they trust.';
  RHint4Link1 = '';
  RHint4Index1 = '';
  RHint4Label2 = 'This is called a "web of trust".  Find out more about';
  RHint4Link2 = 'Setting Your Level of Trust in a Key.';
  RHint4Index2 = 'trust';

  RHint5Title = 'Cryptophane hint: Using Cryptophane';
  RHint5Label1 = 'Find out about how to';
  RHint5Link1 = 'Encrypt and Sign Files.';
  RHint5Index1 = 'encrypt';
  RHint5Label2 = 'Or take a look at Cryptophane''s';
  RHint5Link2 = 'Advanced Features.';
  RHint5Index2 = 'advanced features';


  // TODO : write help on the following:
  RMainBadSignature = 'The data you attempted to decrypt or verify has a bad signature.'#13#10#10 + 'This means that the data has been modified after being signing.';
  RBadSignatureHelp = 'Text will go here to explain how you should get around this problem.';
  // todo end

  REncryptAlwaysTrustWarning = 'Enabling the "allow untrusted recipients" option is not normally a good idea.  Here is why:'#13#10#10 + 'Anyone can make a key with your friend''s e-mail address and upload it to a keyserver.  If you encrypt a message to this imposter''s key, they will be able to read it which defeats the whole point of cryptography.'#13#10#10 + 'Cryptophane makes sure that it trusts the key belongs to the person it says it does by looking at who has signed their key (indicating they believe the key is legitimate) and who you trust.'+'  Disabling this protection could lead to you encrypting a message to someone other than who the key claims to be owned by.'#13#10#10 + 'Are you sure you want to allow untrusted recipients?';

  RSignKeyMustBeValid = 'You can only sign a key that is valid and not expired.';
  RSignKeyNeedValidSecretKey = 'You must have at least one valid secret key on your keyring in order to sign keys.';
  RSignKeyAlreadySigned = 'You have already signed this key with all the valid secret keys in your keyring.';
  RSignKeyMustCheckFingerprint = 'You MUST check that the fingerprint of the key is correct before signing it.';

  RKeyGetInvalidKey = 'Key ID must be a hexidecimal number either 8 or 16 characters long.'#13#10#10'If you want to search for a key, use the Keys/Search Key Server menu option.';
  RKeyGetSuccess = 'Successfully imported key.';
  RKeyGetKeyNotFound = 'The keyserver has no key with that ID.';

  RGenKeyMustSpecifyName = 'You must specify a name for the new key.';
  RGenKeyMustSpecifyPassphrase = 'You must enter a passphrase to protect the new key.';
  RGenKeyPhrasesMustMatch = 'The passphrase and the confirm passphrase fields must contain the passphrase.';
  RGenKeyDateInPast = 'You cannot have an expiry date in the past.';
  RGenKeySuccess = 'Key successfully generated.';
  RGenKeyStatusMessage = 'Generating';

  RExportKeysMustSelect = 'You must check at least one key to export.';

  RChangePassphraseSuccess = 'Passphrase successfully changed.';
  RChangePassphraseFailed = 'Unable to change passphrase of selected key.'; 

implementation

end.
