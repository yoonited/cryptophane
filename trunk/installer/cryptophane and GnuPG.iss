; Cryptophane setup file

#include "common.iss"

[Setup]
AppName=Cryptophane+GnuPG

AppVerName=Cryptophane+GnuPG 0.7.3
AppVersion=0.7.0
OutputBaseFilename="cryptophane-0.7.3-gnupg-1.4.9"

AppCopyright=Cryptophane copyright 2005-2008 eCOSM.

[Files]
Source: "e:\Program Files\GNU\GnuPG\gpg.exe"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\gpgkeys_finger.exe"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\gpgkeys_hkp.exe"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\gpgkeys_curl.exe"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\gpgkeys_ldap.exe"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\Doc\COPYING.txt"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\Doc\README.txt"; DestDir: "{app}\GnuPG"
Source: "e:\Program Files\GNU\GnuPG\Doc\README-W32.txt"; DestDir: "{app}\GnuPG"

[Registry]
Root: HKLM; Subkey: "Software\GNU"
Root: HKLM; Subkey: "Software\GNU\GNUPG"
Root: HKLM; Subkey: "Software\GNU\GNUPG"; ValueType: string; ValueName: "gpgProgram"; ValueData: "{app}\GnuPG\gpg.exe"; flags: createvalueifdoesntexist

