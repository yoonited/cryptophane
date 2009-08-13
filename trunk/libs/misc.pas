unit misc;
// version 1.1
// provides application independant functions, should not use any non-delphi
// units that aren't publically available.

{ NOTE: md5 routines used to be in this unit.  They've been taken out.
  Use "miscmd5" if you want them. }

interface

uses
  registry, windows, graphics;

  // Time/date functions
  function UnixTime: Int64;
  function DateTimeToRFC2822Date(dateAndTime: TDateTime): String;

  // graphics
  procedure ReplaceColor(B: TBitmap; C1, C2: TColor);

  // descriptive string functions
  function bytesToStr(bytes: Integer): String;
  function strToBytes(bytes: string): Integer;
  function describeSeconds(seconds: Integer; const zeroOrLessSecondsMessage: String = ''): String;
  function formatTickCount(ticks: DWORD): string;

  // file stuff
  function DeleteDirectoryCompletely(path: string): boolean;
  function DeleteFileWithWildcard(fileName: String; deleteReadOnly: boolean = false): Integer;
  function FindFileOnPath(fileName: String): String;
  function GetApplicationDataPath: String;
  function GetApplicationPath: String;
  function ResolveShortcut(const shortcut: string; const handle: HWND; out linkedFile, linkedWorkingDirectory, linkedArguments, linkedDescription: string): boolean;  // procedure IncludeTrailingBackslash() - defined in sysUtils. In delphi 6 it's IncludeTrailingPathDelimiter()
  function GetTemporaryFileName(const prefix, extension: String; tempPath: String = ''): String;
  // function copyFile - defined in windows

  // system stuff
  function ExpandPathWithEnvironmentStrings(const path: String): String;
  function ExecCmdLineAndWait(const CmdLine: string; WindowState: word): boolean;
  function SleepUntilMessage(milliseconds: Integer): Boolean;
  function DeleteKeyCompletely(reg: TRegistry; key: String): Boolean;
  function GetFileTypeShellCommand(fileType: string): string;
  function WindowsErrorDescription(errorNumber: DWORD): string;

  // string stuff
  function Urlencode(s : string) : string;
  function SplitString(const stringToSplitOn, S: String; var left, right: String; leftMostSplit: Boolean = true): Boolean; overload
  function CharInString(const S: String; const c: Char): Boolean; overload;
  function parseQuoted(const text: String; quoteChar, escapeChar: Char): String;
  function LTrim(const S: String; const charsToTrim: String = ''): String; overload;
  function RTrim(const S: String; const charsToTrim: String = ''): String; overload;
  function CompareSuffix(const suffix, S: String): Boolean;
  function ComparePrefix(const prefix, S: String): Boolean;
  function StripControlChars(const s: String): String;

  function XORString(const a, b: string): string;

  // WideString stuff
  function SplitString(const stringToSplitOn, S: WideString; var left, right: WideString; leftMostSplit: Boolean = true): Boolean; overload
  function LTrim(const S: widestring; const charsToTrim: widestring = ''): widestring; overload;
  function RTrim(const S: widestring; const charsToTrim: widestring = ''): widestring; overload;
  function StripWideControlChars(const s: widestring): widestring;
  function CharInString(const S: widestring; const c: WideChar): Boolean; overload;


  // Conditional function - replaces (expression ? argTrue : argFalse)
  function Conditional(const expression: boolean; const argTrue, argFalse: string): string; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: widestring): widestring; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: integer): integer; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: int64): int64; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: single): single; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: double): double; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: boolean): boolean; overload;
  function Conditional(const expression: boolean; const argTrue, argFalse: TObject): TObject; overload;

  function ClampBounds(var input: Integer; lower, upper: Integer): Integer; overload;
  function ClampBounds(var input: Double; lower, upper: Double): Double; overload;

  // psudo random number generator
  // (incase you want to use a generator that won't change between
  // versions of delphi, and can be ported)
  procedure PseudoRandomize; overload;
  procedure PseudoRandomize(seed: Integer); overload;
  function  PseudoRandom(range: Integer): Integer;


implementation

uses
//  md5 {ICS can be found at http://users.swing.be/francois.piette/indexuk.htm},
  sysUtils, shlobj, activex, forms, math, dialogs, classes, FileCtrl;

var
  gRandomNumberGeneratorSeed: Integer = 1556; // init this to anything other than 0

function UnixTime: Int64;
begin
  Result := Round((Time - EncodeDate(1970, 1, 1)) * SecsPerDay);
end;


function DateTimeToRFC2822Date(dateAndTime: TDateTime): String;
// returns a date string like what would be found in an email
//
// use this rather than FormatDateTime, as FormatDateTime will use local
// language for the days of the week etc.
// (And this will get the time zone information)
const
  dayNames:   array[0..7]  of string = ('N/A', 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  monthNames: array[0..12] of string = ('N/A', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

var
  timeZone: TTimeZoneInformation;
  zoneOffset, formatString: String;
  dayIndex, monthIndex, yearIndex: Word;
  timeZoneResult: DWORD;
  biasInMinutes: integer;
begin
  timeZoneResult := GetTimeZoneInformation(timeZone);
  biasInMinutes := timeZone.Bias;
  if timeZoneResult = TIME_ZONE_ID_STANDARD then Inc(biasInMinutes, timeZone.StandardBias);
  if timeZoneResult = TIME_ZONE_ID_DAYLIGHT	then Inc(biasInMinutes, timeZone.DaylightBias);

  // add the minutes to zoneOffset
  zoneOffset := intToStr(abs(biasInMinutes) mod 60);
  while length(zoneOffset) < 2 do zoneOffset := '0' + zoneOffset;

  // add the hours to zoneOffset
  zoneOffset := intToStr(abs(biasInMinutes) div 60) + zoneOffset;
  while length(zoneOffset) < 4 do zoneOffset := '0' + zoneOffset;

  // add the sign/direction to zoneOffset
  zoneOffset := Conditional(biasInMinutes > 0, '-', '+') + zoneOffset;

  // we can't just do the following, as in non english speaking countrys, the
  // day and month names will be wrong.
  // dt := FormatDateTime('ddd, d mmm yyyy hh":"nn":"ss', dateAndTime) + ' ' + s;
     
  DecodeDate(dateAndTime, yearIndex, monthIndex, dayIndex);
  formatString := '"' + dayNames[DayOfWeek(dateAndTime)] + '", d "' + monthNames[monthIndex] +
    '" yyyy hh":"nn":"ss "' + zoneOffset + '"';

  Result := FormatDateTime(formatString, dateAndTime);
end;


procedure ReplaceColor(B: TBitmap; C1, C2: TColor);
var
  B2: TBitmap;
begin
  B2 := TBitmap.Create;
  try
    B2.Width := B.Width;
    B2.Height := B.Height;
    B2.Canvas.Draw(0, 0, B);
    B.Canvas.Brush.Color := C2;
    B.Canvas.BrushCopy(Rect(0, 0, B.Width, B.Height), B2, Rect(0, 0, B2.Width, B2.Height), C1);
  finally
    B2.Free;
  end;
end;



function DeleteDirectoryCompletely(path: string): boolean;
// RemoveDir() only works if the directory is empty.
// DeleteDirectoryCompletely() recursively deletes everything and then deletes
// the directory.
// Returns true if successful
var
  matchRec: TSearchRec;
begin
  path := IncludeTrailingBackslash(path);
  if length(path) < 5 then raise Exception.Create('DeleteDirectoryCompletely() called too close to root'); // must specify a path else you'll erase everything

  if DirectoryExists(path) then
  begin
    // First delete all the subdirectories
    if FindFirst(path + '*', faDirectory, matchRec) = 0 then
    begin
      repeat
        if (matchRec.Name <> '.') and
           (matchRec.Name <> '..') and
           DirectoryExists(path + matchRec.Name) then // FindFirst will return more than just directorys, it will return files too, so we need to make sure we are looking at a directory
        begin
          Result := DeleteDirectoryCompletely(path + matchRec.Name);
          if not Result then exit;
        end;
      until FindNext(matchRec) <> 0;
    end;
    SysUtils.FindClose(matchRec);

    // Now delete all the files
    DeleteFileWithWildcard(path + '*', true);

    // Now delete the directory
    Result := RemoveDir(path);
  end
  else Result := false;
end;




function DeleteFileWithWildcard(fileName: String; deleteReadOnly: boolean = false): Integer;
// just like DeleteFile() but the filename can include wildacards like *
// returns the number of files deleted
var
  path, fileToDelete: String;
  matchRec: TSearchRec;
  attributes: integer;
begin
  Result := 0;
  path := ExtractFilePath(fileName); // will need path later, and the matchRec only returns the filename portion

  if FindFirst(fileName, faAnyFile, matchRec) = 0 then
  begin
    repeat
      fileToDelete := path + matchRec.Name;
      if deleteReadOnly then
      begin
        attributes := FileGetAttr(fileToDelete);
        if (attributes and faReadOnly) <> 0 then FileSetAttr(fileToDelete, attributes - faReadOnly);
      end;

      if SysUtils.DeleteFile(path + matchRec.Name) then Inc(Result);
    until FindNext(matchRec) <> 0;
  end;
  Sysutils.FindClose(matchRec);
end;

(*
function expandPathWithEnvironmentStrings(const path: String): String;
// expand any environment variables, such as %PROGRAMFILES%
var
  expandedPath: String;
  ret: Integer;
begin
  SetLength(expandedPath, MAX_PATH + 100);
  FillChar(expandedPath[1], length(expandedPath), 0);

  ret := ExpandEnvironmentStrings(PChar(path), PChar(expandedPath), length(expandedPath) - 1);

  if ret > 0 then Result := Trim(expandedPath) else Result := path;
end;
*)
function ExpandPathWithEnvironmentStrings(const path: String): String;
// expand any environment variables, such as %PROGRAMFILES%
// (this version can handle vars like %path% that can potentially be longer
// than MAX_PATH)
var
  expandedString: String;
  bufferSize, ret: Integer;
  dummy: Char;
begin
  // find the required buffer size
  ret := ExpandEnvironmentStrings(PChar(path), @dummy, 0);
  SetLength(expandedString, ret + 1);

  repeat
    bufferSize := Length(expandedString) - 1;
    ret := ExpandEnvironmentStrings(PChar(path), PChar(expandedString), length(expandedString) - 1);
    if ret > bufferSize then
    begin
      // variable must have changed since we checked the size
      SetLength(expandedString, ret + 1);
    end;
  until ret <= bufferSize;

  Delete(expandedString, ret, length(expandedString)); // not ret+1 as we don't care for the null terminator
  Result := expandedString;
end;



function  ExecCmdLineAndWait(const CmdLine: string; WindowState: word): boolean;
// Execute a complete shell command line and waits until terminated.
// From: http://www.idev.ch

// values for WindowState:
// (from winuser.h)
// SW_HIDE, SW_SHOWNORMAL, SW_NORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED,
// SW_MAXIMIZE, SW_SHOWNOACTIVATE, SW_SHOW, SW_MINIMIZE, SW_SHOWMINNOACTIVE,
// SW_SHOWNA, SW_RESTORE, SW_SHOWDEFAULT, SW_FORCEMINIMIZE, SW_MAX
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, NIL,
                          nil {PChar(ExtractFilePath(Filename))},
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);

  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);

  // use GetExitCodeProcess here if you want the exit code
end;





function bytesToStr(bytes: Integer): String;
// the compliment of strToBytes()
// formats bytes into a string, eg 1024 -> 1KB
begin
  if bytes < 1024 then
  begin
    //Result := intToStr(bytes) + ' bytes';
    Result := '1KB';
  end
  else if bytes < 1048576 then
  begin
    Result := FloatToStrF(bytes / 1024.0, ffNumber, 18, 1) + 'KB';
  end
  else Result := FloatToStrF(bytes / 1048576.0, ffNumber, 18, 1) + 'MB';

end;


function strToBytes(bytes: string): Integer;
// the compliment of bytesToStr()
// taked a formatted bytes string (eg "1 KB", "203 bytes" etc) and returns
// the bytes as an integer
var
  size, suffix, character: String;
  chopPos, scalar, index: Integer;
begin
  // find the first non-numeric character
  bytes := Trim(Uppercase(bytes));
  chopPos := -1;
  for index := 1 to Length(bytes) do
  begin
    character := bytes[index];
    if ((character < '0') or (character > '9')) and (character <> DecimalSeparator) and (character <> ThousandSeparator) then
    begin
      chopPos := index;
      break;
    end;
  end;
  if chopPos <= 1 then
  begin
    //raise exception.Create('C734');
    // this is needed as TAdvStrGrid.QSort is flawed, it takes a copy of the
    // fixed row and then compares that with whatever other row, so we get
    // strToBytes('SIZE') called
    result := 0;
    exit;
  end;

  size := Copy(bytes, 0, chopPos - 1);
  suffix := Copy(bytes, chopPos, Length(bytes) + 1 - chopPos);

  if suffix = 'bytes' then scalar := 1
  else if suffix = 'KB' then scalar := 1024
  else if suffix = 'MB' then scalar := 1048576
  else scalar := 0;

  try
    Result := Round(strToFloat(size) * scalar);
  except
    // this is needed as TAdvStrGrid.QSort is flawed, it takes a copy of the
    // fixed row and then compares that with whatever other row, so we get
    // strToBytes('SIZE') called
    result := 0;
    //raise exception.Create('C735');
  end;
end;

function describeSeconds(seconds: Integer; const zeroOrLessSecondsMessage: String = ''): String;
var
  hr, min, sec: Integer;
begin
  if seconds <= 0 then
  begin
    Result := zeroOrLessSecondsMessage;
  end
  else
  begin
    hr := seconds div 3600;
    min := (seconds div 60) mod 60;
    sec := seconds mod 60;

    Result := '';
    if hr = 1 then Result := intToStr(hr) + ' hour';
    if hr > 1 then Result := intToStr(hr) + ' hours';

    if hr > 0 then
    begin
      if (min = 0) or (sec = 0) then
      begin
        if (min > 0) or (sec > 0) then Result := Result + ' and ';
      end
      else Result := Result + ', ';
    end;

    if min = 1 then Result := Result + intToStr(min) + ' minute';
    if min > 1 then Result := Result + intToStr(min) + ' minutes';
    if (min > 0) and (sec > 0) then Result := Result + ' and ';

    if sec = 1 then Result := Result + intToStr(sec) + ' second';
    if sec > 1 then Result := Result + intToStr(sec) + ' seconds';
  end;
end;


function formatTickCount(ticks: DWORD): string;
var
  temp: dword;
begin
  temp := ticks div 3600000;
  Result := intToStr(temp) + ':';
  Dec(ticks, temp * 3600000);

  temp := ticks div 60000;
  if temp >= 10 then
    Result := Result + intToStr(temp) + ':'
  else
    Result := Result + '0' + intToStr(temp) + ':';
  Dec(ticks, temp * 60000);

  temp := ticks div 1000;
  if temp >= 10 then
    Result := Result + intToStr(temp) + '.'
  else
    Result := Result + '0' + intToStr(temp) + '.';
  Dec(ticks, temp * 1000);

  if ticks > 99 then
    Result := Result + IntToStr(ticks)
  else if ticks > 9 then
    Result := Result + '0' + IntToStr(ticks)
  else
    Result := Result + '00' + IntToStr(ticks);
end;


function Urlencode(s : string) : string;
// escapes characters that shouldn't be used in the values of key/value pairs
// in a URL.
var
  i, o : integer;
  t, v : string;
begin
  v := '0123456789ABCDEF';
  for i := 1 to length(s) do
  begin
    o := ord(s[i]);
    case chr(o) of
      'a'..'z', 'A'..'Z', '0'..'9': t := t + chr(o);

      // There seems to be a fair amount of controversy on the 'net whether + is
      // considered a space or not.  The answer is: according to the original
      // URLspec (RFC 1738) it's not, and according to the HTTP spec (2396/2616)
      // it is.
      //
      // http://lists.w3.org/Archives/Public/w3c-dist-auth/2000AprJun/0040.html
      ' ': t := t + {'%20'}'+';
      else t := t + '%' + v[(o shr 4) + 1] + v[(o and 15) + 1];
    end;
  end;
  result := t;
end;



function XORString(const a, b: string): string;
var
  maxLength, minLength, i: Integer;
begin
  maxLength := max(Length(a), Length(b));
  minLength := min(Length(a), Length(b));

  SetLength(Result, maxLength);

  for i := 1 to minLength do
  begin
    Result[i] := Char(Ord(a[i]) xor Ord(b[i]));
  end;

  if (minLength + 1) > Length(a) then
  begin
    // the rest of the string (if any) does not contain 'a', and so consists
    // only of 'b'
    for i := (minLength + 1) to maxLength do Result[i] := b[i];
  end
  else
  begin
    // the rest of the string (if any) contains 'a', and so must not consist
    // of 'b'
    for i := (minLength + 1) to maxLength do Result[i] := a[i];
  end;
end;



function GetApplicationDataPath: String;
// Returns '' if windows could not provide an application data path
// DOES NOT end in a '\'
// (Can use IncludeTrailingBackslash() if you want a '\' at the end)
var
  ret: HResult;
  ok: Boolean;
  path: String;
  IDListPtr: PItemIDList;
  malloc: IMalloc;
begin
  path := '';

  // get the PIDL of the application data folder
  // (using API calls that are available without having to install shfolder.dll)
  ret := SHGetSpecialFolderLocation(Application.handle, CSIDL_APPDATA, IDListPtr);
  if ret = NOERROR then
  begin
    path := StringOfChar(#0, MAX_PATH);
    ok := SHGetPathFromIDList(IDListPtr, PChar(path));
    if ok then path := trim(path) else path := '';

    // free the item identifier list malloced by SHGetSpecialFolderLocation
    SHGetMalloc(malloc);
    malloc.free(IDListPtr);
  end;

  Result := path;
end;


function GetApplicationPath: String;
// returns the application path, minus quotes, and ending in a \
begin
  Result := ExtractFilePath(Application.ExeName);

  if length(Result) > 0 then
  begin
    if (Result[1] = '"') and (Result[length(Result)] = '"') then
    begin
      // strip quotes
      Result := copy(Result, 2, length(Result) - 2);
    end;

    //Result := IncludeTrailingPathDelimiter(Result); //IncludeTrailingPathDelimiter is the delphi 6 replacement of IncludeTrailingBackslash
    Result := IncludeTrailingBackslash(Result);
  end;
end;


function ResolveShortcut(const shortcut: string; const handle: HWND; out linkedFile, linkedWorkingDirectory, linkedArguments, linkedDescription: string): boolean;
var
  shortcutInterface: IShellLink;
  persistFileInterface: IPersistFile;
  HRes: HResult;
  shortcutData: WIN32_FIND_DATA;
begin
  Result := false;

  // Blank the output strings, and make them long enough to receive the data
  linkedFile             := StringOfChar(#0, MAX_PATH);
  linkedWorkingDirectory := StringOfChar(#0, MAX_PATH);
  linkedArguments        := StringOfChar(#0, MAX_PATH);
  linkedDescription      := StringOfChar(#0, MAX_PATH);


  // Get an IShellLink interface
  HRes := CoCreateInstance(
    CLSID_ShellLink,
    nil,
    CLSCTX_INPROC_SERVER,
    IShellLink,
    shortcutInterface
  );

  if HRes = S_OK then
  begin
    // Get the IPersistFile interface.
    if Supports(shortcutInterface, IPersistFile, persistFileInterface) then
    begin
      // Load the shortcut
      if persistFileInterface.Load(StringToOleStr(shortcut), STGM_READ) = S_OK then
      begin
        // Resolve the link
        if shortcutInterface.Resolve(handle, SLR_ANY_MATCH) = NOERROR then
        begin
          // So far so good, assume success unless one of the following queries
          // fails.
          Result :=
            (shortcutInterface.GetPath(@linkedFile[1], Length(linkedFile), shortcutData, SLGP_UNCPRIORITY) = NOERROR) and
            (shortcutInterface.GetWorkingDirectory(@linkedWorkingDirectory[1], Length(linkedWorkingDirectory)) = NOERROR) and
            (shortcutInterface.GetArguments(@linkedArguments[1], Length(linkedArguments)) = NOERROR) and
            (shortcutInterface.GetDescription(@linkedDescription[1], Length(linkedDescription)) = NOERROR);
        end;
      end;
    end;
  end;

  // Trim the strings
  linkedFile             := Trim(linkedFile);
  linkedWorkingDirectory := Trim(linkedWorkingDirectory);
  linkedArguments        := Trim(linkedArguments);
  linkedDescription      := Trim(linkedDescription);
end;


function GetTemporaryFileName(const prefix, extension: String; tempPath: String = ''): String;
// If tempPath is not specified then the windows tempory directory will be used

  function randomChar: Char;
  const
    charArray = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  begin
    Result := charArray[trunc(random(36)) + 1];
  end;

var
  prospectiveFileName: String;
begin
  if (tempPath = '') or not DirectoryExists(tempPath) then
  begin
    tempPath := StringOfChar(#0, MAX_PATH + 1);
    GetTempPath(MAX_PATH, PChar(tempPath));
    tempPath := Trim(tempPath);
  end;
  tempPath := IncludeTrailingBackslash(tempPath);

  Randomize;

  repeat
    prospectiveFileName := tempPath + prefix +
      randomChar + randomChar + randomChar + randomChar +
      '.' + extension;
  until not FileExists(prospectiveFileName);

  Result := prospectiveFileName;
end;




function SplitString(const stringToSplitOn, S: String; var left, right: String; leftMostSplit: Boolean = true): Boolean;
// returns true if stringToSplitOn was found in s
// if it returns true:
//   left = everything in s left of the left-most occurance of stringToSplitOn
//   right = everything in s right of the left-most occurance of stringToSplitOn
// if it returns false:
//   left = s
//   right = ''
//
// if leftMostSplit is false the the string will split on the right-most
// occurance of stringToSplitOn

var
  i, splitPos: Integer;
  sCopy: String;
begin
  if leftMostSplit then
  begin
    splitPos := Pos(stringToSplitOn, S);
  end
  else
  begin
    splitPos := 0;
    for i := Length(S) - (Length(stringToSplitOn) - 1) downto 1 do
    begin
      if stringToSplitOn = copy(S, i, Length(stringToSplitOn)) then
      begin
        splitPos := i;
        break;
      end;
    end;
  end;
  Result := splitPos > 0;

  if Result then
  begin
    // the copy function appears to be slighty destructive to the string
    // so take a copy of it first
    sCopy := S;
    left := copy(S, 1, splitPos - 1);
    right := copy(sCopy, splitPos + length(stringToSplitOn), length(sCopy));
  end
  else
  begin
    left := S;
    right := '';
  end;
end;


function SplitString(const stringToSplitOn, S: WideString; var left, right: WideString; leftMostSplit: Boolean = true): Boolean;
// returns true if stringToSplitOn was found in s
// if it returns true:
//   left = everything in s left of the left-most occurance of stringToSplitOn
//   right = everything in s right of the left-most occurance of stringToSplitOn
// if it returns false:
//   left = s
//   right = ''
//
// if leftMostSplit is false the the string will split on the right-most
// occurance of stringToSplitOn

var
  i, splitPos: Integer;
  sCopy: WideString;
begin
  if leftMostSplit then
  begin
    splitPos := Pos(stringToSplitOn, S);
  end
  else
  begin
    splitPos := 0;
    for i := Length(S) - (Length(stringToSplitOn) - 1) downto 1 do
    begin
      if stringToSplitOn = copy(S, i, Length(stringToSplitOn)) then
      begin
        splitPos := i;
        break;
      end;
    end;
  end;
  Result := splitPos > 0;

  if Result then
  begin
    // the copy function appears to be slighty destructive to the string
    // so take a copy of it first
    sCopy := S;
    left := copy(S, 1, splitPos - 1);
    right := copy(sCopy, splitPos + length(stringToSplitOn), length(sCopy));
  end
  else
  begin
    left := S;
    right := '';
  end;
end;


function CharInString(const S: String; const c: Char): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 1 to Length(S) do
  begin
    if S[i] = c then
    begin
      Result := true;
      break;
    end;
  end;
end;


function CharInString(const S: widestring; const c: WideChar): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 1 to Length(S) do
  begin
    if S[i] = c then
    begin
      Result := true;
      break;
    end;
  end;
end;


function parseQuoted(const text: String; quoteChar, escapeChar: Char): String;
// parseQuoted('dhfg jdfh jdj "some\\text\"here" hfd hd', '"', '\')
// would return
//
// some\text"here
//
var
  openingQuotePos, i: Integer;
  escaped: Boolean;
  currentChar: Char;
  quotedText: String;
begin
  Result := '';
  quotedText := '';
  openingQuotePos := pos(quoteChar, text);
  if (openingQuotePos > 0) and (openingQuotePos < Length(text)) then
  begin
    i := openingQuotePos + 1;
    escaped := false;

    repeat
      currentChar := Text[i];

      if escaped then
      begin
        quotedText := quotedText + currentChar;
        escaped := false;
      end
      else
      begin
        if currentChar = escapeChar then
        begin
          escaped := true;
        end
        else if currentChar = quoteChar then
        begin
          // we found the closing quote
          Result := quotedText;
          break;
        end
        else quotedText := quotedText + currentChar;
      end;

      Inc(i);
    until i > Length(text);
  end;

end;


function LTrim(const S: String; const charsToTrim: String = ''): String;
// removes characters from the left of the string until a character not
// in charsToTrim is encountered. If charsToTrim is not specified then
// whitespace is trimmed.
var
  i, copyStart: Integer;
  searchString: Boolean;
begin
  searchString := Length(charsToTrim) > 0;
  copyStart := Length(S) + 1;
  for i := 1 to Length(S) do
  begin
    if searchString then
    begin
      if not charInString(charsToTrim, S[i]) then
      begin
        copyStart := i;
        break;
      end;
    end
    else
    begin
      if Ord(S[i]) > 32 then
      begin
        copyStart := i;
        break;
      end;
    end;
  end;

  Result := copy(S, copyStart, Length(S));
end;


function RTrim(const S: String; const charsToTrim: String = ''): String;
// removes characters from the right of the string until a character not
// in charsToTrim is encountered. If charsToTrim is not specified then
// whitespace is trimmed.
var
  i, copyLength: Integer;
  searchString: Boolean;
begin
  searchString := Length(charsToTrim) > 0;
  copyLength := 0;
  for i := Length(S) downto 1 do
  begin
    if searchString then
    begin
      if not charInString(charsToTrim, S[i]) then
      begin
        copyLength := i;
        break;
      end;
    end
    else
    begin
      if Ord(S[i]) > 32 then
      begin
        copyLength := i;
        break;
      end;
    end;
  end;

  Result := copy(S, 1, copyLength);
end;


function LTrim(const S: widestring; const charsToTrim: widestring = ''): widestring;
// removes characters from the left of the string until a character not
// in charsToTrim is encountered. If charsToTrim is not specified then
// whitespace is trimmed.
var
  i, copyStart: Integer;
  searchString: Boolean;
begin
  searchString := Length(charsToTrim) > 0;
  copyStart := Length(S) + 1;
  for i := 1 to Length(S) do
  begin
    if searchString then
    begin
      if not charInString(charsToTrim, S[i]) then
      begin
        copyStart := i;
        break;
      end;
    end
    else
    begin
      if Ord(S[i]) > 32 then
      begin
        copyStart := i;
        break;
      end;
    end;
  end;

  Result := copy(S, copyStart, Length(S));
end;


function RTrim(const S: widestring; const charsToTrim: widestring = ''): widestring;
// removes characters from the right of the string until a character not
// in charsToTrim is encountered. If charsToTrim is not specified then
// whitespace is trimmed.
var
  i, copyLength: Integer;
  searchString: Boolean;
begin
  searchString := Length(charsToTrim) > 0;
  copyLength := 0;
  for i := Length(S) downto 1 do
  begin
    if searchString then
    begin
      if not charInString(charsToTrim, S[i]) then
      begin
        copyLength := i;
        break;
      end;
    end
    else
    begin
      if Ord(S[i]) > 32 then
      begin
        copyLength := i;
        break;
      end;
    end;
  end;

  Result := copy(S, 1, copyLength);
end;


function CompareSuffix(const suffix, S: String): Boolean;
// returns true if 'S' ends in 'suffix'
var
  i, lengthOfS, lengthOfSuffix: Integer;
begin
  lengthOfS := Length(S);
  lengthOfSuffix := Length(suffix);

  if lengthOfS >= lengthOfSuffix then
  begin
    Result := true;
    for i := 0 to lengthOfSuffix - 1 do
    begin
      if suffix[lengthOfSuffix - i] <> S[lengthOfS - i] then
      begin
        Result := false;
        break;
      end;
    end;
  end
  else Result := false;
end;


function ComparePrefix(const prefix, S: String): Boolean;
// returns true if 'S' starts with 'prefix'
var
  i, lengthOfS, lengthOfPrefix: Integer;
begin
  lengthOfS := Length(S);
  lengthOfPrefix := Length(prefix);

  if lengthOfS >= lengthOfPrefix then
  begin
    Result := true;
    for i := 1 to lengthOfPrefix do
    begin
      if prefix[i] <> S[i] then
      begin
        Result := false;
        break;
      end;
    end;
  end
  else Result := false;
end;


// -----------------------------------------------------------------------------
function StripControlChars(const s: String): String;
// strips everything less than 32, except tab (9)
var
  charIndex: Integer;
begin
  Result := '';
  for charIndex := 1 to length(s) do
  begin
    if (Ord(s[charIndex]) >= 32) or (Ord(s[charIndex]) = 9) then Result := Result + s[charIndex];
  end;
end;


// -----------------------------------------------------------------------------
function StripWideControlChars(const s: widestring): Widestring;
// strips everything less than 32, except tab (9)
var
  charIndex: Integer;
begin
  Result := '';
  for charIndex := 1 to length(s) do
  begin
    if (Ord(s[charIndex]) >= 32) or (Ord(s[charIndex]) = 9) then Result := Result + s[charIndex];
  end;
end;




function FindFileOnPath(fileName: String): String;
// returns the fully qualified filename and path if the file was found
// other it returns ''

  function removeFirstPath(var listOfPaths: String; const delimiter: String): String;
  // removes the first path and delimiter from listOfPaths, and returns the
  // path that was removed (ensureing it ends in a '\' and strips any ")
  begin
    splitString(delimiter, listOfPaths, Result, listOfPaths);

    if Length(Result) > 0 then
    begin
      Result := StringReplace(Result, '"', '', [rfReplaceAll]);
      if Result[Length(Result)] <> '\' then Result := Result + '\';
    end;
  end;

var
  systemPath, path: String;
begin
  Result := '';
  systemPath := expandPathWithEnvironmentStrings('%path%');

  path := removeFirstPath(systemPath, ';');
  while length(path) > 0 do
  begin
    if FileExists(path + fileName) then
    begin
      Result := path + fileName;
      break;
    end;

    path := removeFirstPath(systemPath, ';');
  end;
end;


function SleepUntilMessage(milliseconds: Integer): Boolean;
// returns false if the sleep was aborted due to a message being queued
// returns true if the sleep period expired
var
  PHandles, waitResult: DWORD;
begin
  PHandles := 0;
  // give up some CPU time unless any messages show up
  waitResult := MsgWaitForMultipleObjects(0, PHandles, false, milliseconds, QS_ALLINPUT);

  Result := waitResult = WAIT_TIMEOUT;
end;


function DeleteKeyCompletely(reg: TRegistry; key: String): Boolean;
// under winnt (dunno about 2k, XP etc), TRegistry.DeleteKey() only works if
// all its subkeys have already been deleted.
// DeleteKeyCompletely() works on any OS
// As with DeleteKey, it returns true if successful
var
  keyNames: TStringList;
  i: Integer;
  initialKey: String;
begin
  Result := false;

  if key = '' then raise Exception.Create('DeleteKeyCompletely() called on root key'); // must specify a key else you'll erase the entire registry again!

  initialKey := IncludeTrailingBackslash(reg.CurrentPath);

  // The initial key is an absolute path, but openKey() will treat it as a
  // relative path unless it begins with a backslash, so add a backslash
  if length(initialKey) > 0 then
  begin
    if initialKey[1] <> '\' then initialKey := '\' + initialKey;
  end;

  if reg.OpenKey(key, false) then
  begin
    // Note that by successfully opening a key, we have just closed the key
    // that was open when reg was passed to this function, remember to restore
    // it before returning from this function

    // first delete all subkeys
    // (we don't have to delete the values, just the keys)
    try
      keyNames := TStringList.Create;
      if reg.HasSubKeys then reg.GetKeyNames(keyNames);

      Result := true;

      i := 0;
      while Result and (i < keyNames.Count) do
      begin
        Result := DeleteKeyCompletely(reg, keyNames[i]);
        Inc(i);
      end;

      FreeAndNil(keyNames);
    except
      Result := false;
    end;

    // Return reg to the state it was in before DeleteKeyCompletely() was called
    // by reopening what was its current key.
    // reg.CloseKey; // commented out as the OpenKey() below it will call CloseKey()
    reg.OpenKey(initialKey, false);

    // now that the initial key is the one that is open again, we can delete the
    // key that was passed to us
    if Result then Result := reg.DeleteKey(key);
  end;
end;


function GetFileTypeShellCommand(fileType: string): string;
// given an executable type, for example 'mailto', 'MIDFile', or 'WinAmp3.File'
// this function returns the command line to execute it
// substitute %1 for the instance details of what you want executed.

  function FileTypeShellCommand(rootKey: HKEY; key: string): string;
  var
    Reg: TRegistry;
    commandKey: string;
  begin
    // find the default mail client shell command
    Result := '';

    Reg := TRegistry.Create;
    try
      Reg.RootKey := rootKey;
      commandKey := IncludeTrailingBackslash(key) + 'shell\open\command';

      if Reg.KeyExists(commandKey) then
      begin
        if Reg.OpenKeyReadOnly(commandKey) then
        begin
          Result := Reg.ReadString(''); // the shell command is the (Default) registry entry
        end;
      end;
    finally
      Reg.CloseKey;
      Reg.Free;
    end;
  end;

begin
  Result := FileTypeShellCommand(HKEY_CURRENT_USER, 'Software\Classes\' + fileType);
  if Result = '' then
  begin
    Result := FileTypeShellCommand(HKEY_LOCAL_MACHINE, 'Software\Classes\' + fileType);

    // I don't think checking HKEY_CLASSES_ROOT is neccessary after checking
    // HKEY_LOCAL_MACHINE cos I *think* all the root classes get mapped into
    // HKEY_LOCAL_MACHINE, but it can't hurt to check anyway.
    if Result = '' then Result := FileTypeShellCommand(HKEY_CLASSES_ROOT, fileType);
  end;

  if Result <> '' then Result := ExpandPathWithEnvironmentStrings(Result);
end;


function WindowsErrorDescription(errorNumber: DWORD): string;

  function MAKELANGID(sPrimaryLanguage, sSubLanguage: Word): Word;
  begin
    result := (sSubLanguage shl 10) or sPrimaryLanguage;
  end;

var
  messageBuffer: PChar;
begin
  if FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
    nil,
    errorNumber,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    @messageBuffer,
    0,
    nil
  ) > 0 then
  begin
    Result := Trim(messageBuffer);
    LocalFree(LongWord(@messageBuffer[0]));
  end
  else Result := '';
end;

function Conditional(const expression: boolean; const argTrue, argFalse: string): string;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: widestring): widestring;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: integer): integer;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: int64): int64;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: single): single;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: double): double;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: boolean): boolean;
begin
  if expression then Result := argTrue else Result := argFalse;
end;

function Conditional(const expression: boolean; const argTrue, argFalse: TObject): TObject;
begin
  if expression then Result := argTrue else Result := argFalse;
end;


function ClampBounds(var input: Integer; lower, upper: Integer): Integer;
begin
  input := min(upper, max(lower, input));
  Result := input;
end;


function ClampBounds(var input: Double; lower, upper: Double): Double;
begin
  input := min(upper, max(lower, input));
  Result := input;
end;


procedure PseudoRandomize;
begin
  PseudoRandomize(GetTickCount and $7FFFFFFF);
end;

procedure PseudoRandomize(seed: Integer);
begin
  gRandomNumberGeneratorSeed := abs(seed);
end;

function  PseudoRandom(Range: Integer): Integer;
// PseudoRandom returns a random number within the range 0 <= X < Range.
//
// It uses the following generator:
//   RandomNum := (16807 * LastRandomNum) MOD 2147483647
//
// (a widely used and well tested and analyzed generator, but also a widely
// known one (if you are planning on using it to obfuscate a password or
// something))
var
  RandomNum: int64;
begin
  RandomNum := int64(16807) * int64(gRandomNumberGeneratorSeed);
  RandomNum := RandomNum mod 2147483647;
  gRandomNumberGeneratorSeed := RandomNum;

  Result := RandomNum mod abs(Range); // I don't think the abs() actually affects the result, but it makes the behaviour explicit
end;



end.
