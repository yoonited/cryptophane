unit Utils;

interface

uses Classes, Contnrs, SysUtils, Windows;

function YMDToDateTime(ymd: string): TDateTime;
function DateTimeToYMD(dt: TDateTime): string;
function Split(splitChar: char; data: string; splitArray: TStrings): boolean;
function StripCR(data: string): string;
function GetFileVer(aFilename: string; includeBuild: boolean): string;
procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;
  specialkey: Boolean);

type
  KeyNotUniqueException = class(Exception);
  
  TStringPair = class
  private
    FKey, FValue: string;
  public
    constructor Create; overload;
    constructor Create(key, value: string); overload;

    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
  end;

  TStringPairList = class
  private
    FUniqueKeys: boolean;
    
    function GetCount: integer;
    function LookupKey(key: string): string;
    function GetItemKey(index: integer): string;
    function GetItemValue(index: integer): string;
    procedure SetUniqueKeys(const Value: boolean);
    procedure WriteKeyValue(key: string; const Value: string);
    procedure WriteValue(index: integer; const Value: string);
    procedure WriteKey(index: integer; const Value: string);
  protected
    FList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(key, value: string): integer;
    procedure Clear;
    function IndexOf(key: string): integer;
    procedure Delete(index: integer);

    property UniqueKeys: boolean read FUniqueKeys write SetUniqueKeys;
    property Count: integer read GetCount;
    property Keys[index: integer]: string read GetItemKey write WriteKey;
    property Values[index: integer]: string read GetItemValue write WriteValue;
    property Lookup[key: string]: string read LookupKey write WriteKeyValue; default;
  end;

implementation

uses DateUtils, Messages, Forms;

function Pad0(i: integer; count: integer): string;
begin
  result := IntToStr(i);
  while Length(result) < count do result := '0' + result;
end;

function YMDToDateTime(ymd: string): TDateTime;
begin
  try
    result := EncodeDate(
      StrToIntDef(Copy(ymd, 1, 4), -1),
      StrToIntDef(Copy(ymd, 6, 2), -1),
      StrToIntDef(Copy(ymd, 9, 2), -1)
    );
  except
    result := 0;
  end;
end;

function DateTimeToYMD(dt: TDateTime): string;
begin
  result :=
    Pad0(YearOf(dt), 4) + '-' +
    Pad0(MonthOf(dt), 2) + '-' +
    Pad0(DayOf(dt), 2);
end;

function Split(splitChar: char; data: string; splitArray: TStrings): boolean;
var
  i, tokenStart: integer;
begin
  result := true;
  splitArray.Clear;
  if data = '' then Exit;

  tokenStart := 1;
  for i := 1 to Length(data) do
  begin
    if data[i] = splitChar then
    begin
      splitArray.Add(Copy(data, tokenStart, i - tokenStart));
      tokenStart := i + 1;
    end;
  end;
  splitArray.Add(Copy(data, tokenStart, Length(data) - tokenStart + 1));
end;

function StripCR(data: string): string;
var
  n: integer;
begin
  n := Length(data);
  if data[n] = #13 then
    result := Copy(data, 1, n - 1)
  else
    result := data;
end;

function GetFileVer(aFilename: string; includeBuild: boolean): string;
var
  InfoSize: cardinal;
  Wnd: cardinal;
  VerBuf: Pointer;
  VerSize : cardinal;
  FI: PVSFixedFileInfo;
begin
  result := '';
  if (aFilename = '') or (not FileExists(aFilename)) then exit;

  InfoSize := GetFileVersionInfoSize(PChar(aFilename), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(aFilename), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          result :=
            IntToStr(HiWord(FI^.dwFileVersionMS)) + '.' +
            IntToStr(LoWord(FI^.dwFileVersionMS)) + '.' +
            IntToStr(HiWord(FI^.dwFileVersionLS));

          if includeBuild then
          begin
            result := result + '.' + IntToStr(LoWord(FI^.dwFileVersionLS));
          end;
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{ TStringPairList }


constructor TStringPairList.Create;
begin
  FList := TObjectList.Create;
  FUniqueKeys := true;
end;

destructor TStringPairList.Destroy;
begin
  FList.Free;
end;

function TStringPairList.Add(key, value: string): integer;
begin
  result := FList.Add(TStringPair.Create(key, value));
end;

procedure TStringPairList.Delete(index: integer);
begin
  FList.Delete(index);
end;

function TStringPairList.GetItemKey(index: integer): string;
begin
  result := TStringPair(FList[index]).Key;
end;

function TStringPairList.GetItemValue(index: integer): string;
begin
  result := TStringPair(FList[index]).Value;
end;

function TStringPairList.IndexOf(key: string): integer;
var
  i: integer;
begin
  // OK, another weird Delphi bug.
  // If the following result assignation line is at the end of the function,
  // Delphi will not compile the "result := i;" line, leading to much sadness.
  // Turning optimisations off makes it work, so it's probably a problem with
  // the Delphi optimiser.
  // So we'll leave it up the top.
  // Maybe check this out again on a later Delphi version?
  result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if TStringPair(FList[i]).Key = key then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TStringPairList.GetCount: integer;
begin
  result := FList.Count;
end;

procedure TStringPairList.Clear;
begin
  FList.Clear;
end;

function TStringPairList.LookupKey(key: string): string;
begin
  result := TStringPair(FList[IndexOf(key)]).Value;
end;

procedure TStringPairList.SetUniqueKeys(const Value: boolean);
var
  s: string;
  i, j: integer;
begin
  if value and not FUniqueKeys then
  begin
    for i := 0 to FList.Count - 2 do
    begin
      s := TStringPair(FList[i]).Key;
      for j := i + 1 to FList.Count - 1 do
      begin
        if s = TStringPair(FList[j]).Key then raise KeyNotUniqueException.Create('Cannot convert to unique when non-unique keys are present');
      end;
    end;
  end;

  FUniqueKeys := Value;
end;

procedure TStringPairList.WriteKeyValue(key: string; const Value: string);
var
  n: integer;
begin
  if UniqueKeys then
  begin
    n := IndexOf(key);
    if n <> -1 then
    begin
      Values[n] := value;
      exit;
    end;
  end;

  Add(key, value);
end;

procedure TStringPairList.WriteValue(index: integer; const Value: string);
begin
  if (index < 0) or (index >= Count) then raise ERangeError.Create('Out of bounds');
  TStringPair(FList[index]).Value := value;
end;

procedure TStringPairList.WriteKey(index: integer; const Value: string);
begin
  if (index < 0) or (index >= Count) then raise ERangeError.Create('Out of bounds');
  if UniqueKeys and (IndexOf(value) <> -1) then raise KeyNotUniqueException.Create('Not unique');
  TStringPair(FList[index]).Key := value;
end;

{ TStringPair }

constructor TStringPair.Create;
begin

end;

constructor TStringPair.Create(key, value: string);
begin
  FKey := key;
  FValue := value;
end;


procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;
  specialkey: Boolean);
{************************************************************
 * Procedure PostKeyEx
 *
 * Parameters:
 *  hWindow: target window to be send the keystroke
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 *           If this parameter is true, bit 24 of the lparam for
 *           the posted WM_KEY* messages will be set.
 * Description:
 *  This procedure sets up Windows key state array to correctly
 *  reflect the requested pattern of modifier keys and then posts
 *  a WM_KEYDOWN/WM_KEYUP message pair to the target window. Then
 *  Application.ProcessMessages is called to process the messages
 *  before the keyboard state is restored.
 * Error Conditions:
 *  May fail due to lack of memory for the two key state buffers.
 *  Will raise an exception in this case.
 * NOTE:
 *  Setting the keyboard state will not work across applications
 *  running in different memory spaces on Win32 unless AttachThreadInput
 *  is used to connect to the target thread first.
 *Created: 02/21/96 16:39:00 by P. Below
 ************************************************************}
type
  TBuffers = array [0..1] of TKeyboardState;
var
  pKeyBuffers: ^TBuffers;
  lParam: LongWord;
begin
  (* check if the target window exists *)
  if IsWindow(hWindow) then
  begin
    (* set local variables to default values *)
    lParam := MakeLong(0, MapVirtualKey(key, 0));

    (* modify lparam if special key requested *)
    if specialkey then
      lParam := lParam or $1000000;

    (* allocate space for the key state buffers *)
    New(pKeyBuffers);
    try
      (* Fill buffer 1 with current state so we can later restore it.
         Null out buffer 0 to get a "no key pressed" state. *)
      GetKeyboardState(pKeyBuffers^[1]);
      FillChar(pKeyBuffers^[0], SizeOf(TKeyboardState), 0);

      (* set the requested modifier keys to "down" state in the buffer*)
      if ssShift in shift then
        pKeyBuffers^[0][VK_SHIFT] := $80;
      if ssAlt in shift then
      begin
        (* Alt needs special treatment since a bit in lparam needs also be set *)
        pKeyBuffers^[0][VK_MENU] := $80;
        lParam := lParam or $20000000;
      end;
      if ssCtrl in shift then
        pKeyBuffers^[0][VK_CONTROL] := $80;
      if ssLeft in shift then
        pKeyBuffers^[0][VK_LBUTTON] := $80;
      if ssRight in shift then
        pKeyBuffers^[0][VK_RBUTTON] := $80;
      if ssMiddle in shift then
        pKeyBuffers^[0][VK_MBUTTON] := $80;

      (* make out new key state array the active key state map *)
      SetKeyboardState(pKeyBuffers^[0]);
      (* post the key messages *)
      if ssAlt in Shift then
      begin
        PostMessage(hWindow, WM_SYSKEYDOWN, key, lParam);
        PostMessage(hWindow, WM_SYSKEYUP, key, lParam or $C0000000);
      end
      else
      begin
        PostMessage(hWindow, WM_KEYDOWN, key, lParam);
        PostMessage(hWindow, WM_KEYUP, key, lParam or $C0000000);
      end;
      (* process the messages *)
      Application.ProcessMessages;

      (* restore the old key state map *)
      SetKeyboardState(pKeyBuffers^[1]);
    finally
      (* free the memory for the key state buffers *)
      if pKeyBuffers <> nil then
        Dispose(pKeyBuffers);
    end; { If }
  end;
end; { PostKeyEx }

end.
