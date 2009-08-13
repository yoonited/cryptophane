unit DefaultRegistry;

interface

uses Registry;

type
  TDefaultRegistry = class(TRegistry)
    function ReadInteger(name: string; default: integer): integer; overload;
    function ReadBool(name: string; default: boolean): boolean; overload;
    function ReadString(name: string; default: string): string; overload;
  end;

function IfThenElse(b: boolean; sTrue, sFalse: integer): integer; overload;
function IfThenElse(b: boolean; sTrue, sFalse: string): string; overload
function IfThenElse(b: boolean; sTrue, sFalse: TObject): TObject; overload;

implementation

{ TDefaultRegistry }

function TDefaultRegistry.ReadBool(name: string; default: boolean): boolean;
begin
  try
    result := inherited ReadBool(name);
  except
    result := default;
  end;
end;

function TDefaultRegistry.ReadInteger(name: string; default: integer): integer;
begin
  try
    result := inherited ReadInteger(name);
  except
    result := default;
  end;
end;

function TDefaultRegistry.ReadString(name, default: string): string;
begin
  if ValueExists(name) then
    result := inherited ReadString(name)
  else
    result := default;
end;



{ IfThenElse functions }

function IfThenElse(b: boolean; sTrue, sFalse: string): string;
begin
  if b then
    result := sTrue
  else
    result := sFalse;
end;

function IfThenElse(b: boolean; sTrue, sFalse: integer): integer;
begin
  if b then
    result := sTrue
  else
    result := sFalse;
end;

function IfThenElse(b: boolean; sTrue, sFalse: TObject): TObject;
begin
  if b then
    result := sTrue
  else
    result := sFalse;
end;

end.
