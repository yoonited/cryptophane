unit ScaledForm;

interface

uses Forms, Classes;

type
  TScaledForm = class(TForm)
  private
    procedure InitScale;
    procedure ScaleForm;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;

implementation

uses Controls;

constructor TScaledForm.Create(AOwner: TComponent);
begin
  InitScale;
  inherited;
  ScaleForm;
end;

constructor TScaledForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  InitScale;
  inherited;
  ScaleForm;
end;

procedure TScaledForm.InitScale;
begin
  Scaled := false;
  AutoScroll := false;
end;

procedure TScaledForm.ScaleForm;
var
  r: double;
  dx, dy: integer;
  ppi, i: integer;
begin
  ppi := Screen.PixelsPerInch;
  if ppi <> PixelsPerInch then
  begin
    r := ppi / PixelsPerInch;
    dx := Round((Width * r) - Width);
    dy := Round((Height * r) - Height);

    for i := 0 to ControlCount - 1 do
    begin
      if akRight in Controls[i].Anchors then
      begin
        if akLeft in Controls[i].Anchors then
          Controls[i].Width := Controls[i].Width - dx
        else
          Controls[i].Left := Controls[i].Left - dx;
      end;

      if akBottom in Controls[i].Anchors then
      begin
        if akTop in Controls[i].Anchors then
          Controls[i].Height := Controls[i].Height - dy
        else
          Controls[i].Top := Controls[i].Top - dy;
      end;
    end;

    Width := Width + dx;
    Height := Height + dy;
  end;
end;

end.
