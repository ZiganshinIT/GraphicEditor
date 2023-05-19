unit MyDrawing;

interface
  uses MyTypes, System.SysUtils, System.Classes, vcl.Dialogs, System.JSON, System.Types, MyPicker;
type
  TMyDrawing = class(TInterfacedObject, IDrawing)
  public
    fCanvas: ICanvas;
    fFigures: array of TFigure;
    fLastSelectedFigure: Integer;
    fLastSelectedVertex: Integer;
    fHoverFigure: Integer;
    fClickPoint: TPoint;

    fMyPicker: IPicker;
    fPick: TMyPicker;

    isPaintBoxScrollable: Boolean;
    isVertexMoveable: Boolean;
    isFigureMoveable: Boolean;

    procedure Draw;
    procedure PreDraw(InitialPoint, EndPoint: T2DPoint; FigType: TTools; Color: TColor;
      Width: Integer);
    procedure ToolsAction(var Point: T2DPoint; FigType: TTools; Color: TColor; Width: Integer);

    procedure VertexMove(Point: T2DPoint);
    procedure FigureMove(Point: T2DPoint);

    procedure Save(FileName: string);
    procedure Load(FileName: string);

    procedure HoverFigure(p: T2DPoint);

    procedure ResetAddFigure;
    procedure ResetSelection;
    procedure Clean;

    procedure Delete;

    function GetCursor(Point: T2DPoint): Integer;

    constructor Create;
  protected
    fIsFirstTap: Boolean;
    fInitialPoint: T2dPoint;
    fFigure: TFigure;
    fFigureColor: TColor;
    fHoveredFigure: Integer;
  end;

const
  CIRCLE_RADIUS = 100;

implementation

procedure TMyDrawing.ToolsAction(var Point: T2DPoint; FigType: TTools; Color: TColor; Width: Integer);  // переименовать
var
  I: Integer;
begin
  case FigType of
    Line:
    begin
      if fIsFirstTap then
      begin
        fInitialPoint := Point;
      end
      else begin
        fFigure := TLine.CreateLine(fInitialPoint, Point, Width, Color);
        fFigures := fFigures + [fFigure];
      end;
      fIsFirstTap := not fIsFirstTap;
    end;

    Circle:
    begin
      fFigure := TCircle.CreateCircle(Point, CIRCLE_RADIUS, Width, Color);
      fFigures := fFigures + [fFigure];
    end;

    Rectangle:
    begin
      if fIsFirstTap then begin
        fInitialPoint := Point;
      end else begin
        fFigure := TRectangle.CreateRectangle(fInitialPoint, Point, Width, Color);
        fFigures := fFigures + [fFigure];
      end;
      fIsFirstTap := not fIsFirstTap;
    end;

    Hand:
    begin
      if (fLastSelectedFigure <> -1) and (fFigures[fLastSelectedFigure].BelongsVertex(Point)) then begin
        fLastSelectedVertex := fFigures[fLastSelectedFigure].GetFigureVertex(Point);
      end else begin
        for I := 0 to Length(Ffigures) - 1 do begin
          fFigures[I].fSelected := False;
        end;
        fLastSelectedFigure := -1;
        fLastSelectedVertex := -1;
        for I := Length(Ffigures) - 1 downTo 0 do begin
          if fFigures[I].Belongs(Point, fMyPicker) then begin
            fFigures[I].fSelected := True;
            fLastSelectedFigure := I;
            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMyDrawing.Clean;
var
  I: Integer;
begin
  for I := 0 to Length(fFigures) - 1 do fFigures[I].Destroy;
  SetLength(fFigures, 0);
end;

constructor TMyDrawing.Create;
begin
  inherited;
  fLastSelectedFigure := -1;
  fLastSelectedVertex := -1;
  fHoverFigure := -1;
  fIsFirstTap := True;

  fPick := TMyPicker.Create;
  fMyPicker := fPick;
end;

procedure TMyDrawing.Delete;
var
  I, J: Integer;
begin
  if Length(Ffigures) > 0 then begin
    J := 0;
    for I := 0 to Length(fFigures) - 1 do begin
      if I = fLastSelectedFigure then Continue
      else begin
        Ffigures[J] := fFigures[I];
        Inc(J);
      end;
    end;
    SetLength(fFigures, Length(fFigures)-1);
  end else begin
    SetLength(fFigures, 0);
  end;
  fLastSelectedFigure := -1;
  fLastSelectedVertex := -1;
end;

procedure TMyDrawing.Draw();
var
  I: Integer;
begin
  for I := 0 to Length(fFigures) - 1 do begin
    fFigures[I].Draw(fCanvas);
    if fFigures[I].fSelected then fFigures[I].DrawVertex(fCanvas);
  end;
end;

procedure TMyDrawing.FigureMove(Point: T2DPoint);
begin
  if (fLastSelectedFigure <> -1) and (fLastSelectedVertex = -1) then begin
    fFigures[fLastSelectedFigure].Move(Point);
  end;
end;

function TMyDrawing.GetCursor(Point: T2DPoint): Integer;
begin
  begin
  if (fLastSelectedFigure <> - 1) and
    (fFigures[fLastSelectedFigure].BelongsVertex(Point)) then begin
    Result := -7;
  end else if (fLastSelectedFigure <> -1) and
    (fFigures[fLastSelectedFigure].Belongs(Point, fMyPicker)) then begin
    Result := -5;
  end else
    Result := -2;
  end;
end;

procedure TMyDrawing.HoverFigure(p: T2DPoint);
var
  I: Integer;
begin
  for I := Length(Ffigures) - 1 downTo 0 do begin
    if I = fHoveredFigure then begin
      if not fFigures[I].Belongs(p, fMyPicker) then begin
        ffigures[I].fHovered := False;
        ffigures[I].Color := ffigureColor;
        fHoveredFigure := -1;
      end else break;
    end;

    if (fHoveredFigure = -1) and (fFigures[I].Belongs(p, fMyPicker)) then begin
      ffigures[I].fHovered := True;
      fFigureColor := ffigures[I].Color;
      ffigures[I].Color := TColor.Create(0, 0, 255);
      fHoveredFigure := I;
      break;
    end else begin
      ffigures[I].fHovered := False;
    end;
  end;
end;

procedure TMyDrawing.Load(FileName: string);
var
  F: TextFile;
  json: TJSONObject;
  str: string;
  fig: TFigure;
begin
  try
    try
      assignFile(F, FileName);
      Reset(F);

      Clean;

      while not Eof(F) do begin
        ReadLn(F, str);
        Fig := TFigure.Create;
        json := TJSONObject.ParseJSONValue(str) as TJSONObject;
        if Assigned(json.GetValue('TLine')) then begin
          fig := TLine.CreateFromJson(json.GetValue('TLine') as TJSONObject);
          fFigures := fFigures + [Fig];
        end else if Assigned(json.GetValue('TCircle')) then begin
          fig := TCircle.CreateFromJson(json.GetValue('TCircle') as TJSONObject);
          fFigures := fFigures + [Fig];
        end else if Assigned(json.GetValue('TRectangle')) then begin
          fig := TRectangle.CreateFromJSON(json.GetValue('TRectangle') as TJSONObject);
          fFigures := fFigures + [Fig];
        end;
      end;
    except on E : EInOutError do begin
        ShowMessage('Error in Load File');
        CloseFile(F);
      end;
    end;
  finally
    CloseFile(F);
  end;
end;


procedure TMyDrawing.PreDraw(InitialPoint, EndPoint: T2DPoint; FigType: TTools;
  Color: TColor; Width: Integer);
begin
  if ((InitialPoint.X <> 0) and (InitialPoint.Y <> 0) and (FigType = Line)) then
    TLine.CreateLine(InitialPoint, EndPoint, Width, Color).Draw(fCanvas)
  else if (EndPoint.X <> 0) and (EndPoint.Y <> 0) and (FigType = Circle) then
    TCircle.CreateCircle(EndPoint, CIRCLE_RADIUS, Width, Color).Draw(fCanvas)
  else if ((InitialPoint.X <> 0) and (InitialPoint.Y <> 0) and (FigType = REctangle)) then
    TRectangle.CreateRectangle(InitialPoint, EndPoint, Width, Color).Draw(fCanvas);
end;

procedure TMyDrawing.ResetAddFigure;
begin
  fIsFirstTap := True;
end;

procedure TMyDrawing.ResetSelection;
begin
  if  fLastSelectedFigure <> - 1 then begin
    fFigures[fLastSelectedFigure].fSelected := False;
    fLastSelectedVertex := -1;
    fLastSelectedFigure := -1;
  end;
end;

procedure TMyDrawing.Save(FileName: string);
var
  F: TextFile;
  I: Integer;
begin
  try
    try
      AssignFile(F, FileName);
      ReWrite(F);

      for I := 0 to Length(fFigures) - 1 do WriteLn(F, fFigures[I].ToJSON.ToString);
    except on E : EInOutError do begin
        ShowMessage('Error in Save File');
        CloseFile(F);
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TMyDrawing.VertexMove(Point: T2DPoint);
begin
  if (fLastSelectedVertex <> -1) and (fLastSelectedFigure <> -1) then begin
    fFigures[fLastSelectedFigure].VertexMove(Point, fLastSelectedVertex);
  end else begin
    fLastSelectedVertex := -1;
  end;
end;

end.
