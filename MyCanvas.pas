unit MyCanvas;

interface
uses
  MyTypes, Vcl.Graphics, System.Types, Math;

type
  TMyCanvas = class(TInterfacedObject, ICanvas)
  private
    FViewPort: T2DBox;
  public
    fCanvas: TCanvas;
    fPrimitive: TPrimitiveType;
    fVertexArray: array[1..4] of T2DPoint;
    fLastVertex: integer;
    fViewBoxWidth, fViewBoxHeight: Integer;
    fCurrentColor: Integer;

    procedure SetCurrentColor(const Value: Integer);
    procedure SetLineWidth(const Value: integer);
    procedure SetViewPort(Value: T2DBox);
    procedure SetViewBox(Width, Height: Integer);
    procedure BeginPrimitive(Typ: TPrimitiveType);
    procedure EndPrimitive;
    procedure AddVertex(const p: T2DPoint);

    procedure ZoomCanvas(delta: Integer; pbWidth, pbHeight: Integer);
    procedure ScrollCanvas(Point, ClickPoint: TPoint; pbWidth, pbHeight: Integer);
    procedure RePaintMyCanvas;

    property Canvas: TCanvas read fCanvas write fCanvas;
    property ViewPort: T2DBox read FViewPort write SetViewPort;

    function GetCanvasPoint(const p: T2DPoint): TPoint;
    function GetGlobalPoint(p: TPoint): T2DPoint;
  end;

const
  SCROLL_SHAPNESS = 100;
  MAX_WIDTH = 250;

implementation

{ TMyCanvas }

procedure TMyCanvas.AddVertex(const p: T2DPoint);
begin

  case fPrimitive of
    ptLineStrip:
    begin
      if fLastVertex = 0 then begin
        fCanvas.MoveTo(GetCanvasPoint(p).x, GetCanvasPoint(p).y);
        fLastVertex := 1;
      end else begin
        fCanvas.LineTo(GetCanvasPoint(p).x, GetCanvasPoint(p).y);
      end;
    end;

    ptLines:
    begin
      if fLastVertex=0 then begin
        fVertexArray[1] := p;
        fLastVertex := 1;
      end else begin
        fCanvas.MoveTo(GetCanvasPoint(fVertexArray[1]).x, GetCanvasPoint(fVertexArray[1]).y);
        fCanvas.LineTo(GetCanvasPoint(p).x, GetCanvasPoint(p).y);
        fLastVertex := 0;
      end;
    end;

    ptQuads:
    begin
      if fLastVertex = 0 then begin
        fCanvas.MoveTo(GetCanvasPoint(p).x, GetCanvasPoint(p).y);
        fVertexArray[1] := p;
      end else begin
        fCanvas.LineTo(GetCanvasPoint(p).x, GetCanvasPoint(p).y);
      end;
      Inc(fLastVertex);
      if fLastVertex = 4 then begin
        fCanvas.LineTo(GetCanvasPoint(fVertexArray[1]).x, GetCanvasPoint(fVertexArray[1]).y);
        fLastVertex := 0;
      end;
    end;
  end;
end;

procedure TMyCanvas.BeginPrimitive(Typ: TPrimitiveType);
begin
  fPrimitive := Typ;
end;

procedure TMyCanvas.EndPrimitive;
begin
  fPrimitive := ptNone;
  fLastVertex := 0;
end;

function TMyCanvas.GetCanvasPoint(const p: T2DPoint): TPoint;
begin
  var tx := (p.x - ViewPort.Min.X) / (ViewPort.Max.X - ViewPort.Min.X);
  var ty := (p.y - ViewPort.Min.Y) / (ViewPort.Max.Y - ViewPort.Min.Y);

  var px := Round(tx * self.fViewBoxWidth);
  var py := Round(self.fViewBoxHeight *  (1 - ty));

  Result := TPoint.Create(px, py);
end;

function TMyCanvas.GetGlobalPoint(p: TPoint): T2DPoint;
begin
  var tx := p.x / fViewBoxWidth;
  var ty := (fViewBoxHeight - p.y) / fViewBoxHeight;

  var px := Round(ViewPort.Min.X + tx * (ViewPort.Max.X - ViewPort.Min.X));
  var py := Round(ViewPort.Min.Y + ty * (ViewPort.Max.Y - ViewPort.Min.Y));

  Result := T2DPoint.Create(px, py);
end;


procedure TMyCanvas.RePaintMyCanvas;
begin
  fCanvas.Brush.Color:=clWhite;
  fCanvas.FillRect(fCanvas.ClipRect);
end;

procedure TMyCanvas.ScrollCanvas(Point, ClickPoint: TPoint; pbWidth,
  pbHeight: Integer);
begin
  if ((ViewPort.Min.X - Round((Point.X - ClickPoint.X) / 10) > 0)) and
    ((ViewPort.Max.Y - Round((ClickPoint.Y - Point.Y) / 10) < pbHeight)) then begin
    SetViewPort(
      T2DBox.Create(
        T2DPoint.Create(ViewPort.Min.X - Round((Point.X - ClickPoint.X) / SCROLL_SHAPNESS),
          ViewPort.Min.Y - Round((ClickPoint.Y - Point.Y) / SCROLL_SHAPNESS)),
        T2DPoint.Create(ViewPort.Max.X - Round((Point.X - ClickPoint.X) / SCROLL_SHAPNESS),
          ViewPort.Max.Y - Round((ClickPoint.Y - Point.Y) / SCROLL_SHAPNESS))
      )
    );
    ClickPoint := Point;
  end;
end;

procedure TMyCanvas.SetCurrentColor(const Value: Integer);
begin
  fCanvas.Pen.Color := Value;
end;

procedure TMyCanvas.SetLineWidth(const Value: integer);
begin
  fCanvas.Pen.Width := Value;
end;

procedure TMyCanvas.SetViewBox(Width, Height: Integer);
begin
  fViewBoxWidth := Width;
  fViewBoxHeight := Height;
end;

procedure TMyCanvas.SetViewPort(Value: T2DBox);
begin
  FViewPort := Value;
end;

procedure TMyCanvas.ZoomCanvas(delta, pbWidth, pbHeight: Integer);
begin
  if (fViewBoxWidth + delta / 10 > MAX_WIDTH) and
    (fViewBoxHeight + delta / 10 > MAX_WIDTH) then begin
      fViewBoxWidth := Round(fViewBoxWidth + delta / 10 * (fViewBoxWidth / fViewBoxHeight));
      fViewBoxHeight := Round(fViewBoxHeight + delta / 10);
  end;
end;
end.
