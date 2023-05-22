unit MyTypes;

interface
uses System.Types, System.SysUtils, System.JSON;

type
  TColor = packed record
  constructor Create(r, g, b: byte);
  case byte of
    0: (r, g, b, a: byte);
    1: (Value: integer);
  end;

  T2DPoint = record
    X, Y: double;
    constructor Create(p: T2DPoint); overload;
    constructor Create(X, Y: Double); overload;
    class operator Subtract(const p1, p2: T2DPoint): T2DPoint;
    class operator Add(const p1, p2: T2DPoint): T2DPoint;
    class operator Add(const k: double; p: T2DPoint): T2DPoint;
    class operator Multiply(const k: double; const p: T2DPoint): T2DPoint;
    function Length: double;
  end;

  T2DBox = record
    Min,Max: T2DPoint;
    constructor Create(Min, Max:T2DPoint);
  end;

  TPrimitiveType =
  (
    ptNone,
    ptLineStrip,
    ptLines,
    ptQuads
  );

  TTools =
  (
    Line,
    Circle,
    Rectangle,
    Hand
  );

  IDraw = interface(IUnknown)
  ['{5FBAF284-9ECF-4F2F-85FE-E1662FB450F6}']
    procedure BeginPrimitive(Typ: TPrimitiveType);
    procedure EndPrimitive;
    procedure AddVertex(const p: T2DPoint);
  end;

  IPicker = interface(IDraw)
  ['{E07D2374-5BFE-40AA-8BBE-3366C385DC54}']
    function GetResult: Boolean;
    procedure SetResult(Value: Boolean);
  end;

  ICanvas = interface(IDraw)
    ['{18F53F64-7DF4-4558-ABFB-9408BF725CA5}']
    procedure SetCurrentColor(const Value: integer);
    procedure SetViewPort(Value: T2DBox);
    procedure SetViewBox(Width, Height: Integer);
    procedure SetLineWidth(const Value: integer);

    function GetCanvasPoint(const p: T2DPoint): TPoint;
    function GetGlobalPoint(p: TPoint): T2DPoint;
  end;

  TFigure = class
    Width: Integer;
    Color: TColor;
    fSelected: Boolean;
    fHovered: Boolean;
    fVertexArr: array of T2DPoint;
    fDistanceXY: array of T2DPoint;

    procedure Draw(Canvas: ICanvas); virtual; abstract;
    procedure DrawVertex(Canvas: ICanvas);
    procedure Move(MovePoint: T2DPoint); virtual; abstract;
    procedure VertexMove(Point: T2DPoint; lastSelectedVertex: Integer); virtual; abstract;

    function Belongs(p: T2DPoint; Picker: IPicker): Boolean;
    function BelongsVertex(p: T2DPoint): Boolean;
    function GetFigureVertex(p: T2DPoint): Integer;
    function PickFigure(p: T2DPoint; Picker: IPicker): Boolean; virtual; abstract;
    function InVertexRange(p, Vertex: T2DPoint):Boolean;
    function ToJSON: TJSONObject; virtual; abstract;

    function SerializeColor(Color: TColor): TJSONObject;
    function DeserializeColor(ColorObj: TJSONValue): TColor;

    function SerializePoint(Point: T2DPoint): TJSONObject;
    function DeserializePoint(PointObj: TJSONValue): T2DPoint;

    function DeserializeInteger(WidthObj: TJSONValue): Integer;

  end;

   IDrawing = interface(IUnknown)
    ['{AE310F43-05C0-461C-80E3-E534ADD999B1}']
    procedure Draw;
    procedure ToolsAction(var Point: T2DPoint; FigType: TTools; Color: TColor; Width: Integer);
  end;

  TLine = class(TFigure)
    procedure Draw(Canvas: ICanvas); override;
    procedure Move(MovePoint: T2DPoint); override;
    procedure VertexMove(Point: T2DPoint; lastSelectedVertex: Integer); override;

    function PickFigure(p: T2DPoint; Picker: IPicker):Boolean; override;
    function ToJSON: TJSONObject; override;

    constructor CreateLine(StartPoint, EndPoint: T2DPoint; Width: Integer; Color: TColor);
    constructor CreateFromJSON(json: TJSONValue);
  private
  end;

  TCircle = class(TFigure)
    Radius: Double;
    procedure Draw(Canvas: ICanvas); override;
    procedure Move(MovePoint: T2DPoint); override;
    procedure VertexMove(Point: T2DPoint; lastSelectedVertex: Integer); override;

    function PickFigure(p: T2DPoint; Picker: IPicker): Boolean; override;
    function ToJSON: TJSONObject; override;

    constructor CreateCircle(CenterPoint: T2DPoint; Radius: Double; Width: Integer; Color: TColor);
    constructor CreateFromJSON(json: TJSONValue);
  end;

  TRectangle = class(TFigure)
    Angle: Double;
    LowerAngle: Integer;
    procedure Draw(Canvas: ICanvas); override;
    procedure Move(MovePoint: T2DPoint); override;
    procedure VertexMove(Point: T2DPoint; lastSelectedVertex: Integer); override;
    procedure Rotate(Angle: Double);

    function PickFigure(p: T2DPoint; Picker: IPicker): Boolean; override;
    function ToJSON: TJSONObject; override;

    constructor CreateRectangle(MinPoint, MaxPoint: T2DPoint; Width: Integer; Color: TColor; Angle: Double = 0);
    constructor CreateFromJSON(json: TJSONValue);
  end;

  function Dot(const p1, p2: T2DPoint): double;
  function PPDist(const p1, p2: T2DPoint): double;
  function PointLineDist(const p, p1, p2: T2DPoint): double;
  function PointProject(const p, p1, p2: T2DPoint): T2DPoint;

const
  VERTEX_RADIUS = 5;
  VERTEX_WIDTH = 1;
  VERTEX_COLOR = $000000;
  VERTEX_CLICK_ADDITION_AREA = 10;

implementation

uses Math, Dialogs;

function Dot(const p1, p2: T2DPoint): double;
begin
  result := p1.x*p2.x+p1.y*p2.y;
end;

function PPDist(const p1, p2: T2DPoint): double;
begin
  result := (p2-p1).Length;
end;

function PointLineDist(const p, p1, p2: T2DPoint): double;
begin
  var tt1 := p2-p1;
  var tt2 := p-p1;
  var L1 := tt1.Length;
  if L1 <> 0 then begin
    var x := Dot(tt1, tt2)/L1;
    if x < 0 then begin
      result := tt2.Length;
    end else if x > L1 then begin
      result := PPDist(p, p2);
    end else begin
      var t := x / L1;
      var lp := p1+t*tt1;
      result := PPDist(p, lp);
    end;
  end;
end;

 function PointProject(const p, p1, p2: T2DPoint): T2DPoint;
 begin
  var tt1 := p2-p1;
  var tt2 := p-p1;
  var L1 := tt1.Length;
  if L1 <> 0 then begin
    var x := Dot(tt1, tt2)/L1;
    var t := x / L1;
    Result := p1+t*tt1;
  end else Result := p1;
 end;

{ TColor }

constructor TColor.Create(r, g, b: byte);
begin
  self.r := r;
  self.g := g;
  self.b := b;
  self.a := 0;
end;

{ T2DPoint }

class operator T2DPoint.Add(const p1, p2: T2DPoint): T2DPoint;
begin
  result.x := p1.x+p2.x;
  result.y := p1.y+p2.y;
end;

class operator T2DPoint.Add(const k: double; p: T2DPoint): T2DPoint;
begin
  result.x := p.x+k;
  result.y := p.y+k;
end;

constructor T2DPoint.Create(X, Y: Double);
begin
  self.X := X;
  self.Y := Y;
end;

function T2DPoint.Length: double;
begin
  result := sqrt(self.x*self.x+self.y*self.y);
end;

class operator T2DPoint.Multiply(const k: double; const p: T2DPoint): T2DPoint;
begin
  result.x := k*p.x;
  result.y := k*p.y;
end;

class operator T2DPoint.Subtract(const p1, p2: T2DPoint): T2DPoint;
begin
  result.x := p1.x-p2.x;
  result.y := p1.y-p2.y;
end;

constructor T2DPoint.Create(p: T2DPoint);
begin
  self.X := p.X;
  self.Y := p.Y;
end;

{ T2DBox }

constructor T2DBox.Create(Min, Max: T2DPoint);
begin
  self.Min := Min;
  self.Max := Max;
end;

{ TFigure }

function TFigure.Belongs(p: T2DPoint; Picker: IPicker): Boolean;
begin
  Result := PickFigure(p, Picker);
  Picker.SetResult(False);
end;

function TFigure.BelongsVertex(p: T2DPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(fVertexArr) do begin
    if InVertexRange(p, fVertexArr[I]) then begin
      Result := True;
      break;
    end;
  end;
end;

function TFigure.DeserializeColor(ColorObj: TJSONValue): TColor;
begin
  var R := ColorObj.FindValue('R').Value.ToInteger;
  var G := ColorObj.FindValue('G').Value.ToInteger;
  var B := ColorObj.FindValue('B').Value.ToInteger;
  var Color := TColor.Create(R, G, B);
  Result := Color;
end;

function TFigure.DeserializePoint(PointObj: TJSONValue): T2DPoint;
var
  Point: T2DPoint;
begin
  Point.X := PointObj.FindValue('X').Value.ToDouble;
  Point.Y := PointObj.FindValue('Y').Value.ToDouble;
  Result := Point;
end;

function TFigure.DeserializeInteger(WidthObj: TJSONValue): Integer;
begin
  var Value := WidthObj.Value.ToInteger;
  Result := Value;
end;

procedure TFigure.DrawVertex(Canvas: ICanvas);
var
  I: Integer;
begin
  Canvas.SetLineWidth(VERTEX_WIDTH);
  Canvas.SetCurrentColor(VERTEX_COLOR);
  for I := 0 to Length(fVertexArr) - 1 do
  begin
    Canvas.BeginPrimitive(ptQuads);
      Canvas.AddVertex(T2DPoint.Create(fVertexArr[I].X - VERTEX_RADIUS,
        fVertexArr[I].Y - VERTEX_RADIUS));
      Canvas.AddVertex(T2DPoint.Create(fVertexArr[I].X + VERTEX_RADIUS,
        fVertexArr[I].Y - VERTEX_RADIUS));
      Canvas.AddVertex(T2DPoint.Create(fVertexArr[I].X + VERTEX_RADIUS,
        fVertexArr[I].Y + VERTEX_RADIUS));
      Canvas.AddVertex(T2DPoint.Create(fVertexArr[I].X - VERTEX_RADIUS,
        fVertexArr[I].Y + VERTEX_RADIUS));
    Canvas.EndPrimitive;
  end;
end;

function TFigure.GetFigureVertex(p: T2DPoint): Integer;
begin
  for var I := 0 to Length(fVertexArr) do
    if InVertexRange(p, fVertexArr[I]) then Result := I;
end;

function TFigure.InVertexRange(p, Vertex: T2DPoint): Boolean;
begin
  Result :=
    InRange(p.X,
    min(Vertex.X - VERTEX_WIDTH - VERTEX_CLICK_ADDITION_AREA,
    Vertex.X  + VERTEX_WIDTH + VERTEX_CLICK_ADDITION_AREA),
    max(Vertex.X - VERTEX_WIDTH - VERTEX_CLICK_ADDITION_AREA,
    Vertex.X + VERTEX_WIDTH + VERTEX_CLICK_ADDITION_AREA))
    and
    InRange(p.Y,
    min(Vertex.Y - VERTEX_WIDTH - VERTEX_CLICK_ADDITION_AREA,
    Vertex.Y  + VERTEX_WIDTH + VERTEX_CLICK_ADDITION_AREA),
    max(Vertex.Y - VERTEX_WIDTH - VERTEX_CLICK_ADDITION_AREA,
    Vertex.Y + VERTEX_WIDTH + VERTEX_CLICK_ADDITION_AREA))
end;

function TFigure.SerializeColor(Color: TColor): TJSONObject;
begin
  var ColorObj := TJSONObject.Create;
  ColorObj.AddPair('R', IntToStr(Color.R));
  ColorObj.AddPair('G', IntToStr(Color.G));
  ColorObj.AddPair('B', IntToStr(Color.B));
  Result := ColorObj;
end;

function TFigure.SerializePoint(Point: T2DPoint): TJSONObject;
begin
  var PointObj := TJSONObject.Create;
  PointObj.AddPair('X', FloatToStr(Point.X));
  PointObj.AddPair('Y', FloatToStr(Point.Y));
  Result := PointObj;
end;

{ TLine }

function TLine.PickFigure(p: T2DPoint; Picker: IPicker): Boolean;
begin
  for var I := 0 to Length(fDistanceXY) - 1 do
    fDistanceXY[I] := T2DPoint.Create(p.X - fVertexArr[I].X, p.Y - fVertexArr[I].Y);

  Picker.BeginPrimitive(ptLines);
    for var I := 0 to Length(self.fVertexArr) - 1 do Picker.AddVertex(self.fVertexArr[I]);
  Picker.EndPrimitive;

  Result := Picker.GetResult;
end;

procedure TLine.Draw(Canvas: ICanvas);
begin
  Canvas.SetLineWidth(self.Width);
  Canvas.SetCurrentColor(self.Color.Value);

  Canvas.BeginPrimitive(ptLines);
    for var I := 0 to Length(fVertexArr) do Canvas.AddVertex(fVertexArr[I]);
  Canvas.EndPrimitive;
end;

procedure TLine.Move(MovePoint: T2DPoint);
begin
  fVertexArr[0].X := MovePoint.X - fDistanceXY[0].X;
  fVertexArr[0].Y := MovePoint.Y - fDistanceXY[0].Y;
  fVertexArr[1].X := MovePoint.X - fDistanceXY[1].X;
  fVertexArr[1].Y := MovePoint.Y - fDistanceXY[1].Y;
end;

procedure TLine.VertexMove(Point: T2DPoint; lastSelectedVertex: Integer);
begin
  fVertexArr[LastSelectedVertex].X := Point.X;
  fVertexArr[LastSelectedVertex].Y := Point.Y;
end;

function TLine.ToJSON: TJSONObject;
begin
  var json := TJSONObject.Create;
  var IntermediateObj := TJSONObject.Create;

  var StartPointObj := SerializePoint(fVertexArr[0]);
  var EndPointObj := SerializePoint(fVertexArr[1]);
  var ColorObj := SerializeColor(Color);

  IntermediateObj.AddPair('StartPoint', StartPointObj);
  IntermediateObj.AddPair('EndPoint', EndPointObj);
  IntermediateObj.AddPair('Width', IntToStr(Width));
  IntermediateObj.AddPair('Color', ColorObj);

  json.AddPair(self.ClassName, IntermediateObj);

  Result := json;
end;

constructor TLine.CreateFromJSOn(json: TJSONValue);
begin
  var StartPoint := DeserializePoint(json.FindValue('StartPoint'));
  var EndPoint := DeserializePoint(json.FindValue('EndPoint'));
  var Width := DeserializeInteger(json.FindValue('Width'));
  var Color := DeserializeColor(json.FindValue('Color'));

  CreateLine(StartPoint, EndPoint, Width, Color)
end;

constructor TLine.CreateLine(StartPoint, EndPoint: T2DPoint; Width: Integer; Color: TColor);
begin
  self.Width := Width;
  self.Color := Color;

  SetLength(self.fDistanceXY, 2);
  SetLength(self.fVertexArr, 2);
  self.fVertexArr[0] := StartPoint;
  self.fVertexArr[1] := EndPoint;
end;

{ TCircle }

function TCircle.PickFigure(p: T2DPoint; Picker: IPicker): Boolean;
var
  clarity, cRotation: Double;
begin
  fDistanceXY[0] := T2DPoint.Create(p.X - fVertexArr[0].X, p.Y - fVertexArr[0].Y);
  begin
    clarity := 0;
    cRotation := 2 * PI;
    Picker.BeginPrimitive(ptLinestrip);
    while clarity <= cRotation do
    begin
      Picker.AddVertex(
        T2DPoint.Create(fVertexArr[0].X + (self as TCircle).Radius * cos(clarity), fVertexArr[0].Y + (self as TCircle).Radius * sin(clarity))
      );
      clarity := clarity + 0.19634;
      if Picker.GetResult = True then break;
    end;
    Picker.EndPrimitive;
  end;
  Result := Picker.GetResult;
end;

procedure TCircle.Draw(Canvas: ICanvas);
var
  clarity, cRotation: Double;
begin
  Canvas.SetLineWidth(self.Width);
  Canvas.SetCurrentColor(self.Color.Value);
  clarity := 0;
  cRotation := 2 * PI;
  Canvas.BeginPrimitive(ptLinestrip);
  while clarity <= cRotation do
  begin
    Canvas.AddVertex(
      T2DPoint.Create(fVertexArr[0].X + Radius * cos(clarity), fVertexArr[0].Y + Radius * sin(clarity))
    );
    clarity := clarity + 0.19634;
  end;
  Canvas.EndPrimitive;
end;

procedure TCircle.Move(MovePoint: T2DPoint);
begin
  Radius := Abs(fVertexArr[0].X - MovePoint.X);
end;

function TCircle.ToJSON: TJSONObject;
begin
  var json := TJSONObject.Create;
  var IntermediateObj := TJSONObject.Create;

  var CenterPointObj := SerializePoint(fVertexArr[0]);
  var ColorObj := SerializeColor(Color);

  IntermediateObj.AddPair('CenterPoint', CenterPointObj);
  IntermediateObj.AddPair('Radius', FloatToStr(Radius));
  IntermediateObj.AddPair('Width', IntToStr(Width));
  IntermediateObj.AddPair('Color', ColorObj);

  json.AddPair(self.ClassName, IntermediateObj);

  result := json;
end;

procedure TCircle.VertexMove(Point: T2DPoint; lastSelectedVertex: Integer);
begin
  fVertexArr[0].X := Point.X - fDistanceXY[0].X;
  fVertexArr[0].Y := Point.Y - fDistanceXY[0].Y;
end;

constructor TCircle.CreateCircle(CenterPoint: T2DPoint; Radius: Double; Width: Integer; Color: TColor);
begin
  self.Radius := Radius;
  self.Width := Width;
  self.Color := Color;

  SetLength(self.fDistanceXY, 1);
  SetLength(self.fVertexArr, 1);
  self.fVertexArr[0] := CenterPoint;
end;

constructor TCircle.CreateFromJSON(json: TJSONValue);
begin
  var CenterPoint := DeserializePoint(json.FindValue('CenterPoint'));
  var Radius := DeserializeInteger(json.FindValue('Radius'));
  var Width := DeserializeInteger(json.FindValue('Width'));
  var Color := DeserializeColor(json.FindValue('Color'));
  CreateCircle(CenterPoint, Radius, Width, Color)
end;

{ TRectangle }

function TRectangle.PickFigure(p: T2DPoint; Picker: IPicker): Boolean;
begin
  for var I := 0 to Length(fDistanceXY) - 1 do
    fDistanceXY[I] := T2DPoint.Create(p.X - fVertexArr[I].X, p.Y - fVertexArr[I].Y);
  Picker.BeginPrimitive(ptQuads);
    for var I := 0 to Length(fVertexArr) - 1 do begin
      Picker.AddVertex(fVertexArr[I]);
      if Picker.GetResult = True then break;
    end;
  Picker.EndPrimitive;
  Result := Picker.GetResult;
end;

procedure TRectangle.Rotate(Angle: Double);
begin
  self.Angle := self.Angle + Angle;
  var CenterPoint := T2DPoint.Create(fVertexArr[1].X + (fVertexArr[3].X - fVertexArr[1].X) / 2, fVertexArr[1].Y + (fVertexArr[3].Y - fVertexArr[1].Y) / 2);
  for var I := 0 to 3 do begin
    var X := (fVertexArr[I].X - CenterPoint.X) * cos(DegToRad(Angle)) - (fVertexArr[I].Y  - CenterPoint.Y) * sin(DegToRad(Angle)) + CenterPoint.X;
    var Y := (fVertexArr[I].X - CenterPoint.X) * sin(DegToRad(Angle)) + (fVertexArr[I].Y  - CenterPoint.Y) * cos(DegToRad(Angle)) + CenterPoint.Y;
    fVertexArr[I].X := X;
    fVertexArr[I].Y := Y;
  end
end;

procedure TRectangle.Draw(Canvas: ICanvas);
begin
  Canvas.SetCurrentColor(self.Color.Value);
  Canvas.SetLineWidth(self.Width);

  Canvas.BeginPrimitive(ptQuads);
    for var I := 0 to Length(fVertexArr) - 1 do Canvas.AddVertex(fVertexArr[I]);
  Canvas.EndPrimitive;
end;

procedure TRectangle.Move(MovePoint: T2DPoint);
begin
  for var I := 0 to Length(self.fDistanceXY) - 1 do begin
    fVertexArr[I].X := MovePoint.X - fDistanceXY[I].X;
    fVertexArr[I].Y := MovePoint.Y - fDistanceXY[I].Y;
  end;
end;

function TRectangle.ToJSON: TJSONObject;
begin
  var json := TJSONObject.Create;
  var IntermediateObj := TJSONObject.Create;

  var MinPointObj := SerializePoint(fVertexArr[1]);
  var MaxPointObj := SerializePoint(fVertexArr[3]);
  var ColorObj := SerializeColor(Color);

  IntermediateObj.AddPair('MinPoint', MinPointObj);
  IntermediateObj.AddPair('MaxPoint', MaxPointObj);
  IntermediateObj.AddPair('Angle', FloatToStr(Angle));
  IntermediateObj.AddPair('Width', IntToStr(Width));
  IntermediateObj.AddPair('Color', ColorObj);

  json.AddPair(self.ClassName, IntermediateObj);

  result := json;
end;

procedure TRectangle.VertexMove(Point: T2DPoint; lastSelectedVertex: Integer);
begin

  if (LastSelectedVertex = 0) then begin
    fVertexArr[LastSelectedVertex].X := Point.X;
    fVertexArr[LastSelectedVertex].Y := Point.Y;
    if self.Angle = 0  then begin
      fVertexArr[3].Y := Point.Y;
      fVertexArr[1].X := Point.X;
    end else begin
      fVertexArr[3] := PointProject(Point, fVErtexArr[3],fvertexarr[2]);
      fVertexArr[1] := PointProject(Point, fVErtexArr[1],fvertexarr[2]);
    end;

  end else if LastSelectedVertex = 1 then begin
    fVertexArr[LastSelectedVertex].X := Point.X;
    fVertexArr[LastSelectedVertex].Y := Point.Y;
    if self.Angle = 0  then begin
      fVertexArr[2].Y := Point.Y;
      fVertexArr[0].X := Point.X;
    end else begin
      fVertexArr[2] := PointProject(Point, fVErtexArr[2],fvertexarr[3]);
      fVertexArr[0] := PointProject(Point, fVErtexArr[0],fvertexarr[3]);
    end;

   end else if LastSelectedVertex = 2 then begin
    fVertexArr[LastSelectedVertex].X := Point.X;
    fVertexArr[LastSelectedVertex].Y := Point.Y;
    if self.Angle = 0  then begin
      fVertexArr[1].Y := Point.Y;
      fVertexArr[3].X := Point.X;
    end else begin
      fVertexArr[1] := PointProject(Point, fVErtexArr[1],fvertexarr[0]);
      fVertexArr[3] := PointProject(Point, fVErtexArr[0],fvertexarr[3]);
    end;

   end else if LastSelectedVertex = 3 then begin
    fVertexArr[LastSelectedVertex].X := Point.X;
    fVertexArr[LastSelectedVertex].Y := Point.Y;
    if self.Angle = 0  then begin
      fVertexArr[2].Y := Point.Y;
      fVertexArr[0].X := Point.X;
    end else begin
      fVertexArr[2] := PointProject(Point, fVErtexArr[1],fvertexarr[2]);
      fVertexArr[0] := PointProject(Point, fVErtexArr[0],fvertexarr[1]);
    end;
  end;
end;

constructor TRectangle.CreateRectangle(MinPoint, MaxPoint: T2DPoint; Width: Integer; Color: TColor; Angle: Double = 0);
begin
  self.Width := Width;
  self.Color := Color;
  self.Angle := Angle;

  SetLength(self.fDistanceXY, 4);
  SetLength(self.fVertexArr, 4);
  self.fVertexArr[0] := MinPoint;
  self.fVertexArr[1] := T2DPoint.Create(MinPoint.X, MaxPoint.Y);
  self.fVertexArr[2] := MaxPoint;
  self.fVertexArr[3] := T2DPoint.Create(MaxPoint.X, MinPoint.Y);
end;

constructor TRectangle.CreateFromJSON(json: TJSONValue);
begin
  var MinPoint := DeserializePoint(json.FindValue('MinPoint'));
  var MaxPoint := DeserializePoint(json.FindValue('MaxPoint'));
  var Width := DeserializeInteger(json.FindValue('Width'));
  var Color := DeserializeColor(json.FindValue('Color'));
  var Angle := DeserializeInteger(json.FindValue('Angle'));

  CreateRectangle(MinPoint, MaxPoint, Width, Color, Angle);
end;
end.
