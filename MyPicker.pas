unit MyPicker;

interface

uses MyTypes, Vcl.Graphics, Math, vcl.Dialogs, System.Types;

type
  TMyPicker = class(TInterfacedObject, IPicker)
  public
    fPrimitive: TPrimitiveType;
    fVertexArray: array[1..4] of T2DPoint;
    fLastVertex: integer;
    fClickPoint: T2DPoint;
    fResult: Boolean;
    procedure SetClickPoint(p: T2Dpoint);
    procedure BeginPrimitive(Typ: TPrimitiveType);
    procedure EndPrimitive;
    procedure AddVertex(const p: T2DPoint);

    procedure SetResult(Value: Boolean);
    function GetResult: Boolean;

    function BelongsToSegment(SegmentInitPoint, SegmentEndPoint, ClickPoint: T2DPoint): Boolean;
  end;


implementation

{ TMyPicker }

procedure TMyPicker.AddVertex(const p: T2DPoint);
begin
  case fPrimitive of
    ptLines:
    begin
      if fLastVertex = 0 then begin
        fVertexArray[1] := p;
        fLastVertex := 1;
      end else begin
        fVertexArray[2] := p;
        fResult := BelongsToSegment(fVertexArray[1], fVertexArray[2], fClickPoint);
      end;
    end;
    ptLineStrip:
    begin
      if fLastVertex = 0 then begin
        fVertexArray[1] := p;
        fLastVertex := 1;
      end else begin
        fVertexArray[2] := p;
        fResult := BelongsToSegment(fVertexArray[1], fVertexArray[2], fClickPoint);
        fVertexArray[1] := p;
      end;
    end;

    ptQuads:
    begin
      if fLastVertex = 0 then begin
        fVertexArray[1] := p;
        fLastVertex := 1;
      end else begin
        Inc(fLastVertex);
        fVertexArray[fLastVertex] := p;
        fResult := BelongsToSegment(fVertexArray[fLastVertex - 1], fVertexArray[fLastVertex], fClickPoint);
      end;

      if (fResult = False) and (fLastVertex = 4) then begin
        fResult := BelongsToSegment(fVertexArray[4], fVertexArray[1], fClickPoint);
      end;
    end;
  end;
end;


procedure TMyPicker.BeginPrimitive(Typ: TPrimitiveType);
begin
  fPrimitive := Typ;
end;

function TMyPicker.BelongsToSegment(SegmentInitPoint, SegmentEndPoint,
  ClickPoint: T2DPoint): Boolean;
var
  dist: Double;
begin
  dist := PointLineDist(ClickPoint, SegmentInitPoint, SegmentEndPoint);
  if dist < 5 then Result := True
  else Result := False;
end;

procedure TMyPicker.EndPrimitive;
begin
  fPrimitive := ptNone;
  fLastVertex := 0;
end;

function TMyPicker.GetResult: Boolean;
begin
  Result := self.fResult;
end;

procedure TMyPicker.SetClickPoint(p: T2Dpoint);
begin
  fClickPoint := p;
end;

procedure TMyPicker.SetResult(Value: Boolean);
begin
  fResult := Value;
end;
end.
