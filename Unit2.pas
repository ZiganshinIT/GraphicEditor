unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, MyCanvas, MyTypes, MyDrawing,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.TitleBarCtrls, System.ImageList, Vcl.ImgList, Vcl.NumberBox, MyPicker;

type
  TForm2 = class(TForm)
    PaintBox: TPaintBox;

    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    ColorDialog: TColorDialog;

    ImageList: TImageList;
    ToolBar: TToolBar;

    SaveButton: TToolButton;
    LoadButton: TToolButton;
    LineButton: TToolButton;
    CircleButton: TToolButton;
    RectangleButton: TToolButton;
    HandButton: TToolButton;
    ColorButton: TToolButton;

    WidthLabel: TLabel;
    WidthEdit: TEdit;
    WidthUpDown: TUpDown;

    ContextMenu: TPopupMenu;
    DeleteObjectAction: TMenuItem;
    StatusBar: TStatusBar;
    N451: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure SaveButtonClick(Sender: TObject);
    procedure LineButtonClick(Sender: TObject);
    procedure CircleButtonClick(Sender: TObject);
    procedure RectangleButtonClick(Sender: TObject);
    procedure HandButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);

    procedure WidthEditChange(Sender: TObject);
    procedure DeleteObjectActionClick(Sender: TObject);
    procedure N451Click(Sender: TObject);

  protected
    fC: TMyCanvas;
    fMyCanvas: ICanvas;
    fMyDrawing: TMyDrawing;
    fMyPicker: IPicker;

    fIsFirstTap: Boolean;
    fInitialPoint, fEndPoint: TPoint;

    fPenColor, fPenWidth: Integer;

    fCheckedButton: TToolButton;

    procedure ResetFields;
    procedure CheckButton(b:TToolButton);
  end;

var
  Form2: TForm2;
  Tools: TTools;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  fC := TMyCanvas.Create;
  fC.Canvas := self.PaintBox.Canvas;

  fC.ViewPort := T2DBox.Create(
    T2DPoint.Create(0, 0),
    T2DPoint.Create(self.PaintBox.ClientWidth, self.PaintBox.ClientHeight)
  );

  fC.SetViewBox(PaintBox.Width, PaintBox.Height);
  fMyCanvas := fC;

  fMyDrawing := TMyDrawing.Create;
  fMyDrawing.fCanvas := fC;

  fMyPicker := TMyPicker.Create;

  fIsFirstTap := True;

  fCheckedButton := LineButton;
  fCheckedButton.Down := True;

  Screen.Cursors[100] := LoadCursorFromFile('..\..\icons\Move.cur');
end;

procedure TForm2.PaintBoxPaint(Sender: TObject);
var
  InitGlobPoint, EndGlobPoint: T2DPoint;
begin
  fC.RePaintMyCanvas;
  fMyDrawing.Draw;

  InitGlobPoint := fMyCanvas.GetGlobalPoint(fInitialPoint);
  EndGlobPoint := fMyCanvas.GetGlobalPoint(fEndPoint);

  if ((fInitialPoint.X <> 0) and (fInitialPoint.Y <> 0)) or
  ((fEndPoint.X <> 0) and (fEndPoint.Y <> 0) and (Tools = Circle)) then
    fMyDrawing.PreDraw(InitGlobPoint, EndGlobPoint, Tools,
      TColor.Create(GetRValue(fPenColor), GetGValue(fPenColor), GetBValue(fPenColor)), fPenWidth);

  if fMyDrawing.fLastSelectedFigure = -1 then ContextMenu.AutoPopup := False
  else ContextMenu.AutoPopup := True;
end;

procedure TForm2.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  fC.ZoomCanvas(WheelDelta, PaintBox.Width, PaintBox.Height);
  fMyDrawing.ResetAddFigure;
  ResetFields;
  Refresh;
end;

procedure TForm2.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  GlobPoint: T2DPoint;
begin
  fMyDrawing.fClickPoint := TPoint.Create(X, Y);
  fMyDrawing.fPick.SetClickPoint(fMyCanvas.GetGlobalPoint(TPoint.Create(X, Y)));
  GlobPoint := fMyCanvas.GetGlobalPoint(TPoint.Create(X, Y));

  if Button = mbRight then begin
    fMyDrawing.isPaintBoxScrollable := True;
    PaintBox.Cursor := 100;
  end else begin
    if fIsFirstTap then begin
      fInitialPoint := TPoint.Create(X, Y);
    end else begin
      fInitialPoint := TPoint.Create(0, 0);
    end;
    fIsFirstTap := not fIsFirstTap;

    fMyDrawing.ToolsAction(GlobPoint, Tools, TColor.Create(GetRValue(fPenColor),
      GetGValue(fPenColor), GetBValue(fPenColor)), fPenWidth);

    fMyDrawing.isFigureMoveable := True;
    fMyDrawing.isVertexMoveable := True;

    Cursor := fMyDrawing.GetCursor(GlobPoint);

    PaintBox.Refresh;
  end;
end;

procedure TForm2.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  GlobPoint: T2DPoint;
  ViewPortX, ViewPortY: Double;
begin
  fEndPoint := TPoint.Create(X, Y);
  GlobPoint := fMyCanvas.GetGlobalPoint(TPoint.Create(X, Y));

  ViewPortX := fC.ViewPort.Max.X;
  ViewPortY := fC.ViewPort.Max.Y;

  if fMyDrawing.isPaintBoxScrollable then begin
    fC.ScrollCanvas(TPoint.Create(X, Y), fMyDrawing.fClickPoint,
      PaintBox.Width, PaintBox.Height);
    if fIsFirstTap = False then begin
      fInitialPoint.X := Round(fInitialPoint.X + (ViewPortX - fC.ViewPort.Max.X));
      fInitialPoint.Y := Round(fInitialPoint.Y - (ViewPortY - fC.ViewPort.Max.Y));
    end;

  end;

  if fMyDrawing.isVertexMoveable then
    fMyDrawing.VertexMove(GlobPoint);

  if fMyDrawing.isFigureMoveable then
    fMyDrawing.FigureMove(GlobPoint);

  if Tools = Hand then begin
    fMyDrawing.fPick.SetClickPoint(fMyCanvas.GetGlobalPoint(TPoint.Create(X, Y)));
    fMyDrawing.HoverFigure(GlobPoint);
  end;

  Cursor := fMyDrawing.GetCursor(GlobPoint);

  StatusBar.Panels[0].Text := IntToStr(X) + ', ' + IntToStr(Y) + ' οκρ ';

  refresh;
end;

procedure TForm2.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then begin
    fMyDrawing.isPaintBoxScrollable := False;
    PaintBox.Cursor := crDefault;
  end;

  fMyDrawing.isVertexMoveable := False;
  fMyDrawing.isFigureMoveable := False;
  fMyDrawing.fLastSelectedVertex := -1;
end;

procedure TForm2.SaveButtonClick(Sender: TObject);
begin
  fMyDrawing.ResetSelection;

  SaveDialog := TSaveDialog.Create(self);
  SaveDialog.Title := 'Save file';
  SaveDialog.InitialDir := GetCurrentDir;
  saveDialog.Filter := 'Text file|*.rnt|';
  saveDialog.DefaultExt := 'rnt';
  if saveDialog.Execute then begin
    fMyDrawing.save(saveDialog.FileName);
  end;
  saveDialog.Free;
end;

procedure TForm2.LoadButtonClick(Sender: TObject);
begin
  fMyDrawing.ResetSelection;

  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Title := 'Open file';
  OpenDialog.InitialDir := GetCurrentDir;
  OpenDialog.Filter := 'Figure file|*.rnt|';
  OpenDialog.DefaultExt := 'rnt';
  if OpenDialog.Execute then begin
    fMyDrawing.load(OpenDialog.FileName);
  end;
  OpenDialog.Free;
  Refresh;
end;

procedure TForm2.N451Click(Sender: TObject);
var
  CenterPoint: T2DPoint;
  X, Y: Double;
  I: Integer;
begin
  if fMyDrawing.fFigures[fMyDrawing.fLastSelectedFigure].ClassName = 'TRectangle' then begin
    (fMyDrawing.fFigures[fMyDrawing.fLastSelectedFigure] as TRectangle).Rotate(45);
  end;
end;

procedure TForm2.LineButtonClick(Sender: TObject);
begin
  Tools := Line;

  ResetFields;
  fMyDrawing.ResetAddFigure;
  fMyDrawing.ResetSelection;

  CheckButton(LineButton);
end;

procedure TForm2.CircleButtonClick(Sender: TObject);
begin
  Tools := Circle;

  ResetFields;
  fMyDrawing.ResetAddFigure;
  fMyDrawing.ResetSelection;

  CheckButton(CircleButton);
end;

procedure TForm2.RectangleButtonClick(Sender: TObject);
begin
  Tools := Rectangle;

  ResetFields;
  fMyDrawing.ResetAddFigure;
  fMyDrawing.ResetSelection;

  CheckButton(RectangleButton);
end;

procedure TForm2.HandButtonClick(Sender: TObject);
begin
  Tools := Hand;

  ResetFields;
  fMyDrawing.ResetAddFigure;
  fMyDrawing.ResetSelection;

  CheckButton(HandButton);
end;

procedure TForm2.ColorButtonClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    fPenColor := ColorDialog.Color;
    if fMyDrawing.fLastSelectedFigure <> -1 then begin
      fMyDrawing.fFigures[fMyDrawing.fLastSelectedFigure].Color := TColor.Create(GetRValue(fPenColor), GetGValue(fPenColor), GetBValue(fPenColor));
    end;
    refresh;
  end;
end;

procedure TForm2.DeleteObjectActionClick(Sender: TObject);
begin
  fMyDrawing.Delete();
  refresh;
end;

procedure TForm2.WidthEditChange(Sender: TObject);
var
  str: string;
begin
  str := WidthEdit.Text;
  fPenWidth := str.ToInteger;
  if fMyDrawing.fLastSelectedFigure <> -1 then begin
    fMyDrawing.fFigures[fMyDrawing.fLastSelectedFigure].Width := fPenWidth;
  end;
  refresh;
end;

procedure TForm2.ResetFields;
begin
  fInitialPoint := TPoint.Create(0, 0);
  fIsFirstTap := True;
end;

procedure TForm2.CheckButton(b: TToolButton);
begin
  if fCheckedButton <> nil then begin
    fCheckedButton.Down := False;
  end;
  fCheckedButton := b;
  fCheckedButton.Down := True;
end;

end.
