program DelphiPaint;

{$R *.dres}

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  MyTypes in 'MyTypes.pas',
  MyCanvas in 'MyCanvas.pas',
  MyDrawing in 'MyDrawing.pas',
  MyPicker in 'MyPicker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
