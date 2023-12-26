program BadLookingCube;

uses
  Forms,
  BLC_Demo01 in 'BLC_Demo01.pas' {FormCC},
  BLC_Renderer in 'BLC_Renderer.pas' {FormDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bad Looking Cube';
  Application.CreateForm(TFormCC, FormCC);
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
