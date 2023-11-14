program BadLookingCube;

uses
  Forms,
  BLC_Demo01 in 'BLC_Demo01.pas' {FormCC},
  BLC_Renderer in 'BLC_Renderer.pas' {FormDemo},
  BLC_About in 'BLC_About.pas' {FormAbout},
  BLC_Director in 'BLC_Director.pas' {FormDirector};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bad Looking Cube';
  Application.CreateForm(TFormCC, FormCC);
  Application.CreateForm(TFormDirector, FormDirector);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormDemo, FormDemo);

  Application.Run;
end.
