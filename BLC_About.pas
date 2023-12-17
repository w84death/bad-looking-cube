unit BLC_About;

interface

uses
  Windows, Forms, jpeg, Classes, StdCtrls, ExtCtrls, Controls, Graphics;

type
  TFormAbout = class(TForm)
    ButtonOK: TButton;
    ImageP1X: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure CenterWindow();
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

procedure TFormAbout.ButtonOKClick(Sender: TObject);
begin
Close;
end;

procedure TFormAbout.CenterWindow();
begin;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  CenterWindow;
end;

end.
