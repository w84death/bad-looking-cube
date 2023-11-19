unit BLC_About;

interface

uses
  Windows, Forms, jpeg, Classes, StdCtrls, ExtCtrls, Controls, Graphics;

type
  TFormAbout = class(TForm)
    Label1: TLabel;
    GroupCredits: TGroupBox;
    Label2: TLabel;
    ImageP1X: TImage;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    ButtonOK: TButton;
    procedure ButtonOKClick(Sender: TObject);
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

end.
