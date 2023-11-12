unit BLC_Renderer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TFormDemo = class(TForm)
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TPoint3D = record
    X,Y,Z: Double;
  end;

  TTriangle3D = record
    V1,V2,V3: Integer;
  end;

  const
  MeshV: array[0..7] of TPoint3D = (
  (X:-10;Y:-10;Z:-10),(X:10;Y:-10;Z:-10),(X:10;Y:10;Z:-10),(X:-10;Y:10;Z:-10),
  (X:-10;Y:-10;Z:10),(X:10;Y:-10;Z:10),(X:10;Y:10;Z:10),(X:-10;Y:10;Z:10)
  );

  MeshT: array[0..11] of TTriangle3D = (
  (V1:3; V2:2; V3:1),
  (V1:0; V2:2; V3:3),
  (V1:2; V2:5; V3:6),
  (V1:2; V2:6; V3:2),
  (V1:5; V2:4; V3:7),
  (V1:5; V2:7; V3:6),
  (V1:4; V2:0; V3:3),
  (V1:4; V2:3; V3:7),
  (V1:3; V2:2; V3:6),
  (V1:3; V2:6; V3:7),
  (V1:4; V2:5; V3:1),
  (V1:4; V2:1; V3:0)
  );
var
  FormDemo: TFormDemo;

  Camera: TPoint3D = (X:0;Y:0;Z:-20);
  ViewDir: TPoint3D = (X:0;Y:0;Z:-1);
  
implementation

{$R *.dfm}

procedure TFormDemo.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
  Triangle: TTriangle3D;
  Normal: TPoint3D;
  DotProd: Double;
  function Project(const Point: TPoint3D): TPoint;
  var
    Distance: Double;
  begin
    Distance := 50.0;
    Result.X := Round(PaintBox1.Width / 2 + ((Point.X+Camera.X)*Distance/(Point.Z+Camera.Z+Distance)));
    Result.Y := Round(PaintBox1.Height / 2 - ((Point.Y+Camera.Y)*Distance/(Point.Z+Camera.Z+Distance)));
  end;

  function CalcNormal(T: TTriangle3D):TPoint3D;
  var
    U,V: TPoint3D;
    Length: Double;
  begin
    U.X := MeshV[T.V2].X - MeshV[T.V1].X;
    U.Y := MeshV[T.V2].Y - MeshV[T.V1].Y;
    U.Z := MeshV[T.V2].Z - MeshV[T.V1].Z;

    U.X := MeshV[T.V3].X - MeshV[T.V1].X;
    U.Y := MeshV[T.V3].Y - MeshV[T.V1].Y;
    U.Z := MeshV[T.V3].Z - MeshV[T.V1].Z;

    Result.X := U.Y * V.Z - U.Z * V.Y;
    Result.Y := U.Z * V.X - U.X * V.Z;
    Result.Z := U.X * V.Y - U.Y * V.X;
  end;
begin
  for i := Low(MeshT) to High(MeshT) do
  begin
    Triangle := MeshT[i];
    Normal := CalcNormal(Triangle);
    DotProd := Normal.X * ViewDir.X + Normal.Y * ViewDir.Y + Normal.Z * ViewDir.Z;

    with PaintBox1.Canvas do
    begin
      Pen.Color := clGray;
      if DotProd > 0 then
      begin
        Pen.Color := clWhite;
      end;
        MoveTo(Project(MeshV[Triangle.V1]).X, Project(MeshV[Triangle.V1]).Y);
        LineTo(Project(MeshV[Triangle.V2]).X, Project(MeshV[Triangle.V2]).Y);
        LineTo(Project(MeshV[Triangle.V3]).X, Project(MeshV[Triangle.V3]).Y);
        LineTo(Project(MeshV[Triangle.V1]).X, Project(MeshV[Triangle.V1]).Y);
    end;
  end;
end;
end.
