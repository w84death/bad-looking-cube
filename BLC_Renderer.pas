unit BLC_Renderer;

interface

uses
  Windows, OpenGL, SysUtils, Variants, Classes, Controls, Forms,
  ExtCtrls,Messages;

type
  TColor = record
    R, G, B: GLfloat;
  end;

  TVertex = record
    X, Y, Z: GLfloat;
  end;

  TFace = record
    Vec3: array[1..3] of Integer;
    Normal: TVertex;
  end;

  TModel = record
    Vertices: array of TVertex;
    Faces: array of TFace;
    Position: TVertex;
    Rotation: TVertex;
    Material: Integer;
  end;

  TCamera = record
    X, Y, Z: GLfloat;
    AngleX:  GLfloat;
    AngleY:  GLfloat;
    AngleZ:  GLfloat;
  end;

  TFormDemo = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure Render();
    function ReadOBJFile(const FileName: string): TModel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    private
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    public
    { Public declarations }
    protected
      //procedure Paint; override;
   end;

  TWglSwapIntervalEXT = procedure(interval: GLInt); stdcall;

var
  FormDemo: TFormDemo;
  DemoTime: Double = 0.0;
  DemoRunning: Boolean = false;
  MovementVector: TVertex = (X:0.0;Y:0.0;Z:0.0);
  Camera: TCamera;
  Models: array [0..13] of TModel;
  DC: HDC;
  RC: HGLRC;
  wglSwapIntervalEXT: TWglSwapIntervalEXT;

  JeepPosition: TVertex = (X:1.0;Y:0.0;Z:20.0);

  Framebuffer: GLuint;
  Texture: GLuint;
  Width, Height: Integer;

  AmbientLight: array[0..3] of GLfloat = (0.2,0.2,0.2,1.0);
  DiffuseLight: array[0..3] of GLfloat = (0.7,0.6,0.5,1.0);
  SpecularLight: array[0..3] of GLfloat = (0.8,0.754,0.7,1.0);
  LightPosition: array[0..3] of GLfloat = (-5.0,5.0,0.0,1.0);

  Specular: array[0..3] of GLfloat = (0.5,0.5,0.5,1.0);
  EmissionOff: array[0..3] of GLfloat = (0.0,0.0,0.0,1.0);
  Matt: array[0..3] of GLfloat = (0.2,0.2,0.2,1.0);
  Shininess:  array[0..3] of GLfloat = (1.0,1.0,1.0,1.0);
  Color0: array[0..3] of GLfloat = (0.1,0.1,0.1,1.0);
  Color1: array[0..3] of GLfloat = (0.0,0.1,0.45,1.0);
  Color2: array[0..3] of GLfloat = (0.7,0.2,0.4,1.0);
  ColorGreen: array[0..3] of GLfloat = (0.1,0.2,0.0,1.0);
  FogColor: array[0..3] of GLfloat = (0.1,0.1,0.1,1.0);

implementation

{$R *.dfm}

procedure TFormDemo.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure InitVSync;
begin
  if not Assigned(wglSwapIntervalEXT) then
  begin
    wglSwapIntervalEXT := wglGetProcAddress('wglSwapIntervalEXT');
    if not Assigned(wglSwapIntervalEXT) then Exit;
  end;
  wglSwapIntervalEXT(1);
end;

procedure TFormDemo.Timer1Timer(Sender: TObject);
const
  MovementReduction: Double = 0.01;
begin
  if MovementVector.X > 0 then
  begin
    MovementVector.X := MovementVector.X - MovementReduction;
    if MovementVector.X < 0 then MovementVector.X := 0;
  end;
  if MovementVector.X < 0 then
  begin
    MovementVector.X := MovementVector.X + MovementReduction;
    if MovementVector.X > 0 then MovementVector.X := 0;
  end;
  Camera.X := Camera.X + MovementVector.X;

  if MovementVector.Y > 0 then
  begin
    MovementVector.Y := MovementVector.Y - MovementReduction;
    if MovementVector.Y < 0 then MovementVector.Y  := 0;
  end;
  if MovementVector.Y < 0 then
  begin
    MovementVector.Y := MovementVector.Y + MovementReduction;
    if MovementVector.Y > 0 then MovementVector.Y:= 0;
  end;
  Camera.Y := Camera.Y + MovementVector.Y;


  if MovementVector.Z > 0 then
  begin
    MovementVector.Z := MovementVector.Z - MovementReduction;
    if MovementVector.Z < 0 then MovementVector.Z  := 0;
  end;
  if MovementVector.Z < 0 then
  begin
    MovementVector.Z := MovementVector.Z + MovementReduction;
    if MovementVector.Z > 0 then MovementVector.Z:= 0;
  end;
  Camera.Z := Camera.Z + MovementVector.Z;

 if DemoRunning then
 begin
  Models[0].Rotation.Y := DemoTime*7.0;
  Models[4].Position.Y := 0.2+Sin(DemoTime)*0.2;
  Models[5].Position.Y := 0.2+Sin(DemoTime+3.0)*0.2;
  Models[6].Position.Y := 0.2+Sin(DemoTime+4.0)*0.2;
  Models[4].Rotation.Y := Sin(DemoTime)*2.0;
  Models[4].Rotation.X := Sin(DemoTime)*2.0;
  Models[5].Rotation.Y := Sin(DemoTime+6)*2.0;
  Models[5].Rotation.X := Sin(DemoTime+2)*2.0;
  Models[6].Rotation.Y := Sin(DemoTime+8)*2.0;
  Models[6].Rotation.X := Sin(DemoTime+9)*2.0;
  Models[7].Rotation.Y := DemoTime*24.0;

  JeepPosition.Z := 30.0 - 20.0*Sin(DemoTime/16);
  JeepPosition.X := 1.0 - Sin(DemoTime/8)*1.5;
  Models[10].Position := JeepPosition;
  Models[10].Rotation.Y := Sin(DemoTime/8)*6.0;
  Models[10].Rotation.Z := Sin(DemoTime/2)*1.0;
 end;

 Render();
 Invalidate;
end;


function TFormDemo.ReadOBJFile(const FileName: string): TModel;
var
  FileLines: TStringList;
  i: Integer;
  Line: string;
  Parts: TStringList;
  Vertex: TVertex;
  Face: TFace;
  Model: TModel;

function CalculateNormal(V1,V2,V3: TVertex): TVertex;
var
  Edge1, Edge2, Normal: TVertex;
  Length: GLfloat;
begin
  Edge1.X := V2.X - V1.X;
  Edge1.Y := V2.Y - V1.Y;
  Edge1.Z := V2.Z - V1.Z;

  Edge2.X := V3.X - V1.X;
  Edge2.Y := V3.Y - V1.Y;
  Edge2.Z := V3.Z - V1.Z;

  Normal.X := Edge1.Y * Edge2.Z - Edge1.Z * Edge2.y;
  Normal.Y := Edge1.Z * Edge2.X - Edge1.X * Edge2.Z;
  Normal.Z := Edge1.X * Edge2.Y - Edge1.Y * Edge2.X;

  Length := sqrt(Normal.X*Normal.X+Normal.Y*Normal.Y+Normal.Z*Normal.Z);
  if Length <> 0 then
  begin
    Normal.X := Normal.X / Length;
    Normal.Y := Normal.Y / Length;
    Normal.Z := Normal.Z / Length;
  end;

  Result := Normal;
end;

begin
  FileLines := TStringList.Create;
  Parts := TStringList.Create;

  try
    FileLines.LoadFromFile(FileName);

    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      Parts.Delimiter := ' ';
      Parts.DelimitedText := Line;

      if (Parts.Count = 4) and (Parts[0] = 'v') then
      begin
        Vertex.X := StrToFloat(Parts[1]);
        Vertex.Y := StrToFloat(Parts[2]);
        Vertex.Z := StrToFloat(Parts[3]);
        SetLength(Model.Vertices, Length(Model.Vertices) + 1);
        Model.Vertices[High(Model.Vertices)] := Vertex;
      end;

      if (Parts.Count = 4) and (Parts[0] = 'f') then
      begin
        Face.Vec3[1] := StrToInt(Parts[1]);
        Face.Vec3[2] := StrToInt(Parts[2]);
        Face.Vec3[3] := StrToInt(Parts[3]);
        SetLength(Model.Faces, Length(Model.Faces) + 1);
        Model.Faces[High(Model.Faces)] := Face;
      end;
    end;

    for i := 0 to High(Model.Faces) do
    begin
      Model.Faces[i].Normal := CalculateNormal(
          Model.Vertices[Model.Faces[i].Vec3[1]-1],
          Model.Vertices[Model.Faces[i].Vec3[2]-1],
          Model.Vertices[Model.Faces[i].Vec3[3]-1]);
    end;
  finally
    Parts.Free;
    FileLines.Free;
  end;

  Model.Position.X := 0.0;
  Model.Position.Y := 0.0;
  Model.Position.Z := 0.0;

  Model.Rotation.X := 0.0;
  Model.Rotation.Y := 0.0;
  Model.Rotation.Z := 0.0;
  Model.Material := 0;

  Result := Model;
end;

procedure TFormDemo.FormCreate(Sender: TObject);
var
  PixelFormat: TPixelFormatDescriptor;
  FormatIndex: integer;
procedure LoadModel(Id: Integer; Name: string; X,Y,Z: Double; Material: Integer);
var
  Model: TModel;
begin
  Model := ReadOBJFile('Models/'+Name);
  Model.Position.X := X;
  Model.Position.Y := Y;
  Model.Position.Z := Z;
  Model.Material := Material;
  Models[Id] := Model;
end;
begin
  LoadModel(0,'P1X_logo.obj', -3.0,2.0,-1.0,  3);
  LoadModel(1,'terrain.obj',    0.0,0.0,0.0,    1);
  LoadModel(2,'room.obj',     0.0,0.0,0.0,    1);
  LoadModel(3,'desk.obj',     0.0,0.0,0.0,    2);
  LoadModel(4,'pc.obj',       0.0,0.0,0.0,    2);
  LoadModel(5,'kbd.obj',      0.0,0.0,0.0,    2);
  LoadModel(6,'monitor.obj',  0.0,0.0,0.0,    2);
  LoadModel(7,'fan.obj',      0.0,3.0,0.0,    0);
  LoadModel(8,'shelf.obj',    0.0,0.0,0.0,    0);
  LoadModel(9,'sofa.obj',     0.0,0.0,0.0,    0);

  LoadModel(10,'jeep.obj',    JeepPosition.X,
                              JeepPosition.Y,
                              JeepPosition.Z,    1);
  LoadModel(11,'road.obj',    0.0,0.0,0.0,    1);

  LoadModel(12,'tree1.obj',    7.0,0.0,5.0,    1);
  LoadModel(13,'tree2.obj',    -6.0,0.0,4.0,    1);


  Camera.Y := 1.6;
  Camera.Z := 3.0;

  DC := GetDC(Handle);
  FillChar(PixelFormat, SizeOf(PixelFormat), 0);
  with PixelFormat do
    begin
      nSize := SizeOf(PixelFormat);
      nVersion := 1;
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      iPixelType := PFD_TYPE_RGBA;
      cColorBits := 8;
      cDepthBits := 16;
      iLayerType := PFD_MAIN_PLANE;
    end;
  FormatIndex := ChoosePixelFormat(DC, @PixelFormat);
  SetPixelFormat(DC, FormatIndex, @PixelFormat);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);
  InitVSync;

  glViewport(0,0,ClientWidth,ClientHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70.0,ClientWidth/ClientHeight, 0.1, 40.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  glEnable(GL_FOG);
  glFogi(GL_FOG_MODE, GL_EXP2);
  glFogfv(GL_FOG_COLOR, @FogColor);
  glFogf(GL_FOG_DENSITY, 0.08);
  glHint(GL_FOG_HINT, GL_DONT_CARE);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @AmbientLight);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @DiffuseLight);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @SpecularLight);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
end;

procedure TFormDemo.Render();
var
  f,j, m, model: Integer;
  Vert: TVertex;
  Normal: TVertex;
  Face: TFace;
  side: Double;
begin
  glClearColor(0.1, 0.1, 0.1, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();

  gluLookAt(JeepPosition.X+Camera.X, Camera.Y+4.0, JeepPosition.Z+Camera.Z,
  JeepPosition.X,JeepPosition.Y+2.0,JeepPosition.Z-2.0,
  0.0,0.0,-1.0);

  for m := Low(Models) to 11 do
  begin
    glPushMatrix();
    glTranslatef(Models[m].Position.X, Models[m].Position.Y, Models[m].Position.Z);
    glRotate(Models[m].Rotation.X, 1.0,0.0,0.0);
    glRotate(Models[m].Rotation.Y, 0.0,1.0,0.0);
    glRotate(Models[m].Rotation.Z, 0.0,0.0,1.0);

    if Models[m].Material = 0 then
    begin
    glMaterialfv(GL_FRONT,GL_AMBIENT, @Color0);
    glMaterialfv(GL_FRONT,GL_DIFFUSE, @Color0);
    glMaterialfv(GL_FRONT,GL_SHININESS, @Matt);
    end;

    if Models[m].Material = 1 then
    begin
    glMaterialfv(GL_FRONT,GL_AMBIENT, @Color1);
    glMaterialfv(GL_FRONT,GL_DIFFUSE, @Color1);
    glMaterialfv(GL_FRONT,GL_SHININESS, @Shininess);
    end;

    if Models[m].Material = 2 then
    begin
    glMaterialfv(GL_FRONT,GL_AMBIENT, @Color2);
    glMaterialfv(GL_FRONT,GL_DIFFUSE, @Color2);
    glMaterialfv(GL_FRONT,GL_SHININESS, @Matt);
    end;

    glMaterialfv(GL_FRONT,GL_SPECULAR, @Specular);
    glMaterialfv(GL_FRONT,GL_EMISSION, @EmissionOff);

    if Models[m].Material = 3 then
    begin
    glMaterialfv(GL_FRONT,GL_AMBIENT, @Color2);
    glMaterialfv(GL_FRONT,GL_DIFFUSE, @Color2);
    glMaterialfv(GL_FRONT,GL_EMISSION, @Color2);
    glMaterialfv(GL_FRONT,GL_SHININESS, @Matt);
    end;

    for f := Low(Models[m].Faces) to High(Models[m].Faces) do
    begin
      glBegin(GL_TRIANGLES);
        Normal := Models[m].Faces[f].Normal;
        glNormal3f(Normal.X, Normal.Y, Normal.Z);
        Face := Models[m].Faces[f];
        glColor3f(0.5,0.5,0.5);
        for j := 1 to 3 do
        begin
          Vert := Models[m].Vertices[Face.Vec3[j]-1];
          glVertex3f(Vert.X,Vert.Y,Vert.Z);
        end;
      glEnd;
    end;
    glPopMatrix();
  end;

  // TREES

  for m := 0 to 128 do
  begin
    glPushMatrix();
    side :=1.0;
    model := 12;
    if m mod 2 = 0 then
    begin
      side := -1.0;
      model := 13
    end;
    glTranslatef((30+Sin(m*128)*24.0)*side,-0.5+Sin(m*512)*0.5,32+Sin(m*432)*64.0);
    glRotate(Sin(m*128.0)*360.0, 0.0,1.0,0.0);

    glMaterialfv(GL_FRONT,GL_AMBIENT, @ColorGreen);
    glMaterialfv(GL_FRONT,GL_DIFFUSE, @ColorGreen);
    glMaterialfv(GL_FRONT,GL_SHININESS, @Matt);

    for f := Low(Models[model].Faces) to High(Models[model].Faces) do
    begin
      glBegin(GL_TRIANGLES);
        Normal := Models[model].Faces[f].Normal;
        glNormal3f(Normal.X, Normal.Y, Normal.Z);
        Face := Models[model].Faces[f];
        glColor3f(0.5,0.5,0.5);
        for j := 1 to 3 do
        begin
          Vert := Models[model].Vertices[Face.Vec3[j]-1];
          glVertex3f(Vert.X,Vert.Y,Vert.Z);
        end;
      glEnd;
    end;

    glPopMatrix();
  end;
end;

procedure TFormDemo.FormDestroy(Sender: TObject);
begin
  wglDeleteContext(RC);
  ReleaseDC(FormDemo.Handle, DC)
end;

procedure TFormDemo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  MaxSpeed: Double = 0.15;
begin
  case Key of
    Ord('W'): MovementVector.Z := -MaxSpeed;
    Ord('S'): MovementVector.Z := MaxSpeed;
    Ord('Q'): MovementVector.Y := -MaxSpeed;
    Ord('E'): MovementVector.Y := MaxSpeed;
    Ord('A'): MovementVector.X := -MaxSpeed;
    Ord('D'): MovementVector.X := MaxSpeed;
    Ord('J'): Camera.AngleY := Camera.AngleY-MaxSpeed*10.0;
    Ord('K'): Camera.AngleY := Camera.AngleY+MaxSpeed*10.09;
  end;
end;

procedure TFormDemo.FormPaint(Sender: TObject);
begin
  SwapBuffers(DC);
end;

end.
