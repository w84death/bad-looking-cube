unit BLC_Renderer;

interface

uses
  Windows, OpenGL, SysUtils, Variants, Classes, Controls, Forms,
  ExtCtrls,Messages, Math, DateUtils;

type
  TColor = record
    R, G, B: GLfloat;
  end;

  TMatrix4 = array[0..3, 0..3] of GLfloat;

  TVector3 = record
    X, Y, Z: GLfloat;
  end;

  TVertex = record
    X, Y, Z: GLfloat;
    Color: TColor;
  end;

  TFace = record
    Vertices: array[1..3] of Integer;
    Normals: array[1..3] of Integer;
  end;

  TModelClone = record
    Position: TVertex;
    Rotation: TVertex;
  end;

  TModel = record
    Vertices: array of TVertex;
    Normals: array of TVertex;
    Faces: array of TFace;
    Position: TVertex;
    Rotation: TVertex;
    Clones: array of TModelClone;
    Shading: Integer;
  end;

  TCamera = record
    X, Y, Z: GLfloat;
    Direction:  GLfloat;
    Lens: GLfloat;
  end;

  TFog = record
    Enabled: Boolean;
    Color: TColor;
    Density: GLfloat;
  end;

  TScene = record
    Skybox: TModel;
    Sun: TVertex;
    Ambient: TColor;
    Terrain: TModel;
    Camera: TCamera;
    Fog: TFog;
    Models: array of TModel;
  end;

  TScreenplayLine = record
    Timestamp: GLfloat;
    Action: String;
    Name: String;
    Params: array[0..5] of glFloat;
  end;

  TFormDemo = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure Direct();
    procedure Render();
    function ReadOBJFile(const FileName: string): TModel;
    procedure LoadCSVFile();
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetFullScreen(DisplayWidth, DisplayHeight: Integer);
    procedure ResizeViewport();
    procedure FormResize(Sender: TObject);
    procedure CenterWindow();
    procedure FormCreate(Sender: TObject);
    private
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    public
      procedure InitDemo();
      procedure KillDemo();
   end;

  TWglSwapIntervalEXT = procedure(interval: GLInt); stdcall;

const
  CameraNear: Double = 0.25;
  CameraFar: Double = 400;
  SHADE_FLAT = 0;
  SHADE_SMOOTH = 1;
var
  FormDemo: TFormDemo;
  DemoTime: Double = 0.0;
  DemoSpeed: Double = 1.0;
  DemoLength: Double = 120.0;
  Screenplay: array of TScreenplayLine;
  ActiveLine: Integer = 0;
  DemoRunning: Boolean = false;
  MovementVector: TVertex = (X:0.0;Y:0.0;Z:0.0);
  Scene: TScene;
  DC: HDC;
  RC: HGLRC;
  wglSwapIntervalEXT: TWglSwapIntervalEXT;
  LastFrameTime, CurrentTime, FrameTime: TDateTime;
  OldWindowStyle: Longint;
  FogColor: array[0..3] of GLfloat = (0.6,0.6,0.6,1.0);
  DeviceMode: TDevMode;
implementation

{$R *.dfm}


//    ---   ---   ---   ---   ---   ---   ---   ---   KILL FLICKERING
procedure TFormDemo.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   VSYNC (GL)
procedure InitVSync;
begin
  if not Assigned(wglSwapIntervalEXT) then
  begin
    wglSwapIntervalEXT := wglGetProcAddress('wglSwapIntervalEXT');
    if not Assigned(wglSwapIntervalEXT) then Exit;
  end;
  wglSwapIntervalEXT(1);
end;

 
//    ---   ---   ---   ---   ---   ---   ---   ---   TIMER

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
  Scene.Camera.X := Scene.Camera.X + MovementVector.X;

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
  Scene.Camera.Y := Scene.Camera.Y + MovementVector.Y;

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
  Scene.Camera.Z := Scene.Camera.Z + MovementVector.Z;

  //    ---   ---   ---   ---   ---   ---   ---   ---   NEW FAME


  if DemoRunning then
  begin
    //Scene.Models[High(Scene.Models)].Rotation.Z := 360 * Sin(DemoTime);
    //Scene.Models[High(Scene.Models)].Rotation.X := 90 + 10 * Sin(DemoTime);
    DemoTime := DemoTime + FrameTime/1000;
    if DemoTime > DemoLength then
      DemoTime := 0.0;
  end;

  Direct();
  Render();

  Invalidate;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   READ OBJ FILE
function TFormDemo.ReadOBJFile(const FileName: string): TModel;
var
  FileLines: TStringList;
  i,f: Integer;
  Line: string;
  Parts: TStringList;
  FaceParts: TStringList;
  Vertex: TVertex;
  Normal: TVertex;
  Face: TFace;
  Model: TModel;
begin
  FileLines := TStringList.Create;
  Parts := TStringList.Create;
  Parts.Delimiter := ' ';
  FaceParts := TStringList.Create;
  FaceParts.Delimiter := '/';

  try
    FileLines.LoadFromFile(FileName);

    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      Parts.DelimitedText := Line;

      if (Parts.Count = 7) and (Parts[0] = 'v') then
      begin
        Vertex.X := StrToFloat(Parts[1]);
        Vertex.Y := StrToFloat(Parts[2]);
        Vertex.Z := StrToFloat(Parts[3]);
        Vertex.Color.R := StrToFloat(Parts[4]);
        Vertex.Color.G := StrToFloat(Parts[5]);
        Vertex.Color.B := StrToFloat(Parts[6]);

        SetLength(Model.Vertices, Length(Model.Vertices) + 1);
        Model.Vertices[High(Model.Vertices)] := Vertex;
      end;

      if (Parts.Count = 4) and (Parts[0] = 'vn') then
      begin
        Normal.X := StrToFloat(Parts[1]);
        Normal.Y := StrToFloat(Parts[2]);
        Normal.Z := StrToFloat(Parts[3]);

        SetLength(Model.Normals, Length(Model.Normals) + 1);
        Model.Normals[High(Model.Normals)] := Normal;
      end;

      if (Parts.Count = 4) and (Parts[0] = 'f') then
      begin
        for f := 1 to 3 do
        begin
          FaceParts.DelimitedText := StringReplace(Parts[f],'//','/',[rfReplaceAll]);
          Face.Vertices[f] := StrToInt(FaceParts[0]);
          Face.Normals[f] := StrToInt(FaceParts[1]);
        end;

        SetLength(Model.Faces, Length(Model.Faces) + 1);
        Model.Faces[High(Model.Faces)] := Face;
      end;
    end;
  finally
    Parts.Free;
    FaceParts.Free;
    FileLines.Free;
  end;

  Model.Position.X := 0.0;
  Model.Position.Y := 0.0;
  Model.Position.Z := 0.0;

  Model.Rotation.X := 0.0;
  Model.Rotation.Y := 0.0;
  Model.Rotation.Z := 0.0;

  Model.Shading := SHADE_SMOOTH;
  Result := Model;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   DIRECT
procedure TFormDemo.Direct();
const
  _X: Integer = 0;_VAL1: Integer = 0;
  _Y: Integer = 1;_VAL2: Integer = 1;
  _Z: Integer = 2;_VAL3: Integer = 2;
  _RX: Integer = 3;_R: Integer = 6;_VAL4: Integer = 6;
  _RY: Integer = 4;_G: Integer = 4;
  _RZ: Integer = 5;_B: Integer = 5;
var
  l: Integer;
  Pos: glFloat;
  Prev, Next: Integer;
function Lerp(A, B, T: glFloat): glFloat;
begin
  Result := A + (B - A) * T;
end;
function CalcPos(A,B,T: glFloat): glFloat;
begin
  Result := 0;
  if A<>B then
    Result := (T-A)/(B-A);
  Result := Max(0.0,Min(Result,1.0));
end;
begin
  Prev := 0;
  for l := 1 to High(Screenplay) do
  begin
    if Screenplay[l].Action = 'camera' then
      Prev := l;
    Next := l+1;
    if (DemoTime >= Screenplay[Prev].Timestamp) and
      (Screenplay[Prev].Action = 'camera') and
      (Screenplay[Prev].Name = 'pos') then
    begin
      Pos := CalcPos(Screenplay[Prev].Timestamp,Screenplay[Next].Timestamp,DemoTime);

      Scene.Camera.X := Lerp(Screenplay[Prev].Params[_X],Screenplay[Next].Params[_X],Pos);
      Scene.Camera.Y := Lerp(Screenplay[Prev].Params[_Y],Screenplay[Next].Params[_Y],Pos);
      Scene.Camera.Z := Lerp(Screenplay[Prev].Params[_Z],Screenplay[Next].Params[_Z],Pos);
      Scene.Camera.Direction := Lerp(Screenplay[Prev].Params[_RY],Screenplay[Next].Params[_RY],Pos);
    end;
  end;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   RENDER

procedure TFormDemo.Render();
var
  m,c: Integer;
  Vert: TVertex;
  Normal: TVertex;
  Face: TFace;
  Model: TModel;
  CameraTarget: TVector3;
function CalculateLookAtTarget(Direction: GLfloat): TVector3;
const
  DegToRad = Pi / 180.0;
var
  RadAngle: GLfloat;
begin
  RadAngle := -Direction * DegToRad;
  Result.X := Sin(RadAngle);
  Result.Y := 0;
  Result.Z := Cos(RadAngle);
end;
procedure  RenderModel(Model: TModel);
var
  f,v: Integer;
begin
  glPushMatrix();
    glTranslatef(Model.Position.X, Model.Position.Y, Model.Position.Z);
    glRotatef(Model.Rotation.X, 1.0,0.0,0.0);
    glRotatef(Model.Rotation.Y, 0.0,1.0,0.0);
    glRotatef(Model.Rotation.Z, 0.0,0.0,1.0);

    glShadeModel(GL_SMOOTH);
    if Model.Shading = SHADE_FLAT then
      glShadeModel(GL_FLAT);

    for f := Low(Model.Faces) to High(Model.Faces) do
    begin
      glBegin(GL_TRIANGLES);
        Face := Model.Faces[f];
        for v := 1 to 3 do
        begin
          Vert := Model.Vertices[Face.Vertices[v]-1];
          Normal := Model.Normals[Face.Normals[v]-1];
          glColor3f(Vert.Color.R,Vert.Color.G,Vert.Color.B);
          glNormal3f(Normal.X,Normal.Y,Normal.Z);
          glVertex3f(Vert.X,Vert.Y,Vert.Z);
        end;
      glEnd;
    end;
    glPopMatrix();
end;

begin
  CurrentTime := Now;
  FrameTime := MilliSecondsBetween(CurrentTime, LastFrameTime);
  LastFrameTime := CurrentTime;
  Caption := Format('FrameTime: %.2fms', [FrameTime]);

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  CameraTarget := CalculateLookAtTarget(Scene.Camera.Direction);
  gluLookAt(Scene.Camera.X, Scene.Camera.Y, Scene.Camera.Z,
            Scene.Camera.X-CameraTarget.X, Scene.Camera.Y, Scene.Camera.Z-CameraTarget.Z,
            0, 1, 0);

  glDisable(GL_LIGHTING);
  glDepthMask(GL_FALSE);
  glDisable(GL_FOG);
  RenderModel(Scene.Skybox);
  glDepthMask(GL_TRUE);
  glEnable(GL_LIGHTING);

  if Scene.Fog.Enabled then
    glEnable(GL_FOG);

  glLightfv(GL_LIGHT0, GL_POSITION, @Scene.Sun);

  RenderModel(Scene.Terrain);
  for m := Low(Scene.Models) to High(Scene.Models) do
  begin
    Model := Scene.Models[m];
    RenderModel(Model);
    for c := Low(Model.Clones) to High(Model.Clones) do
    begin
      Model.Position := Model.Clones[c].Position;
      Model.Rotation := Model.Clones[c].Rotation;
      RenderModel(Model);
    end
  end;
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
    VK_F11:
     begin
    if WindowState = wsNormal then
    begin
      //SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
      //WindowState := wsMaximized;
      //SetBounds(0, 0, Screen.Width, Screen.Height);
      SetFullScreen(320,200);
      glViewport(0, 20,320,160);
    end
    else
    begin
      //SetWindowLong(Handle, GWL_STYLE, OldWindowStyle);
      //WindowState := wsNormal;
      //Width := 640;
      //Height := 400;
      //CenterWindow; 
      ShowWindow(Application.Handle, SW_RESTORE);
    end;
  end;
  end;
end;

procedure TFormDemo.FormPaint(Sender: TObject);
begin
  SwapBuffers(DC);
end;


//    ---   ---   ---   ---   ---   ---   ---   ---   LOAD CSV FILE

procedure TFormDemo.LoadCSVFile();
const
  _TIMESTAMP: Integer = 0;
  _ACTION: Integer = 1;
  _PROP: Integer = 2;_NAME: Integer = 2;
  _X: Integer = 3;_VAL1: Integer = 3;
  _Y: Integer = 4;_VAL2: Integer = 4;
  _Z: Integer = 5;_VAL3: Integer = 5;
  _RX: Integer = 6;_R: Integer = 6;_VAL4: Integer = 6;
  _RY: Integer = 7;_G: Integer = 7;
  _RZ: Integer = 8;_B: Integer = 8;
var
  CSVFile: TextFile;
  Line: String;
  Fields: TStringList;
  Clone: TModelClone;
  LastModel: Integer;
  ScreenplayLine: TScreenplayLine;
  f: Integer;

//    ---   ---   ---   ---   ---   ---   ---   ---   LOAD MODEL
function LoadModel(Name: string; X,Y,Z: Double; RX,RY,RZ: Double): TModel;
var
  Model: TModel;
begin
  Model := ReadOBJFile('Models/'+Name+'.obj');
  Model.Position.X := X;
  Model.Position.Y := Y;
  Model.Position.Z := Z;
  Model.Rotation.X := RX;
  Model.Rotation.Y := RY;
  Model.Rotation.Z := RZ;
  Result := Model;
end;
//    ---   ---   ---   ---   ---   ---   ---   ---   DECODE CSV DATA
begin
  AssignFile(CSVFile, 'D:\Program Files\Borland\Delphi7\Projects\BadLookingCube\screenplay.csv');
  Reset(CSVFile);
  Fields := TStringList.Create;
  Fields.Delimiter := ',';
  try
    while not Eof(CSVFile) do
    begin
      ReadLn(CSVFile, Line);
      Fields.DelimitedText := Line;

      if StrToFloat(Fields[_TIMESTAMP]) < 0 then
      begin
        if Fields[_ACTION] = 'load' then
        begin
          SetLength(Scene.Models, Length(Scene.Models) + 1);
          Scene.Models[High(Scene.Models)] := LoadModel(
            Fields[_NAME],
            StrToFloat(Fields[_X]), StrToFloat(Fields[_Y]), StrToFloat(Fields[_Z]),
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]));
        end;
        if Fields[_ACTION] = 'clone' then
        begin
          LastModel := High(Scene.Models);
          Clone.Position.X := StrToFloat(Fields[_X]);
          Clone.Position.Y := StrToFloat(Fields[_Y]);
          Clone.Position.Z := StrToFloat(Fields[_Z]);
          Clone.Rotation.X := StrToFloat(Fields[_RX]);
          Clone.Rotation.Y := StrToFloat(Fields[_RY]);
          Clone.Rotation.Z := StrToFloat(Fields[_RZ]);

          SetLength(Scene.Models[LastModel].Clones, Length(Scene.Models[LastModel].Clones) + 1);
          Scene.Models[LastModel].Clones[High(Scene.Models[LastModel].Clones)] := Clone;
        end;
        if Fields[_ACTION] = 'shade' then
        begin
          LastModel := High(Scene.Models);
          if Fields[_PROP] = 'smooth' then
            Scene.Models[LastModel].Shading := SHADE_SMOOTH;
          if Fields[_PROP] = 'flat' then
            Scene.Models[LastModel].Shading := SHADE_FLAT;
        end;
        if Fields[_ACTION] = 'camera' then
        begin
           if Fields[_PROP] = 'pos' then
           begin
            Scene.Camera.X := StrToFloat(Fields[_X]);
            Scene.Camera.Y := StrToFloat(Fields[_Y]);
            Scene.Camera.Z := StrToFloat(Fields[_Z]);
            Scene.Camera.Direction := StrToFloat(Fields[_RY]);
           end;
           if Fields[_PROP] = 'lens' then
            Scene.Camera.Lens := StrToFloat(Fields[_VAL1]);
        end;
        if Fields[_ACTION] = 'fog' then
        begin
          if Fields[_PROP] = 'enable' then
          begin
            Scene.Fog.Enabled := true;
            Scene.Fog.Density := StrToFloat(Fields[_VAL1]);
            Scene.Fog.Color.R := StrToFloat(Fields[_R]);
            Scene.Fog.Color.G := StrToFloat(Fields[_G]);
            Scene.Fog.Color.B := StrToFloat(Fields[_B]);
          end;
          if Fields[_PROP] = 'disable' then
            Scene.Fog.Enabled := false;
        end;
        if Fields[_ACTION] = 'terrain' then
        begin
          Scene.Terrain := LoadModel(
            Fields[_NAME],
            StrToFloat(Fields[_X]), StrToFloat(Fields[_Y]), StrToFloat(Fields[_Z]),
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]));
        end;
        if Fields[_ACTION] = 'sun' then
        begin
          Scene.Sun.X := StrToFloat(Fields[_X]);
          Scene.Sun.Y := StrToFloat(Fields[_Y]);
          Scene.Sun.Z := StrToFloat(Fields[_Z]);
          Scene.Sun.Color.R := StrToFloat(Fields[_R]);
          Scene.Sun.Color.G := StrToFloat(Fields[_G]);
          Scene.Sun.Color.B := StrToFloat(Fields[_B]);
        end;
        if Fields[_ACTION] = 'ambient' then
        begin
          Scene.Ambient.R := StrToFloat(Fields[_R]);
          Scene.Ambient.G := StrToFloat(Fields[_G]);
          Scene.Ambient.B := StrToFloat(Fields[_B]);
        end;
        if Fields[_ACTION] = 'skybox' then
        begin
          Scene.Skybox := LoadModel(
            Fields[_NAME],
            StrToFloat(Fields[_X]), StrToFloat(Fields[_Y]), StrToFloat(Fields[_Z]),
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]));
        end;
      end;

      if StrToFloat(Fields[_TIMESTAMP]) >= 0 then
      begin
        ScreenplayLine.Timestamp :=  StrToFloat(Fields[_TIMESTAMP]);
        ScreenplayLine.Action :=  Fields[_ACTION];
        ScreenplayLine.Name :=  Fields[_NAME];

        if Fields[_ACTION] = 'start' then
        begin
          Break;
        end;

        if Fields[_ACTION] = 'end' then
        begin
          DemoLength := StrToFloat(Fields[_TIMESTAMP]);
          Break;
        end;

        for f := 0 to 5 do
        begin
          ScreenplayLine.Params[f] := StrToFloat(Fields[_X+f]);
        end;
        SetLength(Screenplay, Length(Screenplay) + 1);
        Screenplay[High(Screenplay)] := ScreenplayLine;
      end;
    end;
  finally
    Fields.Free;
    CloseFile(CSVFile);
  end;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   INIT DEMO
procedure TFormDemo.InitDemo();
var
  PixelFormat: TPixelFormatDescriptor;
  FormatIndex: integer;
begin
  LoadCSVFile();

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
  ResizeViewport;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(Scene.Camera.Lens,ClientWidth/ClientHeight, CameraNear, CameraFar);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);

  if Scene.Fog.Enabled then
  begin
    glFogi(GL_FOG_MODE, GL_EXP2);
    glFogfv(GL_FOG_COLOR, @Scene.Fog.Color);
    glFogf(GL_FOG_DENSITY, Scene.Fog.Density);
    glHint(GL_FOG_HINT, GL_DONT_CARE);
  end;

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT_MODEL_AMBIENT);

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @Scene.Ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @Scene.Sun.Color);
  glLightfv(GL_LIGHT0, GL_POSITION, @Scene.Sun);
  LastFrameTime := Now;
end;


procedure TFormDemo.SetFullScreen(DisplayWidth, DisplayHeight: Integer);
begin
  with DeviceMode do
  begin
    dmSize := SizeOf(TDevMode);
    dmPelsWidth := DisplayWidth;
    dmPelsHeight := DisplayHeight;
    dmBitsPerPel := 32;
    dmFields :=DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
  end;
  ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN);
  ShowWindow(Handle, SW_MAXIMIZE);
  SetWindowPos(Handle,
    HWND_TOPMOST, 0, 0,
    DisplayWidth,DisplayHeight,
    SWP_SHOWWINDOW);
end;

procedure TFormDemo.KillDemo();
begin
  DemoTime := 0.0;
  DemoRunning := false;
  SetLength(Scene.Models,0);
  wglDeleteContext(RC);
  ReleaseDC(FormDemo.Handle, DC);
end;

procedure TFormDemo.FormShow(Sender: TObject);
begin
  if FormDemo.Visible then
    KillDemo();
  InitDemo;
end;

procedure TFormDemo.FormDestroy(Sender: TObject);
begin
  KillDemo();
end;

procedure TFormDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  KillDemo();
end;

procedure TFormDemo.ResizeViewport();
const
  DesiredAspectRatio: GLfloat = 320.0 / 200.0;
var
  AspectRatio, ViewportWidth, ViewportHeight: GLfloat;
  ViewportX, ViewportY: Integer;
begin
  AspectRatio := Width / Height;

  if AspectRatio > DesiredAspectRatio then
  begin
    ViewportHeight := Height;
    ViewportWidth := Height * DesiredAspectRatio;
    ViewportX := Round((Width - ViewportWidth) / 2);
    ViewportY := 0;
  end
  else
  begin
    ViewportWidth := Width;
    ViewportHeight := Width / DesiredAspectRatio;
    ViewportX := 0;
    ViewportY := Round((Height - ViewportHeight) / 2);
  end;

  glViewport(ViewportX, ViewportY, Round(ViewportWidth), Round(ViewportHeight));
end;

procedure TFormDemo.FormResize(Sender: TObject);
begin
  ResizeViewport;
end;

procedure TFormDemo.CenterWindow();
begin;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  CenterWindow;
  OldWindowStyle := GetWindowLong(Handle, GWL_STYLE);
end;

end.
