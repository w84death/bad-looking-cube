unit BLC_Renderer;

interface

uses
  Windows, OpenGL, SysUtils, Variants, Classes, Controls, Forms,
  ExtCtrls, Messages, Math, DateUtils;

type
  TColor = record
    R, G, B: GLfloat;
  end;

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
    Scale: GLfloat;
  end;

  TScreenplayLine = record
    Timestamp: GLfloat;
    Action: String;
    Name: String;
    Params: array[0..6] of glFloat;
    Linear: Boolean;
  end;

  TModel = record
    Name: String;
    Vertices: array of TVertex;
    Normals: array of TVertex;
    Faces: array of TFace;
    Position: TVertex;
    Rotation: TVertex;
    Scale: GLfloat;
    Timeline: array of TScreenplayLine;
    Clones: array of TModelClone;
    Shading: Integer;
  end;

  TCamera = record
    Position: TVertex;
    Rotation: TVertex;
    Direction:  GLfloat;
    Lens: GLfloat;
    Free: boolean;
    Timeline: array of TScreenplayLine;
  end;

  TFog = record
    Enabled: Boolean;
    Color: TColor;
    Density: GLfloat;
  end;

  TScene = record
    Skybox: TModel;
    SunPos: array[0..3] of GLfloat;
    SunColor: TColor;
    Ambient: TColor;
    Terrain: TModel;
    Camera: TCamera;
    Fog: TFog;
    Models: array of TModel;
  end;

  TFormDemo = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure Direct();
    procedure Render();
    function FindModelByName(Name: String): Integer;
    function ReadOBJFileFromResource(const ResourceName: string): TModel;
    function LoadModel(Name: string; X,Y,Z: Double; RX,RY,RZ: Double; SCALE: Double): TModel;
    procedure LoadCSVResource();
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
    function ToggleFreeCamera(): Boolean;
    private
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    public
      procedure InitDemo();
      procedure KillDemo();
      function GetFrameTime(): Double;
      function GetPolygons(): Integer;
      procedure SetDemoFullScreen();
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
  DemoRunning: Boolean = true;
  MovementVector: TVertex = (X:0.0;Y:0.0;Z:0.0);
  Scene: TScene;
  DC: HDC;
  RC: HGLRC;
  wglSwapIntervalEXT: TWglSwapIntervalEXT;
  LastFrameTime, CurrentTime, FrameTime: TDateTime;
  OldWindowStyle: Longint;
  FogColor: array[0..3] of GLfloat = (0.6,0.6,0.6,1.0);
  DeviceMode: TDevMode;
  PolyCount: Integer = 0;
  
implementation

{$R *.dfm}
{$R 'models.RES' 'models.rc'}
//{$R 'screenplay.RES' 'screenplay.rc'}

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
begin
 if DemoRunning then
  begin
    DemoTime := DemoTime + FrameTime/1000;
    if DemoTime > DemoLength then
      DemoTime := 0.0;
  end;

  Direct();
  Render();

  Invalidate;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   READ OBJ RESOURCE FILE
function TFormDemo.ReadOBJFileFromResource(const ResourceName: string): TModel;
var
  ResStream: TResourceStream;
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
  ResStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  FileLines := TStringList.Create;
  Parts := TStringList.Create;
  Parts.Delimiter := ' ';
  FaceParts := TStringList.Create;
  FaceParts.Delimiter := '/';

  try
    FileLines.LoadFromStream(ResStream);

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

  Model.Scale := 1.0;

  Model.Shading := SHADE_SMOOTH;
  Result := Model;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   DIRECT
procedure TFormDemo.Direct();
const
  _X: Integer = 0;_VAL1: Integer = 0;
  _Y: Integer = 1;_VAL2: Integer = 1;
  _Z: Integer = 2;_VAL3: Integer = 2;
  _RX: Integer = 3;_R: Integer = 3;_VAL4: Integer = 3;
  _RY: Integer = 4;_G: Integer = 4;
  _RZ: Integer = 5;_B: Integer = 5;
  _SCALE: Integer = 6;
var
  l,m: Integer;
  linear: Boolean;
  Pos: glFloat;
  Prev, Next: Integer;
function Lerp(A, B, T: glFloat; Linear: Boolean): glFloat;
var
  TD: Double;
begin
  if Linear then
  begin
    Result := A + (B - A) * T;
  end
  else
  begin

  // Cosinusoidalnie
  TD := (1-Cos(T*PI))/2;
  Result := A*(1-TD)+B*TD;
  end;
end;
function CalcPos(A,B,T: glFloat): glFloat;
begin
  Result := 0;
  if A<>B then
    Result := (T-A)/(B-A);
  Result := Max(0.0,Min(Result,1.0));
end;
begin

  // CAMERA
  for l := 1 to High(Scene.Camera.Timeline) do
  begin
    Prev := l-1;
    Next := l;
    if (DemoTime >= Scene.Camera.Timeline[Prev].Timestamp) then
    begin
      Pos := CalcPos(Scene.Camera.Timeline[Prev].Timestamp,Scene.Camera.Timeline[Next].Timestamp,DemoTime);
      if Scene.Camera.Free = false then
      begin
        linear := Scene.Camera.Timeline[Next].Linear;
        Scene.Camera.Position.X := Lerp(Scene.Camera.Timeline[Prev].Params[_X],Scene.Camera.Timeline[Next].Params[_X],Pos,linear);
        Scene.Camera.Position.Y := Lerp(Scene.Camera.Timeline[Prev].Params[_Y],Scene.Camera.Timeline[Next].Params[_Y],Pos,linear);
        Scene.Camera.Position.Z := Lerp(Scene.Camera.Timeline[Prev].Params[_Z],Scene.Camera.Timeline[Next].Params[_Z],Pos,linear);

        Scene.Camera.Rotation.X := Lerp(Scene.Camera.Timeline[Prev].Params[_RX],Scene.Camera.Timeline[Next].Params[_RX],Pos,linear);
        Scene.Camera.Rotation.Y := Lerp(Scene.Camera.Timeline[Prev].Params[_RY],Scene.Camera.Timeline[Next].Params[_RY],Pos,linear);
        Scene.Camera.Rotation.Z := Lerp(Scene.Camera.Timeline[Prev].Params[_RZ],Scene.Camera.Timeline[Next].Params[_RZ],Pos,linear);

        Scene.Camera.Lens := Lerp(Scene.Camera.Timeline[Prev].Params[_SCALE],Scene.Camera.Timeline[Next].Params[_SCALE],Pos,linear);
      end;
    end;
  end;

  // MODELS
  for m := 0 to High(Scene.Models) do
  begin

  for l := 1 to High(Scene.Models[m].Timeline) do
  begin
    Prev := l-1;
    Next := l;
    if (DemoTime >= Scene.Models[m].Timeline[Prev].Timestamp) then
    begin
      Pos := CalcPos(Scene.Models[m].Timeline[Prev].Timestamp,Scene.Models[m].Timeline[Next].Timestamp,DemoTime);

      linear := Scene.Models[m].Timeline[Next].Linear;
      Scene.Models[m].Position.X := Lerp(Scene.Models[m].Timeline[Prev].Params[_X],Scene.Models[m].Timeline[Next].Params[_X],Pos,linear);
      Scene.Models[m].Position.Y := Lerp(Scene.Models[m].Timeline[Prev].Params[_Y],Scene.Models[m].Timeline[Next].Params[_Y],Pos,linear);
      Scene.Models[m].Position.Z := Lerp(Scene.Models[m].Timeline[Prev].Params[_Z],Scene.Models[m].Timeline[Next].Params[_Z],Pos,linear);

      Scene.Models[m].Rotation.X := Lerp(Scene.Models[m].Timeline[Prev].Params[_RX],Scene.Models[m].Timeline[Next].Params[_RX],Pos,linear);
      Scene.Models[m].Rotation.Y := Lerp(Scene.Models[m].Timeline[Prev].Params[_RY],Scene.Models[m].Timeline[Next].Params[_RY],Pos,linear);
      Scene.Models[m].Rotation.Z := Lerp(Scene.Models[m].Timeline[Prev].Params[_RZ],Scene.Models[m].Timeline[Next].Params[_RZ],Pos,linear);

      Scene.Models[m].Scale := Lerp(Scene.Models[m].Timeline[Prev].Params[_SCALE],Scene.Models[m].Timeline[Next].Params[_SCALE],Pos,linear);

    end;
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

procedure  RenderModel(Model: TModel);
var
  f,v: Integer;
begin
  glPushMatrix();

    glTranslatef(Model.Position.X, Model.Position.Y, Model.Position.Z);
    glScalef(Model.Scale,Model.Scale,Model.Scale);
    glRotatef(Model.Rotation.X, 1.0,0.0,0.0);
    glRotatef(Model.Rotation.Y, 0.0,1.0,0.0);
    glRotatef(-Model.Rotation.Z, 0.0,0.0,1.0);

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
        PolyCount := PolyCount + 1;
      glEnd;
    end;
  glPopMatrix();
end;

begin
  CurrentTime := Now;
  FrameTime := MilliSecondsBetween(CurrentTime, LastFrameTime);
  LastFrameTime := CurrentTime;

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  gluPerspective(Scene.Camera.Lens,ClientWidth/ClientHeight, CameraNear, CameraFar);

  // WORLD /SKYBOX
  glDisable(GL_LIGHTING);
  glDepthMask(GL_FALSE);
  glDisable(GL_FOG);
  RenderModel(Scene.Skybox);
  glDepthMask(GL_TRUE);
  //glEnable(GL_LIGHTING);

  if Scene.Fog.Enabled then
    glEnable(GL_FOG);


  // CAMERA
  glRotatef(90-Scene.Camera.Rotation.X, 1.0,0.0,0.0);
  glRotatef(-Scene.Camera.Rotation.Y, 0.0,1.0,0.0);
  glRotatef(-Scene.Camera.Rotation.Z, 0.0,0.0,1.0);
  glTranslatef(-Scene.Camera.Position.X, -Scene.Camera.Position.Y, -Scene.Camera.Position.Z);

  glLightfv(GL_LIGHT0, GL_POSITION, @Scene.SunPos);

  // MODELS
  PolyCount := 0;

  // TERRAIN
  RenderModel(Scene.Terrain);

  for m := Low(Scene.Models) to High(Scene.Models) do
  begin
    Model := Scene.Models[m];
    RenderModel(Model);
    for c := Low(Model.Clones) to High(Model.Clones) do
    begin
      Model.Position := Model.Clones[c].Position;
      Model.Rotation := Model.Clones[c].Rotation;
      Model.Scale := Model.Clones[c].Scale;
      RenderModel(Model);
    end
  end;
end;

function TFormDemo.ToggleFreeCamera(): Boolean;
begin
  Scene.Camera.Free := not Scene.Camera.Free;
  Result := Scene.Camera.Free;
end;

procedure TFormDemo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Application.Terminate;
  end;
end;

procedure TFormDemo.FormPaint(Sender: TObject);
begin
  SwapBuffers(DC);
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   LOAD MODEL
function TFormDemo.LoadModel(Name: string; X,Y,Z: Double; RX,RY,RZ: Double; SCALE: Double): TModel;
var
  Model: TModel;
begin
  Model := ReadOBJFileFromResource(Name);
  Model.Name := Name;
  Model.Position.X := X;
  Model.Position.Y := Y;
  Model.Position.Z := Z;
  Model.Rotation.X := RX;
  Model.Rotation.Y := RY;
  Model.Rotation.Z := RZ;
  Model.Scale := SCALE;
  Result := Model;
end;

function TFormDemo.FindModelByName(Name: String): Integer;
var
  i: Integer;
  modelID: Integer;
begin
  modelID := -1;
  for i := 0 to High(Scene.Models) do
  begin
    if Scene.Models[i].Name = Name then
    begin
      modelID := i;
      break;
    end;
  end;
  
  Result := modelID;
end;

//    ---   ---   ---   ---   ---   ---   ---   ---   LOAD CSV RESOURCE/FILE
procedure TFormDemo.LoadCSVResource();
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
  _SCALE: Integer = 9;
  SCREENPLAY_FROM_RES: Boolean = true;
var
  //ResStream: TResourceStream;
  //CSVContent: TStringList;
  CSVFile: TextFile;
  Line: String;
  Fields: TStringList;
  Clone: TModelClone;
  LastModel: Integer;
  ScreenplayLine: TScreenplayLine;
  f: Integer;
  ID: Integer;
begin
  //ResStream := TResourceStream.Create(HInstance, 'screenplay', RT_RCDATA);
  //CSVContent := TStringList.Create;
  //CSVContent.LoadFromStream(ResStream);

  AssignFile(CSVFile, 'screenplay.csv');
  Reset(CSVFile);

  Fields := TStringList.Create;
  Fields.Delimiter := ',';

  SetLength(Scene.Models, 0);
  SetLength(Scene.Camera.Timeline, 0);

  //    ---   ---   ---   ---   ---   ---   ---   ---   DECODE CSV DATA
  try
    //for l := 0 to CSVContent.Count - 1 do
    while not Eof(CSVFile) do
    begin

      //Line := CSVContent[l];
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
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]),
            StrToFloat(Fields[_SCALE]));
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
          Clone.Scale := StrToFloat(Fields[_SCALE]);

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
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]),
            StrToFloat(Fields[_SCALE]));
        end;
        if Fields[_ACTION] = 'sun' then
        begin
          Scene.SunPos[0] := StrToFloat(Fields[_X]);
          Scene.SunPos[1] := StrToFloat(Fields[_Y]);
          Scene.SunPos[2] := StrToFloat(Fields[_Z]);
          Scene.SunPos[3] := 0.0;
          Scene.SunColor.R := StrToFloat(Fields[_R]);
          Scene.SunColor.G := StrToFloat(Fields[_G]);
          Scene.SunColor.B := StrToFloat(Fields[_B]);
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
            StrToFloat(Fields[_RX]), StrToFloat(Fields[_RY]), StrToFloat(Fields[_RZ]),
            StrToFloat(Fields[_SCALE]));
        end;
      end;
   // ----------------- ------------------ ------------------ RUNTIME
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

        if (Fields[_ACTION] = 'pos') or (Fields[_ACTION] = 'pos_') then
        begin
          for f := 0 to 6 do
          begin
            ScreenplayLine.Params[f] := StrToFloat(Fields[_X+f]);
          end;

          if Fields[_PROP] = 'Camera' then
          begin
            if Fields[_ACTION] = 'pos_' then
              ScreenplayLine.Linear := true;
            SetLength(Scene.Camera.Timeline, Length(Scene.Camera.Timeline) + 1);
            Scene.Camera.Timeline[High(Scene.Camera.Timeline)] := ScreenplayLine;
          end
          else
          begin
            ScreenplayLine.Linear := false;
            if Fields[_ACTION] = 'pos_' then
              ScreenplayLine.Linear := true;
            ID := FindModelByName(Fields[_NAME]);
            if ID>=0 then
            begin
              SetLength(Scene.Models[ID].Timeline, Length(Scene.Models[ID].Timeline) + 1);
              Scene.Models[ID].Timeline[High(Scene.Models[ID].Timeline)] := ScreenplayLine;
            end;
          end;
        end;
      end;
    end;
  finally
    //ResStream.Free;
    //CSVContent.Free;
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
  LoadCSVResource();

  DC := GetDC(Handle);
  FillChar(PixelFormat, SizeOf(PixelFormat), 0);

  with PixelFormat do
    begin
      nSize := SizeOf(PixelFormat);
      nVersion := 1;
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      iPixelType := PFD_TYPE_RGBA;
      cColorBits := 8;
      cDepthBits := 32;
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
  glLightf(GL_LIGHT0, GL_CONSTANT_ATTENUATION, 1.0);
  glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.0);
  glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @Scene.Ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @Scene.SunColor);
  glLightfv(GL_LIGHT0, GL_POSITION, @Scene.SunPos);

  
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
  
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  OldWindowStyle := GetWindowLong(Handle, GWL_STYLE);
end;


function TFormDemo.GetFrameTime(): Double;
begin
  Result := FrameTime;
end;

function TFormDemo.GetPolygons(): Integer;
begin
  Result := PolyCount;
end;

procedure TFormDemo.SetDemoFullScreen();
begin
  BorderStyle := bsNone;
  SetFullScreen(320,200);
  glViewport(0,0,320,200);
end;
end.
