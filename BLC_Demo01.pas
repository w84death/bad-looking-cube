unit BLC_Demo01;

interface

uses
  Windows, SysUtils, Variants, Classes, Controls, Graphics, Forms,
  Dialogs, StdCtrls, jpeg, ComCtrls, ExtCtrls;

type

  TFormCC = class(TForm)
    ButtonTerminate: TButton;
    ButtonRunDemo: TButton;
    ButtonPause: TButton;
    ButtonSeekForward: TButton;
    ButtonEnd: TButton;
    ButtonSeekBack: TButton;
    ButtonBegin: TButton;
    GroupPlayback: TGroupBox;
    GroupTimer: TGroupBox;
    LabelTimeStamp: TLabel;
    ButtonCloseDermo: TButton;
    TimerDemo: TTimer;
    GroupDemo: TGroupBox;
    GroupApp: TGroupBox;
    ButtonAbout: TButton;
    TrackBarDemoTime: TTrackBar;
    ButtonShowDirector: TButton;
    procedure ButtonTerminateClick(Sender: TObject);
    procedure ButtonRunDemoClick(Sender: TObject);
    procedure ButtonCloseDermoClick(Sender: TObject);
    procedure RefreshTimer();
    procedure TimerDemoTimer(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonBeginClick(Sender: TObject);
    procedure ButtonEndClick(Sender: TObject);
    procedure ButtonSeekForwardClick(Sender: TObject);
    procedure ButtonSeekBackClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure TrackBarDemoTimeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonShowDirectorClick(Sender: TObject);
    procedure CenterWindow();
  public
    procedure OpenDemoWindow();
    function ToggleFreeCamera(): Boolean;
  end;

const
  SeekTime: Double = 2.0;
var
  FormCC: TFormCC;
  ActiveLine: Double;
  CamSpeed: Double = 1.0;
implementation

uses BLC_Renderer, BLC_About, Math, BLC_Director;

{$R *.dfm}

procedure TFormCC.ButtonTerminateClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormCC.OpenDemoWindow();
begin
  FormDemo.Close;
  FormDemo.Show;
  FormDemo.InitDemo;
  GroupPlayback.Visible := true;
  DemoTime := 0.0;
  TimerDemo.Enabled := true;
  BLC_Renderer.DemoRunning := true;
  RefreshTimer;
end;

function TFormCC.ToggleFreeCamera(): Boolean;
begin
  Result := FormDemo.ToggleFreeCamera();
end;

procedure TFormCC.ButtonRunDemoClick(Sender: TObject);
begin
  OpenDemoWindow;
end;

procedure TFormCC.ButtonCloseDermoClick(Sender: TObject);
begin
  FormDemo.Close;
  GroupPlayback.Visible := false;
  DemoTime := 0.0;
  TimerDemo.Enabled := false;
  BLC_Renderer.DemoRunning := false;
  RefreshTimer;
end;

procedure TFormCC.RefreshTimer();
var
  time : String;
begin
  time := Format('%.2f', [DemoTime]);
  LabelTimeStamp.Caption := time;
  BLC_Renderer.DemoTime := DemoTime;
  TrackBarDemoTime.Position := Round(DemoTime*100);

  if TimerDemo.Enabled  then
  begin
    ButtonPause.Caption := 'PAUSE';
    LabelTimeStamp.Font.Color := clYellow;
  end
  else
  begin
    ButtonPause.Caption := 'PLAY';
    LabelTimeStamp.Font.Color := clMedGray;
  end;

  if TrackBarDemoTime.Max <> DemoLength then
    TrackBarDemoTime.Max := Round(DemoLength*100);
end;

procedure TFormCC.TimerDemoTimer(Sender: TObject);
begin
  RefreshTimer();
end;



procedure TFormCC.ButtonPauseClick(Sender: TObject);
begin
  if TimerDemo.Enabled  then
  begin
    TimerDemo.Enabled := false;
    DemoRunning := false;
    FormDemo.MediaPlayer1.Pause;
  end
  else
  begin
    TimerDemo.Enabled := true;
    DemoRunning := true;
    FormDemo.MediaPlayer1.Resume;
  end;
  RefreshTimer;
end;

procedure TFormCC.ButtonBeginClick(Sender: TObject);
begin
  DemoTime := 0.0;
  RefreshTimer;
end;

procedure TFormCC.ButtonEndClick(Sender: TObject);
begin
  DemoTime := DemoLength;
  RefreshTimer;
end;

procedure TFormCC.ButtonSeekForwardClick(Sender: TObject);
begin
  if DemoTime +  SeekTime <= DemoLength then
  begin
    DemoTime := DemoTime + SeekTime;
  end
  else
  begin
    DemoTime := DemoLength;
  end;
  RefreshTimer;
end;

procedure TFormCC.ButtonSeekBackClick(Sender: TObject);
begin
  if DemoTime - SeekTime >= 0.0 then
  begin
    DemoTime := DemoTime - SeekTime;
  end
  else
  begin
    DemoTime := 0.0;
  end;
  RefreshTimer;
end;

procedure TFormCC.ButtonAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormCC.TrackBarDemoTimeChange(Sender: TObject);
begin
     DemoTime := TrackBarDemoTime.Position/100;
     RefreshTimer;
end;

procedure TFormCC.FormCreate(Sender: TObject);
begin
  CenterWindow;
  TrackBarDemoTime.Max := Round(DemoLength*100);
end;

procedure TFormCC.ButtonShowDirectorClick(Sender: TObject);
begin
  FormDirector.Show;
end;

procedure TFormCC.CenterWindow();
begin;
  Left := (Screen.Width - Width) div 2;
  Top := 8;
end;

end.
