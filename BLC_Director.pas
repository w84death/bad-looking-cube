unit BLC_Director;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids, DB, DBClient, jpeg;

type
  TFormDirector = class(TForm)
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    DBGrid1: TDBGrid;
    PanelTools: TPanel;
    GroupB: TGroupBox;
    LabelCamZ: TLabel;
    GroupCamera: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    GroupA: TGroupBox;
    DBNavigator1: TDBNavigator;
    ButtonSave: TButton;
    PanelMain: TPanel;
    ButtonRunDemo: TButton;
    ButtonClose: TButton;
    GroupApp: TGroupBox;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonRunDemoClick(Sender: TObject); 
    procedure CenterWindow();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDirector: TFormDirector;

implementation

uses BLC_Demo01;

{$R *.dfm}

procedure TFormDirector.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TFormDirector.FormCreate(Sender: TObject);
var
  CSVFile: TextFile;
  Line: String;
  Fields: TStringList;
  i: Integer;
begin
  CenterWindow;
  ClientDataSet1.CreateDataSet;
  ClientDataSet1.Open;
  AssignFile(CSVFile, 'D:\Program Files\Borland\Delphi7\Projects\BadLookingCube\screenplay.csv');
  Reset(CSVFile);

  Fields := TStringList.Create;
  try
    Fields.Delimiter := ',';
    while not Eof(CSVFile) do
    begin
      ReadLn(CSVFile, Line);
      Fields.DelimitedText := Line;
      ClientDataSet1.Append;
      for i := 0 to Fields.Count - 1 do
        ClientDataSet1.Fields[i].AsString := Fields[i];
      ClientDataSet1.Post;
    end;
  finally
    Fields.Free;
    CloseFile(CSVFile);
  end;
end;

procedure TFormDirector.ButtonSaveClick(Sender: TObject);
var
  CSVFile: TextFile;
  Line: String;
  i: Integer;
begin
  AssignFile(CSVFile, 'D:\Program Files\Borland\Delphi7\Projects\BadLookingCube\screenplay.csv');
  Rewrite(CSVFile);
  ClientDataSet1.First;
  while not ClientDataSet1.Eof do
  begin
    Line := '';
    for i := 0 to ClientDataSet1.FieldCount - 1 do
    begin
      Line := Line + ClientDataSet1.Fields[i].AsString;
      if i < ClientDataSet1.FieldCount - 1 then
        Line := Line + ',';
    end;
    WriteLn(CSVFile, Line);
    ClientDataSet1.Next;
  end;
  CloseFile(CSVFile);
end;

procedure TFormDirector.ButtonRunDemoClick(Sender: TObject);
begin
  FormCC.OpenDemoWindow;
end;

procedure TFormDirector.CenterWindow();
begin;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height - 24) div 2;;
end;

end.
