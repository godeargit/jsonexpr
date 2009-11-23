unit UJEDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, ExtCtrls, StdCtrls, Buttons, UJSONExpr;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Splitter1: TSplitter;
    Panel2: TPanel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Panel3: TPanel;
    valEditor: TValueListEditor;
    VarMemo: TMemo;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VarMemoChange(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    { Private declarations }
  public
    Parser:TJSONExprParser;
    VHelper:TSimpleVarHelper;
    procedure AfterEval;
    procedure VarHelperToValList;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{$D+}

uses
  uJSON;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Parser:=TJSONExprParser.Create;
  VHelper:=TSimpleVarHelper.Create;
  Parser.AddVarHelper(VHelper);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Parser.Free;
  VHelper.Free;
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
var
  J:JSONObject;
begin
  J:=TJSONExprParser.ExprToJSON(Memo1.Text);
  if J=nil then
    Memo2.Text:=''
  else begin
    Memo2.Text:=StringReplace(J.ToString2(2),#10,#13#10,[rfReplaceAll]);
    J.Free;
  end;
end;

procedure TfrmMain.BitBtn2Click(Sender: TObject);
var
  J:JSONObject;
begin
  try
    J:=JSONObject.Create(Memo2.Text);
  except
    Memo2.SetFocus;
    MessageDlg('Invalid JSON !', mtError, [mbOK], 0);
    exit;
  end;
  Memo1.Text:=TJSONExprParser.JSONToExpr(J,0);  
end;

procedure TfrmMain.BitBtn3Click(Sender: TObject);
var
  J:JSONObject;
  v:Variant;
begin
  try
    J:=JSONObject.Create(Memo2.Text);
  except
    Memo2.SetFocus;
    Memo2.SelectAll;
    MessageDlg('Invalid JSON text!', mtError, [mbOK], 0);
    exit;
  end;
  if J=nil then exit;
  v:=Parser.Eval(J);
  AfterEval;
  MessageDlg(Format('The result of expression is: %s',[VarToStrDef(v,'N/A')]), mtInformation, [mbOK], 0);
end;

procedure TfrmMain.BitBtn4Click(Sender: TObject);
var
  J:JSONObject;
  v:Double;
  mstr:String;
begin
  J:=JSONObject.Create(Memo2.Text);
  if J=nil then exit;
  if not Parser.EvalNumber(J,v) then
    mstr:='N/A'
  else
    mstr:=FloatToStr(v);
  AfterEval;
  MessageDlg(Format('The result of expression is: %s',[VarToStrDef(v,'N/A')]), mtInformation, [mbOK], 0);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  J:JSONObject;
  i:Integer;
  mstr,vstr:String;
begin
  Timer1.Enabled:=false;
  try
    J:=JSONObject.Create(VarMemo.Text);
  except
    exit;
  end;
  valEditor.Strings.Clear;
  with VHelper do
  begin
    Clean;
    ValImport(J);
  end;
  J.Free;
  VarHelperToValList;
end;

procedure TfrmMain.VarMemoChange(Sender: TObject);
begin
  if VarMemo.Enabled then
    Timer1.Enabled:=true
  else
    Timer1.Enabled:=false;
end;

procedure TfrmMain.AfterEval;
var
  J:JSONObject;
begin
  J:=JSONObject.Create;
  VHelper.ValExport(J);
  with VarMemo do
  begin
    Enabled:=false;
    Text:=StringReplace(J.ToString2(2),#10,#13#10,[rfReplaceAll]);
    Enabled:=true;
  end;
  VarHelperToValList;
end;

procedure TfrmMain.VarHelperToValList;
var
  i:Integer;
  mstr,vstr:String;
begin
  valEditor.Strings.Clear;
  with VHelper do
  begin
    for i:=0 to Pred(VarCount) do
    begin
      mstr:=VarNames[i];
      vstr:=VarToStrDef(GetVarDef(mstr,Null),'');
      ValEditor.InsertRow(mstr,vstr,true);
    end;
  end;
end;

end.
