unit JETransBasic;

{
As
Const
Declare
Dim
Function
Name
Open
Private
Property Get
Property Let
Property Set
Public
ReDim
Static
Sub
Type
Binary
Open
Option Compare
ByRef
Sub
ByValByVal
Declare
Sub
Date
Else
If...Then...Else
Select Case
Empty
Error
On Error
False
For...Next
For Each...Next
Open
Friend
}

interface

uses
  Classes, JEParser, uJSON, UJSONExpr, SysUtils;

type
  TJEBasicParser=class(TJEParser)
  private
    FTransForToFOR: Boolean;
    FASPMode: Boolean;
    FInEchoExpr: Boolean;
    FCurFuncIdStr: String;
    procedure SetTransForToFOR(const Value: Boolean);
  protected
    procedure InitParser; override;
    function GetStrChCharSet:TJECharSet; override;
    function GetMathOpCharSet:TJECharSet; override;
    function GetSpecialCharSet:TJECharSet; override;
    function GetLineCommentOps:TStrings; override;
    function GetOpInfo(var Op: String; out IsOp2: Boolean; out Rank: Byte):Boolean; override;
    function GetOpRank(const Op: String):Byte; override;
    function GetSetValOpRank:Byte; override;
    function OnSpecialHeadChar(ACh: Char; APos: Integer):Boolean; override;
    function OnKeyword(const Str: String; KWIdx: Integer):Integer; override;
    function OnPerfix(const Str: String; KWIdx: Integer):Integer; override;
    function DefaultVisibility:String; override;
    function CurExprIsArray:Boolean; override;
    function CurNodeIsFunc:Boolean; override;
    function ParseCall:Integer;
    function ParseClass:Integer;
    function ParseConst:Integer;
    function ParseDeclare:Integer;
    function ParseDim:Integer;
    function ParseVarMemberBody:Integer;  // In class define:  public Name,Age ...
    function ParseDo:Integer;
    function ParseExit:Integer;
    function ParseFor:Integer;
    function ParseFunction:Integer;
    function ParseGoto:Integer;
    function ParseIf:Integer;
    function ParseNew:Integer;
    function ParseOn:Integer;
    function ParseOption:Integer;
    function ParseReDim:Integer;
    function ParseSelect:Integer;
    function ParseSet:Integer;
    function ParseSub:Integer;
    function ParseWhile:Integer;
    function WordIsFunc:Boolean; override;
    function WordIsBuildInFunc(const AName: String):Boolean; override;
    function ParseDefines:Integer; override;
    function ParseParameterDef:Integer; override;
    function ParseMethodBody(const EndStr, PerfixStr: String; out IdStr: String):Integer;
    function Parse_Vars(const PerfixStr: String; NeedNext: Boolean):Integer;
    function Parse_Sub(const PerfixStr: String; NeedNext: Boolean):Integer;
    function Parse_Func(const PerfixStr: String; NeedNext: Boolean):Integer;
    function StrOpResultIsString(const StrVal: String; BranchNode: TJEBranch):TBool3; override;
    procedure PushRawText(const Text:String; AddLnBreak: Boolean=true); override;
  public
    property TransForToFOR:Boolean read FTransForToFOR write SetTransForToFOR;
    property ASPMode:Boolean read FASPMode;
    procedure DoInit; override;
  protected
    procedure BeforeTransTree; override;
    function SetValOp:String; override;
    function ParamDefDiv:String; override;
    function LineDivStr:String; override;
    function LineEndStr:String; override;
    function GetJETreePerfix:String; override;
    function GetJETreePostfix:String; override;
    function TransLanStatmentFunc(JObj: TJEBranch; const Lan, Op: String; Ident: Integer):String; override;
    function TransTypeVal(const ValStr: String; Ident: Integer):String; override;
    function TransINCLUDE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIS(JObj: TJEBranch; Ident: Integer):String; override;
    function TransDEC(JObj: TJEBranch; Ident: Integer):String; override;
    function TransINC(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFOR(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFOREACH(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFORTO(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCASE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransECHO(JObj: TJEBranch; Ident: Integer):String; override;
    function TransEVAL(JObj: TJEBranch; Ident: Integer):String; override;
    function TransEXIT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransPRED(JObj: TJEBranch; Ident: Integer):String; override;
    function TransSUCC(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWAIT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransBREAK(JObj: TJEBranch; Ident: Integer):String; override;
    function TransLOOP(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILENOT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransREPEAT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransVAR(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCONST(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCLASS(JObj: TJEBranch; Ident: Integer):String; override;
    function Trans_BodyDef(JObj: TJENode; Ident: Integer; const ObjName: String):String; override;
    function StdMakeBlock(const Str, IdentStr:String; NoHeadIdent: Boolean):String; override;
    function StdMakeIfThen(const Expr, IdentStr: String):String; override;
    function StdMakeElseIf(const Expr, IdentStr: String):String; override;
    function StdMakeElseEnd(IsElse: Boolean; const IdentStr: String):String; override;
    function StdMakeFuncHead(const FuncName, Perfix, ParamStr, TypeStr, IdentStr: String;
      IsProc: Boolean):String; override;
    function StdMakeFuncBody(const FuncBody, IdentStr: String;
      IsProc: Boolean):String; override;
    class function GetFilePostfixs:TStrings; override;
  public
    class function Lan:ShortString; override;
    class function HasResultVar:Boolean; override;
    class function ArrayUseUBound:Boolean; override;
    function StrForLan(const Str: String):String; override;
    function VarForLan(const VarName: String):String; override;
    function TransCommonOp(JObj: TJEBranch; const Op: String; Ident: Integer; PrnRank: Integer=-1):String; override;
    function PackSrc(const Source: String; out WordCount: Integer):String; override;
  end;

implementation

{$D+}

{
if 1=0 then
  a=2+3.02
  if a>2 then
    a=Round(a) mod CInt(Mid("+23-4",2,2))
  end if
else
  a=CInt("0123")*9
end if
a=CStr(a)
}

const
  JBasic_Mod='MOD';
  JBasic_DIV='\';
  JBasic_XOR='XOR';
  JBasic_AND='AND';
  JBasic_OR='OR';
  JBasic_NOT='NOT';
  //JBasic_SHR='SHR';
  //JBasic_SHL='SHL';
  JBasic_SetValue='=';
  JBasic_RawEnd='<%';

var
  SingleOpMap:array [Char] of String;
  BASKW_AS,
  BASKW_BYREF,
  BASKW_BYVAL,
  BASKW_CALL,
  BASKW_CASE,
  BASKW_CLASS,
  BASKW_CONST,
  BASKW_DECLARE,
  BASKW_DIM,
  BASKW_DO,
  BASKW_EACH,
  BASKW_ELSE,
  BASKW_ELSEIF,
  BASKW_END,
  BASKW_ERROR,
  BASKW_EXIT,
  BASKW_EXPLICIT,
  BASKW_FOR,
  BASKW_FUNCTION,
  BASKW_GOTO,
  BASKW_IF,
  BASKW_IN,
  BASKW_LOOP,
  BASKW_NEW,
  BASKW_NEXT,
  BASKW_ON,
  BASKW_OPTION,
  BASKW_PRIVATE,
  BASKW_PUBLIC,
  BASKW_REDIM,
  BASKW_RESUME,
  BASKW_SELECT,
  BASKW_SET,
  BASKW_STEP,
  BASKW_SUB,
  BASKW_THEN,
  BASKW_TO,
  BASKW_UNTIL,
  BASKW_WEND,
  BASKW_WHILE
  :Integer;
  BASOP_AND,
  BASOP_OR,
  BASOP_XOR,
  BASOP_NOT,
  BASOP_MOD
  :Integer;
  BASCV_FALSE,
  BASCV_TRUE,
  BASCV_NULL
  :Integer;
  BASFN_CINT,
  BASFN_CSTR,
  BASFN_CBOOL,
  BASFN_CBYTE,
  BASFN_CDATE,
  BASFN_CDBL,
  BASFN_CDEC,
  BASFN_CLONG,
  BASFN_LEN,
  BASFN_LBOUND,
  BASFN_PRINT,
  BASFN_UBOUND
  :Integer;
  OpRank:array [TOpChar] of Byte;
  OpRanks:TStringList;
  LanFuncs:TStringList;
  BF_ABS,
  BF_ARRAY,
  BF_ASC,
  BF_ATN,
  BF_CBOOL,
  BF_CBYTE,
  BF_CCUR,
  BF_CDATE,
  BF_CDBL,
  BF_CHR,
  BF_CINT,
  BF_CLNG,
  BF_COS,
  BF_CREATEOBJECT,
  BF_CSNG,
  BF_CSTR,
  BF_DATE,
  BF_DATEADDFUNCTION,
  BF_DATEDIFF,
  BF_DATEPART,
  BF_DATESERIAL,
  BF_DATEVALUE,
  BF_DAY,
  BF_EVAL,
  BF_EXP,
  BF_FILTER,
  BF_FIX,
  BF_FORMATCURRENCY,
  BF_FORMATDATETIME,
  BF_FORMATNUMBER,
  BF_FORMATPERCENT,
  BF_GETOBJECT,
  BF_GETREF,
  BF_HEX,
  BF_HOUR,
  BF_INPUTBOX,
  BF_INSTR,
  BF_INSTRREV,
  BF_INT,
  BF_ISARRAY,
  BF_ISDATE,
  BF_ISEMPTY,
  BF_ISNULL,
  BF_ISNUMERIC,
  BF_ISOBJECT,
  BF_JOIN,
  BF_LBOUND,
  BF_LCASE,
  BF_LEFT,
  BF_LEN,
  BF_LOADPICTURE,
  BF_LOG,
  BF_LTRIM,
  BF_MID,
  BF_MINUTE,
  BF_MONTH,
  BF_MONTHNAME,
  BF_MSGBOX,
  BF_NOW,
  BF_OCT,
  BF_REPLACE,
  BF_RGB,
  BF_RIGHT,
  BF_RND,
  BF_ROUND,
  BF_RTRIM,
  BF_SCRIPTENGINE,
  BF_SCRIPTENGINEBUILDVERSION,
  BF_SCRIPTENGINEMAJORVERSION,
  BF_SCRIPTENGINEMINORVERSION,
  BF_SECOND,
  BF_SGN,
  BF_SIN,
  BF_SPACE,
  BF_SPLIT,
  BF_SQR,
  BF_STRCOMP,
  BF_STRING,
  BF_STRREVERSE,
  BF_TAN,
  BF_TIME,
  BF_TIMER,
  BF_TIMESERIAL,
  BF_TIMEVALUE,
  BF_TRIM,
  BF_TYPENAME,
  BF_UBOUND,
  BF_UCASE,
  BF_VARTYPE,
  BF_WEEKDAY,
  BF_WEEKDAYNAME,
  BF_YEAR:Integer;

{
运算符的计算优先级顺序如下：

算术运算符和串联运算符
求幂 (^)
一元标识和非（+、–）
乘法和浮点除法（*、/）
整数除法 (\)
取模 (Mod)
加法和减法（+、–），字符串连接 (+)
字符串连接 (&)
算术移位（<<、>>）

比较运算符
所有比较运算符（=、<>、<、<=、>、>=、Is、IsNot、Like、TypeOf...Is）

逻辑运算符和位运算符
非 (Not)
与 (And、AndAlso)
或 (Or、OrElse)
异或 (Xor)
}
procedure InitOpRank;
var
  r:Byte;
begin
  OpRanks:=TStringList.Create;
  //OpRank[OpCh_Func]:=OpRank_Func;  //Function
  r:=240;
  OpRank['.']:=r; OpRank['[']:=r; Dec(r,20);
  OpRank['^']:=r; Dec(r,10);
  OpRank['*']:=r; OpRank['/']:=r; Dec(r,20);
  OpRank['\']:=r; Dec(r,20);
  OpRank['%']:=r; OpRanks.AddObject('MOD',TObject(r)); Dec(r,20);
  OpRank['+']:=r; OpRank['-']:=r; Dec(r,10);
  OpRank['&']:=r; Dec(r,10);
  {RK_Shift:=r;}    Dec(r,10);   // << >>  <<<  >>>
  OpRank['=']:=r; OpRank['>']:=r; OpRank['<']:=r;
  OpRanks.AddObject('<>',TObject(r));
  OpRanks.AddObject('>=',TObject(r));
  OpRanks.AddObject('<=',TObject(r)); Dec(r,10);
  Dec(r,20);
  OpRanks.AddObject('NOT',TObject(r)); Dec(r,10);
  OpRanks.AddObject('AND',TObject(r));
  OpRanks.AddObject('ANDALSO',TObject(r)); Dec(r,10);
  OpRanks.AddObject('OR',TObject(r));
  OpRanks.AddObject('ORELSE',TObject(r)); Dec(r,10);
  OpRanks.AddObject('XOR',TObject(r)); Dec(r,10);
  //OpRank[':']:=r; RK_SetValue:=r; Dec(r,10);  //:=
  //OpRank[',']:=r; Dec(r,10);  // ,
  //OpRank[';']:=r; RK_Sentence:=r; //Sentence end
  OpRanks.CaseSensitive:=true;
  OpRanks.Sorted:=true;
end;

function GetRank(const AOp: String):Integer;
begin
  if Length(AOp)=1 then
  begin
    Result:=OpRank[AOp[1]];
    exit;
  end;
  Result:=OpRanks.IndexOf(AOp);
  if Result>=0 then
    Result:=Integer(OpRanks.Objects[Result])
  else
    Result:=0;
end;

procedure InitSingleOp2Func;
begin
  SingleOpMap['%']:=JBasic_MOD;
  SingleOpMap['\']:=JBasic_DIV;
  SingleOpMap['&']:=JEOP_StrJoin;
  SingleOpMap['|']:=JBasic_OR;
  SingleOpMap['^']:=JBasic_XOR;
  SingleOpMap['!']:=JBasic_NOT;
end;

procedure InitLanFuncs;
begin
  LanFuncs:=TStringList.Create;
  with LanFuncs do
  begin
    BF_ABS:=Add('ABS');
    BF_ARRAY:=Add('ARRAY');
    BF_ASC:=Add('ASC');
    BF_ATN:=Add('ATN');
    BF_CBOOL:=Add('CBOOL');
    BF_CBYTE:=Add('CBYTE');
    BF_CCUR:=Add('CCUR');
    BF_CDATE:=Add('CDATE');
    BF_CDBL:=Add('CDBL');
    BF_CHR:=Add('CHR');
    BF_CINT:=Add('CINT');
    BF_CLNG:=Add('CLNG');
    BF_COS:=Add('COS');
    BF_CREATEOBJECT:=Add('CREATEOBJECT');
    BF_CSNG:=Add('CSNG');
    BF_CSTR:=Add('CSTR');
    BF_DATE:=Add('DATE');
    BF_DATEADDFUNCTION:=Add('DATEADDFUNCTION');
    BF_DATEDIFF:=Add('DATEDIFF');
    BF_DATEPART:=Add('DATEPART');
    BF_DATESERIAL:=Add('DATESERIAL');
    BF_DATEVALUE:=Add('DATEVALUE');
    BF_DAY:=Add('DAY');
    BF_EVAL:=Add('EVAL');
    BF_EXP:=Add('EXP');
    BF_FILTER:=Add('FILTER');
    BF_FIX:=Add('FIX');
    BF_FORMATCURRENCY:=Add('FORMATCURRENCY');
    BF_FORMATDATETIME:=Add('FORMATDATETIME');
    BF_FORMATNUMBER:=Add('FORMATNUMBER');
    BF_FORMATPERCENT:=Add('FORMATPERCENT');
    BF_GETOBJECT:=Add('GETOBJECT');
    BF_GETREF:=Add('GETREF');
    BF_HEX:=Add('HEX');
    BF_HOUR:=Add('HOUR');
    BF_INPUTBOX:=Add('INPUTBOX');
    BF_INSTR:=Add('INSTR');
    BF_INSTRREV:=Add('INSTRREV');
    BF_INT:=Add('INT');
    BF_ISARRAY:=Add('ISARRAY');
    BF_ISDATE:=Add('ISDATE');
    BF_ISEMPTY:=Add('ISEMPTY');
    BF_ISNULL:=Add('ISNULL');
    BF_ISNUMERIC:=Add('ISNUMERIC');
    BF_ISOBJECT:=Add('ISOBJECT');
    BF_JOIN:=Add('JOIN');
    BF_LBOUND:=Add('LBOUND');
    BF_LCASE:=Add('LCASE');
    BF_LEFT:=Add('LEFT');
    BF_LEN:=Add('LEN');
    BF_LOADPICTURE:=Add('LOADPICTURE');
    BF_LOG:=Add('LOG');
    BF_LTRIM:=Add('LTRIM');
    BF_MID:=Add('MID');
    BF_MINUTE:=Add('MINUTE');
    BF_MONTH:=Add('MONTH');
    BF_MONTHNAME:=Add('MONTHNAME');
    BF_MSGBOX:=Add('MSGBOX');
    BF_NOW:=Add('NOW');
    BF_OCT:=Add('OCT');
    BF_REPLACE:=Add('REPLACE');
    BF_RGB:=Add('RGB');
    BF_RIGHT:=Add('RIGHT');
    BF_RND:=Add('RND');
    BF_ROUND:=Add('ROUND');
    BF_RTRIM:=Add('RTRIM');
    BF_SCRIPTENGINE:=Add('SCRIPTENGINE');
    BF_SCRIPTENGINEBUILDVERSION:=Add('SCRIPTENGINEBUILDVERSION');
    BF_SCRIPTENGINEMAJORVERSION:=Add('SCRIPTENGINEMAJORVERSION');
    BF_SCRIPTENGINEMINORVERSION:=Add('SCRIPTENGINEMINORVERSION');
    BF_SECOND:=Add('SECOND');
    BF_SGN:=Add('SGN');
    BF_SIN:=Add('SIN');
    BF_SPACE:=Add('SPACE');
    BF_SPLIT:=Add('SPLIT');
    BF_SQR:=Add('SQR');
    BF_STRCOMP:=Add('STRCOMP');
    BF_STRING:=Add('STRING');
    BF_STRREVERSE:=Add('STRREVERSE');
    BF_TAN:=Add('TAN');
    BF_TIME:=Add('TIME');
    BF_TIMER:=Add('TIMER');
    BF_TIMESERIAL:=Add('TIMESERIAL');
    BF_TIMEVALUE:=Add('TIMEVALUE');
    BF_TRIM:=Add('TRIM');
    BF_TYPENAME:=Add('TYPENAME');
    BF_UBOUND:=Add('UBOUND');
    BF_UCASE:=Add('UCASE');
    BF_VARTYPE:=Add('VARTYPE');
    BF_WEEKDAY:=Add('WEEKDAY');
    BF_WEEKDAYNAME:=Add('WEEKDAYNAME');
    BF_YEAR:=Add('YEAR');
    Sorted:=true;
  end;
end;

{ TJEBasicParser }

class function TJEBasicParser.ArrayUseUBound: Boolean;
begin
  Result:=true;
end;

procedure TJEBasicParser.BeforeTransTree;
begin
  inherited;
  FASPMode:=false;
end;

class function TJEBasicParser.GetFilePostfixs: TStrings;
begin
  Result:=inherited GetFilePostfixs;
  Result.Add('asp');
  Result.Add('vbs');
  Result.Add('bas');
end;

function TJEBasicParser.GetJETreePerfix: String;
begin
  if ASPMode then
    Result:='<%'#13#10
  else
    Result:='';
end;

function TJEBasicParser.GetJETreePostfix: String;
begin
  if ASPMode then
    Result:=#13#10'%>'
  else
    Result:='';
end;

function TJEBasicParser.GetOpRank(const Op: String): Byte;
begin
  Result:=GetRank(Op);
  if Result>0 then exit;  
  Result:=inherited GetOpRank(Op);
end;

class function TJEBasicParser.Lan: ShortString;
begin
  Result:='Basic';
end;

function TJEBasicParser.LineDivStr: String;
begin
  Result:=#13#10;
end;

function TJEBasicParser.LineEndStr: String;
begin
  Result:='';
end;

function TJEBasicParser.PackSrc(const Source: String; out WordCount: Integer): String;
var
  mstr:String;
  i,Len,n,Status:Integer;
  LastNone:Boolean;
begin
  WordCount:=0;
  Status:=1;
  n:=0;
  i:=1;
  LastNone:=false;
  Len:=Length(Source);
  SetLength(mstr,Len);
  while i<=Len do
  begin
    case Source[i] of
      '"':
        begin
          Inc(WordCount);
          Inc(n);
          mstr[n]:=Source[i];
          Inc(i);
          while true do
          begin
            if Source[i]='"' then
            begin
              if Source[i+1]='"' then
              begin
                mstr[n+1]:='"';
                mstr[n+2]:='"';
                Inc(n,2);
                Inc(i,2);
              end
              else begin
                Inc(n);
                mstr[n]:='"';
                Inc(i);
                break;
              end;
            end
            else begin
              Inc(n);
              mstr[n]:=Source[i];
              Inc(i);
            end;
          end;
          Status:=1;
          continue;
        end;
      '''':
        begin
          repeat
            Inc(i);
          until Source[i-1]<' ';
          continue;
        end;
    end;
    case Status of
      0:
        begin
          if Source[i] in Alpha_Number then
          begin
            if LastNone then
            begin
              Inc(WordCount);
              Inc(n);
              mstr[n]:=' ';
            end;
            Inc(n);
            mstr[n]:=Source[i];
            LastNone:=false;
          end
          else if Source[i]<=' ' then
            LastNone:=true
          else begin
            Inc(n);
            mstr[n]:=Source[i];
            Status:=1;
          end;
        end;
      1:
        begin
          if Source[i] in Alpha_Number then
          begin
            Inc(WordCount);
            Inc(n);
            mstr[n]:=Source[i];
            Status:=0;
            LastNone:=false;
          end
          else if Source[i]>' ' then
          begin
            Inc(n);
            mstr[n]:=Source[i];
          end;
        end;
    end;
    Inc(i);
  end;
  Result:=Copy(mstr,1,n);
end;

function TJEBasicParser.ParamDefDiv: String;
begin
  Result:=',';
end;

function TJEBasicParser.SetValOp: String;
begin
  Result:=JBasic_SetValue;
end;

function TJEBasicParser.StdMakeBlock(const Str, IdentStr: String;
  NoHeadIdent: Boolean): String;
begin
  if NoHeadIdent then
    Result:=#13#10+IdentStr+Str
  else
    Result:=#13#10+Str
end;

function TJEBasicParser.StdMakeElseEnd(IsElse: Boolean;
  const IdentStr: String): String;
begin
  if IsElse then
    Result:=#13#10+IdentStr+'Else'
  else
    Result:=#13#10+IdentStr+'End If';
end;

function TJEBasicParser.StdMakeElseIf(const Expr, IdentStr: String): String;
begin
  Result:=#13#10+IdentStr+'ElseIf '+Expr+' then';
end;

function TJEBasicParser.StdMakeFuncBody(const FuncBody, IdentStr: String;
  IsProc: Boolean): String;
begin
  Result:=#13#10+IdentStr+FuncBody+#13#10+IdentStr+'End ';
  if IsProc then
    Result:=Result+'Sub'
  else
    Result:=Result+'Function';
end;

function TJEBasicParser.StdMakeFuncHead(const FuncName, Perfix, ParamStr,
  TypeStr, IdentStr: String; IsProc: Boolean): String;
begin
  Result:=IdentStr;
  if Perfix<>'' then Result:=Result+' '+Perfix+' ';
  if IsProc then Result:=Result+'Sub ' else Result:=Result+'Function ';
  Result:=Result+FuncName+'('+ParamStr+')';
  if not IsProc and (TypeStr<>'') then Result:=Result+' '':'+TypeStr;
end;

function TJEBasicParser.StdMakeIfThen(const Expr, IdentStr: String): String;
begin
  Result:=IdentStr+'If '+Expr+' Then';
end;

function TJEBasicParser.StrForLan(const Str: String): String;
begin
  Result:=DblQuotedStr(Str);
end;

function TJEBasicParser.StrOpResultIsString(const StrVal: String; BranchNode: TJEBranch): TBool3;
var
  mstr:String;
  n:Integer;
begin
  n:=Length(StrVal);
  if (n>=3) and (n<=8) then
  begin
    mstr:=UpperCase(StrVal);
    if ((n=3) and ((mstr='CHR') or (mstr='MID') or (mstr='HEX') or (mstr='OCT')))
      or ((n=4) and ((mstr='LEFT') or (mstr='CSTR') or (mstr='JOIN')))
      or ((n=5) and ((mstr='RIGHT') or (mstr='LCASE') or (mstr='UCASE') or (mstr='SPACE')
        or (mstr='LTRIM') or (mstr='RTRIM')))
      or ((n=6) and (mstr='STRING'))
      or ((n=7) and (mstr='REPLACE'))
      or ((n=8) and (mstr='TYPENAME'))  then
    begin
      Result:=b3True;
      exit;
    end;
  end
  else if (n=10) and (LowerCase(StrVal)='strreverse') then
  begin
    Result:=b3True;
    exit;
  end
  else if (n=11) and (LowerCase(StrVal)='weekdayname') then
  begin
    Result:=b3True;
    exit;
  end
  else if (n>=12) then
  begin
    if (StrVal[1] in ['F','f']) and (StrVal[2] in ['o','O']) then
    begin
      {FormatCurrency 函数
       FormatDateTime 函数
       FormatNumber 函数
       FormatPercent 函数
      }
      mstr:=LowerCase(Copy(StrVal,3,255));
      if (mstr='rmatcurrency') or (mstr='rmatdatetime') or (mstr='rmatnumber') or (mstr='rmatpercent') then
      begin
        Result:=b3True;
        exit;
      end;
    end
    else if (StrVal[1] in ['S','s']) and (StrVal[2] in ['c','C']) then
    begin
      {ScriptEngine 函数
       ScriptEngineBuildVersion 函数
       ScriptEngineMajorVersion 函数
       ScriptEngineMinorVersion 函数
      }
      mstr:=LowerCase(Copy(StrVal,3,255));
      if (mstr='riptengine') or (mstr='riptenginebuildversion') or (mstr='riptenginemajorversion') or (mstr='riptengineminorversion') then
      begin
        Result:=b3True;
        exit;
      end;
    end;
  end;
  Result:=inherited StrOpResultIsString(StrVal,BranchNode);
end;

function TJEBasicParser.TransBREAK(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Result:='Exit';
  Z:=GetCycleLevelNode;
  if not (Z is TJEBranch) then exit;
  with TJEBranch(Z) do
  begin
    mstr:=OptString(JEP_Operator);
    if mstr=JEF_WhileNot then
      Result:=Result+' Do'
    else if mstr=JEF_Loop then
      Result:=Result+' Do'
    else if mstr=JEF_While then
      Result:=Result+' While'
    else if mstr=JEF_Repeat then
      Result:=Result+' Do'
    else
      Result:=Result+' For';
  end;
end;

function TJEBasicParser.TransCASE(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
  IdentStr:String;
  Z,Z2:TJENode;
begin
  IdentStr:=IdentLen(Ident);
  Result:=IdentStr+'Select Case '+TransANode(JObj.Opt(JEP_Param1),0);
  i:=2;
  while True do
  begin
    Z:=JObj.Opt(JEP_ParamHeader+IntToStr(i));
    Z2:=JObj.Opt(JEP_ParamHeader+IntToStr(i+1));
    if Z=nil then break;
    if Z2=nil then
    begin
      Result:=Result+#13#10+IdentLen(Ident+1)+'Case Else'
        +#13#10+TransANode(Z,Ident+2);
      break;
    end;
    Result:=Result+#13#10+IdentLen(Ident+1)+'Case '+TransANode(Z,0)
      +#13#10+TransANode(Z2,Ident+2);
    Inc(i,2);
  end;
  Result:=Result+#13#10+IdentStr+'End Select';
end;

function TJEBasicParser.TransCLASS(JObj: TJEBranch; Ident: Integer): String;
var
  mstr:String;
begin
  mstr:=GetPerfixStr(JObj);
  if mstr<>'' then
    Result:=mstr+' Class '
  else
    Result:='Class ';
  Result:=IdentLen(Ident)+Result+TransANode(JObj.Opt(JEP_Param1),0)+#13#10;
  mstr:=TransANode(JObj.Opt(JEDN_Members),Ident+1);
  if mstr<>'' then
    Result:=Result+mstr;
  Result:=Result+#13#10+IdentLen(Ident)+'End Class';
end;

function TJEBasicParser.TransCommonOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
var
  Ch:Char;
begin
  case Length(Op) of
    1:
    begin
      Ch:=Op[1];
      if Ch in ['%','\','&','|','^'] then
      begin
        Result:=Trans_Mid(JObj,SingleOpMap[Op[1]],GetOpRank(Op),PrnRank);
        exit;
      end;
    end;
    2:
    begin
      if Op[2]='=' then
      begin
        case Op[1] of
          '+','-','*','/':
          begin
            Result:=Trans_CalcAndSet(JObj,Op[1],GetOpRank(Op[1]));
            exit;
          end;
          '%','\','&','|','^','!':
          begin
            Result:=Trans_CalcAndSet(JObj,SingleOpMap[Op[1]],GetOpRank(Op[1]));
            exit;
          end;
        end;
      end
      else if Op=StdJEOps[jeopShl] then
      begin
        Result:=Trans_Mid(JObj,'* 2^',RK_Multi,PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopShr] then
      begin
        Result:=Trans_Mid(JObj,'\ 2^',RK_Multi,PrnRank);
        exit;
      end
      else if Op=JEOP_StrJoin then
      begin
        Result:=Trans_Mid(JObj,'&',GetOpRank('&'),PrnRank);
        exit;
      end;
    end;
    3:
    begin
      if Op[3]='=' then
      begin
        if (Op[1] in ['>','<']) and (Op[2]=Op[1]) then
        begin
          if Op[1]='>' then
            Result:=Trans_CalcAndSet(JObj,'\ 2^',RK_Multi)
          else //Op[1]='<'
            Result:=Trans_CalcAndSet(JObj,'* 2^',RK_Multi);
          exit;
        end;
      end;
    end;
  end;
  Result:=inherited TransCommonOp(JObj,Op,Ident,PrnRank);
end;

function TJEBasicParser.TransCONST(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
begin
  Result:=GetPerfixStr(JObj);
  if Result='' then
    Result:=IdentLen(Ident)+'Const '
  else
    Result:=IdentLen(Ident)+GetPerfixStr(JObj)+' Const ';
  with JObj do
    for i:=1 to Pred(length()) do
    begin
      if Keys[i]=JEP_Perfix then continue;
      Result:=Result+TransANode(ValObjByIndex[i],0)+',';
    end;
  Delete(Result,Length(Result),1);
end;

function TJEBasicParser.TransDEC(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransECHO(JObj: TJEBranch; Ident: Integer): String;
begin
  if ASPMode then
    Result:=IdentLen(Ident)+'%><%='+TransANode(JObj.Opt(JEP_Param1),0)+'%><%'
  else
    Result:=IdentLen(Ident)+'Response.Write( '+TransANode(JObj.Opt(JEP_Param1),0)+' )';
end;

function TJEBasicParser.TransEVAL(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransEXIT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident)+'Exit';
  Z:=GetProcLevelNode;
  if not (Z is TJEBranch) then exit;
  with TJEBranch(Z) do
  begin
    if OptString(JEP_Operator)=' '+JED_Proc then
      Result:=Result+' Sub'
    else
      Result:=Result+' Function';
  end;
end;

function TJEBasicParser.TransFOR(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Result:=IdentLen(Ident)+'''JE::FOR Begin'#13#10;
  Result:=Result+TransANode(JObj.Opt(JEP_Param1),Ident);
  Result:=Result+#13#10+IdentLen(Ident)+'Do'+#13#10;
  Result:=Result+IdentLen(Ident+1)+'If not ('+TransANode(JObj.Opt(JEP_Param2),0)+') Then  Exit Do'+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident+1)+'''JE::INC Part'#13#10;
  Result:=Result+TransANode(JObj.Opt(JEP_Param3),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Loop';
  Result:=Result+IdentLen(Ident)+#13#10+IdentLen(Ident)+'''JE::FOR End'#13#10;
(*
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'For '+TransANode(Z,0)+' to '+TransANode(JObj.Opt(JEP_Param2),0);
  Result:=Result+' '''+TransANode(JObj.Opt(JEP_Param3),0)+#13#10;  { TODO : Calc Step here }
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Next';*)
end;

function TJEBasicParser.TransFOREACH(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'For Each '+TransANode(Z,0)+' In '+TransANode(JObj.Opt(JEP_Param2),0)+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Next';
end;

function TJEBasicParser.TransFORTO(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'For '+TransANode(Z,0)+' To ';
  Z:=JObj.Opt(JEP_Param2);
  Result:=Result+TransANode(Z,0);
  mstr:=TransANode(JObj.Opt(JEP_Param3),0);
  if mstr<>'1' then
    Result:=Result+' Step '+mstr;
  Result:=Result+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Next';
end;
function TJEBasicParser.TransIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=StdTransIFEx(JObj,Ident);
end;

function TJEBasicParser.TransIIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=StdTransIFEx(JObj,Ident,true);
end;

function TJEBasicParser.TransINC(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Z:=JObj.Opt(JEP_Param1);
  mstr:=TransANode(Z,0);
  Result:=IdentLen(Ident)+mstr+' = '+mstr+' + 1';
end;

function TJEBasicParser.TransINCLUDE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:='%><!--#INCLUDE FILE='+TransANode(JObj.Opt(JEP_Param1),0)+'--><%'
end;

function TJEBasicParser.TransIS(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransLanStatmentFunc(JObj: TJEBranch; const Lan,
  Op: String; Ident: Integer): String;
begin
  if Op='ONERROR' then
  begin
    Result:=IdentLen(Ident)+'On Error';
    if JObj.Opt(JEP_Param1)=CNULL then
      Result:=Result+' Resume Next'
    else
      Result:=Result+' Goto '+TransANode(JObj.Opt(JEP_Param1),0);
  end
  else
    Result:=inherited TransLanStatmentFunc(JObj,Lan,Op,Ident);
end;

function TJEBasicParser.TransLOOP(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident)+'Do'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param1),Ident+1);
  Result:=Result+#13#10'+IdentLen(Ident)+Loop While '+TransANode(JObj.Opt(JEP_Param2),0,0);
end;

function TJEBasicParser.TransPRED(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransREPEAT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident)+'Do'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param1),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Loop Until '+TransANode(JObj.Opt(JEP_Param2),0,0);
end;

function TJEBasicParser.TransSUCC(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransTypeVal(const ValStr: String; Ident: Integer): String;
begin
  Result:=Copy(ValStr,3,MaxInt);
  if Result='' then exit;
  case ValStr[2] of
    JEPT_Hex: Result:='&H'+Result;
    JEPT_EchoStr:
    begin
      Result:='%>'+Result+'<%';
      FASPMode:=true;
    end;
  end;
end;

function TJEBasicParser.TransVAR(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
begin
  Result:=GetPerfixStr(JObj);
  if Result='' then
    Result:=IdentLen(Ident)+'Dim '
  else
    Result:=IdentLen(Ident)+Result+' ';
  with JObj do
    for i:=1 to Pred(length()) do
    begin
      if Keys[i]=JEP_Perfix then continue;
      Result:=Result+TransANode(ValObjByIndex[i],0)+',';
    end;
  Delete(Result,Length(Result),1);
end;

function TJEBasicParser.TransWAIT(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEBasicParser.TransWHILE(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'While '+TransANode(Z,0)+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Wend';
end;

function TJEBasicParser.TransWHILENOT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'Do Until '+TransANode(Z,0)+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Loop';
end;

function TJEBasicParser.Trans_BodyDef(JObj: TJENode; Ident: Integer;
  const ObjName: String): String;
begin
  FCurFuncIdStr:=ObjName;  //将 JEV_ResultRep 替换为函数名
  try
    SetNextNeedVal(false);
    Result:=inherited Trans_BodyDef(JObj,Ident,ObjName);
  finally
    FCurFuncIdStr:='';
  end;
end;

function TJEBasicParser.VarForLan(const VarName: String): String;
begin
  //在函数内部，将返回值替换为函数名
  if (FCurFuncIdStr<>'') and ((VarName=JEV_ResultRep) or (VarName=JEV_Result)) then
    Result:=FCurFuncIdStr
  else
    Result:=inherited VarForLan(VarName);
end;

function TJEBasicParser.CurExprIsArray: Boolean;
var
  lv:Integer;
begin
  lv:=GetStatementLevel;
  if lv>=0 then
  begin
    with FLevelNodes[lv] do
    begin
      // new Cars("MyCar","BLUE")
      if (BC=FCurBlockCount) and (Op='NEW') then
      begin
        Result:=false;
        exit;
      end;
    end;
  end;
  Result:=inherited CurExprIsArray;
end;

function TJEBasicParser.CurNodeIsFunc: Boolean;
var
  lv:Integer;
begin
  lv:=GetStatementLevel;
  if lv>=0 then
  begin
    with FLevelNodes[lv] do
    begin
      // new Cars("MyCar","BLUE")
      if (BC=FCurBlockCount) and (Op='NEW') then
      begin
        Result:=true;
        exit;
      end;
    end;
  end;
  Result:=inherited CurNodeIsFunc;
end;

function TJEBasicParser.DefaultVisibility: String;
begin
  Result:='PUBLIC';
end;

procedure TJEBasicParser.DoInit;
var
  mstr:String;
begin
  inherited;
  FASPMode:=(Pos('<%',FSource)>0) or (Pos('</',FSource)>0);
  if FASPMode then
  begin
    mstr:=NextTo('<%',true,true);
    if mstr<>'' then
      PushRawText(mstr);
  end;
end;

function TJEBasicParser.GetLineCommentOps: TStrings;
begin
  Result:=TStringList.Create;
  Result.Add('''');
end;

function TJEBasicParser.GetMathOpCharSet: TJECharSet;
begin
  Result:=['+', '-', '*', '/', '\', '^', '&', '.'];
end;

function TJEBasicParser.GetOpInfo(var Op: String; out IsOp2: Boolean;
  out Rank: Byte): Boolean;
var
  n:Integer;
begin
  Result:=true;
  n:=Length(Op);
  if n=3 then
  begin
    if Op='MOD' then
    begin
      Op:='%';
      Rank:=OpRank['%'];
      IsOp2:=true;
      exit;
    end
    else if Op='NOT' then
    begin
      Rank:=GetRank(Op);
      IsOp2:=false;
      exit;
    end;
    IsOp2:=true;
    Rank:=GetRank(Op);
  end
  else if (n=2) and (Op=JEOP_StrJoin) then
  begin
    Result:=inherited GetOpInfo(Op,IsOp2,Rank);    
  end
  else begin
    Rank:=GetRank(Op);
    IsOp2:=true;
  end;
  Result:=Rank>0;
end;

{function TJEBasicParser.GetOpRank(const Op: String): Byte;
begin
  if Length(Op)=3 then
  begin
    if Op='MOD' then
    begin
      Result:=OpRank['%'];
    end
    else if Op='NOT' then
    begin
      Result:=GetRank(Op);
    end
    else
      Result:=GetRank(Op);
  end
  else
    Result:=GetRank(Op);
end;}

function TJEBasicParser.GetSetValOpRank: Byte;
begin
  Result:=OpRank['='];
end;

function TJEBasicParser.GetSpecialCharSet: TJECharSet;
begin
  Result:=['&','<','%'];
end;

function TJEBasicParser.GetStrChCharSet: TJECharSet;
begin
  Result:=['"'];
end;

class function TJEBasicParser.HasResultVar: Boolean;
begin
  Result:=true;
end;

procedure TJEBasicParser.InitParser;
begin
  inherited;
  FWordNoCase:=true;
  FLineBreakSentence:=true;
  FLineBreakCh:=':';
  FLineJoinCh:='_';
  FSetValOp:='=';
  FEqualOp:='=';
  FNotEqualOp:='<>';
  FStrCh:=['"'];
  FPascalTypeStr:=true;
  FArrayUseFuncBracket:=true;
  FAddAsStrJoin:=true;
  FFuncNameAsResult:=true;
  FSessionVarName:='SESSION';
  BASKW_AS:=RegKeyword('AS');
  BASKW_BYREF:=RegKeyword('BYREF');
  BASKW_BYVAL:=RegKeyword('BYVAL');
  BASKW_CALL:=RegHeadKeywordMethod('CALL',ParseCall);
  BASKW_CASE:=RegKeyword('CASE');
  BASKW_CLASS:=RegHeadKeywordMethod('CLASS',ParseClass);
  BASKW_CONST:=RegHeadKeywordMethod('CONST',ParseConst);
  BASKW_DECLARE:=RegHeadKeywordMethod('DECLARE',ParseDeclare);
  BASKW_DIM:=RegHeadKeywordMethod('DIM',ParseDim);
  BASKW_DO:=RegHeadKeywordMethod('DO',ParseDo);
  BASKW_EACH:=RegKeyword('EACH');
  BASKW_ELSE:=RegKeyword('ELSE');
  BASKW_ELSEIF:=RegKeyword('ELSEIF');
  BASKW_END:=RegKeyword('END');
  BASKW_ERROR:=RegKeyword('ERROR');
  BASKW_EXIT:=RegHeadKeywordMethod('EXIT',ParseExit);
  BASKW_EXPLICIT:=RegKeyword('EXPLICIT');
  BASKW_FOR:=RegHeadKeywordMethod('FOR',ParseFor);
  BASKW_FUNCTION:=RegHeadKeywordMethod('FUNCTION',ParseFunction);
  BASKW_GOTO:=RegHeadKeywordMethod('GOTO',ParseGoto);
  BASKW_IF:=RegHeadKeywordMethod('IF',ParseIf);
  BASKW_IN:=RegKeyword('IN');
  BASKW_LOOP:=RegKeyword('LOOP');
  BASKW_NEW:=RegHeadKeywordRetVal('NEW',ParseNew);    //2012-05-30
  BASKW_NEXT:=RegKeyword('NEXT');
  BASKW_ON:=RegHeadKeywordMethod('ON',ParseOn);
  BASKW_OPTION:=RegHeadKeywordMethod('OPTION',ParseOption);
  BASKW_REDIM:=RegHeadKeywordMethod('REDIM',ParseReDim);
  BASKW_PRIVATE:=RegKeyword('PRIVATE',[ktPerfix]);
  BASKW_PUBLIC:=RegKeyword('PUBLIC',[ktPerfix]);
  BASKW_RESUME:=RegKeyword('RESUME');
  BASKW_SELECT:=RegHeadKeywordMethod('SELECT',ParseSelect);
  BASKW_SET:=RegHeadKeywordMethod('SET',ParseSet);
  BASKW_STEP:=RegKeyword('STEP');
  BASKW_SUB:=RegHeadKeywordMethod('SUB',ParseSub);
  BASKW_THEN:=RegKeyword('THEN');
  BASKW_TO:=RegKeyword('TO');
  BASKW_UNTIL:=RegKeyword('UNTIL');
  BASKW_WEND:=RegKeyword('WEND');
  BASKW_WHILE:=RegHeadKeywordMethod('WHILE',ParseWhile);
  //
  BASOP_AND:=RegLanOp('AND');
  BASOP_OR:=RegLanOp('OR');
  BASOP_XOR:=RegLanOp('XOR');
  BASOP_NOT:=RegLanOp('NOT',false);
  BASOP_MOD:=RegLanOp('MOD');
  //
  BASCV_FALSE:=RegLanVal('FALSE');
  BASCV_TRUE:=RegLanVal('TRUE');
  BASCV_NULL:=RegLanVal('NULL');
  //
  BASFN_CINT:=RegLanFunc('CINT');
  BASFN_CSTR:=RegLanFunc('CSTR');
  BASFN_CBOOL:=RegLanFunc('CBOOL');
  BASFN_CBYTE:=RegLanFunc('CBYTE');
  BASFN_CDATE:=RegLanFunc('CDATE');
  BASFN_CDBL:=RegLanFunc('CDBL');
  BASFN_CDEC:=RegLanFunc('CDEC');
  BASFN_CLONG:=RegLanFunc('CLONG');
  BASFN_LEN:=RegLanFunc('LEN');
  BASFN_LBOUND:=RegLanFunc('LBOUND');
  BASFN_PRINT:=RegLanFunc('PRINT');
  BASFN_UBOUND:=RegLanFunc('UBOUND');
end;

function TJEBasicParser.OnKeyword(const Str: String; KWIdx: Integer):Integer;
begin
  if KWIdx=BASKW_IF then
    ParseIf;
end;

function TJEBasicParser.OnPerfix(const Str: String; KWIdx: Integer): Integer;
var
  mstr:String;
  idx2:Integer;
begin
  mstr:=Str;
  idx2:=NextTokenKW;
  if idx2=BASKW_DIM then
    Result:=Parse_Vars(mstr,true)
  else if idx2=BASKW_SUB then
    Result:=Parse_Sub(mstr,true)
  else if idx2=BASKW_FUNCTION then
    Result:=Parse_Func(mstr,true)
  else if idx2<0 then
    Result:=Parse_Vars(mstr,false)
  else
    PrintErr('SUB, FUNCTION or identifier expected.');
end;

function TJEBasicParser.OnSpecialHeadChar(ACh: Char; APos: Integer): Boolean;
var
  n,spos:Integer;
  mstr:String;
begin
  Result:=false;
  n:=Length(FSource);
  Inc(APos);
  if APos>n then exit;
  if ACh='&' then
  begin
    if FSource[APos] in ['h','H'] then
    begin
      Inc(APos);
      mstr:=JEP_TypeHead+JEPT_Hex;
      while FSource[APos] in ['0'..'9','A'..'F','a'..'f'] do
      begin
        mstr:=mstr+FSource[APos];
        Inc(APos);
        if APos>n then break;
      end;
      SetCurToken(mstr,tkNUMERIC,FCurPos);
    end
    else
      SetCurToken('+>',tkOperator,FCurPos);
    FCurPos:=APos;
    Result:=true;
  end
  else if ACh='<' then
  begin
    if FSource[APos]<>'%' then exit;
    Inc(APos);
    if APos<n then
    begin
      // <%=Some Expr%>  =>  Response.Write(Some Expr)
      if FSource[APos]='=' then
      begin
        Inc(APos);
        PushLineBreakOp;
        //PushVar('Response');
        //PushOp('.');
        PushFunc('ECHO');
        //PushBracket('(');
        SetCurToken('(',tkBRACKET,FCurPos);
        FInEchoExpr:=true;
      end;
    end;
    if not FInEchoExpr then
    begin
      if CurToken.Kind<>tkLINEDIV then  //2012-08-03  <% %> 内已经加了换行符，无需Space
        SetCurToken('',tkSPACE,FCurPos);
    end;
    FCurPos:=APos;
    Result:=true;
  end
  else if ACh='%' then
  begin 
    if FSource[APos]<>'>' then exit;
    if FInEchoExpr then
    begin
      PushBracket(')');
      PushLineBreakOp;
      //SetCurToken(':',tkLINEDIV,FCurPos);
      FInEchoExpr:=false;
    end;
    FCurPos:=APos+1;
    spos:=FCurPos;
    mstr:=NextTo(JBasic_RawEnd,true,true);  //退回到 <% 符号之前，以便解析 <%=
    SetCurToken(mstr,tkRawText,spos);
    {
    //if mstr<>'' then
      PushHTMLStr(mstr);
    if FCurPos>n then
      SetCurToken('',tkEND,FCurPos)
    else
      SetCurToken('',tkLINEDIV,FCurPos);
      //SetCurToken(JBasic_RawEnd,tkRawEnd,FCurPos);}
    Result:=true;
  end;
end;

function TJEBasicParser.ParseCall:Integer;
begin
  PushStatement(MakeLanFunc('CALL'));
  NextToken;
  if ParseStatements(true)<0 then
    PrintErr('Statement expected.');    
  StatementEnd;
end;

function TJEBasicParser.ParseClass:Integer;
begin
  PushDefStatement(' '+JED_Class,JED_Class);
  Result:=FCurNodeLevel;
  NextToken;
  if ParseIdentifier<>'' then
  begin
    GoNextStatementParam;
    NextTokenKW;
    MakeArrayParam(JEDN_Members);
    ParseDefines;
    if CurToken.KWIdx1=BASKW_END+1 then
    begin
      NextToken;
      ExpectKW;
      if CurToken.KWIdx1=BASKW_CLASS+1 then
      begin
        StatementWithDefEnd;
        NextToken;
      end
      else begin
        PrintErr('Expect "CLASS" here.');
        StatementWithDefEnd;
      end;
      exit;
    end;
  end;
  StatementWithDefEnd;
end;

function TJEBasicParser.ParseConst:Integer;
begin
  // [Public | Private] Const constname = expression
  PushDefStatement(' CONST','CONST');
  Result:=FCurNodeLevel;
  NextToken;
  while ParseIdentifier<>'' do
  begin
    if CurToken.Token='=' then
    begin
      PushSetValOp;
      NextToken;
      if ParseExpr<0 then
        PrintErr('Expect expr here.',Result+1);
    end
    else
      PrintErr('Expect "=" here.');
    if CurToken.Token<>',' then break;
    GoNextStatementParam;
    NextToken;
  end;
  if not (CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND]) then
    PrintErr('Except identifier here.');
  StatementWithDefEnd;
end;

function TJEBasicParser.ParseDeclare;
begin
  Result:=FCurNodeLevel;
  NextToken;
end;

function TJEBasicParser.ParseDefines: Integer;
var
  idx,idx2,lv:Integer;
  mstr:String;
begin
  Result:=-1;
  lv:=-1;
  ExpectKW;
  while CurToken.Kind<>tkEND do
  begin
    if CurToken.Kind=tkComment then
    begin
      PushCommentToken(false);
      NextToken;
    end
    else begin
      idx:=CurToken.KWIdx1-1;
      if idx=BASKW_END then exit;
      mstr:='PUBLIC';  //Default visibility
      if idx=BASKW_DIM then
        lv:=Parse_Vars(mstr,true)
      else if idx=BASKW_SUB then
        lv:=Parse_Sub(mstr,true)
      else if idx=BASKW_FUNCTION then
        lv:=Parse_Func(mstr,true)
      else if (idx=BASKW_PUBLIC) or (idx=BASKW_PRIVATE) then
      begin
        if idx=BASKW_PRIVATE then mstr:='PRIVATE';
        idx2:=NextTokenKW;
        if idx2=BASKW_DIM then
          lv:=Parse_Vars(mstr,true)
        else if idx2=BASKW_SUB then
          lv:=Parse_Sub(mstr,true)
        else if idx2=BASKW_FUNCTION then
          lv:=Parse_Func(mstr,true)
        else if idx2<0 then
          lv:=Parse_Vars(mstr,false)
        else
          PrintErr('SUB, FUNCTION or identifier expected.');
      end
      else begin
        PrintErr('Member define expected.');
        NextToken;
      end;
      if Result<0 then Result:=lv;
    end;
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
      NextToken;
    ExpectKW;
  end;
end;

function TJEBasicParser.ParseDim:Integer;
begin
  // DIM a, b(), c(10)
  PushDefStatement(' VAR','VAR');
  Result:=FCurNodeLevel;
  NextToken;
  ParseVarMemberBody;
  StatementWithDefEnd;
end;

function TJEBasicParser.ParseDo;
var
  OneLine,IsConst:Boolean;
  Lv,idx,idx2:Integer;
  EndExpr,ZeroExpr:TJENode;
  mstr:String;
begin
(*
Do [{While | Until} condition]
[statements]
[Exit Do]
[statements]
Loop 
也可以使用下面的语法：

Do
[statements]
[Exit Do]
[statements]
Loop [{While | Until} condition]
*)
  PushStatement(JEF_While);
  Result:=FCurNodeLevel;
  NextToken;
  ExpectKW;
  idx:=CurToken.KWIdx1-1;
  if (idx=BASKW_WHILE) or (idx=BASKW_UNTIL) then
  begin
    idx2:=idx;
    if idx=BASKW_UNTIL then
      ModifyFuncName(Result,JEF_WhileNot);
    NextToken;
  end
  else begin
    ModifyFuncName(Result,JEF_Repeat);
    idx2:=-1;
  end;
  if idx2>=0 then
    if ParseExpr<0 {ParseStatements(true)<0} then
    begin
      PrintErr('Expect expression here.');
      PushBool(true);
    end;
  while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
    NextToken;
  GoNextStatementParam;
  ParseStatements;
  ExpectKW;
  if CurToken.KWIdx1=BASKW_LOOP+1 then
    NextToken
  else
    PrintErr('Expect "LOOP" here.');
  if idx2<0 then
  begin
    ExpectKW;
    if (idx=BASKW_WHILE) or (idx=BASKW_UNTIL) then
    begin
      if idx=BASKW_WHILE then
        ModifyFuncName(Result,JEF_Loop);
      NextToken;
    end
    else
      PrintErr('Expect "WHILE" or "UNTIL" here.');
    if ParseExpr<0 {ParseStatements(true)<0} then
    begin
      PrintErr('Expect expression here.');
      PushBool(true);
    end;
  end;
  StatementEnd;
  NextToken;
end;

function TJEBasicParser.ParseExit;
var
  idx:Integer;
begin
  PushStatement(JEF_Exit);
  Result:=FCurNodeLevel;
  NextToken;
  ExpectKW;
  idx:=CurToken.KWIdx1-1;
  if idx>=0 then
  begin
    if (idx=BASKW_DO) or (idx=BASKW_FOR) then
      ModifyFuncName(Result,'BREAK');
    NextToken;
  end
  else
    PrintErr('Except keyword here.');
  StatementEnd;
end;

function TJEBasicParser.ParseFor;
var
  OneLine,IsConst,IsForEach:Boolean;
  Lv,idx:Integer;
  StepVal:Double;
  CycleVar,StepExpr,EndExpr,ZeroExpr:TJENode;
  mstr:String;
begin
//
//For counter = start To end [Step step]
//[statements]
//[Exit For]
//[statements]
//Next
//
//For Each element In group
//[statements]
//[Exit For]
//[statements]
//Next [element]
//
  PushStatement('FOR');
  Result:=FCurNodeLevel;
  NextToken;
  ExpectKW;
  IsForEach:=CurToken.KWIdx1=BASKW_EACH+1;
  if IsForEach then
  begin
    ModifyFuncName(Result,'FOREACH');
    NextToken;
    if ParseExpr<0 then
      PrintErr('Expect expr here.');
  end
  else if not FTransForToFOR then       
    ModifyFuncName(Result,'FORTO');;
  if not IsForEach then
  begin
    if ParseStatements(true)<0 then
      PrintErr('Expect expr here.');
    CycleVar:=ExtractVarInSetValueLv(Result+1);
    if CycleVar=nil then
      PrintErr('No cycle var in FOR!')
    else
      ; //PrintErr(CycleVar.toString);
    if CurToken.KWIdx1=BASKW_TO+1 then
    begin
      GoNextStatementParam;
      NextToken;
    end
    else //if CurToken.KWIdx1=0 then
    begin
      PrintErr('Expect "TO" here.');
      if CurToken.Kind=tkEND then exit;
      GoNextStatementParam;
    end;
    if ParseExpr<0 then
    begin
      PrintErr('Expect expression after "TO".');
      PushFloat(0);
    end;
    StepExpr:=nil;
    if CurToken.KWIdx1=BASKW_STEP+1 then
    begin
      GoNextStatementParam;
      NextToken;
      lv:=ParseExpr;
      IsConst:=false;
      if FTransForToFOR and (CycleVar<>nil) then
      begin
        if (lv>0) and (lv-1=Result) then
        begin
          FCurNodeLevel:=Result;
          with TJEBranch(FLevelNodes[Result].Obj) do
          begin
            StepExpr:=RemoveLastKey;
            MakeVarInc(CycleVar.Clone,StepExpr);
            IsConst:=IsConstNumber(StepExpr,StepVal);
          end;
        end;
      end;
    end
    else if not FTransForToFOR then
    begin
      GoNextStatementParam;
      PushFloat(1);
    end;
    if CycleVar<>nil then
    begin
      if FTransForToFOR then
      begin
        if StepExpr=nil then
        begin
          FCurNodeLevel:=Result;
          MakeVarInc1(CycleVar.Clone);
          IsConst:=true;
          StepVal:=1;
        end;
        //将 to 表达式转换为 <= 或 >= 比较表达式
        // step 参数可以是正数或负数。step 参数值决定循环的执行情况，如下所示：
        //   值        如果 ... 则循环执行
        // 正数或 0    counter <= end
        // 负数        counter >= end
        FCurNodeLevel:=Result;
        with TJEBranch(FLevelNodes[Result].Obj) do
        begin
          if IsConst then
          begin
            if StepVal>=0 then
              mstr:='<='
            else
              mstr:='>=';
            Put(JEP_Param2,MakeOpExpr(mstr,CycleVar.Clone,Remove(JEP_Param2)));
          end
          else begin
            //非常量的情况  ((StepVal>=0) and (i<=EndVal)) or ((StepVal<0) and (i>=EndVal))
            EndExpr:=Remove(JEP_Param2);
            ZeroExpr:=MakeZeroVal;
            Put(JEP_Param2,
              MakeOpExpr('OR',
                MakeOpExpr('AND',
                  MakeOpExpr('>=',StepExpr.Clone,ZeroExpr.Clone),
                  MakeOpExpr('<=',CycleVar.Clone,EndExpr.Clone)),
                MakeOpExpr('AND',
                  MakeOpExpr('<',StepExpr.Clone,ZeroExpr.Clone),
                  MakeOpExpr('>=',CycleVar.Clone,EndExpr.Clone))));
            EndExpr.Free;
            ZeroExpr.Free;
          end;
          //恢复各参数的次序
          Put(JEP_Param3,Remove(JEP_Param3));
        end;
      end;
    end;
  end
  else begin
    if CurToken.KWIdx1=BASKW_IN+1 then
    begin
      GoNextStatementParam;
      NextToken;
    end
    else begin
      PrintErr('Expect "IN" here.');
      if CurToken.Kind=tkEND then exit;
      GoNextStatementParam;
    end;
    if ParseExpr<0 then
    begin
      PrintErr('Expect expr after "IN".');
      PushFloat(0);
    end;
  end;
  GoNextStatementParam;
  OneLine:=true;
  while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
  begin
    OneLine:=false;
    NextToken;
  end;
  ParseStatements(OneLine);
  ExpectKW;
  if CurToken.KWIdx1=BASKW_NEXT+1 then
  begin
    NextToken;
    while not (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
    begin
      if CurToken.Kind=tkEND then exit;
      NextToken;
    end;
    StatementEnd;
    NextToken;
  end
  else begin
    PrintErr('Expect "NEXT" here.');
    StatementEnd;
  end;
end;

function TJEBasicParser.ParseFunction;
var
  IDStr:String;
begin
  PushStatement(' '+JED_Func);
  Result:=FCurNodeLevel;
  NextToken;
  ParseMethodBody('FUNCTION',DefaultVisibility,IDStr);
  ReplaceVarToResult(FLevelNodes[Result].Obj,IDStr);
end;

function TJEBasicParser.ParseGoto;
begin
  Result:=FCurNodeLevel;
  NextToken;
end;

function TJEBasicParser.ParseIf;
var
  OneLine,LvChange,ElseOrEnd:Boolean;
  idx:Integer;
begin
  // if ... then
  //   ...
  // elseif ... then
  //   ...
  // else
  //   ...
  // end if
  PushStatement('IF');
  Result:=FCurNodeLevel;
  NextToken;
  ParseExpr;
  LvChange:=false;
  if CurToken.KWIdx1=0 then
  begin
    PrintErr('Expect "THEN" here.');
    OneLine:=true;
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
    begin
      OneLine:=false;
      NextToken;
    end;
    if CurToken.Kind=tkEND then exit;
    GoNextStatementParam;
  end
  else if CurToken.KWIdx1=BASKW_THEN+1 then
  begin
    GoNextStatementParam;
    NextToken;
    if CurToken.Kind in [tkEOLN,tkLINEDIV,tkRawText] then
    begin
      OneLine:=false;
      if CurToken.Kind<>tkRawText then  //2012-12-23
        NextToken;
    end
    else
      OneLine:=true;
  end;
  if LvChange then
  begin
    idx:=ExpectKW-1;
    ElseOrEnd:=(idx=BASKW_ELSEIF) or (idx=BASKW_ELSE) or(idx=BASKW_END);
  end
  else
    ElseOrEnd:=false;
  if not ElseOrEnd then
    TryParseStatements(OneLine);
  ExpectKW;
  if CurToken.KWIdx1=BASKW_ELSEIF+1 then
  begin
    //ModifyFuncName(Result,'IFELSE');  //2012-05-29
    repeat
      if not OneLine and not (LastToken.Kind in [tkEOLN,tkLINEDIV]) then
        PrintErr('"ELSEIF" must be first statement on the line.');
      GoNextStatementParam;
      NextToken;
      ParseExpr;
      if CurToken.KWIdx1=0 then
      begin
        PrintErr('Expect "THEN" here.');
        while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
          NextToken;
        if CurToken.Kind=tkEND then exit;
        GoNextStatementParam;
      end
      else if CurToken.KWIdx1=BASKW_THEN+1 then
      begin
        GoNextStatementParam;
        NextToken;
      end;
      while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
        NextToken;
      TryParseStatements(OneLine);  //ParseStatements(OneLine);
      ExpectKW;
    until CurToken.KWIdx1<>BASKW_ELSEIF+1;
  end;
  if CurToken.KWIdx1=BASKW_ELSE+1 then
  begin
    if not OneLine and not (LastToken.Kind in [tkEOLN,tkLINEDIV]) then
      PrintErr('"ELSE" must be first statement on the line.');
    GoNextStatementParam;
    NextToken;
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
      NextToken;
    ParseStatements(OneLine);
  end
  else if (CurToken.Kind in [tkEOLN,tkLINEDIV]) then
  begin
    if OneLine then
    begin
      StatementEnd;
      NextToken;
      exit;
    end;
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
      NextToken;
    ExpectKW;
  end;
  if OneLine and (CurToken.Kind in [tkEOLN,tkLINEDIV]) then
  begin
    //NextToken;
  end
  else if CurToken.KWIdx1=BASKW_END+1 then
  begin
    NextToken;
    ExpectKW;
    if CurToken.KWIdx1=BASKW_IF+1 then
      NextToken
    else
      PrintErr('Expect "IF" here.');
  end
  else
    PrintErr('Expect "END IF" here.');
  StatementEnd;
end;

function TJEBasicParser.ParseMethodBody(const EndStr, PerfixStr: String; out IdStr: String): Integer;
begin
  Result:=-1;
  IdStr:=ParseIdentifier;
  if IdStr<>'' then
  begin
    RegUserFunc(IdStr);
    Result:=FCurNodeLevel-1;
    if CurToken.Token='(' then
    begin
      GoNextStatementParam;
      NextToken;
      Inc(FCurBlockCount);
      MakeArrayParam(JEDN_Params);
      ParseParameterDef;
      if CurToken.Token<>')' then
        PrintErr('Expect ")" here.')
      else
        NextToken;
      Dec(FCurBlockCount);
    end;
    if CurToken.Kind=tkComment then
    begin
      PushCommentToken(false);
      NextToken;
    end;
    if not (CurToken.Kind in [tkEOLN,tkLINEDIV]) then
    begin
      PrintErr('Expect EOLN here.');
      while not (CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND]) do
        NextToken;
    end;
    GoNextStatementParam;
    NextToken;
    ParseStatements;
  end;
  if Result>=0 then
  begin
    FCurNodeLevel:=Result;
    RenameLastNodeAsBody;
    PushPerfix(PerfixStr);
  end;
  if CurToken.KWIdx1=BASKW_END+1 then
  begin
    NextToken;
    ExpectKW;
    if CurToken.CaseOKToken=EndStr then
    begin
      StatementEnd;
      NextToken;
    end
    else begin
      PrintErr('Expect "'+EndStr+'" here.');
      StatementEnd;
    end;
  end
  else begin
    PrintErr('Expect "END" here.');
    StatementEnd;
  end;
end;

function TJEBasicParser.ParseNew: Integer;
begin
  // new Student
  // new Car("MyCar", clBlue)
  PushStatement('NEW');
  Result:=FCurNodeLevel;
  NextToken;
  ParseExpr;
  StatementEnd(false);
end;

function TJEBasicParser.ParseOn;
begin
  Result:=FCurNodeLevel;
  NextTokenKW;
  if CurToken.KWIdx1<>BASKW_ERROR+1 then
  begin
    PrintErr('Expect "ERROR".');
    exit;
  end;
  PushStatement(MakeLanFunc('ONERROR'));
  NextTokenKW;
  if CurToken.KWIdx1=BASKW_RESUME+1 then
  begin
    NextTokenKW;
    if CurToken.KWIdx1<>BASKW_NEXT+1 then
      PrintErr('Expect "NEXT".')
    else
      NextToken;
    PushNull;
  end
  else if CurToken.KWIdx1=BASKW_GOTO+1 then
  begin
    NextToken;
    ParseExpr;
  end
  else
    PrintErr('Expect "RESUME" or "GOTO".');
  StatementEnd;
end;

function TJEBasicParser.ParseOption;
begin
  Result:=FCurNodeLevel;
  NextTokenKW;
  if CurToken.KWIdx1<>BASKW_EXPLICIT+1 then
  begin
    PrintErr('Expect "EXPLICIT".');
    exit;
  end;
  PushStatement(MakeLanFunc('OPTIONEXPLICIT'));
  StatementEnd;
end;

function TJEBasicParser.ParseParameterDef: Integer;
var
  mstr:String;
begin
  Result:=-1;
  while CurToken.Kind=tkWORD do
  begin
    ExpectKW;
    if (CurToken.KWIdx1=BASKW_BYREF+1) or (CurToken.KWIdx1=BASKW_BYVAL+1) then
    begin
      mstr:=CurToken.CaseOKToken;
      NextToken;
      PrintErr('Perfix '+mstr+' ignored...');
    end;
    if ParseIdentifier='' then
      PrintErr('Except identifier here.')
    else if Result<0 then         
      Result:=FCurNodeLevel;
    if CurToken.Token<>',' then break;
    GoNextParam;
    NextToken;
  end;
  if CurToken.Token<>')' then
    PrintErr('Except ")" here.');
end;

function TJEBasicParser.ParseReDim;
var
  IdNode:TJENode;
  Id:String;
begin
  // REDIM a(0),b(200)
  PushStatement(MakeLanFunc('REDIM'));
  Result:=FCurNodeLevel;
  NextToken;
  while true do
  begin
    Id:=ParseIdentifier;
    if Id='' then break;
    RegUserVarArray(Id);
    if CurToken.Token='(' then
    begin
      PushBracket('(');
      NextToken;
      while true do
      begin
        if ParseExpr<0 then
          PrintErr('Except integer expression.');
        //2012-06-06 Redim 的目标长度不必是常量，可以是复杂的表达式
        //2012-06-07 Redim 支持多维数组  REDIM AY(100,2,3)
        if CurToken.Token<>',' then break;
        GoNextParam;
        NextToken;
      end;
      if CurToken.Token=')' then
      begin
        PushBracket(')');
        NextToken;
      end
      else begin
        PushBracket(')');
        PrintErr('Except ")".');
      end;
    end
    else
      PrintErr('Except "(".');
    if CurToken.Token<>',' then break;
    GoNextStatementParam;
    NextToken;
  end;
  if not (CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND]) then
    PrintErr('Except identifier here.');
  StatementEnd;
end;

function TJEBasicParser.ParseSelect;
var
  Expr:TJENode;
  i,n:Integer;
begin
  PushStatement(JEF_Case);
  Result:=FCurNodeLevel;
  NextToken;
  ExpectKW;
  if CurToken.KWIdx1<>BASKW_CASE+1 then
  begin
    PrintErr('Except "case" here.');
    StatementEnd;
    exit;
  end;
  NextToken;
  ParseExpr;
  while true do
  begin
    JumpOverLineBreaks;
    ExpectKW;
    if CurToken.KWIdx1<>BASKW_CASE+1 then
    begin
      if CurToken.KWIdx1=BASKW_END+1 then break;
      PrintErr('Except "case" or "end" here.');
      StatementEnd;
      exit;
    end;
    NextToken;
    ParseExpr;
    GoNextStatementParam;
    JumpOverLineBreaks;
    ParseStatements;
    FCurNodeLevel:=Result;
    GoNextStatementParam;
  end;
  if CurToken.KWIdx1<>BASKW_END+1 then
    PrintErr('Except "case" or "end" here.')
  else begin
    NextToken;
    ExpectKW;
    if CurToken.KWIdx1<>BASKW_SELECT+1 then
      PrintErr('Except "select" here.')
    else
      NextToken;
  end;
  StatementEnd;  
end;

function TJEBasicParser.ParseSet;
begin
  PushStatement(MakeLanFunc('SET'));
  NextToken;
  ParseVar;
  while CurToken.Token<>'=' do
  begin
    PrintErr('Expect "=" here.');
    if CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND] then break;
    NextToken;
  end;
  if CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND] then
  begin
    PrintErr('Expect expression here.');
    StatementEnd;
    exit;
  end;
  PushOp2(JOP_SetValue,GetOpRank('='));
  NextToken;
  ParseExpr;
  StatementEnd;
end;

function TJEBasicParser.ParseSub;
var
  mstr:String;
begin
  PushStatement(' '+JED_Proc);
  Result:=FCurNodeLevel;
  NextToken;
  ParseMethodBody('SUB',DefaultVisibility,mstr);
end;

function TJEBasicParser.ParseVarMemberBody: Integer;
var
  mstr:String;
begin
  // DIM a, b(), c(10)
  // Public aa, bb
  Result:=FCurNodeLevel;
  while ParseIdentifier<>'' do
  begin
    mstr:=LastToken.CaseOKToken;
    if CurToken.Token='(' then
    begin
      RegUserVarArray(mstr);
      PushBracket('[');
      NextToken;
      if (ParseExpr>0) and (FCurNodeLevel>Result+2) then
        PrintErr('Warning: Except integer constant.',Result+1);
      if CurToken.Token=')' then
      begin
        PushBracket(']');
        NextToken;
      end
      else begin
        PushBracket(']');
        PrintErr('Except ")".');
      end;
    end
    else
      RegUserVar(mstr);
    if CurToken.Token<>',' then break;
    GoNextStatementParam;
    NextToken;
  end;
  if not (CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND]) then
    PrintErr('Except identifier here.');
end;

function TJEBasicParser.ParseWhile;
var
  Lv,idx:Integer;
  StepVal:Double;
  CycleVar,StepExpr,EndExpr,ZeroExpr:TJENode;
  mstr:String;
begin
//
//While condition
// [statements]
//Wend
//
  PushStatement('WHILE');
  Result:=FCurNodeLevel;
  NextToken;
  if ParseExpr<0 then
    PrintErr('Expect expr here.');
  if CurToken.Kind in [tkEOLN,tkLINEDIV] then
  begin
    while CurToken.Kind in [tkEOLN,tkLINEDIV] do
      NextToken;
  end
  else
    PrintErr('Expect EOLN here.');
  GoNextStatementParam;
  ParseStatements(false);
  ExpectKW;
  if CurToken.KWIdx1=BASKW_WEND+1 then
  begin
    NextToken;
    while not (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
    begin
      if CurToken.Kind=tkEND then exit;
      NextToken;
    end;
    StatementEnd;
    NextToken;
  end
  else begin
    PrintErr('Expect "WEND" here.');
    StatementEnd;
  end;
end;

function TJEBasicParser.Parse_Func(const PerfixStr: String; NeedNext: Boolean):Integer;
var
  IdStr:String;
begin
  PushStatement(' FUNCTION');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseMethodBody('FUNCTION',PerfixStr,IdStr);
  ReplaceVarToResult(FLevelNodes[Result].Obj,IdStr);
end;

function TJEBasicParser.Parse_Sub(const PerfixStr: String; NeedNext: Boolean):Integer;
var
  IDStr:String;
begin
  PushStatement(' PROCEDURE');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseMethodBody('SUB',PerfixStr,IDStr);
end;

function TJEBasicParser.Parse_Vars(const PerfixStr: String; NeedNext: Boolean):Integer;
begin
  PushDefStatement(' VAR','VAR');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseVarMemberBody;
  FCurNodeLevel:=Result;
  PushPerfix(PerfixStr);
  StatementWithDefEnd;
end;

procedure TJEBasicParser.PushRawText(const Text: String; AddLnBreak: Boolean);
var
  str1,str2,mstr:String;
  i,n0,n1,p1,p2,pc,TxtLen:Integer;
begin
  //要扫描 <!--  --> 内的 #include 指令
  n0:=1; n1:=1;
  TxtLen:=Length(Text);
  while true do
  begin
    p1:=FastPos(Text,'<!--',TxtLen,4,n1);
    if p1=0 then break;
    p2:=FastPos(Text,'-->',TxtLen,3,p1);
    if p2=0 then break;
    //Scan for #include xx
    pc:=FastPos(Text,'#',p2-10,1,p1+4);
    if pc>0 then
    begin
      mstr:=LowerCase(Copy(Text,pc+1,8));
      if mstr='include ' then
      begin
        if p1>n0 then
        begin
          PushOutStrVal(Copy(Text,n0,p1-n0),AddLnBreak);
          PushLineBreakOp;
        end;
        n0:=p2+3; n1:=n0;
        i:=pc+9;
        while (Text[i]=' ') and (i<p2) do Inc(i);
        while (Text[i]<>'=') and (i<p2) do Inc(i);
        if i<p2 then
        begin
          if ScanStrParam(Text,i+1,p2,mstr)>0 then
          begin
            PushFunc(JEF_Include);
            PushString(mstr);
            PushLineBreakOp;
          end;
        end;
      end
      else
        n1:=p2+3;
    end
    else begin
      n1:=p2+3;
    end;
  end;
  if n0=1 then
    PushOutStrVal(Text,AddLnBreak)
  else
    PushOutStrVal(Copy(Text,n0,TxtLen),AddLnBreak);
end;

procedure TJEBasicParser.SetTransForToFOR(const Value: Boolean);
begin
  FTransForToFOR := Value;
end;

function TJEBasicParser.WordIsBuildInFunc(const AName: String): Boolean;
begin
  Result:=LanFuncs.IndexOf(AName)>=0;
end;

function TJEBasicParser.WordIsFunc: Boolean;
begin
  Result:=false;
end;

initialization
  InitOpRank;
  InitSingleOp2Func;
  InitLanFuncs;
  TJEParser.RegisterParser(TJEBasicParser);

finalization
  OpRanks.Free;
  LanFuncs.Free;

end.
