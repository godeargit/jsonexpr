unit JETransPHP;

{
}

interface

uses
  Classes, JEParser, uJSON, UJSONExpr, SysUtils;

type
  TJEPHPParser=class(TJEParser)
  private
    FTransForToFOR: Boolean;
    //FASPMode: Boolean;
    FInEchoExpr: Boolean;
    procedure SetTransForToFOR(const Value: Boolean);
  protected
    FConsts:TStrings;
    FInDefId:Boolean;
    procedure InitParser; override;
    function GetStrChCharSet:TJECharSet; override;
    function GetMathOpCharSet:TJECharSet; override;
    function GetSpecialCharSet:TJECharSet; override;
    function GetLineCommetOps:TStrings; override;
    function GetOpInfo(var Op: String; out IsOp2: Boolean; out Rank: Byte):Boolean; override;
    function GetOpRank(const Op: String):Byte; override;
    function GetSetValOpRank:Byte; override;
    function OnSpecialHeadChar(ACh: Char; APos: Integer):Boolean; override;
    function OnKeyword(const Str: String; KWIdx: Integer):Integer; override;
    function OnPerfix(const Str: String; KWIdx: Integer):Integer; override;
    function DefaultVisibility:String; override;
    function ParseCase:Integer;
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
    function ParseReDim:Integer;
    function ParseSwitch:Integer;
    function ParseSet:Integer;
    function ParseSub:Integer;
    function ParseWhile:Integer;
    function VarIsArray:Boolean; override;
    function WordIsFunc:Boolean; override;
    function WordIsBuildInFunc(const AName: String):Boolean; override;
    function ParseDefines:Integer; override;
    function ParseParameterDef:Integer; override;
    function ParseMethodBody(const EndStr, PerfixStr: String):Integer;
    function Parse_Vars(const PerfixStr: String; NeedNext: Boolean):Integer;
    function Parse_Sub(const PerfixStr: String; NeedNext: Boolean):Integer;
    function Parse_Func(const PerfixStr: String; NeedNext: Boolean):Integer;
    procedure AddConst(const AName: String);
    procedure BeforeTransTree; override;
    function GetLanOp(const StdOp: String): String; override;
    function SetValOp:String; override;
    function ParamDefDiv:String; override;
    function LineDivStr:String; override;
    function AddLineEnd(const Str: String):String; override;
    function GetJETreePerfix:String; override;
    function GetJETreePostfix:String; override;
    function GetPerfixStr(JObj: TJEBranch):String; override;
    function TransLanStatmentFunc(JObj: TJEBranch; const Lan, Op: String; Ident: Integer):String; override;
    function TransOtherLanBuildInFunc(JObj: TJEBranch; const Op, ALan: String;
      Ident: Integer):String; override;
    function TransTypeVal(const ValStr: String; Ident: Integer):String; override;
    function TransINCLUDE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIS(JObj: TJEBranch; Ident: Integer):String; override;
    function TransDEC(JObj: TJEBranch; Ident: Integer):String; override;
    function TransINC(JObj: TJEBranch; Ident: Integer):String; override;
    function TransNEW(JObj: TJEBranch; Ident: Integer):String; override;
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
    function TransCONTINUE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransLOOP(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILENOT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransREPEAT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransPROCEDURE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFUNCTION(JObj: TJEBranch; Ident: Integer):String; override;
    function TransVAR(JObj: TJEBranch; Ident: Integer):String; override;
    function TransVAR1(JObj: TJENode; Ident: Integer; InClass, HasPerfix: Boolean):String; 
    function TransCONST(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCONST1(JObj: TJEBranch; Ident: Integer):String; 
    function TransCLASS(JObj: TJEBranch; Ident: Integer):String; override;
    function StdMakeBlock(const Str, IdentStr:String; NoHeadIdent: Boolean):String; override;
    function StdMakeIfThen(const Expr, IdentStr: String):String; override;
    function StdMakeElseIf(const Expr, IdentStr: String):String; override;
    function StdMakeElseEnd(IsElse: Boolean; const IdentStr: String):String; override;
    function GetArrayDefStr(ANode: TJEBranch; CurDimIdx: Integer=1):String; override;
    class function GetFilePostfixs: TStrings; override;
  public
    class function CommaDivArrayDim:Boolean; override;
    function StrForLan(const Str: String):String; override;
    function VarForLan(const VarName: String):String; override;
    function TransCommonOp(JObj: TJEBranch; const Op: String; Ident: Integer; PrnRank: Integer=-1):String; override;
    class function Lan:ShortString; override;
    property TransForToFOR:Boolean read FTransForToFOR write SetTransForToFOR;
    procedure DoInit; override;
    constructor Create; override;
    destructor Destroy; override;
    { 是否为不含需转义字符的简单字符串（可以含'及\符号） }
    class function IsSimpleString(const Str: String):Boolean;
    class function SimpleStrToPHPStr(const S: String):String;  //仅对'及\进行转义
    class function CommonStrToPHPStr(const S: String):String;
    class function StrValToPHPStr(const S: String):String;
    function PackSrc(const Source: String; out WordCount: Integer):String; override;
  end;

implementation

{
算术运算符
+  -  *  /  %

赋值运算符
=、+=、-=、*=、/=、.=

位运算符
&  |  ^  ~  <<  >>  

比较运算符
==  ===  !=  <>  !==  <  >  <=  >=

出错控制符
@

自增/自减符
++  --

逻辑运算符
and  or  xor  !  &&  ||  

字符串运算符
.  .=

类运算符
instanceof 

其它运算符
()  []  ->  ,

运算符的优先级
( )  
[ ] 
->  
!  
~  
++  -- 
-
(强制转化类型)
*  /  %
-   
<<  >>
<  <=  >  >=
==  !=
&
^
|
&&
||
?:
=  +=  -=  *=  /=  %=  &=  ^=  |=  <<=  >>=
,
}

const
  JPHP_MEMBER='->';
  JPHP_XOR='xor';
  JPHP_AND='and';
  JPHP_OR='or';
  JPHP_NOT='!';
  //JPHP_SHR='SHR';
  //JPHP_SHL='SHL';
  JPHP_SetValue='=';

var
  SingleOpMap:array [Char] of String;
  PHPKW_AS,
  PHPKW_BREAK,
  //PHPKW_BYVAL,
  PHPKW_CASE,
  PHPKW_CLASS,
  PHPKW_CONST,
  PHPKW_DEFINE,
  PHPKW_DIE,
  PHPKW_DO,
  PHPKW_ELSE,
  PHPKW_ELSEIF,
  PHPKW_ERROR,
  PHPKW_EXIT,
  PHPKW_FOR,
  PHPKW_FUNCTION,
  PHPKW_GOTO,
  PHPKW_IF,
  PHPKW_IN,
  PHPKW_PRIVATE,
  PHPKW_PROTECTED,
  PHPKW_PUBLIC,
  PHPKW_RETURN,
  PHPKW_SWITCH,
  PHPKW_VAR,
  PHPKW_WHILE
  :Integer;
  BASOP_AND,
  BASOP_OR,
  BASOP_XOR,
  BASOP_NOT,
  BASOP_MOD
  :Integer;
  BASCV_FALSE,
  BASCV_TRUE
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

{
下表从低到高列出了运算符的优先级。
结合方向 运算符
左 ,
左 or
左 xor
左 and
右 print
右 = += -= *= /= .= %= &= |= ^= ~= <<= >>=
左 ? :
左 ||
左 &&
结合方向 运算符
左 |
左 ^
左 &
无 == != === !==
无 < <= > >=
左 << >>
左 + - .
左 * / %
右 ! ~ ++ -- (int) (float) (string) (array) (object) @
右 [
无 new 
}
procedure InitOpRank;
var
  r:Byte;
begin
  OpRanks:=TStringList.Create;
  //OpRank[OpCh_Func]:=OpRank_Func;  //Function
  r:=240;
  OpRank['[']:=r; Dec(r,10);
  OpRank['!']:=r; OpRank['~']:=r; OpRank['@']:=r; OpRanks.AddObject('++',TObject(r)); OpRanks.AddObject('--',TObject(r)); Dec(r,10);
  OpRank['*']:=r; OpRank['/']:=r; OpRank['%']:=r; Dec(r,20);
  OpRank['+']:=r; OpRank['-']:=r; OpRank['.']:=r; Dec(r,10);
  OpRanks.AddObject('<<',TObject(r)); OpRanks.AddObject('>>',TObject(r)); Dec(r,10);
  {RK_Shift:=r;}    Dec(r,10);   // << >>  <<<  >>>
  OpRank['=']:=r; OpRank['>']:=r; OpRank['<']:=r;
  OpRanks.AddObject('>=',TObject(r));
  OpRanks.AddObject('<=',TObject(r)); Dec(r,10);
  OpRanks.AddObject('==',TObject(r)); OpRanks.AddObject('!=',TObject(r));
  OpRanks.AddObject('===',TObject(r)); OpRanks.AddObject('!==',TObject(r));
  Dec(r,20);
  OpRank['&']:=r; Dec(r,10);
  OpRank['^']:=r; Dec(r,10);
  OpRank['!']:=r; Dec(r,10);
  OpRanks.AddObject('&&',TObject(r)); OpRanks.AddObject('||',TObject(r)); Dec(r,10);
  OpRank['?']:=r; Dec(r,20);
  OpRank['=']:=r; Dec(r,10);
  OpRanks.AddObject('AND',TObject(r)); Dec(r,10);
  OpRanks.AddObject('XOR',TObject(r)); Dec(r,10);
  OpRanks.AddObject('OR',TObject(r)); Dec(r,10);
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
  SingleOpMap['.']:=JPHP_MEMBER;
  SingleOpMap['&']:=JPHP_AND;
  SingleOpMap['|']:=JPHP_OR;
  SingleOpMap['^']:=JPHP_XOR;
  SingleOpMap['!']:=JPHP_NOT;
end;

{ TJETransPHP }

procedure TJEPHPParser.AddConst(const AName: String);
begin
  FConsts.Add(AName);
end;

function TJEPHPParser.AddLineEnd(const Str: String): String;
begin
  if Str='' then
    Result:=''
  else if not (Str[Length(Str)] in [';','}',' ',#13,#10]) then
    Result:=Str+';'
  else
    Result:=Str;
end;

procedure TJEPHPParser.BeforeTransTree;
begin
  inherited;
end;

class function TJEPHPParser.CommaDivArrayDim: Boolean;
begin
  Result:=false;
end;

class function TJEPHPParser.CommonStrToPHPStr(const S: String): String;
var
   b,c: char;
   i, len: integer;
   sb, t: string;
begin
  c:=#0;
  len:=Length(s);
  t:='';
  sb:=sb +'"';
  i:=1;
  while i<=len do
  begin
    b:=c;
    c:=s[i];
    case c of
      '\', '"', '$':
      begin
        sb:=sb + '\';
        sb:=sb + c;
      end;
      //Output special character smaller than space.
      #0..#8: sb:=sb + '\x0'+Char(Byte('0')+Byte(c));
      #9: sb:=sb + '\t';   //水平方向的 tab
      #10: sb:=sb + '\n';  //换行
      #11: sb:=sb + '\v';  //竖直方向的 tab (VT or 0x0B (11) in ASCII) (since PHP 5.2.5)
      #12: sb:=sb + '\f';  //换页 (FF or 0x0C (12) in ASCII) (since PHP 5.2.5)
      #13: sb:=sb + '\r';  //回车
      else
      begin
        sb:=sb + c;
      end;
    end;
    Inc(i);
  end;
  sb:=sb + '"';
  Result:=sb;
end;

constructor TJEPHPParser.Create;
begin
  inherited;
  FConsts:=TStringList.Create;
  RootNeedValue:=false;
end;

destructor TJEPHPParser.Destroy;
begin
  FConsts.Free;
  inherited;
end;

function TJEPHPParser.GetArrayDefStr(ANode: TJEBranch;
  CurDimIdx: Integer): String;
var
  NumStr:String;
begin
  if CurDimIdx=1 then
  begin
    //考虑从Basic这样的以上界为声明的语言转过来
    if (TreeBaseLanClass<>nil) and TreeBaseLanClass.ArrayUseUBound then
      NumStr:=TransANode_Inc1(ANode.Opt(JEP_Param1),0)
    else
      NumStr:=TransANode(ANode.Opt(JEP_Param1),0);
    Result:='array_fill(0,'+NumStr+','
      +GetArrayDefStr(ANode,CurDimIdx+1)+')';
  end
  else if CurDimIdx>1 then
  begin
    //考虑从Basic这样的以上界为声明的语言转过来
    if (TreeBaseLanClass<>nil) and TreeBaseLanClass.ArrayUseUBound then
      NumStr:=TransANode_Inc1(ANode.Opt(JEP_ParamHeader+IntToStr(CurDimIdx)),0)
    else
      NumStr:=TransANode(ANode.Opt(JEP_ParamHeader+IntToStr(CurDimIdx)),0);
    if NumStr<>'' then
      Result:='array_fill(0,'+NumStr+','+GetArrayDefStr(ANode,CurDimIdx+1)+')'
    else
      Result:='null';
  end
  else
    Result:='';
end;

class function TJEPHPParser.GetFilePostfixs: TStrings;
begin
  Result:=inherited GetFilePostfixs;
  Result.Add('php');
end;

function TJEPHPParser.GetJETreePerfix: String;
begin
  Result:='<?php'#13#10
end;

function TJEPHPParser.GetJETreePostfix: String;
begin
  //if ASPMode then
    Result:=#13#10'?>'
  //else
  //  Result:='';
end;

function TJEPHPParser.GetLanOp(const StdOp: String): String;
begin
  Result:=StdOp;
  case Length(StdOp) of
    1:
    begin
      case StdOp[1] of
        '=': Result:='==';
      end;
    end;
    2:
    begin
      if StdOp='OR' then
        Result:='||';
    end;
    3:
    begin
      if StdOp='AND' then
        Result:='&&'
      else if StdOp='NOT' then
        Result:='!'
      else if StdOp='XOR' then
        Result:='^';
    end;
  end;
end;

function TJEPHPParser.GetOpRank(const Op: String): Byte;
begin
  Result:=GetRank(Op);
  if Result>0 then exit;  
  Result:=inherited GetOpRank(Op);
end;

function TJEPHPParser.GetPerfixStr(JObj: TJEBranch): String;
var
  Z:TJENode;
  i:Integer;
  mstr:String;
begin
  Result:='';
  if JObj=nil then exit;
  Z:=JObj.Opt(JEP_Perfix);
  if Z=nil then exit;
  if Z is JSONArray then
  begin
    with JSONArray(Z) do
      for i:=0 to Pred(length()) do
      begin
        mstr:=LowerCase(get(i).toString);
        if i=0 then
          Result:=mstr
        else
          Result:=Result+' '+mstr;
      end;
  end
  else
    Result:=TransANode(Z,0);
end;

function TJEPHPParser.LineDivStr: String;
begin
  Result:=#13#10;
end;

function TJEPHPParser.PackSrc(const Source: String; out WordCount: Integer): String;
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
      '/':
        begin
          if i<Len then
          begin
            Inc(i);
            if Source[i]='*' then
            begin
              repeat
                Inc(i);
              until Source[i-1]='*';
              LastNone:=true;
              Status:=1;
            end
            else if Source[i]='/' then
            begin
              repeat
                Inc(i);
                if i>Len then exit;                
              until Source[i-1]<' ';
              continue;
            end;
            continue;
          end;
        end;
      '''':
        begin
          Inc(WordCount);
          Inc(n);
          mstr[n]:=Source[i];
          Inc(i);
          while true do
          begin
            if Source[i]='''' then
            begin
              if Source[i+1]='''' then
              begin
                mstr[n+1]:='''';
                mstr[n+2]:='''';
                Inc(n,2);
                Inc(i,2);
              end
              else begin
                Inc(n);
                mstr[n]:='''';
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

function TJEPHPParser.ParamDefDiv: String;
begin
  Result:=',';
end;

function TJEPHPParser.SetValOp: String;
begin
  Result:=JPHP_SetValue;
end;

class function TJEPHPParser.SimpleStrToPHPStr(const S: String): String;
var
  c: char;
  i, len: integer;
  sb, t: string;
begin
  len:=length(s);
  if len=0 then
  begin
    Result:= '''''';
    exit;
  end;

  c:=#0;
  t:='';
  sb:='''';
  i:=1;
  while i<=len do
  begin
    c:=s[i];
    case c of
      '\':
      begin
        sb:=sb+'\\';
      end;
      '''':
      begin
        sb:=sb+'\''';
      end;
      else
        sb:=sb+c;
    end;
    Inc(i);
  end;
  sb:=sb+'''';
  Result:=sb;
end;

function TJEPHPParser.StdMakeBlock(const Str, IdentStr: String;
  NoHeadIdent: Boolean): String;
begin
  if NoHeadIdent then
    Result:='{'#13#10+Str+#13#10+IdentStr+'}'
  else if Str<>'' then
    Result:=IdentStr+'{'#13#10+Str+#13#10+IdentStr+'}'
  else //空语句
    Result:=IdentStr+'{}';
end;

function TJEPHPParser.StdMakeElseEnd(IsElse: Boolean;
  const IdentStr: String): String;
begin
  if IsElse then
    Result:=#13#10+IdentStr+'else'
  else
    Result:='';
end;

function TJEPHPParser.StdMakeElseIf(const Expr, IdentStr: String): String;
begin
  Result:=#13#10+IdentStr+'elseif('+Expr+')'#13#10;
end;

function TJEPHPParser.StdMakeIfThen(const Expr, IdentStr: String): String;
begin
  Result:=IdentStr+'if('+Expr+')'#13#10;
end;

function TJEPHPParser.StrForLan(const Str: String): String;
begin
  Result:=StrValToPHPStr(Str);
  if Result='' then exit;
end;

class function TJEPHPParser.StrValToPHPStr(const S: String): String;
begin
  if IsSimpleString(S) then
    Result:=SimpleStrToPHPStr(S)
  else
    Result:=CommonStrToPHPStr(S);
end;

function TJEPHPParser.TransBREAK(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident);
  Z:=JObj.Opt(JEP_Param1);
  if Z<>nil then
    Result:=Result+'break '+TransANode(Z,0,0)+';'
  else
    Result:=Result+'break;';
end;

function TJEPHPParser.TransCASE(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
  IdentStr:String;
  Z,Z2:TJENode;
begin
  IdentStr:=IdentLen(Ident);
  Result:=IdentStr+'switch('+TransANode(JObj.Opt(JEP_Param1),0)+')'
    +#13#10+IdentStr+'{';
  i:=2;
  while True do
  begin
    Z:=JObj.Opt(JEP_ParamHeader+IntToStr(i));
    Z2:=JObj.Opt(JEP_ParamHeader+IntToStr(i+1));
    if Z=nil then break;
    if Z2=nil then
    begin
      Result:=Result+#13#10+IdentLen(Ident+1)+'default:'
        +#13#10+TransANode(Z,Ident+2);
      break;
    end;
    Result:=Result+#13#10+IdentLen(Ident+1)+'case '+TransANode(Z,0)
      +':'#13#10+TransANode(Z2,Ident+2)+#13#10+IdentLen(Ident+2)+'break;';
    Inc(i,2);
  end;
  Result:=Result+#13#10+IdentStr+'}';
end;

function TJEPHPParser.TransCLASS(JObj: TJEBranch; Ident: Integer): String;
var
  mstr:String;
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  if Z.ClassType<>_String then
  begin
    Result:='/* Invalid class name "'+TransANode(Z,0)+'" */';
    exit;
  end;
  Result:=Z.toString;
  mstr:=GetPerfixStr(JObj);
  if mstr<>'' then
    Result:=mstr+' class '+Result
  else
    Result:='class '+Result;
  Result:=IdentLen(Ident)+Result+' {'#13#10;
  mstr:=TransANode(JObj.Opt(JEDN_Members),Ident+1);
  if mstr<>'' then
    Result:=Result+mstr;
  Result:=Result+#13#10+IdentLen(Ident)+'}';
end;

function TJEPHPParser.TransCommonOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
var
  Ch:Char;
  mstr1,mstr2:String;
  r:Integer;
begin
  case Length(Op) of
    1:
    begin
      if Op[1] in ['\','.','&','|','^','!'] then
      begin
        if Op[1]='\' then
        begin
          r:=GetOpRank('/');
          mstr1:=TransANode(JObj.Opt(JEP_Param1),0,r);
          mstr2:=TransANode(JObj.Opt(JEP_Param2),0,r);
          Result:=IdentLen(Ident)+'(int)('+mstr1+'/'+mstr2+')';
          exit;
        end;
        Result:=IdentLen(Ident)+Trans_Mid(JObj,SingleOpMap[Op[1]],GetOpRank(Op),PrnRank);
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
      else if Op=JEOP_StrJoin then
      begin
        Result:=Trans_Mid(JObj,'.',GetOpRank(Op),PrnRank);
        exit;
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

function TJEPHPParser.TransCONST(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
begin
  Result:='';
  with JObj do
    for i:=1 to Pred(length()) do
    begin
      if Keys[i]=JEP_Perfix then continue;
      Result:=Result+IdentLen(Ident)+TransCONST1(TJEBranch(ValObjByIndex[i]),0);
    end;
end;

function TJEPHPParser.TransCONST1(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  if Z.ClassType<>_String then
  begin
    Result:='/* Invalid const name "'+TransANode(Z,0)+'" */';
    exit;
  end;
  Result:=Z.toString;
  AddConst(Result);
  Result:=IdentLen(Ident){+GetPerfixStr(JObj)}+'define('
    +QuotedStr(Result)+','
    +TransANode(JObj.Opt(JEP_Param2),0)+');';
end;

function TJEPHPParser.TransCONTINUE(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident);
  Z:=JObj.Opt(JEP_Param1);
  if Z<>nil then
    Result:=Result+'continue '+TransANode(Z,0,0)+';'
  else
    Result:=Result+'continue;';
end;

function TJEPHPParser.TransDEC(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransECHO(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=IdentLen(Ident)+'echo '+TransANode(JObj.Opt(JEP_Param1),0)+';';
end;

function TJEPHPParser.TransEVAL(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransEXIT(JObj: TJEBranch; Ident: Integer): String;
var
  lv:Integer;
begin
  //有返回值变量的函数内部exit需要特殊处理
  if not Self.HasResultVar and (GetParentDefStatement(Lv)=' '+JED_Func) then
  begin
    Result:=IdentLen(Ident)+MakeReturn(VarForLan(JEV_ResultRep));
  end
  else
    Result:=IdentLen(Ident)+'return;';
end;

function TJEPHPParser.TransFOR(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransFOREACH(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'foreach('+TransANode(JObj.Opt(JEP_Param2),0)+' as '+TransANode(Z,0)+'){'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param3),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'}';
end;

function TJEPHPParser.TransFORTO(JObj: TJEBranch; Ident: Integer): String;
var
  Z,Z2,CycleVar,StepExpr:TJENode;
  mstr,VarStr,StepStr,EndStr:String;
  IsConst,BEZero:Boolean;
  StepVal:Double;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=TransANode(Z,0);
  //赋值语句末尾已经有 ";"
  if Copy(Result,Length(Result),1)<>';' then
    Result:=Result+';';
  Result:=IdentLen(Ident)+'for('+Result;
  Z2:=JObj.Opt(JEP_Param2);
  EndStr:=TransANode(Z2,0,0);
  if EndStr='' then EndStr:=' 0-0 ';
  if Z is TJEBranch then
    CycleVar:=ExtractVarInSetValue(TJEBranch(Z))
  else
    CycleVar:=nil;
  VarStr:=TransANode(CycleVar,0,0);
  //if CycleVar=nil then PrintErr('No cycle var in FOR!');
  StepExpr:=JObj.Opt(JEP_Param3);
  IsConst:=IsConstNumber(StepExpr,StepVal);
  if CycleVar<>nil then
  begin
    if (StepExpr=nil) or (StepExpr=CNULL) then
    begin
      IsConst:=true;
      StepVal:=1;
      StepStr:='1';
    end
    else
      StepStr:=TransANode(StepExpr,0,0);
    //将 to 表达式转换为 <= 或 >= 比较表达式
    // step 参数可以是正数或负数。step 参数值决定循环的执行情况，如下所示：
    //   值        如果 ... 则循环执行
    // 正数或 0    counter <= end
    // 负数        counter >= end
    if IsConst then
    begin
      if StepVal>=0 then
        mstr:='<='
      else
        mstr:='>=';
      Result:=Result+VarStr+mstr+EndStr+'; '+VarStr;
      if StepVal=1 then
        Result:=Result+'++'
      else if StepVal=-1 then
        Result:=Result+'--'
      else begin
        Result:=Result+'+='+StepStr;
      end;
    end
    else begin
      //非常量的情况  ((StepVal>=0) and (i<=EndVal)) or ((StepVal<0) and (i>=EndVal))
      Result:=Result+'(('+StepStr+'>=0) && ('+VarStr+'<='+EndStr+'))'
        +' || (('+StepStr+'<0) && ('+VarStr+'>='+EndStr+'));'
        +VarStr+'+='+StepStr;
    end;
  end;
  Result:=Result+')'#13#10+IdentLen(Ident)+'{'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1,0);
  Result:=Result+#13#10+IdentLen(Ident)+'}';
end;

function TJEPHPParser.TransFUNCTION(JObj: TJEBranch; Ident: Integer): String;
var
  mstr,IdentStr,IdStr:String;
begin
  IdentStr:=IdentLen(Ident);
  FInDefId:=true;  //避免给标识符加上 $ 前缀...
  try
    IdStr:=TransANode(JObj.Opt(JEP_Param1),0);
  finally
    FInDefId:=false;
  end;
  Result:=IdentStr;
  if GetClassLevelNode<>nil then  //只有class内部允许有可见性修饰符
    Result:=Result+GetPerfixStr(JObj)+' ';
  Result:=Result+'function '+IdStr;
  mstr:=Trans_ParamDefs(JObj.Opt(JEDN_Params));
  //if mstr<>'' then  //2012-05-29  PHP的函数必须带()
  Result:=Result+'('+mstr+')';
  Result:=Result+#13#10+IdentStr+'{'#13#10;
  SetNextNeedVal(false);
  mstr:=Trans_BodyDef(JObj.Opt(JEDN_Body),Ident+1);
  if mstr<>'' then
    Result:=Result+mstr;
  //如有必要，在函数的最后加上return返回值的语句
  if TreeBaseLanClass.HasResultVar then
  begin
    Result:=Result+#13#10+IdentLen(Ident+1)+MakeReturn(VarForLan(JEV_ResultRep));
  end;
  Result:=Result+#13#10+IdentStr+'}';
end;

function TJEPHPParser.TransIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=StdTransIFEx(JObj,Ident);
end;

function TJEPHPParser.TransIIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=StdTransIFEx(JObj,Ident);
end;

function TJEPHPParser.TransINC(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransINCLUDE(JObj: TJEBranch; Ident: Integer): String;
var
  mstr:String;
begin
  mstr:=TransANode(JObj.Opt(JEP_Param1),0);
  TransPostfixInStr(mstr,TreeBaseLanClass);
  Result:=IdentLen(Ident)+'include('+mstr+');';
end;

function TJEPHPParser.TransIS(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransLanStatmentFunc(JObj: TJEBranch; const Lan,
  Op: String; Ident: Integer): String;
var
  i:Integer;
  MyObj:TJEBranch;
begin
  case Op[1] of
    'C':
    begin
      if Op='CALL' then
      begin
        Result:=IdentLen(Ident)+'/*::'+Lan+'.'+Op+'*/'+TransANode(JObj.Opt(JEP_Param1),0);
        exit;
      end;
    end;
    'O':
    begin
      if Op='ONERROR' then
      begin
        Result:=IdentLen(Ident)+'On Error';
        if JObj.Opt(JEP_Param1)=CNULL then
          Result:=Result+' Resume Next'
        else
          Result:=Result+' Goto '+TransANode(JObj.Opt(JEP_Param1),0);
        exit;
      end;
    end;
    'R':
    begin
      if Op='REDIM' then
      begin
        Result:='';
        i:=1;
        while true do
        begin
          MyObj:=JObj.OptJSONObject(JEP_ParamHeader+IntToStr(i));
          if MyObj=nil then break;
          Result:=Result+TransANode(MyObj.Opt(JEP_Operator),0)
            +'='+GetArrayDefStr(MyObj);
          Inc(i);
        end;
        Result:=IdentLen(Ident)+TrimRight(Result);
        exit;
      end;
    end;
    'S':
    begin
      if Op='SET' then
      begin
        //SET A=new B  => {op:"=Basic::SET", p1:{op:":=", p1:"A", p2:{op:"new", p1:"B"}}}
        MyObj:=JObj.OptJSONObject(JEP_Param1);
        if MyObj=nil then exit;
        Result:=TransANode(MyObj,Ident);
        exit;
      end;
    end;
  end;
  Result:=inherited TransLanStatmentFunc(JObj,Lan,Op,Ident);
end;

function TJEPHPParser.TransLOOP(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident)+'do{'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param1),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'}while('+TransANode(JObj.Opt(JEP_Param2),0,0)+')';
end;

function TJEPHPParser.TransNEW(JObj: TJEBranch; Ident: Integer): String;
begin
  FInDefId:=true;
  try
    Result:=IdentLen(Ident)+'new '+TransANode(JObj.Opt(JEP_Param1),0);
  finally
    FInDefId:=false;
  end;
end;

function TJEPHPParser.TransOtherLanBuildInFunc(JObj: TJEBranch; const Op,
  ALan: String; Ident: Integer): String;
var
  OpName:String;
begin
  if Op='' then
  begin
    Result:=inherited TransOtherLanBuildInFunc(JObj,Op,ALan,Ident);
    exit;
  end;
  Result:='';
  if ALan='Basic' then
  begin
    OpName:=UpperCase(Op);
    case OpName[1] of
      'A':
      begin
        if OpName='ASC' then
          Result:=TransFuncOp(TJEBranch(JObj),'ord',Ident);
      end;
      'C':
      begin
        if OpName='CHR' then
          Result:=TransFuncOp(TJEBranch(JObj),'chr',Ident);
      end;
      'I':
      begin
        if OpName='INSTR' then
          Result:=TransFuncOp(TJEBranch(JObj),'strpos',Ident)
        else if OpName='ISARRAY' then
          Result:=TransFuncOp(TJEBranch(JObj),'is_array',Ident)
        else if OpName='ISEMPTY' then
          Result:=TransFuncOp(TJEBranch(JObj),'empty',Ident)
        else if OpName='ISNULL' then
          Result:=TransFuncOp(TJEBranch(JObj),'is_null',Ident);
      end;
      'L':
      begin
        if OpName='LCASE' then
          Result:=TransFuncOp(TJEBranch(JObj),'strtolower',Ident)
        else if OpName='LEN' then
          Result:=TransFuncOp(TJEBranch(JObj),'strlen',Ident)
      end;
      'M':
      begin
        if OpName='MID' then  //2012-08-06   Mid(Str,A,B) => substr(Str,A-1,B)
          with TJEBranch(JObj) do
            Result:=IdentLen(Ident)+'substr('+TransANode(Opt(JEP_Param1),0)+','
              +TransANode_Dec1(Opt(JEP_Param2),0)+','+TransANode(Opt(JEP_Param3),0)+')';
      end;
      'U':
      begin
        if OpName='UCASE' then
          Result:=TransFuncOp(TJEBranch(JObj),'strtoupper',Ident)
        else if OpName='UBOUND' then
          Result:='( '+TransFuncOp(TJEBranch(JObj),'count',Ident)+' -1)';
      end;
    end;
  end;
  if Result='' then
    Result:=inherited TransOtherLanBuildInFunc(JObj,Op,ALan,Ident);
end;

function TJEPHPParser.TransPRED(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransPROCEDURE(JObj: TJEBranch; Ident: Integer): String;
var
  mstr,IdentStr,IdStr:String;
begin
  IdentStr:=IdentLen(Ident);
  FInDefId:=true;  //避免给标识符加上 $ 前缀...
  try
    IdStr:=TransANode(JObj.Opt(JEP_Param1),0);
  finally
    FInDefId:=false;
  end;
  Result:=IdentStr;
  if GetClassLevelNode(false)<>nil then  //只有class内部允许有可见性修饰符
    Result:=Result+GetPerfixStr(JObj)+' ';
  Result:=Result+'function '+IdStr;
  mstr:=Trans_ParamDefs(JObj.Opt(JEDN_Params));
  //if mstr<>'' then
    Result:=Result+'('+mstr+')';
  Result:=Result+'{'#13#10;
  SetNextNeedVal(false);
  mstr:=Trans_BodyDef(JObj.Opt(JEDN_Body),Ident+1);
  if mstr<>'' then
    Result:=Result+mstr;
  Result:=Result+#13#10+IdentLen(Ident)+'}';
end;

function TJEPHPParser.TransREPEAT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Result:=IdentLen(Ident)+'do{'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param1),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'}while(! '+TransANode(JObj.Opt(JEP_Param2),0,0)+')';
end;

function TJEPHPParser.TransSUCC(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransTypeVal(const ValStr: String; Ident: Integer): String;
begin
  Result:=Copy(ValStr,3,MaxInt);
  if Result='' then exit;
  case ValStr[2] of
    JEPT_Hex: Result:='0x'+Result;
    JEPT_EchoStr:
    begin
      Result:='?>'+Result+'<?php ';
    end;
  end;
end;

function TJEPHPParser.TransVAR(JObj: TJEBranch; Ident: Integer): String;
var
  i,c:Integer;
  IsClassVar,HasPerfix:Boolean;
  IdentStr:String;
begin
  IsClassVar:=DefInClass;
  Result:=GetPerfixStr(JObj);
  HasPerfix:=Trim(Result)<>'';
  IdentStr:=IdentLen(Ident);
  if Result='' then
    Result:=IdentStr
  else
    Result:=IdentStr+Result+' ';
  c:=0;
  with JObj do
    for i:=1 to Pred(length()) do
    begin
      if Keys[i]=JEP_Perfix then continue;
      if c>0 then
      begin
        if IsClassVar then
          Result:=Result+', '
        else
          Result:=Result+'; ';
      end;
      Result:=Result+TransVAR1(ValObjByIndex[i],0,IsClassVar,HasPerfix);
      Inc(c);
    end;
  Result:=Result+';';
end;

function TJEPHPParser.TransVAR1(JObj: TJENode; Ident: Integer; InClass, HasPerfix: Boolean): String;
var
  Z,Z2:TJENode;
  mstr:String;
begin
  Result:=IdentLen(Ident);
  if InClass and not HasPerfix then
    Result:=Result+'var ';
  if JObj is TJEBranch then  //  a(12)  =>  $a=range(0,12);
  begin
    Z:=TJEBranch(JObj).Opt(JEP_Operator);
    if Z<>nil then
    begin
      mstr:=Z.toString;
      if mstr=':=' then
      begin

      end
      else if mstr='[' then
      begin
        Result:=Result+TransANode(TJEBranch(JObj).Opt(JEP_Param1),0)+'=range(0,';
        Z2:=TJEBranch(JObj).Opt(JEP_Param2);
        if Z2=nil then  // A()
          Result:=Result+'0'
        else
          Result:=Result+TransANode(Z2,0);
        Result:=Result+')';
      end
      else begin
        {Result:=Result+TransANode(Z,0)+'=range(0,';
        Z2:=TJEBranch(JObj).Opt(JEP_Param1);
        if Z2=nil then  // A()
          Result:=Result+'0'
        else
          Result:=Result+TransANode(Z2,0);
        Result:=Result+');';}
      end;
    end;
  end
  else begin
    Result:=Result+TransANode(JObj,0);
    if not InClass then
      Result:=Result+' = null ';
  end;
  //Result:=Result+';';
end;

function TJEPHPParser.TransWAIT(JObj: TJEBranch; Ident: Integer): String;
begin

end;

function TJEPHPParser.TransWHILE(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  IdentStr:String;
begin
  IdentStr:=IdentLen(Ident);
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentStr+'while('+TransANode(Z,0)+'){'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident);
  Result:=Result+#13#10+IdentStr+'}';
end;

function TJEPHPParser.TransWHILENOT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'Do Until '+TransANode(Z,0)+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'Loop';
end;

function TJEPHPParser.VarForLan(const VarName: String): String;
begin
  if (FConsts.IndexOf(VarName)<0) and not FInDefId then
    Result:='$'+VarName
  else
    Result:=VarName;
end;

{ TJEPHPParser }

function TJEPHPParser.DefaultVisibility: String;
begin
  Result:='PUBLIC';
end;

procedure TJEPHPParser.DoInit;
var
  mstr:String;
begin
  inherited;
  //FASPMode:=(Pos('<%',FSource)>0) or (Pos('</',FSource)>0);
  //if FASPMode then
  begin
    mstr:=NextTo('<?php ',true,true);
    if mstr<>'' then
      PushOutStrVal(mstr);
  end;
end;

function TJEPHPParser.GetLineCommetOps: TStrings;
begin
  Result:=TStringList.Create;
  Result.Add('''');
end;

function TJEPHPParser.GetMathOpCharSet: TJECharSet;
begin
  Result:=['+', '-', '*', '/', '^', '&', '|', '.'];
end;

function TJEPHPParser.GetOpInfo(var Op: String; out IsOp2: Boolean;
  out Rank: Byte): Boolean;
begin
  Result:=true;
  if Length(Op)=3 then
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
  else begin
    Rank:=GetRank(Op);
    IsOp2:=true;
  end;
  Result:=Rank>0;
end;

function TJEPHPParser.GetSetValOpRank: Byte;
begin
  Result:=OpRank['='];
end;

function TJEPHPParser.GetSpecialCharSet: TJECharSet;
begin
  Result:=['&','<','%'];
end;

function TJEPHPParser.GetStrChCharSet: TJECharSet;
begin
  Result:=['"'];
end;

procedure TJEPHPParser.InitParser;
begin
  inherited;
  FWordNoCase:=false;
  FLineBreakCh:=';';
  FSetValOp:='=';
  FEqualOp:='==';
  FNotEqualOp:='<>';
  FStrCh:=['"',''''];
  FPascalTypeStr:=true;
  PHPKW_AS:=RegKeyword('AS');
  PHPKW_CASE:=RegKeyword('PHPKW_CASE');
  PHPKW_CLASS:=RegHeadKeywordMethod('CLASS',ParseClass);
  PHPKW_CONST:=RegHeadKeywordMethod('CONST',ParseConst);
  PHPKW_DEFINE:=RegHeadKeywordMethod('DEFINE',ParseDeclare);
  PHPKW_DIE:=RegHeadKeywordMethod('DIE',ParseDim);
  PHPKW_DO:=RegHeadKeywordMethod('DO',ParseDo);
  PHPKW_ELSE:=RegKeyword('ELSE');
  PHPKW_ELSEIF:=RegKeyword('ELSEIF');
  PHPKW_ERROR:=RegKeyword('ERROR');
  PHPKW_EXIT:=RegHeadKeywordMethod('EXIT',ParseExit);
  PHPKW_FOR:=RegHeadKeywordMethod('FOR',ParseFor);
  PHPKW_FUNCTION:=RegHeadKeywordMethod('FUNCTION',ParseFunction);
  PHPKW_GOTO:=RegHeadKeywordMethod('GOTO',ParseGoto);
  PHPKW_IF:=RegHeadKeywordMethod('IF',ParseIf);
  PHPKW_IN:=RegKeyword('IN');
  PHPKW_PRIVATE:=RegKeyword('PRIVATE',[ktPerfix]);
  PHPKW_PUBLIC:=RegKeyword('PUBLIC',[ktPerfix]);
  PHPKW_WHILE:=RegHeadKeywordMethod('WHILE',ParseWhile);
  //
  BASOP_AND:=RegLanOp('AND');
  BASOP_OR:=RegLanOp('OR');
  BASOP_XOR:=RegLanOp('XOR');
  BASOP_NOT:=RegLanOp('NOT',false);
  BASOP_MOD:=RegLanOp('MOD');
  //
  BASCV_FALSE:=RegLanVal('FALSE');
  BASCV_TRUE:=RegLanVal('TRUE');
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

class function TJEPHPParser.IsSimpleString(const Str: String): Boolean;
var
  i:Integer;
begin
  for i:=1 to Length(Str) do
  begin
    if Str[i]<' ' then
    begin
      Result:=false;
      exit;
    end;
  end;
  Result:=true;
end;

class function TJEPHPParser.Lan: ShortString;
begin
  Result:='PHP';
end;

function TJEPHPParser.OnKeyword(const Str: String; KWIdx: Integer):Integer;
begin
  if KWIdx=PHPKW_IF then
    ParseIf;
end;

function TJEPHPParser.OnPerfix(const Str: String; KWIdx: Integer): Integer;
var
  mstr:String;
  idx2:Integer;
begin
  mstr:=Str;
  idx2:=NextTokenKW;
  if idx2=PHPKW_VAR then
    Result:=Parse_Vars(mstr,true)
  else if idx2=PHPKW_FUNCTION then
    Result:=Parse_Func(mstr,true)
  else if idx2<0 then
    Result:=Parse_Vars(mstr,false)
  else
    PrintErr('VAR, FUNCTION or identifier expected.');
end;

function TJEPHPParser.OnSpecialHeadChar(ACh: Char; APos: Integer): Boolean;
var
  n:Integer;
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
    else begin
      Inc(APos);
      SetCurToken('+',tkOperator,FCurPos);
    end;
    Result:=true;
  end
  else if ACh='<' then
  begin
    if FSource[APos]<>'?' then exit;
    Inc(APos,4);
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
      SetCurToken('',tkSPACE,FCurPos);
    FCurPos:=APos;
    Result:=true;
  end
  else if ACh='?' then
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
    mstr:=NextTo('<?php ',true,true);  //退回到 <% 符号之前，以便解析 <%=
    //if mstr<>'' then
      PushOutStrVal(mstr);
    if FCurPos>n then
      SetCurToken('',tkEND,FCurPos)
    else
      SetCurToken(':',tkLINEDIV,FCurPos);
    Result:=true;
  end;
end;

function TJEPHPParser.ParseCase:Integer;
begin
  //
end;

function TJEPHPParser.ParseClass:Integer;
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
    {if CurToken.KWIdx1=PHPKW_END+1 then
    begin
      NextToken;
      ExpectKW;
      if CurToken.KWIdx1=PHPKW_CLASS+1 then
      begin
        StatementWithDefEnd;
        NextToken;
      end
      else begin
        PrintErr('Expect "CLASS" here.');
        StatementWithDefEnd;
      end;
      exit;
    end;}
  end;
  StatementWithDefEnd;
end;

function TJEPHPParser.ParseConst:Integer;
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

function TJEPHPParser.ParseDeclare;
begin
  Result:=FCurNodeLevel;
  NextToken;
end;

function TJEPHPParser.ParseDefines: Integer;
var
  idx,idx2,lv:Integer;
  mstr:String;
begin
  Result:=-1;
  lv:=-1;
  ExpectKW;
  while CurToken.Kind<>tkEND do
  begin
    idx:=CurToken.KWIdx1-1;
    if (idx=-1) and (CurToken.Token='}') then exit;
    mstr:='PUBLIC';  //Default visibility
    if idx=PHPKW_VAR then
      lv:=Parse_Vars(mstr,true)
    else if idx=PHPKW_FUNCTION then
      lv:=Parse_Func(mstr,true)
    else if (idx=PHPKW_PUBLIC) or (idx=PHPKW_PRIVATE) then
    begin
      if idx=PHPKW_PRIVATE then mstr:='PRIVATE';
      idx2:=NextTokenKW;
      if idx2=PHPKW_VAR then
        lv:=Parse_Vars(mstr,true)
      else if idx2=PHPKW_FUNCTION then
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
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
      NextToken;
    ExpectKW;
  end;
end;

function TJEPHPParser.ParseDim:Integer;
begin
  // DIM a, b(), c(10)
  PushDefStatement(' VAR','VAR');
  Result:=FCurNodeLevel;
  NextToken;
  ParseVarMemberBody;
  StatementWithDefEnd;
end;

function TJEPHPParser.ParseDo;
var
  OneLine,IsConst:Boolean;
  Lv,idx,idx2:Integer;
  EndExpr,ZeroExpr:TJENode;
  mstr:String;
begin
(*
do {
excuteExpression;
…..
go_out_expression;
} while ( conditionExpression ); 
*)
  PushStatement(JEF_Loop);
  Result:=FCurNodeLevel;
  repeat
    NextToken;
  until (CurToken.Token='{') or (CurToken.Token='while') or (CurToken.Kind=tkEND);
  ParseCBlock;
  {if CurToken.KWIdx1=PHPKW_LOOP+1 then
    NextToken
  else
    PrintErr('Expect "LOOP" here.');
  if idx2<0 then
  begin
    ExpectKW;
    if (idx=PHPKW_WHILE) or (idx=PHPKW_UNTIL) then
    begin
      if idx=PHPKW_WHILE then
        ModifyFuncName(Result,JEF_Loop);
      NextToken;
    end
    else
      PrintErr('Expect "WHILE" or "UNTIL" here.');
    if ParseStatements(true)<0 then
    begin
      PrintErr('Expect expression here.');
      PushBool(true);
    end;
  end;}
  StatementEnd;
  NextToken;
end;

function TJEPHPParser.ParseExit;
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
    if (idx=PHPKW_DO) or (idx=PHPKW_FOR) then
      ModifyFuncName(Result,'BREAK');
    NextToken;
  end
  else
    PrintErr('Except keyword here.');
  StatementEnd;
end;

function TJEPHPParser.ParseFor;
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
    {if CurToken.KWIdx1=PHPKW_TO+1 then
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
    if CurToken.KWIdx1=PHPKW_STEP+1 then
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
    end;}
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
    if CurToken.KWIdx1=PHPKW_IN+1 then
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
  {ExpectKW;
  if CurToken.KWIdx1=PHPKW_NEXT+1 then
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
  else} begin
    PrintErr('Expect "NEXT" here.');
    StatementEnd;
  end;
end;

function TJEPHPParser.ParseFunction;
begin
  PushStatement(' '+JED_Func);
  Result:=FCurNodeLevel;
  NextToken;
  ParseMethodBody('FUNCTION',DefaultVisibility);
end;

function TJEPHPParser.ParseGoto;
begin
  Result:=FCurNodeLevel;
  NextToken;
end;

function TJEPHPParser.ParseIf;
var
  OneLine:Boolean;
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
  ;//else if CurToken.KWIdx1=PHPKW_THEN+1 then
  begin
    GoNextStatementParam;
    NextToken;
    if CurToken.Kind in [tkEOLN,tkLINEDIV] then
    begin
      OneLine:=false;
      NextToken;
    end
    else
      OneLine:=true;
  end;
  ParseStatements(OneLine);
  ExpectKW;
  if CurToken.KWIdx1=PHPKW_ELSEIF+1 then
  begin
    //ModifyFuncName(Result,'IFELSE');
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
      {else if CurToken.KWIdx1=PHPKW_THEN+1 then
      begin
        GoNextStatementParam;
        NextToken;
      end;};
      while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
        NextToken;
      ParseStatements(OneLine);
      ExpectKW;
    until CurToken.KWIdx1<>PHPKW_ELSEIF+1;
  end;
  if CurToken.KWIdx1=PHPKW_ELSE+1 then
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
  {if CurToken.KWIdx1=PHPKW_END+1 then
  begin
    NextToken;
    ExpectKW;
    if CurToken.KWIdx1=PHPKW_IF+1 then
    begin
      StatementEnd;
      NextToken;
    end
    else begin
      PrintErr('Expect "IF" here.');
      StatementEnd;
    end;
  end;}
end;

function TJEPHPParser.ParseMethodBody(const EndStr, PerfixStr: String): Integer;
begin
  Result:=-1;
  if ParseIdentifier<>'' then
  begin
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
  {if CurToken.KWIdx1=PHPKW_END+1 then
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
  else} begin
    PrintErr('Expect "END" here.');
    StatementEnd;
  end;
end;

function TJEPHPParser.ParseParameterDef: Integer;
var
  mstr:String;
begin
  Result:=-1;
  while CurToken.Kind=tkWORD do
  begin
    ExpectKW;
    {if (CurToken.KWIdx1=PHPKW_BYREF+1) or (CurToken.KWIdx1=PHPKW_BYVAL+1) then
    begin
      mstr:=CurToken.CaseOKToken;
      NextToken;
      PrintErr('Perfix '+mstr+' ignored...');
    end;}
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

function TJEPHPParser.ParseReDim;
var
  IdNode:TJENode;
  Id:String;
begin
  // REDIM a(0),b(200)
  PushStatement(MakeLanFunc('REDIM'));
  Result:=FCurNodeLevel;
  NextToken;
  while ParseIdentifier<>'' do
  begin
    if CurToken.Token='(' then
    begin
      PushBracket('(');
      NextToken;
      if ParseExpr<0 then
        PrintErr('Except integer constant.')
      else if (FCurNodeLevel>Result+2) then
        PrintErr('Warning: Except integer constant.',Result+1);
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

function TJEPHPParser.ParseSwitch;
begin
(*
swith ( $what ) {
case case1:  {
executeExpression;
….;
[ break ; ]  // 如果不加 break，后面的句子也将会被执行。
}
case case2: {
executeExpression;
….
[ break ; ]
}
…..
[ default : {  // 当上述几种情况都不是时，执行这一句。
executeExpression;
….
} ]
*)
  Result:=FCurNodeLevel;
  NextToken;
end;

function TJEPHPParser.ParseSet;
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

function TJEPHPParser.ParseSub;
begin
  PushStatement(' '+JED_Proc);
  Result:=FCurNodeLevel;
  NextToken;
  ParseMethodBody('SUB',DefaultVisibility);
end;

function TJEPHPParser.ParseVarMemberBody: Integer;
begin
  // DIM a, b(), c(10)
  // Public aa, bb
  Result:=FCurNodeLevel;
  while ParseIdentifier<>'' do
  begin
    if CurToken.Token='(' then
    begin
      PushBracket('(');
      NextToken;
      if (ParseExpr>0) and (FCurNodeLevel>Result+2) then
        PrintErr('Warning: Except integer constant.',Result+1);
      if CurToken.Token=')' then
      begin
        PushBracket(')');
        NextToken;
      end
      else begin
        PushBracket(')');
        PrintErr('Except ")".');
      end;
    end;
    if CurToken.Token<>',' then break;
    GoNextStatementParam;
    NextToken;
  end;
  if not (CurToken.Kind in [tkEOLN,tkLINEDIV,tkEND]) then
    PrintErr('Except identifier here.');
end;

function TJEPHPParser.ParseWhile;
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
  {if CurToken.KWIdx1=PHPKW_WEND+1 then
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
  else} begin
    PrintErr('Expect "WEND" here.');
    StatementEnd;
  end;
end;

function TJEPHPParser.Parse_Func(const PerfixStr: String; NeedNext: Boolean):Integer;
begin
  PushStatement(' FUNCTION');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseMethodBody('FUNCTION',PerfixStr);
end;

function TJEPHPParser.Parse_Sub(const PerfixStr: String; NeedNext: Boolean):Integer;
begin
  PushStatement(' PROCEDURE');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseMethodBody('SUB',PerfixStr);
end;

function TJEPHPParser.Parse_Vars(const PerfixStr: String; NeedNext: Boolean):Integer;
begin
  PushDefStatement(' VAR','VAR');
  Result:=FCurNodeLevel;
  if NeedNext then NextToken;
  ParseVarMemberBody;
  FCurNodeLevel:=Result;
  PushPerfix(PerfixStr);
  StatementWithDefEnd;
end;

procedure TJEPHPParser.SetTransForToFOR(const Value: Boolean);
begin
  FTransForToFOR := Value;
end;

function TJEPHPParser.VarIsArray: Boolean;
var
  mstr:String;
begin
  mstr:=CurToken.Token;
  if mstr<>'' then
  begin
    if mstr[Length(mstr)]='s' then
    begin
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

function TJEPHPParser.WordIsBuildInFunc(const AName: String): Boolean;
begin
  Result:=false;
end;

function TJEPHPParser.WordIsFunc: Boolean;
begin
  Result:=false;
end;

initialization
  InitOpRank;
  InitSingleOp2Func;
  TJEParser.RegisterParser(TJEPHPParser);

finalization
  OpRanks.Free;

end.
