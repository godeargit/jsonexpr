unit JETransPascal;

interface

uses
  JEParser, uJSON, UJSONExpr, SysUtils;

const
  JPascal_Mod='MOD';
  JPascal_DIV='DIV';
  JPascal_XOR='XOR';
  JPascal_AND='AND';
  JPascal_OR='OR';
  JPascal_NOT='NOT';
  JPascal_SHR='SHR';
  JPascal_SHL='SHL';
type
  TJEPascalParser=class(TJEParser)
  protected
    procedure InitParser; override;
    function GetOpRank(const Op: String):Byte; override;
    function LineDivStr:String; override;
    function TransLanStatmentFunc(JObj: TJEBranch; const Lan, Op: String;
      Ident: Integer):String; override;
    function TransOtherLanBuildInFunc(JObj: TJEBranch; const Op, ALan: String;
      Ident: Integer):String; override;
    function TransTypeVal(const ValStr: String; Ident: Integer):String; override;
    //function TransSetValue(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIF_(JObj: TJEBranch; Ident: Integer; NeedVal: Boolean):String;
    function TransIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransIIF(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFORTO(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransWHILENOT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCASE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransECHO(JObj: TJEBranch; Ident: Integer):String; override;
    function TransEXIT(JObj: TJEBranch; Ident: Integer):String; override;
    function TransPROCEDURE(JObj: TJEBranch; Ident: Integer):String; override;
    function TransFUNCTION(JObj: TJEBranch; Ident: Integer):String; override;
    function TransVAR(JObj: TJEBranch; Ident: Integer):String; override;
    function TransVAR1(JObj: TJENode; Ident: Integer; InClass, HasPerfix: Boolean):String;
    function TransCONST(JObj: TJEBranch; Ident: Integer):String; override;
    function TransCONST1(JObj: TJEBranch; Ident: Integer):String; 
    function TransCLASS(JObj: TJEBranch; Ident: Integer):String; override;
    function StdMakeFuncHead(const FuncName, Perfix, ParamStr, TypeStr, IdentStr: String;
      IsProc: Boolean):String; override;
    function StdMakeFuncBody(const FuncBody, IdentStr: String;
      IsProc: Boolean):String; override;
    function StdMakeBlock(const Str, IdentStr:String; NoHeadIdent: Boolean):String; override;
    function StdMakeIfThen(const Expr, IdentStr: String):String; override;
    function StdMakeElseIf(const Expr, IdentStr: String):String; override;
    function StdMakeElseEnd(IsElse: Boolean; const IdentStr: String):String; override;
  public
    function StrForLan(const Str: String):String; override;
    class function Lan:ShortString; override;
    function TransCommonOp(JObj: TJEBranch; const Op: String;
      Ident: Integer; PrnRank: Integer=-1):String; override;
    { 是否为不含需转义字符的简单字符串（可以含'及\符号） }
    class function IsSimpleString(const Str: String):Boolean;
    class function SimpleStrToPascalStr(const S: String):String;  //仅对'及\进行转义
    class function CommonStrToPascalStr(const S: String):String;
    function PackSrc(const Source: String; out WordCount: Integer):String; override;
  end;

implementation

{
PASCAL运算符优先级
共四级

单目运算符 (最高优先级)
@ 取变量或函数的地址(返回一个指针)
not 逻辑取反或按位取反

乘除及按位运算符
* 相乘或集合交集
/ 浮点相除
div 整数相除
mod 取模 (整数相除的余数)
as 程序运行阶段类型转换 (RTTI运算符)
and 逻辑或按位求和
shl 按位左移
shr 按位右移

加减运算符
+ 相加、集合并集、字符串连接或指针增加一个偏移量
- 相减、集合差集或指针减少一个偏移量
or 逻辑或按位或运算
xor 逻辑或按位异或运算

关系及比较运算符(最低优先级)
= 判断是否相等
<> 判断是否不相等
< 判断是否小于
> 判断是否大于
<= 判断是否小于或等于,或是否是一个集合的子集
>= 判断是否大于或等于,或是否是一个集合的父集
in 判断是否是集合成员
is 判断对象是否类型兼容 (又一个RTTI运算符)
}

var
  SingleOpMap:array [Char] of String;

procedure InitSingleOp2Func;
begin
  SingleOpMap['%']:=JPascal_MOD;
  SingleOpMap['\']:=JPascal_DIV;
  SingleOpMap['&']:=JPascal_AND;
  SingleOpMap['|']:=JPascal_OR;
  SingleOpMap['^']:=JPascal_XOR;
  SingleOpMap['!']:=JPascal_NOT;
end;

{
test:
  IF(A%100>50,X:=1+IF(B,2*A,0.5*A);Y:=-2*X,X:=A;X-=C+D);
}

{ TJETransPascal }

class function TJEPascalParser.CommonStrToPascalStr(const S: String): String;
var
   b,c: char;
   i, len: integer;
   sb, mstr: string;
   InQuote:Boolean;
begin
  c:=#0;
  len:=Length(s);
  sb:=sb +'''';
  InQuote:=true;
  i:=1;
  while i<=len do
  begin
    b:=c;
    c:=s[i];
    case c of
      //Output special character smaller than space.
      #0..#31:
      begin
        mstr:='#'+IntToStr(Byte(c));
        //2012-12-19  考虑第一个字符的情况，避免以 ''#XX 开头的模式
        if InQuote and (i>1) then  // 'Abc' +> #13
        begin
          sb:=sb+''''+mstr;
          InQuote:=false;
        end
        else if not InQuote then   //  #8 => #9
          sb:=sb+mstr
        else begin //is the first char
          sb:=mstr;
          InQuote:=false;
        end;
      end;
      '''':
      begin
        if not InQuote then
        begin
          InQuote:=true;
          sb:=sb+'''';
        end;
        sb:=sb + '''''';
      end;
      else
      begin
        if not InQuote then
        begin
          InQuote:=true;
          sb:=sb+'''';
        end;
        sb:=sb + c;
      end;
    end;
    Inc(i);
  end;
  if InQuote then
    sb:=sb+'''';
  Result:=sb;
end;

function TJEPascalParser.GetOpRank(const Op: String): Byte;
begin
  if (Op='AND') or (Op='<<') or (Op='>>') or (Op='&') then
    Result:=RK_Multi
  else if (Op='OR') or (Op='XOR') or (Op='|') or (Op='^') then
    Result:=RK_Add
  else if (Op='IN') or (Op='IS') then
    Result:=RK_Compare
  else
    Result:=inherited GetOpRank(Op);
end;

procedure TJEPascalParser.InitParser;
begin
  inherited;
  FWordNoCase:=true;
  FPascalTypeStr:=true;
  FAddAsStrJoin:=true;
end;

class function TJEPascalParser.IsSimpleString(const Str: String): Boolean;
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

class function TJEPascalParser.Lan: ShortString;
begin
  Result:='Pascal';
end;

function TJEPascalParser.LineDivStr: String;
begin
  Result:=#13#10;
end;

function TJEPascalParser.PackSrc(const Source: String; out WordCount: Integer): String;
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
      '{':
        begin
          if Source[i+1]='$' then
          begin
            repeat
              Inc(n);
              mstr[n]:=Source[i];
              Inc(i);
            until Source[i-1]='}';
            Status:=1;
          end
          else begin
            repeat
              Inc(i);
            until Source[i-1]='}';
          end;
          LastNone:=true;
          continue;
        end;
      '(':
        begin
          if Source[i+1]='*' then
          begin
            Inc(i,2);
            while i<Len do
            begin
              if (Source[i-1]='*') and (Source[i-1]='*') then break;
              Inc(i);
            end;
            LastNone:=true;
            Status:=1;
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
      '/':
        begin
          if Source[i+1]='/' then
          begin
            repeat
              Inc(i);
            until Source[i-1]<' ';
            continue;
          end;
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

class function TJEPascalParser.SimpleStrToPascalStr(const S: String): String;
begin
  Result:=QuotedStr(S);
end;

function TJEPascalParser.StdMakeBlock(const Str, IdentStr: String;
  NoHeadIdent: Boolean): String;
begin
  if NoHeadIdent then
    Result:='begin'#13#10+Str+#13#10+IdentStr+'end'
  else if Str<>'' then
    Result:=IdentStr+'begin'#13#10+Str+#13#10+IdentStr+'end'
  else //空语句
    Result:=IdentStr+'begin end';
end;

function TJEPascalParser.StdMakeElseEnd(IsElse: Boolean;
  const IdentStr: String): String;
begin
  if IsElse then
    Result:=#13#10+IdentStr+'else '
  else
    Result:='';
end;

function TJEPascalParser.StdMakeElseIf(const Expr, IdentStr: String): String;
begin
  Result:=#13#10+IdentStr+'else if '+Expr+' then'#13#10;
end;

function TJEPascalParser.StdMakeFuncBody(const FuncBody, IdentStr: String;
  IsProc: Boolean): String;
begin
  Result:=#13#10+IdentStr+StdMakeBlock(FuncBody,IdentStr,false);
end;

function TJEPascalParser.StdMakeFuncHead(const FuncName, Perfix, ParamStr,
  TypeStr, IdentStr: String; IsProc: Boolean): String;
begin
  Result:=IdentStr;
  if Perfix<>'' then Result:=Result+' '+Perfix+' ';
  if IsProc then Result:=Result+'procedure ' else Result:=Result+'function ';
  Result:=Result+FuncName+'('+ParamStr+')';
  if not IsProc and (TypeStr<>'') then Result:=Result+':'+TypeStr;
  Result:=Result+';';
end;

function TJEPascalParser.StdMakeIfThen(const Expr, IdentStr: String): String;
begin
  Result:=IdentStr+'if '+Expr+' then'#13#10;
end;

function TJEPascalParser.StrForLan(const Str: String): String;
begin
  if IsSimpleString(Str) then
    Result:=SimpleStrToPascalStr(Str)
  else
    Result:=CommonStrToPascalStr(Str);
end;

function TJEPascalParser.TransCASE(JObj: TJEBranch; Ident: Integer): String;
var
  i:Integer;
  IdentStr:String;
  Z,Z2:TJENode;
begin
  IdentStr:=IdentLen(Ident);
  Result:=IdentStr+'case '+TransANode(JObj.Opt(JEP_Param1),0);
  i:=2;
  while True do
  begin
    Z:=JObj.Opt(JEP_ParamHeader+IntToStr(i));
    Z2:=JObj.Opt(JEP_ParamHeader+IntToStr(i+1));
    if Z=nil then break;
    if Z2=nil then
    begin
      Result:=Result+#13#10+IdentLen(Ident+1)+'else'
        +#13#10+TransANode(Z,Ident+2);
      break;
    end;
    Result:=Result+#13#10+IdentLen(Ident+1)+TransANode(Z,0)+' :'
      +#13#10+TransANode(Z2,Ident+2);
    Inc(i,2);
  end;
  Result:=Result+#13#10+IdentStr+'end;';
end;

function TJEPascalParser.TransCLASS(JObj: TJEBranch; Ident: Integer): String;
var
  mstr:String;
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  if Z.ClassType<>_String then
  begin
    Result:='{ Invalid class name "'+TransANode(Z,0)+'" }';
    exit;
  end;
  Result:=Z.toString;
  mstr:=GetPerfixStr(JObj);
  if mstr<>'' then
    Result:=Result+'= {'+mstr+'} class'
  else
    Result:=Result+'=class';
  Result:=IdentLen(Ident)+Result+#13#10;
  mstr:=TransANode(JObj.Opt(JEDN_Members),Ident+1);
  if mstr<>'' then
    Result:=Result+mstr;
  Result:=Result+#13#10+IdentLen(Ident)+'end';
end;

function TJEPascalParser.TransCommonOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
var
  Ch:Char;
begin
  case Length(Op) of
    1:
    begin
      if Op[1] in ['%','\','&','|','^'] then
      begin
        Result:=Trans_Mid(JObj,SingleOpMap[Op[1]],GetOpRank(Op),PrnRank);
        exit;
      end
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
      else if Op=JEOP_StrJoin then  //2013-01-01  处理 +> 拼接符
      begin
        Result:=Trans_Mid(JObj,'+',GetOpRank(Op),PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopShl] then
      begin
        Result:=Trans_Mid(JObj,JPascal_SHL,RK_Multi,PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopShr] then
      begin
        Result:=Trans_Mid(JObj,JPascal_SHR,RK_Multi,PrnRank);
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
            Result:=Trans_CalcAndSet(JObj,JPascal_SHR,RK_Multi)
          else //Op[1]='<'
            Result:=Trans_CalcAndSet(JObj,JPascal_SHL,RK_Multi);
          exit;
        end;
      end;
    end;
  end;
  Result:=inherited TransCommonOp(JObj,Op,Ident,PrnRank);
end;

function TJEPascalParser.TransCONST(JObj: TJEBranch; Ident: Integer): String;
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

function TJEPascalParser.TransCONST1(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
begin
  Z:=JObj.Opt(JEP_Param1);
  if Z.ClassType<>_String then
  begin
    Result:='{ Invalid const name "'+TransANode(Z,0)+'" }';
    exit;
  end;
  Result:=Z.toString;
  AddConst(Result);
  Result:=IdentLen(Ident){+GetPerfixStr(JObj)}+'const '
    +Result+'='
    +TransANode(JObj.Opt(JEP_Param2),0)+';';
end;

function TJEPascalParser.TransECHO(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=inherited TransECHO(JObj,Ident);
end;

function TJEPascalParser.TransEXIT(JObj: TJEBranch; Ident: Integer): String;
var
  lv:Integer;
begin
  //有返回值变量的函数内部exit需要特殊处理
  if not Self.HasResultVar and (GetParentDefStatement(Lv)=' '+JED_Func) then
  begin
    Result:=IdentLen(Ident)+MakeReturn(VarForLan(JEV_ResultRep));
  end
  else
    Result:=IdentLen(Ident)+'exit';
end;

function TJEPascalParser.TransFORTO(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  mstr:String;
begin
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentLen(Ident)+'for '+TransANode(Z,0)+' to ';
  Z:=JObj.Opt(JEP_Param2);
  Result:=Result+TransANode(Z,0);
  mstr:=TransANode(JObj.Opt(JEP_Param3),0);
  if mstr<>'1' then
    Result:=Result+' Step '+mstr;
  Result:=Result+' do'#13#10+IdentLen(Ident)+'begin'+#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param4),Ident+1);
  Result:=Result+#13#10+IdentLen(Ident)+'end';
end;

function TJEPascalParser.TransFUNCTION(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=Self.StdTransFuncEx(JObj,Ident,false);
end;

{
test:
  1+IF(A<B,IF(B,100,0.5)*1.5,IF(A<>C,2-3,4+5))
}
function TJEPascalParser.TransIF(JObj: TJEBranch; Ident: Integer): String;
begin
  //Result:=TransIF_(JObj,Ident,false);
  Result:=StdTransIFEx(JObj,Ident);
end;

function TJEPascalParser.TransIF_(JObj: TJEBranch; Ident: Integer; NeedVal: Boolean): String;
var
  Str0,Str1,LStr,TmpVar:String;
  MyObj:TJENode;
  P:PNodeData;
  NVal:Boolean;
begin
  MyObj:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  Str0:=TransANode(MyObj,0);
  if Str0='' then Str0:='true';
  Gather_BeforeAfter(Ident);
  Result:='if '+Str0+' then';
  NVal:=NeedVal and NodeNeedVal;
  //需要IF表达式的返回值  X := IF( ?, A, B )    Y:=IF(?,X+IF(?,1,2),3)+100
  if NVal then
  begin
    P:=ParentNodePtr;
    if P<>nil then
    begin
      LStr:=P^.LeftStr;
      if LStr<>'' then
        SetParentNoLeft
      else begin
        TmpVar:=GenTempVar;
        LStr:=TmpVar;
      end;
    end;
  end
  else
    P:=nil;
  MyObj:=JObj.Opt(JEP_Param2);
  SetNextNeedVal(NVal);
  Str1:=TransANode(MyObj,0);
  if Str1<>'' then
  begin
    if P<>nil then
    begin
      Str1:=LStr+SetValOp+Str1;
    end;
    Combine_SubBeforeAfter(Str1);
    Clear_BeforeAfter;
    Result:=Result+#13#10'begin'#13#10+AddIdent(Str1)+#13#10'end';
  end;
  MyObj:=JObj.Opt(JEP_Param3);
  SetNextNeedVal(NVal);
  Str1:=TransANode(MyObj,0);
  if Str1<>'' then
  begin
    if P<>nil then
    begin
      Str1:=LStr+SetValOp+Str1;
    end;
    Combine_SubBeforeAfter(Str1);
    Result:=Result+#13#10'else begin'#13#10+AddIdent(Str1)+#13#10'end';
  end;
  if TmpVar<>'' then
  begin
    Set_Before(Result);
    Result:=TmpVar;
  end;
end;

function TJEPascalParser.TransIIF(JObj: TJEBranch; Ident: Integer): String;
begin
  //Result:=TransIF_(JObj,Ident,true);
  Result:=StdTransIFEx(JObj,Ident,true);
end;

function TJEPascalParser.TransLanStatmentFunc(JObj: TJEBranch; const Lan,
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
        Result:=IdentLen(Ident)+'{::'+Lan+'.'+Op+'}'+TransANode(JObj.Opt(JEP_Param1),0);
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

function TJEPascalParser.TransOtherLanBuildInFunc(JObj: TJEBranch; const Op,
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
          Result:=TransFuncOp(TJEBranch(JObj),'Byte',Ident);
      end;
      'C':
      begin
        if OpName='CHR' then
          Result:=TransFuncOp(TJEBranch(JObj),'Char',Ident);
      end;
      'I':
      begin
        if OpName='INSTR' then
          Result:=TransFuncOp(TJEBranch(JObj),'Pos',Ident)
        else if OpName='ISARRAY' then
          Result:=TransFuncOp(TJEBranch(JObj),'VarIsArray',Ident)
        else if OpName='ISEMPTY' then
          Result:=TransFuncOp(TJEBranch(JObj),'VarIsEmpty',Ident)
        else if OpName='ISNULL' then
          Result:=TransFuncOp(TJEBranch(JObj),'VarIsNull',Ident);
      end;
      'L':
      begin
        if OpName='LCASE' then
          Result:=TransFuncOp(TJEBranch(JObj),'LowerCase',Ident)
        else if OpName='LEN' then
          Result:=TransFuncOp(TJEBranch(JObj),'Length',Ident)
      end;
      'M':
      begin
        if OpName='MID' then
          with TJEBranch(JObj) do
            Result:=IdentLen(Ident)+'Copy('+TransANode(Opt(JEP_Param1),0)+','
              +TransANode_Dec1(Opt(JEP_Param2),0)+','+TransANode(Opt(JEP_Param3),0)+')';
      end;
      'U':
      begin
        if OpName='UCASE' then
          Result:=TransFuncOp(TJEBranch(JObj),'UpperCase',Ident)
        else if OpName='UBOUND' then
          Result:='( '+TransFuncOp(TJEBranch(JObj),'High',Ident)+' -1)';
      end;
    end;
  end;
  if Result='' then
    Result:=inherited TransOtherLanBuildInFunc(JObj,Op,ALan,Ident);
end;

function TJEPascalParser.TransPROCEDURE(JObj: TJEBranch;
  Ident: Integer): String;
begin
  Result:=Self.StdTransFuncEx(JObj,Ident,true);
end;

function TJEPascalParser.TransTypeVal(const ValStr: String; Ident: Integer): String;
begin
  Result:=Copy(ValStr,3,MaxInt);
  if Result='' then exit;
  case ValStr[2] of
    JEPT_Hex: Result:='$'+Result;
    JEPT_EchoStr:
    begin
      Result:='Echo('+StrForLan(Result)+')';
    end;
  end;
end;

function TJEPascalParser.TransVAR(JObj: TJEBranch; Ident: Integer): String;
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
  Result:=Result+'var ';
  c:=0;
  with JObj do
    for i:=1 to Pred(length()) do
    begin
      if Keys[i]=JEP_Perfix then continue;
      if c>0 then
      begin
        Result:=Result+', ';
      end;
      Result:=Result+TransVAR1(ValObjByIndex[i],0,IsClassVar,HasPerfix);
      Inc(c);
    end;
  Result:=Result+';';
end;

function TJEPascalParser.TransVAR1(JObj: TJENode; Ident: Integer; InClass,
  HasPerfix: Boolean): String;
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
        Result:=Result+TransANode(TJEBranch(JObj).Opt(JEP_Param1),0)+'=array [0,';
        Z2:=TJEBranch(JObj).Opt(JEP_Param2);
        if Z2=nil then  // A()
          Result:=Result+'0'
        else
          Result:=Result+TransANode(Z2,0);
        Result:=Result+'] of TypeX';
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
      Result:=Result;
  end;
end;


function TJEPascalParser.TransWHILE(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  IdentStr:String;
begin
  IdentStr:=IdentLen(Ident);
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentStr+'while '+TransANode(Z,0)+' do'#13#10+IdentStr+'begin'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident+1);
  Result:=Result+#13#10+IdentStr+'end';
end;

function TJEPascalParser.TransWHILENOT(JObj: TJEBranch; Ident: Integer): String;
var
  Z:TJENode;
  IdentStr:String;
begin
  IdentStr:=IdentLen(Ident);
  Z:=JObj.Opt(JEP_Param1);
  Result:=IdentStr+'while not ('+TransANode(Z,0)+') do'#13#10+IdentStr+'begin'#13#10;
  SetNextNeedVal(false);
  Result:=Result+TransANode(JObj.Opt(JEP_Param2),Ident+1);
  Result:=Result+#13#10+IdentStr+'end';
end;

initialization
  InitSingleOp2Func;
  TJEParser.RegisterParser(TJEPascalParser);

end.
