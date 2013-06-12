{
  Copyright 2009,2010,2011  creation_zy
  creation_zy@sina.com

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation.

  JSON Expression Parser/Evaluator

  Author: Yi Zhang
  Date: 2009-11-19



Change Logs:

2009-11-20
Ver 0.1  By creation_zy  (无尽愿)

  Class:
    TJSONExprParser    JSON表达式分析器
    TJSONVarHelper     变量获取器
    TJSONFuncHelper    函数求值器
    TSimpleVarHelper   简单变量获取器

  Function:
    ExprToJSON  //With VarHelper
    JSONToExpr  //With VarHelper
    Eval
    VarNeeded

  Support:
    Math operation:    + - * / \ % & | ^ ~ ! >> <<
    Bool operation:    AND OR XOR
    Compare operation: = > < <> >= <=
    Object operation:  . IS
    Value collection:  IN (a,b)
    Condition expr:    IF( condition, value1, value2)

2009-11-21
  Support:
    Set value          X := Y
    Sentence end tag   ;
  VarHelper can talk with common JSONObject.

2009-11-22
ver 0.2  By creation_zy  (无尽愿)
  支持赋值以及语句结束符
  JSONToExpr now support the tag ":=" and ";".
  支持Len字符串函数
  Support Len string function.
  字符串定义支持 #123 的形式
  String defination support things like "#123".  eg: 'Hello!'#13#10
  相邻的字符串定义将被合并为一个字符串
  Adjacent string expression can combine automatically.  eg:  'ABC'  'abc' => 'ABCabc'
  变量名可以通过后续的普通字符串进行延长
  Variable name can extent by the next string expression.  eg:  Michael' Jordan' => var "Michael Jordan"
  变量名可以包含大于#128的字符（可以使用国标汉字）
  Variable name can hold character biger than #128.
  可在IF中嵌套多个语句，实现分支效果
  Sentence in IF function is available.  eg:  if(X>2,(Y:=X-2;Z:=X*Y;),Z:=-X)
  支持语句行执行事件(之后触发)、变量写入事件(之前触发)
  Support event trigger after a line execute and before value assignment.

2009-11-23
By creation_zy  (无尽愿)
  Increase the speed of parser.
  Fix bug that can't process "XOR".
  New TMemVarHelper class to access local variables.

2009-11-29
ver 0.2.2  By creation_zy  (无尽愿)
  Support collection operations:  + - & | ^ LEN    eg: (1,2,4,8) - (3,4,5) => (1,2,8)
  Support scientific number:  1.02E-4
  Add conditional define to customize the function of parser.

2010-04-02
ver 0.2.3  By creation_zy  (无尽愿)
  Support variable name surround with "
  Support operator ! and ~

2010-04-04
ver 0.3.0  By creation_zy  (无尽愿)
  Support record or object member    eg: P1.X  Score1.(100+Max-Min)

2010-06-28
ver 0.3.1  By creation_zy  (无尽愿)
  Optimize sentence structure, Support multiple sentence in one Object.

2010-08-05
ver 0.3.2  By creation_zy  (无尽愿)
  Add "Inc", "Dec" operator.
  Implement "While" loop. Support "break" statement.
  Fix bug in FuncHelper parameter transport.

2010-08-12
ver 0.3.3  By creation_zy  (无尽愿)
  Add "For", "Repeat", "Times", "IsNull" statement.
  Add Line comment "//"
  Add function "Now"
  Add type cast function: Int, String, Bool, Float

2011-09-01
ver 0.3.4  By creation_zy  (无尽愿)
  Realize array parse and eval.
  Bug fix for expr like "Now()-1".
  Allow "+" at begin of expression just as "-".
  Add type cast function: Date, Time, Datetime
  Add Print procedure.

2011-09-07
ver 0.3.5  By creation_zy  (无尽愿)
  Support operator transfer like "==" -> "=".

2011-09-20
ver 0.4.0  By creation_zy  (无尽愿)
  Can parse define with perfix like:
      public function Add(A,B):=(A+B);
      const PI:=3.14159;
      abstract class TPlane(Object);
  Add function "IsArray"

2011-09-23
ver 0.4.1  By creation_zy  (无尽愿)
  Add "IFELSE" and "CASE" statement:
    ifelse(A=B,F1(),A>B,F2(),F3())
    case(A,B,F1(),C,F2(),D,F3(),E)
  Support ++ and -- operator like:  A++  B--

2011-10-03
ver 0.4.2  By creation_zy  (无尽愿)
  Add "continue", "Reverse", "Pred", "Succ", "return".
  Some bug fixed.
  Some statement can convert to Pascal with TJETransPascal.
    Like:
      IF(X<100,Y:=X,Y:=X-100;Z:=X*Y);
      X*=1+IF(A<B,IF(B,100,0.5)*1.5,IF(A<>C,2,4+5));

2011-12-20
  Add "Loop", "WhileNot", "ForTo", "ForEach"
  Add "Echo"
  Add Level parameter to Break and Continue
  Add operator "+>" to join string, "==" to Type&Val compare.

2012-02-19
  Remove "IFELSE"
  Add "IIF" -- if statement return value. Old if statement will not return val now.

2012-03-19
  Add "NOWTIME"

2012-05-30
  Add "NEW"

2012-06-17
  Add "NOWDATE"

2012-06-17
ver 0.5.0  By creation_zy
  Bug fix for "multi block end" which found by rinospro@gmail.com.

2012-08-27
ver 0.5.1  By creation_zy
  Bug fix for "IIF".
  Support SuperObject JSON Toolkit -- Just can be compiled.
  Core unit can be compiled in Delphi 7.

2013-03-31
ver 0.5.2  By creation_zy
  Support "Session" var in language translating.
  Can keep comment in translating.

2013-04-23
ver 0.5.3  By creation_zy
  Fix bugs on object attr access.

2013-06-12
ver 0.5.4  By creation_zy
  Add stop/continue @ time limit (in cycle statememts).
}

unit UJSONExpr;

interface

//{$DEFINE SUPEROBJECT}

uses
  SysUtils, Classes, Variants, {$IFDEF SUPEROBJECT}SuperObject{$ELSE}uJSON{$ENDIF};

// If the parser is too complex for you, you can remove some function from it.
// -- Just use the conditonal define below.
//{$DEFINE NO_IF}
//{$DEFINE NO_COLLECTION}
//{$DEFINE NO_ASSIGNMENT}
//{$DEFINE NO_TRACE}
{$DEFINE NO_RECMEMBER}
//{$DEFINE NO_COMPLEXOBJ}
//{$DEFINE NO_OPTRANSLATE}

const
  MaxJETreeLevel=256;
  varObject=varAny+$1000;
  JEP_Operator='op';
  JEP_Name='pn';
  JEP_Perfix='p0';
  JEP_LanDef='p_';  //决定一个分支下的基础语言
  JEP_Type='tp';
  JEP_Body='pb';
  JEP_ParamHeader='p';
  JEP_Param1='p1';
  JEP_Param2='p2';
  JEP_Param3='p3';
  JEP_Param4='p4';
  JEP_StrParamHeader='''';    //为了将字符串与变量名相区别，所有字符串值均带单引号前缀
  JEP_TypeHead='?';           //十六进制、八进制、二进制、日期、时间等类型的头部标识
  JEP_SpaceBreak=';S';        //空断句（主要用于表示语句和注释之间的非换行隔断）
  JEPT_Hex='H';
  JEPT_Oct='O';
  JEPT_Bin='B';
  JEPT_Date='d';
  JEPT_Time='t';
  JEPT_DateTime='D';
  JEPT_EchoStr='E';           //如ASP中 %>  <% 之间的部分
  JEPT_EmptyItem=' ';         //空语素，用于前置算子的表示，如 --A 转化为 ?e -- A
  JE_EmptyItemStr=JEP_TypeHead+JEPT_EmptyItem;
  JEP_VarTag='"';             //支持由"包围的变量名，如 "No.1 Var", ":-)", "He ""!""" ——变量名不能以'开头
  JEP_BodyDefOp=':=';         //  Add(x,y):=(x+y);
  JEP_Class='class';          //  TFoo:Class(TObject)::=(Name:String; ID:Int);
  //Standard Operators
  JEOP_StrJoin='+>';
  //Standart Functions
  JEF_Between='BETWEEN';
  JEF_Break='BREAK';
  JEF_Case='CASE';
  JEF_Continue='CONTINUE';
  JEF_Dec='DEC';
  JEF_Echo='ECHO';
  JEF_Eval='EVAL';            //表达式求值
  JEF_Exit='EXIT';
  JEF_For='FOR';              // for(i:=0,i<=10,i+=2,n+=i)
  JEF_ForEach='FOREACH';      // foreach(i,MyArray,n+=i)
  JEF_ForTo='FORTO';          // forto(i:=0,10,2,n+=i)
  JEF_If='IF';
  JEF_IIf='IIF';
  JEF_Is='IS';
  //JEF_IfElse='IFELSE';
  JEF_Inc='INC';
  JEF_Include='INCLUDE';
  JEF_IsArray='ISARRAY';
  JEF_IsNull='ISNULL';
  JEF_Len='LEN';
  JEF_Loop='LOOP';            // Loop ... While BoolExpr
  JEF_New='NEW';
  JEF_Pred='PRED';
  JEF_Print='PRINT';
  JEF_Repeat='REPEAT';        // Repeat ... Until BoolExpr
  JEF_Reverse='REVERSE';
  JEF_Return='RETURN';
  JEF_Succ='SUCC';
  JEF_Times='TIMES';
  JEF_Wait='WAIT';
  JEF_While='WHILE';          // While BoolExpr ...
  JEF_WhileNot='WHILENOT';    // While NOT BoolExpr ...
  JEF_Rem='REM';              // Line commet
  JEF_Comment='COMMENT';      // Block commet
  JEF_DocRem='DOCREM';        // Document commet
  JEF_DirRem='DIRREM';        // Directive commet
  //定义相关关键字
  JED_HeadChar=' ';
  JED_Proc='PROCEDURE';         //方法
  JED_Func='FUNCTION';          //函数
  JED_Class='CLASS';            //类
  JED_Space='SPACE';            //空间
  JED_Var='VAR';                //变量
  JED_Const='CONST';            //常量
  JEDN_Members='MEMBERS';       //成员(类或空间的)
  JEDN_Params='PARAMS';         //参数表(方法的)
  JEDN_Body='BODY';             //主体(方法的)
  JEDN_As='AS';                 //类型标识
  JEDN_Extend='EXTEND';         //继承(类型)
  JEDN_Implement='IMPLEMENT';   //实现(接口)
  JEV_Result='RESULT';          //返回值(变量)
  JEV_ResultRep='ResulT';       //用于代换的 返回值(变量)
  JEP_Session='Session';        //Web脚本中的会话对象
  JEP_Request='Request';        //Web脚本中的请求对象
  //
  JEH_LineRem='//';             //单行注释
  JEH_DocRem='///';             //文档注释
  OpRank_Func:Byte=255;
  //Std chars
  Digits: set of Char=['0'..'9'];
  VarBegin: set of Char=['a'..'z', 'A'..'Z', '_', '$', '@', #129..#254];  //允许$,@以及汉字
  VarBody: set of Char=['a'..'z', 'A'..'Z', '_', '$', '@', '0'..'9', #129..#254];
  MathOp1: set of Char=['!', '~'];  //单目运算符
  MathOp2: set of Char=['+', '-', '*', '/', '\', '%', '^', '&', '|', '.', ':', ';', '?'];  //双目运算符
  CompOp2: set of Char=['=', '>', '<'];  //比较运算符
  MathOp3: set of Char=['+', '-', '*', '/', '\', '%', '^', '&', '|'];  //双目数学运算符
  MathOp4: set of Char=['*', '/', '\', '%', '^', '&', '|'];  //非符号双目数学运算符
  SetenceDiv: set of Char=[';'];
  ValOps: set of Char=[
    '!', '~',
    '+', '-', '*', '/', '\', '%', '^', '&', '|', '.', ':', '?',
    '=', '>', '<'];
type
  TOpChar=Char;
const
  OpCh_Sentence:TOpChar=';';
  OpCh_Define:TOpChar=' ';   //2011-09-18
type
  TParamIdxs=(pi1,pi2,pi3,pi4,pi5,pi6,pi7,pi8);
  TParamSet=set of TParamIdxs;
  TJSONLeaf={$IFDEF SUPEROBJECT}ISuperObject{$ELSE}TZAbstractObject{$ENDIF};
  TJSONObj={$IFDEF SUPEROBJECT}ISuperObject{$ELSE}JSONObject{$ENDIF};
  TTraceLineFunc=procedure (Sender: TObject; LineData: TJSONLeaf; const LineVal: Variant);
  TTraceValueFunc=procedure (Sender: TObject; const VarName: String; const Val: Variant);
  TVarToStrDefFunc=function (v: Variant; const ADefault: String): String;
  TPrintFunc=procedure (const Str: String);
  TConfirmFunc=function (const Str: String):Boolean;
  TExprType=(etEmpty, etNumber, etBool, etString, etMixed);
  TJEVarHelper=class;
  TJEFuncHelper=class;
  TJEOpHelper=class;
  { Expression Parser/Evaluator/Analyser 表达式解析/计算/分析器
    可以将类似 (X+3)*Y 的文本表达式解析为JSON对象，并可以对这个JSON对象进行计算求值
    语句求值规则：
      运算/比较表达式的值就是该表达式的结果  eg:  (2+3)*4  =>  20    IF(1>=0,1,0)  =>  1
      赋值语句的值是位于右侧的值表达式的值   eg:  X:=80+100  =>  180
      多个语句的值，是最后被执行的语句的值   eg:  X:=2; Y:=12; Y<<X;  =>  48
  }
  TJSONExprParser=class
  private
    FFuncHelper: TJEFuncHelper;
    FVarHelper: TJEVarHelper;
    FTraceOnLine: Boolean;
    FOnLineComplete: TTraceLineFunc;
    FPrintFunc: TPrintFunc;
    FVarToStrDefFunc: TVarToStrDefFunc;
    FOpHelper: TJEOpHelper;
    FUseVarHelperOnTextGen: Boolean;
    FUseVarHelperOnParse: Boolean;
    FEchoFunc: TPrintFunc;
    FConfirmFunc: TConfirmFunc;
    FTimeLimitConfirm: Integer;
    FStartEvalTime, FLastConfirmTime: TDateTime;
    procedure SetOnLineComplete(const Value: TTraceLineFunc);
    procedure SetTraceOnLine(const Value: Boolean);
    procedure SetPrintFunc(const Value: TPrintFunc);
    procedure SetVarToStrDefFunc(const Value: TVarToStrDefFunc);
    procedure SetOpHelper(const Value: TJEOpHelper);
    procedure SetUseVarHelperOnParse(const Value: Boolean);
    procedure SetUseVarHelperOnTextGen(const Value: Boolean);
    procedure SetEchoFunc(const Value: TPrintFunc);
    function TypeStrToVar(const Str: String):Variant;
    function VHEnterObj(const ObjName: String):TJEVarHelper;
    procedure SetConfirmFunc(const Value: TConfirmFunc);
    procedure SetTimeLimitConfirm(const Value: Integer);
    function CheckTimeLimit:Boolean;
  public
    property VarHelper: TJEVarHelper read FVarHelper;
    property FuncHelper: TJEFuncHelper read FFuncHelper;
  {$IFNDEF NO_OPTRANSLATE}
    property OpHelper: TJEOpHelper read FOpHelper write SetOpHelper;
  {$ENDIF}
    property UseVarHelperOnParse:Boolean read FUseVarHelperOnParse write SetUseVarHelperOnParse;
    property UseVarHelperOnTextGen:Boolean read FUseVarHelperOnTextGen write SetUseVarHelperOnTextGen;
    //是否在一个语句执行完毕时触发
    property TraceOnLine:Boolean read FTraceOnLine write SetTraceOnLine;
    property OnLineComplete:TTraceLineFunc read FOnLineComplete write SetOnLineComplete;
    property VarToStrDefFunc:TVarToStrDefFunc read FVarToStrDefFunc write SetVarToStrDefFunc;
    property PrintFunc:TPrintFunc read FPrintFunc write SetPrintFunc;
    property EchoFunc:TPrintFunc read FEchoFunc write SetEchoFunc;
    property ConfirmFunc:TConfirmFunc read FConfirmFunc write SetConfirmFunc;
    property TimeLimitConfirm:Integer read FTimeLimitConfirm write SetTimeLimitConfirm;
    function Eval(AObj: TJSONLeaf):Variant;
    function EvalNumber(AObj: TJSONLeaf; out Val:Double):Boolean;
    procedure InitForEval(EnableTimeLimit: Boolean);
    function OptimizeJSON(AObj: TJSONObj):TJSONObj;
    procedure AddVarHelper(AHelper: TJEVarHelper);
    procedure AddFuncHelper(AHelper: TJEFuncHelper);
    procedure RemoveVarHelper(AHelper: TJEVarHelper);
    procedure RemoveFuncHelper(AHelper: TJEFuncHelper);
    //将文本表达式转换为JSON表达式树，允许代入变量的值  2+(X*Sin(Y)) => {op:"+",p1:2,p2:{op:"*",p1:"X",p2:{op:"SIN",p1:"Y"}}}
    function ExprToJSON(const Expr: String; PStart: PInteger=nil; PEnd: PInteger=nil;
      POutExprLen: PInteger=nil):TJSONObj;
    function ExprToJSONStr(const Expr: String):String;
    class function GetLastExprType:TExprType;
    { 将JSON表达式还原为文本表达式
      如果指定了VarHelper，会将变量的值代入结果文本
      如果ParentOpRank<0，会为所有表达式加上括号
      如果ParentOpRank>0，会根据运算符优先级，返回尽量不带括号的结果
    }
    function JSONToExpr(AObj: TJSONObj; ParentOpRank: Integer=-1):String;
    class function VarToExprStr(V: Variant):String;
    class function VarNeeded(AObj: TJSONObj; var Vars: TStrings):Integer;
    class function Version:ShortString;
  end;
  { 变量值获取类
    可以通过传入的变量名或JSON格式的变量描述，提取变量的值。
    通过设置NextHelper属性，可以形成搜索链。
  }
  TJEVarHelper=class
  private
    FNextHelper: TJEVarHelper;
    procedure SetNextHelper(const Value: TJEVarHelper);
    function GetAsJSONString: String;
    procedure SetAsJSONString(const Value: String);
  protected
    function GetVarNames(const Idx: Integer): String; virtual;
    function GetVarCount: Integer; virtual;
    function GetTraceOnSet: Boolean; virtual;
    procedure SetTraceOnSet(const Value: Boolean); virtual;
  public
    property NextHelper: TJEVarHelper read FNextHelper write SetNextHelper;
    property TraceOnSet:Boolean read GetTraceOnSet write SetTraceOnSet;
    property AsJSONString:String read GetAsJSONString write SetAsJSONString;
    //对变量名进行规范化
    function CheckAndTransName(var VarName: String):Boolean; virtual;
    //Read Value
    function GetVar(const VarName: String; out Val: Variant):Boolean; virtual;
  {$IFNDEF NO_RECMEMBER}
    function GetVar2(AObj: TJSONObj; out Val: Variant):Boolean; virtual;
  {$ENDIF}
    function GetVarObj(const VarName: String; out Obj: TJSONObj):Boolean; virtual;
    function GetVarDef(const VarName: String; const Default: Variant):Variant;
    //Write Value
    function SetVar(const VarName: String; const Val: Variant):Boolean; virtual;
  {$IFNDEF NO_RECMEMBER}
    function SetVar2(AObj: TJSONObj; const Val: Variant):Boolean; virtual;
  {$ENDIF}
  {$IFNDEF NO_COMPLEXOBJ}
    //Member support
    function EnterObj(const ObjName: String):Boolean; virtual;
    function LeaveObj(const ObjName: String):Boolean; virtual;
  {$ENDIF}
    function VarIsObj(const VarName: String):Boolean; virtual;
    function SetObjectVar(const VarName: String; JObj: TJSONObj):Boolean; virtual;
    function SetObjVar(const VarName: String; AObj: TObject):Boolean; virtual;
    function GetObjAttr(const Attr: String):Variant; virtual;
    function SetObjAttr(const Attr: String; const Val: Variant):Boolean; virtual;
    function GetObjElement(AObj: TObject; const Index: Variant):Variant; virtual;
    function SetObjElement(AObj: TObject; const Index: Variant; Val:Variant):Boolean; virtual;
    function CallObjFunc(AObj: TObject; const Func: String; Args: array of Variant):Variant; virtual;
    //
    property VarCount:Integer read GetVarCount;
    property VarNames[const Idx:Integer]:String read GetVarNames;
    procedure Clean; virtual;
    //JSON I/O
    function ValImport(PlainObj: TJSONObj):Integer; virtual;
    function ValExport(PlainObj: TJSONObj):Integer; virtual;
  end;
  { 函数值获取类
    可以通过传入的JSON格式的函数，提取结果值。
    通过设置NextHelper属性，可以形成搜索链。
  }
  TJEFuncHelper=class
  private
    FNextHelper: TJEFuncHelper;
    procedure SetNextHelper(const Value: TJEFuncHelper);
  public
    property NextHelper: TJEFuncHelper read FNextHelper write SetNextHelper;
    function GetValue(Sender: TJSONExprParser; const Func: String;
      var Params: array of Variant; out Val:Variant;
      out OutParamIdx: TParamSet):Boolean; virtual;
    function GetValue2(Sender: TJSONExprParser; FuncObj: TJSONObj;
      var Params: array of Variant; out Val:Variant;
      out OutParamIdx: TParamSet):Boolean; virtual;
  end;
  { 符号转换类
    用于实现与其它语言之间的符号级兼容
  }
  TJEOpHelper=class
  public
    function TranslateOperator(const Op: ShortString; JNode: TJSONLeaf):ShortString; virtual; abstract;
    function RestoreOperator(const Op: ShortString; JNode: TJSONLeaf):ShortString; virtual; abstract;
  end;
  TSimpleVarHelper=class(TJEVarHelper)
  private
    FValueHolder:TJSONObj;
    FTraceOnSet: Boolean;
    FOnTrace: TTraceValueFunc;
  {$IFNDEF NO_COMPLEXOBJ}
    FRootHolder:TJSONObj;
    FValObjStatck:TStrings;
    {$IFDEF SUPEROBJECT}
    FValItfs:TInterfaceList;
    {$ENDIF}
  {$ENDIF}
    procedure SetOnTrace(const Value: TTraceValueFunc);
  protected
    function GetVarNames(const Idx: Integer): String; override;
    function GetVarCount: Integer; override;
    function GetTraceOnSet: Boolean; override;
    procedure SetTraceOnSet(const Value: Boolean); override;
  {$IFNDEF NO_COMPLEXOBJ}
    function ComplexValObjByName(const VarName: String):TJSONLeaf;
    function ComplexValObjByNameEx(const VarName: String; out AValHolder: TJSONObj):TJSONLeaf;
  {$ENDIF}
  public
    property OnTrace:TTraceValueFunc read FOnTrace write SetOnTrace;
    procedure Put(const VarName: String; V: Boolean); overload;
    procedure Put(const VarName: String; V: Double); overload;
    procedure Put(const VarName: String; V: Integer); overload;
    procedure Put(const VarName: String; const V: String); overload;
    procedure PutNull(const VarName: String);
    procedure Delete(const VarName: String);
    procedure Clean; override;
    function GetVar(const VarName: String; out Val:Variant):Boolean; override;
    function GetVarObj(const VarName: String; out Obj: TJSONObj):Boolean; override;
    function SetVar(const VarName: String; const Val: Variant):Boolean; override;
    function VarIsObj(const VarName: String):Boolean; override;
    function SetObjectVar(const VarName: String; JObj: TJSONObj):Boolean; override;
    function GetObjAttr(const Attr: String):Variant; override;
    function SetObjAttr(const Attr: String; const Val: Variant):Boolean; override;
  {$IFNDEF NO_COMPLEXOBJ}
    function EnterObj(const ObjName: String):Boolean; override;
    function LeaveObj(const ObjName: String):Boolean; override;
  {$ENDIF}
    constructor Create;
    destructor Destroy; override;
  end;
  { Var Helper can directly register and access variables in memory.  直接访问本地变量
  }
  TMemVarHelper=class(TJEVarHelper)
  private
    FVals:TStringList;
    FTypes:TStrings;
  protected
    function GetVarNames(const Idx: Integer): String; override;
    function GetVarCount: Integer; override;
  public
    procedure RegInt(const VarName: String; const P: PInteger);
    procedure RegDouble(const VarName: String; const P: PDouble);
    procedure RegBool(const VarName: String; const P: PBoolean);
    procedure RegChar(const VarName: String; const P: PChar);
    procedure RegByte(const VarName: String; const P: PByte);
    procedure RegWord(const VarName: String; const P: PWord);
    procedure RegLongWord(const VarName: String; const P: PLongWord);
    procedure RegShortInt(const VarName: String; const P: PShortInt);
    procedure RegInt64(const VarName: String; const P: PInt64);
    procedure RegSingle(const VarName: String; const P: PSingle);
    function GetVar(const VarName: String; out Val:Variant):Boolean; override;
    function SetVar(const VarName: String; const Val: Variant):Boolean; override;
    procedure Sort;
    function Sorted:Boolean;
    procedure Clean; override;
    constructor Create;
    destructor Destroy; override;
  end;

function VarEqual(const v1, v2: Variant):Boolean;
function VarToJSONObj(v: Variant):TJSONLeaf;
function VarFromJSON(Z:TJSONLeaf):Variant;
function Obj2Var(O: TObject): Variant; {$IF COMPILERVERSION>=18}inline;{$IFEND}
function Var2Obj(V: Variant): TObject; {$IF COMPILERVERSION>=18}inline;{$IFEND}
function IsNormalVarName(const Name: String):Boolean;
function GetStdOpRank(FirstCh: Char; const Op: String):Byte;

var
  DblQuotationAsString:Boolean=false;    //是否将双引号内的内容当成字符串
  UpperCaseNormalFuncName:Boolean=true;  //是否将一般的函数名转换成大写（引号内的函数名不做处理）
  Keep_Comment:Boolean=true;  //2013-03-29  是否保留代码中的注释
  OpRank:array [TOpChar] of Byte;  //操作符优先级表（根据操作符的首字母来确定）
  RK_Multi, RK_Add, RK_Shift, RK_Compare, RK_SetValue, RK_Block, RK_Sentence, RK_SpaceBreak:Byte;

implementation

type
  TVarAy=array of Variant;
  TBreakException=class(Exception)
    Level:Integer;
    constructor Create(Lv:Integer=1);
  end;
  TContinueException=class(Exception)
    Level:Integer;
    constructor Create(Lv:Integer=1);
  end;
  TExitException=class(Exception)
    ReturnVal: Variant;
    constructor CreateVal(V: Variant);
  end;

threadvar
  LastExprType:TExprType;
const
  OpCh_None:TOpChar=#0;
  OpCh_Func:TOpChar=#255;
  NumVarTypes: set of Byte=[varSmallint,varInteger,varSingle,varDouble,varCurrency,varByte,varWord,varLongWord];

function Obj2Var(O: TObject): Variant;
begin
  TVarData(Result).VType := varObject;
  TVarData(Result).vPointer := O;
end;

function Var2Obj(V: Variant): TObject;
begin
  if VarType(V)=varObject then
    Result:=TObject(TVarData(V).vPointer)
  else
    Result:=nil;
end;

function OctToInt(const S: String): Integer;
const OctMap:array [Char] of SmallInt =
  (
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    0, 1, 2, 3, 4, 5, 6, 7,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
   );
var
  i, n: Integer;
begin
  n:=Length(S);
  Result:=0;
  for i:=n downto 1 do
    Result:=Result*8+OctMap[S[i]];
end;

function BinToInt(const S: String): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=Length(S) downto 1 do
    if S[i]='1' then
      Result:=Result*2+1
    else
      Result:=Result*2;
end;

procedure InitOpRank;
var
  r:Byte;
begin                                                     
  OpRank[OpCh_Func]:=OpRank_Func;  //Function
  r:=240;
  OpRank['(']:=r; OpRank['[']:=r; OpRank['.']:=r; Dec(r,20);
  OpRank['!']:=r; OpRank['~']:=r; OpRank['N']:=r; Dec(r,20);  //NOT
  OpRank['*']:=r; OpRank['/']:=r; OpRank['\']:=r; OpRank['%']:=r; RK_Multi:=r;  Dec(r,20);
  OpRank['+']:=r; OpRank['-']:=r; RK_Add:=r;      Dec(r,10);
  RK_Shift:=r;    Dec(r,10);   // << >>  <<<  >>>
  OpRank['>']:=r; OpRank['<']:=r; OpRank['=']:=r; RK_Compare:=r;  Dec(r,20);
  OpRank['&']:=r; OpRank['|']:=r; OpRank['^']:=r; Dec(r,20);
  OpRank['A']:=r; Dec(r,20);  //AND
  OpRank['O']:=r; OpRank['X']:=r; Dec(r,20);  //OR XOR
  OpRank['I']:=r; Dec(r,20);  //IN IS
  OpRank[':']:=r; RK_SetValue:=r; Dec(r,10);  //:=
  OpRank[',']:=r; Dec(r,10);  // ,
  OpRank[';']:=r; RK_Sentence:=r; //Sentence end
  OpRank[OpCh_Define]:=r+2;  //Define  2011-09-18
  RK_SpaceBreak:=RK_Sentence-1; //2013-03-31
end;

function GetStdOpRank(FirstCh: Char; const Op: String):Byte;
var
  n:Integer;
  LastCh:Char;
begin
  n:=Length(Op);
  if n>1 then
  begin
    LastCh:=Op[n];
    if FirstCh in ['<','>'] then  //  >>  <<  >>>  <<<  ....
    begin
      if FirstCh=LastCh then
      begin
        Result:=RK_Shift;
        exit;
      end
      else if LastCh='=' then  //  >=  >>=  <=  <<=  ...
      begin
        if n=2 then  // >=  <=
          Result:=OpRank[FirstCh]
        else
          Result:=RK_SetValue;
        exit;
      end;
    end
    else if (FirstCh<>':') and (LastCh='=') then   // +=  *=  ^=  ...
    begin
      Result:=RK_SetValue;
      exit;
    end;
  end;
  Result:=OpRank[FirstCh];
end;

function IsNormalVarName(const Name: String):Boolean;
var
  i:Integer;
begin
  Result:=false;
  if (Name='') or not (Name[1] in VarBegin) then exit;  
  for i:=2 to Length(Name) do
    if not (Name[i] in VarBody) then exit;
  Result:=true;
end;

{$IFDEF SUPEROBJECT}
function JSONParamCount(AObj: TJSONObj):Integer;
begin
  Result:=0;
  if AObj=nil then exit;
  with AObj.GetEnumerator do
  while Current<>nil do
    begin
      Inc(Result);
      if not MoveNext then break;
    end;
end;

var
  _CNULL:TJSONObj=nil;
function CNULL:TJSONObj;
begin
  if _CNULL=nil then
    _CNULL:=SO('{"z":0}');
  Result:=_CNULL;
end;

function HexToInt(const S: String): Integer;
const HexMap:array [Char] of SmallInt =
  (
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
   -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
   );
var
  i, n, l: Integer;
begin
  Result:=0;
  l:=Length(S);
  if l=0 then exit;  
  if S[1]='$' then
    n:=2
  else if (l>=2) and (S[2] in ['x','X']) then
    n:=3
  else
    n:=1;
  for i:=n to l do
    Result:=Result*16+HexMap[S[i]];
end;
{$ENDIF}

function VarToJSONObj(v: Variant):TJSONLeaf;
var
  i:Integer;
begin
{$IFDEF SUPEROBJECT}
  Result:=SO(v);
{$ELSE}
  case VarType(v) of
    varNull:
      Result:=CNULL;
    varSmallInt, varInteger, varShortInt,  varWord:
      Result:=_Integer.create(Integer(v));
    varSingle, varDouble, varCurrency:
      Result:=_Double.create(Double(v));
    varDate:
      Result:=_String.create(DateTimeToStr(TDateTime(v)));
    varOleStr:
      Result:=_String.create(String(v));
    varBoolean:
      Result:=_Boolean.create(Boolean(v));
    varArray..varArray+varAny:
    begin
      Result:=JSONArray.create;
      for i:=VarArrayLowBound(v,1) to VarArrayHighBound(v,1) do
        JSONArray(Result).put(VarToJSONObj(v[i]));
    end;
    else
      Result:=_String.create(String(v));
  end;
{$ENDIF}
end;

procedure PutVarToJSON(JObj: TJSONObj; const VarName: String; const v: Variant);
begin
{$IFDEF SUPEROBJECT}
  JObj.O[VarName]:=VarToJSONObj(v);
{$ELSE}
  JObj.Put(VarName,VarToJSONObj(v));
{$ENDIF}
end;

procedure PutObjToJSON(JObj: TJSONObj; const VarName: String; Obj: TJSONLeaf);
begin
{$IFDEF SUPEROBJECT}
  JObj.O[VarName]:=Obj;
{$ELSE}
  JObj.Put(VarName,Obj);
{$ENDIF}
end;

function VarFromJSON(Z:TJSONLeaf):Variant;
var
  idx:Integer;
begin
  if Z=nil then exit;
{$IFDEF SUPEROBJECT}
  if Z.DataType=stString then
    Result:=Z.AsString
  else if Z.DataType=stString then
    Result:=Z.AsString
  else if Z.DataType=stInt then
    Result:=Z.AsInteger
  else if Z.DataType=stBoolean then
    Result:=Z.AsBoolean
  else if Z.DataType=stDouble then
    Result:=Z.AsDouble
  else if Z.DataType=stCurrency then
    Result:=Z.AsCurrency
  else if Z.DataType=stArray then
  begin
    with Z.AsArray do
    begin
      Result:=VarArrayCreate([0,length-1],varVariant);
      for idx:=0 to Pred(length) do
        Result[idx]:=VarFromJSON(O[idx]);
    end;
  end
  else
{$ELSE}
  if Z.ClassType=_String then
    Result:=Z.toString
  else if Z.ClassType=_Boolean then
    Result:=_Boolean(Z).boolValue
  else if Z.ClassType=_Double then
    Result:=_Double(Z).doubleValue
  else if Z.ClassType=_Integer then
    Result:=_Integer(Z).intValue
  else if Z.ClassType=JSONArray then
  begin
    with JSONArray(Z) do
    begin
      Result:=VarArrayCreate([0,length-1],varVariant);
      for idx:=0 to Pred(length) do
        Result[idx]:=VarFromJSON(get(idx));
    end;
  end
  else
{$ENDIF}
    Result:=Null;
end;

function VarCanCompare(const vt1, vt2: Integer):Boolean;
begin
  Result:=(vt1=vt2) or ((vt1 in NumVarTypes) and (vt2 in NumVarTypes));
end;

function CollectionOp(c1,c2:Variant; Op:Char):Variant;
var
  i,j,n1,n2,c:Integer;
  ay:array of Boolean;
  v:Variant;
  b:Boolean;
begin
  if not VarCanCompare(VarType(c2),VarType(c1)) then
  begin
    Result:=Null;
    exit;
  end;
  n1:=VarArrayHighBound(c1,1);
  n2:=VarArrayHighBound(c2,1);
  case Op of
    '+': //简单拼接，不考虑重复的情况
    begin
      Result:=VarArrayCreate([0,n1+n2+1],varVariant);
      for i:=0 to n1 do
        Result[i]:=c1[i];
      for i:=1 to n2+1 do
        Result[i+n1]:=c2[i-1];
    end;
    '-': //从c1中扣除在c2内的元素
    begin
      SetLength(ay,n1);
      c:=0;
      for i:=0 to n1 do
      begin
        b:=false;
        v:=c1[i];
        for j:=0 to n2 do
        begin
          if VarCanCompare(VarType(v),VarType(c2[j])) then
            if v=c2[j] then
            begin
              b:=true;
              Inc(c);
              break;
            end;
        end;
        ay[i]:=b;
      end;
      Result:=VarArrayCreate([0,n1-c],varVariant);
      c:=0;
      for i:=0 to n1 do
      begin
        if not ay[i] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      SetLength(ay,0);
    end;
    '^': //c1、c2中不在对方内的元素
    begin
      SetLength(ay,n1+n2+2);
      c:=0;
      for i:=0 to n1 do
      begin
        b:=false;
        v:=c1[i];
        for j:=0 to n2 do
        begin
          if VarCanCompare(VarType(v),VarType(c2[j])) then
            if v=c2[j] then
            begin
              b:=true;
              Inc(c);
              break;
            end;
        end;
        ay[i]:=b;
      end;
      for i:=0 to n2 do
      begin
        b:=false;
        v:=c2[i];
        for j:=0 to n1 do
        begin
          if VarCanCompare(VarType(v),VarType(c1[j])) then
            if v=c1[j] then
            begin
              b:=true;
              Inc(c);
              break;
            end;
        end;
        ay[i+n1+1]:=b;
      end;
      Result:=VarArrayCreate([0,n1+n2+1-c],varVariant);
      c:=0;
      for i:=0 to n1 do
      begin
        if not ay[i] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      for i:=0 to n2 do
      begin
        if not ay[i+n1+1] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      SetLength(ay,0);
    end;
    '&': //c1、c2的交集
    begin
      SetLength(ay,n1+n2+2);
      c:=0;
      for i:=0 to n1 do
      begin
        b:=false;
        v:=c1[i];
        for j:=0 to n2 do
        begin
          if VarCanCompare(VarType(v),VarType(c2[j])) then
            if v=c2[j] then
            begin
              b:=true;
              Inc(c);
              break;
            end;
        end;
        ay[i]:=b;
      end;
      Result:=VarArrayCreate([0,c-1],varVariant);
      c:=0;
      for i:=0 to n1 do
      begin
        if ay[i] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      SetLength(ay,0);
    end;
    '|': //c1、c2的并集
    begin
      SetLength(ay,n1+n2+2);
      c:=0;
      for i:=0 to n1 do
      begin
        b:=false;
        v:=c1[i];
        for j:=0 to n2 do
        begin
          if VarCanCompare(VarType(v),VarType(c2[j])) then
            if v=c2[j] then
            begin
              b:=true;
              Inc(c);
              break;
            end;
        end;
        ay[i]:=b;
      end;
      for i:=0 to n2 do
      begin
        b:=false;
        v:=c2[i];
        ay[i+n1+1]:=b;
      end;
      Result:=VarArrayCreate([0,c+n2],varVariant);
      c:=0;
      for i:=0 to n1 do
      begin
        if ay[i] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      for i:=0 to n2 do
      begin
        if ay[i+n1+1] then
        begin
          Result[c]:=c1[i];
          Inc(c);
        end;
      end;
      SetLength(ay,0);
    end;
  end;
end;

function CollectionCompare(c1,c2:Variant; const Op: String):Boolean;
var
  i,j,n1,n2:Integer;
  v:Variant;
  b:Boolean;
begin
  if not VarCanCompare(VarType(c2),VarType(c1)) or (Op='') then
  begin
    Result:=false;
    exit;
  end;
  n1:=VarArrayHighBound(c1,1);
  n2:=VarArrayHighBound(c2,1);
  case Op[1] of
    '=':
    begin
      Result:=(n1=n2) and (Length(Op)=1);
      if not Result then exit;
      for i:=0 to n1 do
      begin
        v:=c1[i];
        b:=false;
        for j:=0 to n2 do
        begin
          if VarEqual(v,c2[j]) then
          begin
            b:=true;
            break;
          end;
        end;
        if not b then
        begin
          Result:=false;
          exit;
        end;
      end;
    end;
    '>':
    begin
      if (Length(Op)>1) and (Op<>'>=') then
      begin
        Result:=false;
        exit;
      end;
      Result:=true;
      for i:=0 to n2 do
      begin
        v:=c2[i];
        b:=false;
        for j:=0 to n1 do
        begin
          if VarEqual(v,c1[j]) then
          begin
            b:=true;
            break;
          end;
        end;
        if not b then
        begin
          Result:=false;
          exit;
        end;
      end;
      if Op<>'>=' then
        Result:=Result and (n1>n2);
    end;
    '<':
    begin
      if (Length(Op)>1) and (Op<>'<=') then
      begin
        Result:=false;
        exit;
      end;
      Result:=true;
      for i:=0 to n1 do
      begin
        v:=c1[i];
        b:=false;
        for j:=0 to n2 do
        begin
          if VarEqual(v,c2[j]) then
          begin
            b:=true;
            break;
          end;
        end;
        if not b then
        begin
          Result:=false;
          exit;
        end;
      end;
      if Op<>'<=' then
        Result:=Result and (n1<n2);
    end;
    else
      Result:=false;
  end;
end;

function VarEqual(const v1, v2: Variant):Boolean;
begin
  Result:=VarCanCompare(VarType(v1),VarType(v2));
  if not Result then exit;
  if VarIsArray(v1) then
    Result:=CollectionCompare(v1,v2,'=')
  else
    Result:=v1=v2;
end;

{ TJSONExprParser }

procedure TJSONExprParser.AddFuncHelper(AHelper: TJEFuncHelper);
begin
  if AHelper=nil then exit;
  if VarHelper<>nil then
    AHelper.NextHelper:=FuncHelper;
  FFuncHelper:=AHelper;
end;

procedure TJSONExprParser.AddVarHelper(AHelper: TJEVarHelper);
var
  vh:TJEVarHelper;
begin
  if AHelper=nil then exit;
  if VarHelper<>nil then
  begin
    vh:=VarHelper;
    while vh.NextHelper<>nil do
      vh:=vh.NextHelper;
    vh.NextHelper:=AHelper;
  end
  else
    FVarHelper:=AHelper;
end;

function TJSONExprParser.CheckTimeLimit: Boolean;
var
  ms:Double;
begin
  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
  begin
    if FLastConfirmTime=0 then FLastConfirmTime:=FStartEvalTime;
    ms:=(Now-FLastConfirmTime)*86400000;
    if ms>=FTimeLimitConfirm then
      if Assigned(FConfirmFunc) then
      begin
        if FConfirmFunc('Reach time limit ('+Format('%.2f',[ms*0.001])+' sec).'#13#10'Stop or continue?  (Yes = Stop)') then
          raise TExitException.CreateVal(ms)
        else
          FLastConfirmTime:=Now;
      end
      else
        raise TExitException.CreateVal(ms);
  end;
end;

function TJSONExprParser.Eval(AObj: TJSONLeaf): Variant;
  function GetP1:Variant;
  begin
    Result:=Eval(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1));
  end;
  function GetP2:Variant;
  begin
    Result:=Eval(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2));
  end;
  function GetP3:Variant;
  begin
    Result:=Eval(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param3));
  end;
  function GetPN(n:Integer):Variant;
  begin
    Result:=Eval(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(n)));
  end;
  function GetParams(JObj: TJSONObj):TVarAy;
  var
    i,nn:Integer;
  begin
  {$IFDEF SUPEROBJECT}
    nn:=JSONParamCount(AObj);
    if nn<=1 then exit;
    SetLength(Result,nn-1);
    i:=0;
    with AObj.GetEnumerator do
      repeat
        Result[i]:=Eval(Current);
        Inc(i);
      until not MoveNext;
  {$ELSE}
    with TJSONObj(AObj) do
    begin
      if Length<=1 then exit;
      SetLength(Result,Length-1);
      for i:=0 to High(Result) do
        Result[i]:=GetPN(i);
    end;
  {$ENDIF}
  end;
  function GetP1Left:TJSONLeaf;
  begin
    Result:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
    if Result=nil then exit;
  {$IFDEF SUPEROBJECT}
    if Result.DataType<>stObject then
  {$ELSE}
    if not (Result is TJSONObj) then
  {$ENDIF}
    begin
      Result:=nil;
      exit;
    end;
    Result:=TJSONObj(Result).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
  end;
  function Func_IN:Boolean;
  var
    i:Integer;
    NullVal:Boolean;
    Z,Z2:TJSONLeaf;
    JObj:TJSONObj;
    v1,v2:Variant;
    vt1,vt2:Word;
  begin
    Result:=false;
    v1:=GetP1;
    NullVal:=VarIsNull(v1);
    Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2);
    if Z=nil then exit;
    if (Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF})
      and (TJSONObj(Z).{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator)='(') then
    begin
      for i:=1 to {$IFDEF SUPEROBJECT}64{$ELSE}Pred(TJSONObj(Z).Length){$ENDIF} do
      begin
        Z2:=TJSONObj(Z).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(i));
        {$IFDEF SUPEROBJECT}if Z2=nil then break;{$ENDIF}
        v2:=Eval(Z2);
        if NullVal then
        begin
          if VarIsNull(v2) then
            Result:=true;
        end
        else if not VarIsNull(v2) then
        begin
          vt1:=VarType(v1);
          vt2:=VarType(v2);
          if vt1=vt2 then
            Result:=v1=v2
          else begin  //不同类型  2011-09-01
            try
              Result:=v1=v2;
            except
            end;
          end;
        end;
        if Result then break;
      end;
    end
    else begin
      v2:=GetP2;
      if VarIsArray(v2) then
      begin
        for i:=VarArrayLowBound(v2,1) to VarArrayHighBound(v2,1) do
        begin
          if NullVal then
          begin
            if VarIsNull(v2[i]) then
              Result:=true;
          end
          else if not VarIsNull(v2[i]) then
            Result:=v1=v2[i];
          if Result then break;
        end;
      end
      else if NullVal then
      begin
        if VarIsNull(v2) then
          Result:=true;
      end
      else if not VarIsNull(v2) then
        Result:=v1=v2;
    end;
  end;
  function Func_Collection:Variant;
  var
    i,n:Integer;
    v1:Variant;
  begin
  {$IFDEF SUPEROBJECT}
    n:=1;
    while AObj.O[JEP_ParamHeader+IntToStr(n)]<>nil do
      Inc(n);
  {$ELSE}
    n:=TJSONObj(AObj).Length;
  {$ENDIF}
    Result:=VarArrayCreate([0,n-2],varVariant);
    for i:=1 to Pred(n) do
    begin
      v1:=GetPN(i);
      Result[i-1]:=v1;
    end;
  end;
  function Func_ArrayItem:Variant;   // A[10,2]  =>  {op:"[",p1:"A",p2:10,p3:2}
  var
    i,idx:Integer;
    v1:Variant;
  begin
    Result:=GetP1;
  {$IFDEF SUPEROBJECT}
    i:=1;
    while true do
    begin
      Inc(i);
      if AObj.O[JEP_ParamHeader+IntToStr(i)]=nil then break;
  {$ELSE}
    for i:=2 to Pred(TJSONObj(AObj).Length) do
    begin
  {$ENDIF}
      if not VarIsArray(Result) then
      begin
        if VarType(Result)=varString then  //2011-09-24  Bug fixed.
        begin
          try
            Result:=String(Result)[Integer(GetP2)];
          except
            Result:=Null
          end;
        end
        else
          Result:=Null;
        exit;
      end;
      try
        v1:=Eval(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(i)));
        idx:=v1;
        if (idx>=0) and (idx<=VarArrayHighBound(Result,1)) then
          Result:=Result[idx];
      except
        Result:=Null;
      end;
    end;
  end;
  procedure SetVal(Z: TJSONLeaf; Val: Variant; const PName: String=JEP_Param1);
  var
    Z2:TJSONLeaf;
    mstr,TmpStr:String;
    vh:TJEVarHelper;
  begin
    Result:=Val;  //将右侧表达式的值做为整个赋值过程的值
    if Z.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF} then
    begin
      mstr:=Z.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
      if mstr='' then exit;
      if not (mstr[1] in [JEP_StrParamHeader,JEP_TypeHead]) then
        VarHelper.SetVar(mstr,Result);
    end
    else if Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF} then  //赋值表达式的左部是复合表达式  2010-04-04
    begin
      mstr:={$IFDEF SUPEROBJECT}Z.S[JEP_Operator]{$ELSE}TJSONObj(Z).ValByIndex[0]{$ENDIF};
      if mstr='.' then
      begin
      {$IFDEF NO_COMPLEXOBJ}
      {$IFNDEF NO_RECMEMBER}
        VarHelper.SetVar2(TJSONObj(Z),Result);
      {$ENDIF}
      {$ELSE}
        Z2:=TJSONObj(Z).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
        if Z2{$IFDEF SUPEROBJECT}.DataType=stString{$ELSE}is _String{$ENDIF} then
        begin   // Obj.xx := ...
          TmpStr:=Z2.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
          vh:=VHEnterObj(TmpStr);
          if vh<>nil then
            with vh do
            begin
              SetObjAttr(TJSONObj(Z).{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Param2),Result);
              LeaveObj(TmpStr);
            end
          else //Obj not found...
            ;
        end
        else if Z2{$IFDEF SUPEROBJECT}.DataType=stObject{$ELSE}is TJSONObj{$ENDIF} then
        begin   // Obj.Prop.xxx := ...

        end;
      {$ENDIF}
      end;
    end;
  end;
  procedure SetValue(Val: Variant; const PName: String=JEP_Param1);
  begin
    SetVal(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(PName),Val);  //将右侧表达式的值做为整个赋值过程的值
  end;
  procedure IncVal(AObj: TJSONLeaf; Val: Variant);
  begin
    SetVal(AObj,Eval(AObj)+Val);
  end;
  function DoEval(const ExprStr: String):Variant;
  var
    JObj:TJSONObj;
  begin
    Result:=Null;
    if ExprStr='' then exit;
    try
      JObj:=ExprToJSON(ExprStr);
      if JObj=nil then exit;
      try
        Result:=Eval(JObj);
      finally
        JObj{$IFDEF SUPEROBJECT}:=nil{$ELSE}.Free{$ENDIF};
      end;
    except
    end;
  end;
var
  Func,mstr,TmpStr:String;
  Func1:AnsiChar;
  v1,v2,v3:Variant;
  VParams:TVarAy;
  OutSet:TParamSet;
  Done:Boolean;
  Z,Z2:TJSONLeaf;
  ParamCnt,i,n,c,EndVal,StepVal:Integer;
  w1,w2,w3,w4,w5,w6,w7:Word;
  vh:TJEVarHelper;
begin
  Result:=Null;
  if AObj=nil then exit;
  if AObj.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>TJSONObj{$ENDIF} then
  begin
    if AObj.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF} then
    begin
      mstr:={$IFDEF SUPEROBJECT}AObj.AsString{$ELSE}_String(AObj).toString{$ENDIF};
      if (mstr<>'') and not (mstr[1] in [JEP_StrParamHeader,JEP_TypeHead]) then  //变量名
      begin
        if VarHelper<>nil then
          VarHelper.GetVar(mstr,Result);
      end
      else if mstr[1]=JEP_StrParamHeader then //加了前缀的字符串，此时应当去掉前缀
        Result:=Copy(mstr,2,MaxInt)
      else
        Result:=TypeStrToVar(mstr);
    end
    else if AObj.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_Boolean{$ENDIF} then
      Result:={$IFDEF SUPEROBJECT}AObj.AsBoolean{$ELSE}_Boolean(AObj).boolValue{$ENDIF}
    else if AObj{$IFDEF SUPEROBJECT}.DataType in [stDouble,stCurrency,stInt]{$ELSE}is _Number{$ENDIF} then
      Result:={$IFDEF SUPEROBJECT}AObj.AsDouble{$ELSE}_Number(AObj).doubleValue{$ENDIF};
    exit;
  end;
  with TJSONObj(AObj) do
  begin
  {$IFDEF SUPEROBJECT}
    ParamCnt:=JSONParamCount(AObj)-1;
  {$ELSE}
    ParamCnt:=Length-1;
  {$ENDIF}
    if ParamCnt<0 then exit;  //不含任何数据
    Z:={$IFDEF SUPEROBJECT}O[JEP_Operator]{$ELSE}ValObjByIndex[0]{$ENDIF};  //第一个成员，应当是 BIOS_Operator
  end;
  if (Z<>nil) and (Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF}) then  //操作符是JSON对象
  begin
    if FuncHelper<>nil then
    begin
      case ParamCnt+1{TJSONObj(AObj).Length} of
        2:
        begin
          SetLength(VParams,1);
          VParams[0]:=GetP1;  //[GetP1];
          FuncHelper.GetValue2(Self,TJSONObj(Z),VParams,Result,OutSet);
          if OutSet<>[] then  //Write back Out Param   2010-08-12
            SetValue(VParams[0]);
          SetLength(VParams,0);
        end;
        3:
        begin
          SetLength(VParams,2);
          VParams[0]:=GetP1;  //[GetP1,GetP2];
          VParams[1]:=GetP2;
          FuncHelper.GetValue2(Self,TJSONObj(Z),VParams,Result,OutSet);
          if OutSet<>[] then
          begin
            if pi1 in OutSet then
              SetValue(VParams[0],JEP_Param1);
            if pi2 in OutSet then
              SetValue(VParams[1],JEP_Param2);
          end;
          SetLength(VParams,0);
        end;
        else
        begin
          VParams:=GetParams(TJSONObj(AObj));
          FuncHelper.GetValue2(Self,TJSONObj(Z),VParams,Result,OutSet);
          if OutSet<>[] then
          begin
            for i:=Integer(Low(TParamIdxs)) to Integer(High(TParamIdxs)) do
            begin
              if TParamIdxs(i) in OutSet then
                SetValue(VParams[i],JEP_ParamHeader+IntToStr(i));
            end;
          end;
        end;
      end;
    end;
    exit;
  end;
  if Z=nil then
    Func:=''
  else
    Func:=Z.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
  if Func='' then  //如果操作符为空，就用第一个参数的值
  begin
    if ParamCnt>1 then  //无操作符，但有多个参数，说明是集合  (A,B,...)  2011-09-22
    begin
      Result:=Func_Collection;
    end
    else
      Result:=GetP1;
    exit;
  end;
  Func1:=Func[1];
  try
    Done:=true;
    case Length(Func) of
      1:
      begin
        if Func1='.' then  //something like:  Plan.Max
        begin
          if VarHelper<>nil then
          begin
          {$IFDEF NO_COMPLEXOBJ}
          {$IFNDEF NO_RECMEMBER}
            VarHelper.GetVar2(TJSONObj(AObj),Result);
          {$ENDIF}
          {$ELSE}
            Z2:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
            if Z2{$IFDEF SUPEROBJECT}.DataType=stString{$ELSE} is _String{$ENDIF} then
            begin
              TmpStr:=Z2.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
              vh:=VHEnterObj(TmpStr);
              if vh<>nil then
                with vh do
                begin
                  Z2:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2);
                  if Z2{$IFDEF SUPEROBJECT}.DataType=stString{$ELSE} is _String{$ENDIF} then
                    Result:=GetObjAttr(Z2.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF})
                  else
                    Result:=GetP2;
                  LeaveObj(TmpStr);
                end;
            end;
          {$ENDIF}
          end;
          exit;
        end
        else if Func1 in (MathOp2+CompOp2) then
        begin
          if Func1=';' then
          begin
            //尽量获取后面那个表达式的值，如果只有一个表达式，那就获取第一个表达式的值
            //eg:  X:=10; Y:=3; X*Y-9   => 21
            n:=ParamCnt+1{TJSONObj(AObj).Length};
            if n>2 then
            begin
              Result:=GetP1;
            {$IFNDEF NO_TRACE}
              if TraceOnLine then
              begin
                Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,TJSONObj(Z),Result);
              end;
            {$ENDIF}
              Result:=GetP2;
            {$IFNDEF NO_TRACE}
              if TraceOnLine then
              begin
                Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2);
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,TJSONObj(Z),Result);
              end;
            {$ENDIF}
              //允许 ; 表达式有多于两个的参数  2010-6-27
              for i:=3 to n-1 do
              begin
                Result:=GetPN(i);
              {$IFNDEF NO_TRACE}
                if TraceOnLine then
                begin
                  Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(i));
                  if Assigned(FOnLineComplete) then
                    FOnLineComplete(Self,TJSONObj(Z),Result);
                end;
              {$ENDIF}
              end;
            end
            else begin
              Result:=GetP1;
            {$IFNDEF NO_TRACE}
              if TraceOnLine then
              begin
                Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,TJSONObj(Z),Result);
              end;
            {$ENDIF}
            end;
            exit;
          end;
          v1:=GetP1;
          v2:=GetP2;
          if VarIsNull(v1) or VarIsNull(v2) then  //两个表达式有一个为空的情况
          begin
            if Func[1]='=' then
            begin
              if VarIsNull(v1) and VarIsNull(v2) then
                Result:=true
              else
                Result:=false;
              exit;
            end;
            Result:=Null;
            exit;
          end;
        {$IFNDEF NO_COLLECTION}
          //Collection operation
          if VarIsArray(v1) then
          begin
            case Func[1] of
              '+','-','^','&','|': Result:=CollectionOp(v1,v2,Func[1]);
              '=','>','<': Result:=CollectionCompare(v1,v2,Func);//
              else Done:=false;
            end;
          end
          else
        {$ENDIF}
            case Func1 of
              '+': Result:=v1+v2;  //String? Number?
              '-': Result:=v1-v2;
              '*': Result:=v1*v2;
              '/': Result:=v1/v2;
              '\': Result:=Integer(v1) div Integer(v2);
              '%': Result:=Integer(v1) mod Integer(v2);
              '^': Result:=Integer(v1) xor Integer(v2);
              '&': Result:=Integer(v1) and Integer(v2);
              '|': Result:=Integer(v1) or Integer(v2);
              '=': Result:=v1=v2;
              '>': Result:=v1>v2;
              '<': Result:=v1<v2;
              else Done:=false;
            end;
        end
        else if Func1='!' then
        begin
          Result:=not Boolean(GetP1);
        end
        else if Func1='~' then
        begin
          Result:=not Integer(GetP1);
        end
        else if Func1='(' then  //Collection
        begin
          Result:=Func_Collection;
        end
        else if Func1='[' then  //Array Element  2011-09-01
        begin
          Result:=Func_ArrayItem;
        end
        else
          Done:=false;
      end;
      2:
      begin
        if (Func1 in MathOp3) and (Func[2]='=') then   //2011-09-03 +=  -=  *=  ....
        begin
          v1:=GetP1;
          v2:=GetP2;
          case Func1 of
            '+': Result:=v1+v2;
            '-': Result:=v1-v2;
            '*': Result:=v1*v2;
            '/': Result:=v1/v2;
            '\': Result:=Integer(v1) div Integer(v2);
            '%': Result:=Integer(v1) mod Integer(v2);
            '^': Result:=Integer(v1) xor Integer(v2);
            '&': Result:=Integer(v1) and Integer(v2);
            '|': Result:=Integer(v1) or Integer(v2);
            else Done:=false;
          end;
          SetValue(Result);
        end
        else if (Func1 in ['+','-']) and (Func[2]=Func1) then  //2011-09-23  ++  --
        begin
          v1:=GetP1;
          if Func1='+' then
            Result:=v1+1
          else
            Result:=v1-1;
          SetValue(Result);
        end
        else if Func1 in CompOp2 then
        begin
          v1:=GetP1;
        {$IFNDEF NO_COLLECTION}
          //Collection operation
          if VarIsArray(v1) then
            Result:=CollectionCompare(v1,GetP2,Func)
          else
        {$ENDIF}
          if Func='>=' then
            Result:=v1>=GetP2
          else if Func='<=' then
            Result:=v1<=GetP2
          else if Func='<>' then
            Result:=v1<>GetP2
          else if Func='==' then  //2011-12-31
          begin
            v2:=GetP2;
            if VarType(v1)=VarType(v2) then
              Result:=v1=v2
            else
              Result:=false;
          end
          else if Func='>>' then
            Result:=Integer(v1) shr Integer(GetP2)
          else if Func='<<' then
            Result:=Integer(v1) shl Integer(GetP2)
          else
            Done:=false;
        end
        else begin
          case Func1 of
            '+':
            begin
              if Func[2]='>' then  //+>  String join
                Result:=String(GetP1)+String(GetP2)
              else
                Done:=false;
            end;
          {$IFNDEF NO_ASSIGNMENT}
            ':':
            begin
              if Func[2]='=' then  //:=  Set variable value
              begin
                if VarHelper<>nil then
                  SetValue(GetP2);
              end
              else
                Done:=false;
            end;
          {$ENDIF}
            'I':
            begin
              case Func[2] of
              {$IFNDEF NO_IF}
                'F': //IF
                begin
                  Result:=Null;
                  v1:=GetP1;
                  if VarType(v1)=varBoolean then
                  begin
                    if Boolean(v1) then
                      GetP2
                    else if ParamCnt<=3 then  // if(b1,v2,v3)
                      GetP3
                    else begin // if(b1,v2,b3,v4...)
                      i:=3;
                      while i<ParamCnt do
                      begin
                        v1:=GetPN(i);
                        if VarType(v1)=varBoolean then
                          if Boolean(v1) then
                          begin
                            GetPN(i+1);
                            i:=-1;  //避免break后被当成没有匹配成功
                            break;
                          end;
                        Inc(i,2);
                      end;
                      //最后一个ELSE
                      if i=ParamCnt then
                        GetPN(i);
                    end;
                  end;
                end;
              {$ENDIF}
                'N': //IN
                begin
                  Result:=Func_IN;
                end;
                'S': //IS
                begin
                  Result:=false;
                  v1:=GetP1;
                  v2:=GetP2;
                  Result:=(VarIsNull(v1) xor not VarIsNull(v2));
                end;
                else
                  Done:=false;
              end;
            end;
            'O':
            begin
              if Func[2]='R' then  //OR
                Result:=GetP1 or GetP2
              else
                Done:=false;
            end;
            else
              Done:=false;
          end;
        end;
      end;
      3:
      begin
        case Func1 of
          'A':
            if Func='AND' then
              Result:=Boolean(GetP1) and Boolean(GetP2)
            else
              Done:=false;
          'D':
            if Func=JEF_Dec then
              SetValue(GetP1-1)
            else
              Done:=false;
          'F':
            if Func=JEF_For then
            begin
              n:=0;
              try
                GetP1;  //Init
                while VarEqual(GetP2,true) do  //Check
                begin
                  Inc(n);
                  try
                    GetPN(4); //Body
                  except
                    on e:TContinueException do
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1);
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                  GetP3;    // Inc(i)
                end;
              except
                on e:TExitException do
                begin
                  raise TExitException.Create('');
                end;
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;
            end
            else
              Done:=false;
          'I':
            if Func=JEF_Inc then
              SetValue(GetP1+1)
            else if Func='INT' then  //2010-08-11
              Result:=Integer(GetP1)
            else if Func='IIF' then  //2012-02-19
            begin
              Result:=Null;
              v1:=GetP1;
              if VarType(v1)=varBoolean then
              begin
                if Boolean(v1) then
                  Result:=GetP2
                else if ParamCnt<=3 then  // iif(b1,v2,v3)
                  Result:=GetP3
                else // ParamCnt>3    iif(b1,v2,b3,v4.....)
                begin
                  i:=3;
                  while i<ParamCnt do
                  begin
                    v1:=GetPN(i);
                    if VarType(v1)=varBoolean then
                      if Boolean(v1) then
                      begin
                        Result:=GetPN(i+1);
                        i:=-1;  //见if
                        break;
                      end;
                    Inc(i,2);
                  end;
                  if i=ParamCnt then
                    Result:=GetPN(i);
                end;
              end;
            end
            else
              Done:=false;
          'L':
            if Func=JEF_Len then
            begin
              v1:=GetP1;
            {$IFNDEF NO_COLLECTION}
              if VarIsArray(v1) then  //array length
              begin
                if ParamCnt>1 then
                  Result:=VarArrayHighBound(v1,GetP2)+1
                else
                  Result:=VarArrayHighBound(v1,0)+1;
              end
              else
            {$ENDIF}
                Result:=Length(String(GetP1));  //String length
            end
            else
              Done:=false;
          'N':
            if Func='NOT' then
            begin
              Result:=GetP1;
              if VarIsNull(Result) then
                Result:=Unassigned
              else if VarIsEmpty(Result) then
                Result:=Null
              else
                Result:=not Boolean(Result);
            end
            else if Func='NOW' then
            begin
              Result:=Now;
              v1:=GetP1;  //通过参数实现时间取整  2011-06-23
              if not VarIsNull(v1) then
              begin
                mstr:=v1;
                if mstr<>'' then
                begin
                  case mstr[1] of
                    'D','d':
                    begin
                      Result:=Trunc(Result);
                    end;
                    'M','m':
                    begin
                      DecodeDate(Result,w1,w2,w3);
                      Result:=EncodeDate(w1,w2,1);
                    end;
                    'Y','y':
                    begin
                      DecodeDate(Result,w1,w2,w3);
                      Result:=EncodeDate(w1,1,1);
                    end;
                    'H','h':
                    begin
                      DecodeTime(Result,w4,w5,w5,w5);
                      Result:=Trunc(Result)+EncodeTime(w4,0,0,0);
                    end;
                    'N','n':
                    begin
                      DecodeTime(Result,w4,w5,w6,w6);
                      Result:=Trunc(Result)+EncodeTime(w4,w5,0,0);
                    end;
                    'S','s':
                    begin
                      DecodeTime(Result,w4,w5,w6,w7);
                      Result:=Trunc(Result)+EncodeTime(w4,w5,w6,0);
                    end;
                  end;
                  Result:=TDateTime(Result);
                end;
              end;
            end
            else if Func='NEW' then  //2012-05-30
            begin
              raise Exception.Create('Not implemented!');
            end
            else
              Done:=false;
          'X':
            if Func='XOR' then
              Result:=Boolean(GetP1) xor Boolean(GetP2)
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      4:
      begin
        case Func1 of
          'B':
            if Func='BOOL' then
            begin
              Result:=Boolean(GetP1);
            end
            else
              Done:=false;
          'C':
            if Func=JEF_Case then  //2011-09-22
            begin
              v1:=GetP1;
              i:=2;
              while i<ParamCnt do
              begin
                if VarEqual(v1,GetPN(i)) then
                begin
                  Result:=GetPN(i+1);
                  break;
                end;
                Inc(i,2);
              end;
              //最后一个ELSE
              if (i=ParamCnt) and VarIsNull(Result) then
                Result:=GetPN(i);
            end
            else
              Done:=false;
          'D':
            if Func='DATE' then  //2011-09-01
            begin
              Result:=StrToDate(GetP1);
            end
            else
              Done:=false;
          'E':
            if Func=JEF_Echo then  //2011-09-03
            begin
              if Assigned(EchoFunc) then
                EchoFunc(GetP1);
              Result:=0;
            end
            else if Func=JEF_Eval then  //2011-09-03
            begin
              Result:=DoEval(GetP1);
            end
            else if Func=JEF_Exit then  //2011-09-03
            begin
              raise TExitException.Create('');
            end
            else
              Done:=false;
          'L':
            if Func=JEF_Loop then
            begin
              n:=0;
              try
                repeat
                  Inc(n);
                  try
                    GetP1;
                  except
                    on e:TContinueException do
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1)
                      else
                        continue;
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                until not VarEqual(GetP2,true);
              except
                on e:TExitException do
                  raise TExitException.Create('');
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;
            end
            else
              Done:=false;
          'P':
            if Func=JEF_Pred then  //2011-09-28
            begin
              Result:=Pred(Integer(GetP1));
            end
            else
              Done:=false;
          'S':
            if Func=JEF_Succ then  //2011-09-28
            begin
              Result:=Succ(Integer(GetP1));
            end
            else
              Done:=false;
          'T':
            if Func='TIME' then  //2011-09-01
            begin
              Result:=StrToTime(GetP1);
            end
            else
              Done:=false;
          'W':
            if Func=JEF_Wait then  //2011-09-06
            begin
              Result:=Cardinal(GetP1);
              Sleep(Result);
            end
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      5:
      begin
        case Func1 of
          'B':
            if Func=JEF_Break then
            begin
              Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
              if Z=nil then
                raise TBreakException.Create //跳出循环结构
              else
                raise TBreakException.Create(Eval(Z))
            end
            else
              Done:=false;
          'F':
            if Func='FLOAT' then
            begin
              Result:=Double(GetP1);
            end
            else if Func=JEF_ForTo then
            begin
              //  forto i:=0 to 100 step 10
              Z:=GetP1Left;  // Extract variable
              if Z=nil then exit;
              n:=0;
              try
                GetP1;  //Init
                EndVal:=GetP2;
                StepVal:=GetP3;
                v1:=Eval(Z);
                while ((StepVal>=0) and (v1<=EndVal) or ((StepVal<0) and (v1>=EndVal))) do  //Check
                begin
                  Inc(n);
                  try
                    GetPN(4); //Body
                  except
                    on e:TContinueException do
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1);
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                  IncVal(Z,StepVal);
                  v1:=Eval(Z);
                end;
              except
                on e:TExitException do
                  raise TExitException.Create('');
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;
            end
            else
              Done:=false;
          'P':
            if Func=JEF_Print then
            begin
              if Assigned(VarToStrDefFunc) then
                Result:=VarToStrDefFunc(GetP1,'')
              else
                Result:=VarToStrDef(GetP1,'');
              if Assigned(PrintFunc) then
              begin
                PrintFunc(Result);
              end;
            end
            else
              Done:=false;
          'T':
            if Func=JEF_Times then
            begin
              n:=GetP1;
              c:=0;
              try
                for i:=n-1 downto 0 do
                begin
                  Inc(c);
                  try
                    GetP2;
                  except
                    on e:TContinueException do  //2011-09-24
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1)
                      else
                        continue;
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                end;
              except
                on e:TExitException do
                  raise TExitException.Create('');
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=c;
            end
            else
              Done:=false;
          'W':
            if Func=JEF_While then
            begin
              n:=0;
              try
                while VarEqual(GetP1,true) do
                begin
                  Inc(n);
                  try
                    GetP2;
                  except
                    on e:TContinueException do  //2011-09-24
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1)
                      else
                        continue;
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                end;
              except
                on e:TExitException do
                  raise TExitException.Create('');
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;  //循环结构的返回值就是进入循环体的次数
            end
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      6:
      begin
        case Func1 of
          'I':
            if Func=JEF_IsNull then
            begin
              Result:=GetP1;
              if VarIsNull(Result) then
                Result:=GetP2;
            end
            else
              Done:=false;
          'R':
            if Func=JEF_Repeat then
            begin
              n:=0;
              try
                repeat
                  Inc(n);
                  try
                    GetP1;
                  except
                    on e:TContinueException do  //2011-09-24
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1)
                      else
                        continue;
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                until VarEqual(GetP2,true);
              except
                on e:TExitException do
                begin
                  raise TExitException.Create('');
                end;
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;  //循环结构的返回值就是进入循环体的次数
            end
            else if Func=JEF_Return then
            begin
              raise TExitException.CreateVal(GetP1);
            end
            else
              Done:=false;
          'S':
            if Func='STRING' then
            begin
              Result:=String(GetP1);
            end
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      7:
      begin
        case Func1 of
          'B':
            if Func=JEF_Between then
            begin
              Result:=false;
              if ParamCnt>=3 then  //2011-09-01
              begin
                v1:=GetP1;
                v2:=GetP2;
                v3:=GetP3;
                Result:=(v1>=v2) and (v1<=v3);
              end;
            end
            else
              Done:=false;
          'F':
            if Func=JEF_ForEach then
            begin
              Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
              v2:=GetP2;
              if VarIsArray(v2) then
              begin
                for i:=VarArrayLowBound(v2,1) to VarArrayHighBound(v2,1) do
                begin
                  SetVal(Z,v2[i]);
                  GetP3;
                end;
              end;
            end
            else
              Done:=false;
          'I':
            if Func=JEF_IsArray then
            begin
              Result:=GetP1;
              if VarIsArray(Result) then
                Result:=GetP2;
            end
            else
              Done:=false;
          {'R':
            if Func=JEF_Reverse then
            begin
              Result:=GetP1;
              if VarIsArray(Result) then
                Result:=GetP2;
            end
            else
              Done:=false;}
          'N':
            if Func='NOWDATE' then  //2012-06-17
            begin
              Result:=Date;
            end
            else if Func='NOWTIME' then  //2012-03-19
            begin
              Result:=GetTime;
              v1:=GetP1;  //时间取整
              if not VarIsNull(v1) then
              begin
                mstr:=v1;
                if mstr<>'' then
                  case mstr[1] of
                    'H','h':
                    begin
                      DecodeTime(Result,w4,w5,w5,w5);
                      Result:=EncodeTime(w4,0,0,0);
                    end;
                    'N','n':
                    begin
                      DecodeTime(Result,w4,w5,w6,w6);
                      Result:=EncodeTime(w4,w5,0,0);
                    end;
                    'S','s':
                    begin
                      DecodeTime(Result,w4,w5,w6,w7);
                      Result:=EncodeTime(w4,w5,w6,0);
                    end;
                  end;
              end;
              Result:=TDateTime(Result);
            end
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      8:
      begin
        case Func1 of
          'C':
            if Func=JEF_Continue then  //2011-09-24
            begin
              Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
              if Z=nil then
                raise TContinueException.Create  //产生异常，跳出循环结构
              else
                raise TContinueException.Create(Eval(Z));
            end
            else
              Done:=false;
          'D':
            if Func='DATETIME' then  //2011-09-01
            begin
              Result:=StrToDateTime(GetP1);
            end
            else
              Done:=false;
          'W':
            if Func=JEF_WhileNot then
            begin
              n:=0;
              try
                while not VarEqual(GetP1,true) do
                begin
                  Inc(n);
                  try
                    GetP2;
                  except
                    on e:TContinueException do
                    begin
                      if e.Level>1 then
                        raise TContinueException.Create(e.Level-1)
                      else
                        continue;
                    end;
                  end;
                  if (FTimeLimitConfirm>0) and (FStartEvalTime<>0) then
                    CheckTimeLimit;
                end;
              except
                on e:TExitException do
                  raise TExitException.Create('');
                on e:TBreakException do
                begin
                  if e.Level>1 then
                    raise TBreakException.Create(e.Level-1);
                end;
              end;
              Result:=n;  //循环结构的返回值就是进入循环体的次数
            end
            else
              Done:=false;
          else
            Done:=false;
        end;
      end;
      else
        Done:=false;
    end;
    if Done then exit;
    if FuncHelper<>nil then
      case ParamCnt+1 of
        2: //FuncHelper.GetValue(Self,Func,[GetP1],Result,OutSet);
        begin
          SetLength(VParams,1);
          VParams[0]:=GetP1;  //[GetP1];
          FuncHelper.GetValue(Self,Func,VParams,Result,OutSet);
          if OutSet<>[] then  //Write back Out Param   2010-08-12
            SetValue(VParams[0]);
          SetLength(VParams,0);
        end;
        3: //FuncHelper.GetValue(Self,Func,[GetP1,GetP2],Result,OutSet);
        begin
          SetLength(VParams,2);
          VParams[0]:=GetP1;  //[GetP1,GetP2];
          VParams[1]:=GetP2;
          FuncHelper.GetValue(Self,Func,VParams,Result,OutSet);
          if OutSet<>[] then
          begin
            if pi1 in OutSet then
              SetValue(VParams[0],JEP_Param1);
            if pi2 in OutSet then
              SetValue(VParams[1],JEP_Param2);
          end;
          SetLength(VParams,0);
        end;
        else begin //FuncHelper.GetValue(Self,Func,GetParams(TJSONObj(AObj)),Result,OutSet);
          VParams:=GetParams(TJSONObj(AObj));
          FuncHelper.GetValue(Self,Func,VParams,Result,OutSet);
          if OutSet<>[] then
          begin
            for i:=Integer(Low(TParamIdxs)) to Integer(High(TParamIdxs)) do
            begin
              if TParamIdxs(i) in OutSet then
                SetValue(VParams[i],JEP_ParamHeader+IntToStr(i+1));
            end;
          end;
        end;
      end;
  except
    on e:TExitException do
    begin
      raise TExitException.Create(e.Message);
    end;
    on e:TBreakException do
    begin
      raise TBreakException.Create(e.Level);
    end;
    on e:TContinueException do
    begin
      raise TContinueException.Create(e.Level);
    end;
    on e:Exception do
      Result:=Null;
  end;
end;

function TJSONExprParser.EvalNumber(AObj: TJSONLeaf; out Val: Double):Boolean;
  function GetP1(var OK: Boolean):Double;
  begin
    OK:=EvalNumber(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1),Result);
  end;
  function GetP2(var OK: Boolean):Double;
  begin
    OK:=EvalNumber(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2),Result);
  end;
  function GetPN(n:Integer; var OK: Boolean):Double;
  begin
    OK:=EvalNumber(TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(n)),Result);
  end;
  function GetParams(JObj: TJSONObj; var OK: Boolean):TVarAy;
  var
    k,Len:Integer;
    f:Double;
  begin
    with TJSONObj(AObj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF};
      if Len<=1 then exit;
      SetLength(Result,Len-1);
      for k:=0 to High(Result) do
      begin
        f:=GetPN(k,OK);
        if not OK then exit;
        Result[k]:=f;
      end;
    end;
  end;
  procedure SetValue;
  var
    Z:TJSONLeaf;
    mstr:String;
  begin
    Val:=GetP2(Result);  //将右侧表达式的值做为整个赋值过程的值
    if not Result then exit;
    Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
    if Z<>nil then
    begin
      if Z.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF} then
      begin
        mstr:=Z.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
        if mstr='' then exit;
        if mstr[1]<>JEP_StrParamHeader {in VarBegin} then
          VarHelper.SetVar(mstr,Val);
      end;
    end;
  end;
var
  Func,mstr:String;
  F1:Char;
  v:Variant;
  va:TVarAy;
  VParams:TVarAy;
  OutSet:TParamSet;
  v1,v2:Double;
  Done:Boolean;
  Z:TJSONLeaf;
  i,n:Integer;
begin
  Result:=false;
  if AObj=nil then exit;
  if AObj.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>TJSONObj{$ENDIF} then
  begin
    if AObj.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF} then
    begin
      mstr:={$IFDEF SUPEROBJECT}AObj.AsString{$ELSE}_String(AObj).toString{$ENDIF};
      if (mstr<>'') and (mstr[1]<>JEP_StrParamHeader) then  //变量名
      begin
        if VarHelper<>nil then
          VarHelper.GetVar(mstr,v);
        Val:=Double(v);
        Result:=true;
      end;
    end
    else if AObj{$IFDEF SUPEROBJECT}.DataType in [stDouble,stCurrency,stInt]{$ELSE}is _Number{$ENDIF} then
    begin
      Val:={$IFDEF SUPEROBJECT}AObj.AsDouble{$ELSE}_Number(AObj).doubleValue{$ENDIF};
      Result:=true;
    end;
    exit;
  end;
  Z:=TJSONObj(AObj).{$IFDEF SUPEROBJECT}O[JEP_Operator]{$ELSE}ValObjByIndex[0]{$ENDIF};  //第一个成员，应当是 BIOS_Operator
  if (Z<>nil) and (Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF}) then  //操作符是JSON对象
    exit;
  if Z=nil then
    Func:=''
  else
    Func:=Z.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
  if Func='' then  //如果操作符为空，就用第一个参数的值
  begin
    Val:=GetP1(Result);
    exit;
  end;
  F1:=Func[1];
  Done:=true;
  case Length(Func) of
    1:
    begin
      if F1 in (MathOp2+CompOp2) then
      begin
        if F1=';' then
        begin
          //同上
        {$IFDEF SUPEROBJECT}
          i:=1;
          while true do
          begin
            Z:=AObj.O[JEP_ParamHeader+IntToStr(i)];
            if Z=nil then
            begin
              if i=1 then
                Result:=Null;
              break;
            end;
            Val:=Eval(Z);
            Inc(i);
          end;
        {$ELSE}
          n:=TJSONObj(AObj).Length;
          if n>2 then
          begin
            GetP1(Result);
            Val:=GetP2(Result);
            //允许 ; 表达式有多于两个的参数  2010-6-27
            for i:=3 to n-1 do
              Val:=GetPN(i,Result);
          end
          else
            Val:=GetP1(Result);
        {$ENDIF}
          exit;
        end;
        Result:=true;
        v1:=GetP1(Result);
        v2:=GetP2(Result);
        if not Result then exit;
        case F1 of
          '+': Val:=v1+v2;
          '-': Val:=v1-v2;
          '*': Val:=v1*v2;
          '/': Val:=v1/v2;
          '\': Val:=Trunc(v1) div Trunc(v2);
          '%': Val:=Trunc(v1) mod Trunc(v2);
          '^': Val:=Trunc(v1) xor Trunc(v2);
          '&': Val:=Trunc(v1) and Trunc(v2);
          '|': Val:=Trunc(v1) or Trunc(v2);
          '=','>','<':
          begin
            Result:=false;
            exit;
          end;
          else Done:=false;
        end;
      end
      else begin
        case F1 of
          '.':  //something like:  Plan.Max
          begin
            (*
            if VarHelper<>nil then
            begin
            {$IFDEF NO_COMPLEXOBJ}
            {$IFNDEF NO_RECMEMBER}
              Result:=VarHelper.GetVar2(TJSONObj(AObj),v);
            {$ENDIF}
            {$ELSE}
              with VarHelper do
              begin
                //if EnterObj('') then
              end;
            {$ENDIF}
            *)
            if Result then
              Val:=Double(v);
            exit;
          end;
          else if F1='!' then
          begin
            Result:=false;
            exit;
          end
          else
            Done:=false;
        end;
      end;
    end;
    2:
    begin
      if F1 in CompOp2 then
      begin
        Result:=true;
        if Func='>>' then
          Val:=Trunc(GetP1(Result)) shr Trunc(GetP2(Result))
        else if Func='<<' then
          Val:=Trunc(GetP1(Result)) shl Trunc(GetP2(Result))
        else
          Done:=false;
        if not Result then exit;
      end
      else
        Done:=false;
    end
    else
      Done:=false;
  end;
  if Done or not Result then exit;
  if FuncHelper<>nil then
  begin
    case {$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}TJSONObj(AObj).Length{$ENDIF} of
      2:
      begin
        v1:=GetP1(Result);
        if Result then
        begin
          SetLength(VParams,1);
          VParams[0]:=v1;
          Result:=FuncHelper.GetValue(Self,Func,VParams,v,OutSet);
          SetLength(VParams,0);
        end;
      end;
      3:
      begin
        v1:=GetP1(Result);
        if Result then
        begin
          v2:=GetP2(Result);
          SetLength(VParams,2);
          VParams[0]:=v1;
          VParams[1]:=v2;
          Result:=FuncHelper.GetValue(Self,Func,VParams,v,OutSet);
          SetLength(VParams,0);
        end;
      end;
      else begin
        va:=GetParams(TJSONObj(AObj),Result);
        if Result then
          Result:=FuncHelper.GetValue(Self,Func,va,v,OutSet);
      end;
    end;
    if Result then
      try Val:=Double(v); except Result:=false; end;
  end;
end;

function TJSONExprParser.ExprToJSON(const Expr: String;
  PStart, PEnd, POutExprLen: PInteger): TJSONObj;
label DoAddOp;
type
  TAddMode=(amNone, amBlock, amFunc, amOperator, amBlockOp);
  TLevelRec=record
    Obj:TJSONLeaf;
    BC:Integer;  //Block depth in eath level.  括号层级
    OpCh:Char;   //各个层次的操作符首字符  如果是普通函数或者没有操作符，就是#0
    Rank:Byte; //操作符优先级
  end;
var
  SubString: String;
  //Use this function to check whether the name is a var or a function.
  function NextIsBlockBegin(StartPos: Integer):Boolean;//{$IF COMPILERVERSION>=18}inline;{$IFEND}
  begin
    Result:=false;
    for StartPos:=StartPos to Pred(Length(SubString)) do
    begin
      if SubString[StartPos]<=' ' then continue;
      Result:=SubString[StartPos]='(';
      break;
    end;
  end;
var
  Levels:array[0..MaxJETreeLevel] of TLevelRec;
  JLevel,BlockCnt:Integer;
  EndPos:Integer;
  LastIsVar:Boolean;
  function ExprLevel:Integer;//{$IF COMPILERVERSION>=18}inline;{$IFEND}
  begin
    Result:=JLevel;
    with Levels[JLevel] do
    if (Obj=nil) or (Obj.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF}) then exit;
    if Result>0 then Dec(Result);
  end;
  function FuncLevel:Integer;
  begin
    Result:=ExprLevel;
    while Levels[Result].BC>=BlockCnt do
    begin
      Dec(Result);
      if Result<0 then
      begin
        Inc(Result);
        break;
      end;
      if (Levels[Result].BC<BlockCnt) and not (Levels[Result].OpCh in [OpCh_Func,'(','[']) then
      begin
        Inc(Result);
        exit;
      end;
    end;
  end;
  function BlockLevel:Integer;
  begin
    Result:=ExprLevel;
    while Result>0 do  //2012-06-22  Bug fixed
    begin
      with Levels[Result] do
        if (BC<BlockCnt) or (BC<Levels[Result-1].BC) then break;
      Dec(Result);
      if Levels[Result].BC<BlockCnt then
      begin
        //一步跨出两层的情况
        Inc(Result);
        break;
      end;
    end;
  end;
  function InBlockLevel(Rank: Byte=1):Integer;  // Func( X+Y*2    1+X.Next.Val
  var
    Op:String;
    Ch:Char;
  begin
    Result:=ExprLevel;
    //If the expression is inside a block or not...
    if Levels[Result].BC>BlockCnt then  // ... A ? (B ? C) / ...  or  ... A ?  Func( B ? C ) / ...
    begin
      repeat
        Dec(Result);
        if Result<0 then break;
        if Levels[Result].OpCh=OpCh_Func then continue; //函数优先级最高，无需比较
        Op:=TJSONObj(Levels[Result].Obj).{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
        if Op<>'' then
        begin
          Ch:=Op[1];
          if GetStdOpRank(Ch,Op)<Rank then break;
          if (Ch in ['(','[','{']) and (Result>0) and (Levels[Result-1].BC<BlockCnt) then break
        end;
      until Levels[Result].BC<BlockCnt;
    end
    else  // ... A ? B / ...
      while Levels[Result].BC=BlockCnt do
      begin
        Dec(Result);
        if Result<0 then break;
        Op:=TJSONObj(Levels[Result].Obj).{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
        if Op<>'' then
          if GetStdOpRank(Op[1],Op)<Rank then break;
      end;
    Inc(Result);
  end;
  procedure WriteEmpty;
  var
    e,Len:Integer;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len)]:=nil{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),null){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  procedure WriteFloat(F: Double);
  var
    e,Len:Integer;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}D[JEP_ParamHeader+IntToStr(Len)]:=F{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),F){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  procedure WriteObjStr(const S: String);
  var
    e,Len:Integer;
    J:TJSONObj;
  begin
    try
      J:={$IFDEF SUPEROBJECT}SO(S){$ELSE}TJSONObj.Create(S){$ENDIF};
    except
      exit;
    end;
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len)]:=J{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),J){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  procedure WriteBool(ABool: Boolean);
  var
    e,Len:Integer;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}B[JEP_ParamHeader+IntToStr(Len)]:=ABool{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),ABool){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  procedure WriteNull;
  var
    e,Len:Integer;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len)]:=CNULL{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),CNULL){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  procedure PushEmptyItem;
  var
    e,Len:Integer;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      {$IFDEF SUPEROBJECT}S[JEP_ParamHeader+IntToStr(Len)]:=JE_EmptyItemStr{$ELSE}Put(JEP_ParamHeader+IntToStr(Len),JE_EmptyItemStr){$ENDIF};
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValObjByIndex[Len]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
  end;
  function NewFuncObj(const AFunc: String):TJSONObj; {$IF COMPILERVERSION>=18}inline;{$IFEND}
  begin
    Result:={$IFDEF SUPEROBJECT}SO('{"'+JEP_Operator+'":"'+AFunc+'"}'){$ELSE}TJSONObj.Create.Put(JEP_Operator,AFunc){$ENDIF};
  end;
  function WritePerfix(const Perfix: String; const IsEnd: Boolean):Boolean;
  var
    e,Len,idx:Integer;
    mstr,KeyStr:String;
    Z,Z2:TJSONLeaf;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      mstr:={$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
      if mstr<>'' then  //有操作符 -- 可能是  ... ; var X ...
      begin
        if mstr<>' ' then        
        begin
          //将最后一个提取出来
          //  类型指示符
          KeyStr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(Len-1){$ELSE}KeyByIndex[Len-1]{$ENDIF};
          Levels[JLevel].Obj:=NewFuncObj(' ');
          {$IFDEF SUPEROBJECT}Z:=O[KeyStr];O[KeyStr]:=nil{$ELSE}Z:=Remove(KeyStr){$ENDIF};
          TJSONObj(Levels[JLevel].Obj).{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,Z);
          {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(KeyStr,Levels[JLevel].Obj);
          Inc(JLevel);
          with Levels[JLevel] do
          begin
            Obj:=Z;
            BC:=BlockCnt;
            OpCh:=OpCh_Define;
            Rank:=OpRank[OpCh_Define];
          end;
          Result:=WritePerfix(Perfix,IsEnd);
          exit;
        end
        else begin  {op:" ",p1:"public",p2:"abstract",p3...}
          //IsStdOp:=true;
        end;
      end;
      if not IsEnd then
        {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_ParamHeader+IntToStr(Len),Perfix)
      else begin
        if Len{length}>1 then
        begin
          //将修饰符转化为 perfix （数组）
          //如 'private const' -> {op:"",p1:"public",p2:"const"}
          //转化为: {op:" const",pf:["private"], p1:"A", ....... }
          //  类型指示符
          KeyStr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(Len-1){$ELSE}KeyByIndex[Len-1]{$ENDIF};
          mstr:=' '+{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(KeyStr);
          {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,mstr);
          {$IFDEF SUPEROBJECT}O[KeyStr]:=nil{$ELSE}CleanKey(KeyStr){$ENDIF};
          Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
          if Len>1 then
          begin
            Z:={$IFDEF SUPEROBJECT}SO('[]'){$ELSE}JSONArray.Create{$ENDIF};
            for idx:=1 to Len-1 do
            begin
              Z2:={$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(JEP_ParamHeader+IntToStr(idx));
              if Z2<>nil then
                {$IFDEF SUPEROBJECT}Z.AsArray.Add{$ELSE}JSONArray(Z).put{$ENDIF}(Z2);
            end;
            {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Perfix,Z);
          end;
          with Levels[e] do
          begin
            OpCh:=OpCh_Define;
            Rank:=OpRank[OpCh_Define];
          end;
          {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Param1,Perfix);
          with Levels[JLevel] do
          begin
            OpCh:=OpCh_Define;
            Rank:=OpRank[OpCh_Define];
          end;
        end;
      end;
    end;
    Result:=true;
  end;
  procedure InitLevel;
  begin
    //初始的情况
    with Levels[JLevel] do
      if (Obj=nil) then
      begin
        Obj:=NewFuncObj('');
        OpCh:=OpCh_None;
        Rank:=0;
      end;
  end;
  procedure AddOp({$IFDEF NO_OPTRANSLATE}const {$ENDIF}Func: String; AddMode: TAddMode=amOperator; UsePiror: Boolean=false);
  var
    IsEmpty:Boolean;
    idx,nn,Len:Integer;
    Z,Z2:TJSONLeaf;
    J,JP,JN:TJSONObj;
    mstr:String;
    MyRank:Byte;
    label CommonCase;
  begin
    LastIsVar:=false;
  {$IFNDEF NO_OPTRANSLATE}
    if OpHelper<>nil then  //2011-09-04
      Func:=OpHelper.TranslateOperator(Func,Levels[JLevel].Obj);
  {$ENDIF}
    if (Func<>'') and ((AddMode=amOperator) or (AddMode=amBlockOp) or (AddMode=amBlock)) then
      MyRank:=GetStdOpRank(Func[1],Func)
    else
      MyRank:=0;
    //最后一个Symbol是简单值或变量的情况 -- 应n当将其提升，嵌入到Func表达式内
    if Levels[JLevel].Obj.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>TJSONObj{$ENDIF} then
    begin
      //简单变量后跟双目操作符
      if UsePiror then
      begin
        nn:=JLevel-1;
        JP:=TJSONObj(Levels[nn].Obj); //表达式JSON对象
        //没有操作符的表达式 -- 填入Func
        if Levels[nn].OpCh=OpCh_None then
        begin
          //处理 ... var A:... 的情况  2011-09-18
          with JP do
          begin
            Len:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}Length{$ENDIF};
            if Len>2 then
            begin
              //将修饰符转化为 perfix （数组）
              //如 'private const A' -> {op:"",p1:"public",p2:"const",p3:"A"}
              //转化为: {op:" const",pf:["private"], pn:"A", ....... }
              //倒数第二个是类型指示符
              mstr:=' '+{$IFDEF SUPEROBJECT}S[JEP_ParamHeader+IntToStr(Len-2)]{$ELSE}ValByIndex[Len-2]{$ENDIF};
              {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,mstr);
            {$IFDEF SUPEROBJECT}
              O[JEP_ParamHeader+IntToStr(JSONParamCount(JP)-2)]:=nil;
            {$ELSE}
              Delete(Length-2);
            {$ENDIF}
              Len:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}Length{$ENDIF};
              if Len>2 then
              begin
                Z:={$IFDEF SUPEROBJECT}SO('[]'){$ELSE}JSONArray.Create{$ENDIF};
                for idx:=1 to Len-2 do
                begin
                  Z2:={$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(JEP_ParamHeader+IntToStr(idx));
                  if Z2<>nil then
                    {$IFDEF SUPEROBJECT}Z.AsArray.Add{$ELSE}JSONArray(Z).put{$ENDIF}(Z2);
                end;
                {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Perfix,Z);
              end;
              Len:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}Length{$ENDIF};
              mstr:={$IFDEF SUPEROBJECT}S[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValByIndex[Len-1]{$ENDIF};
            {$IFDEF SUPEROBJECT}
              O[JEP_ParamHeader+IntToStr(Len-1)]:=nil;
            {$ELSE}
              Delete(Len-1);
            {$ENDIF}
              //将操作符置于内层  如： var A:int;  const M=100;
            {$IFDEF SUPEROBJECT}
              Levels[JLevel].Obj:=NewFuncObj(Func);
              Levels[JLevel].Obj.PutS(JEP_Param1,mstr);
            {$ELSE}
              Levels[JLevel].Obj:=NewFuncObj(Func).Put(JEP_Param1,mstr);
            {$ENDIF}
              {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,Levels[JLevel].Obj);
              with Levels[nn] do
              begin
                OpCh:=OpCh_Define;
                Rank:=OpRank[' '];
              end;
              with Levels[JLevel] do
                if AddMode=amOperator then
                begin
                  OpCh:=Func[1]; //AddMode;
                  Rank:=MyRank;
                end
                else begin
                  OpCh:=OpCh_Func;
                  Rank:=OpRank_Func;
                end;
              exit;
            end;
          end;
          JP.{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,Func);
          with Levels[nn] do
            if (AddMode=amOperator) or (AddMode=amBlockOp) then  //2011-09-01
            begin
              OpCh:=Func[1];
              Rank:=MyRank;
            end
            else if AddMode=amFunc then
            begin
              OpCh:=OpCh_Func;
              Rank:=OpRank_Func;
            end;
          Dec(JLevel);
          exit;
        end;
        with Levels[nn] do
        begin
          if (BC>=BlockCnt) and (OpCh<>'[') then  //当前双目操作符和前一个表达式位于相同的括号层次
          begin
            if (AddMode=amOperator) and (Func<>'') then
            begin
              if (OpCh<>OpCh_None) and (MyRank<=Rank) then
              begin
                JLevel:=InBlockLevel(MyRank); //InBlockLevel; //n;
                goto CommonCase;
              end;
            end;
          end;
        end;
        with JP do
        begin
          Len:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}Length{$ENDIF};
          mstr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(Len-1){$ELSE}KeyByIndex[Len-1]{$ENDIF};  //最后一个Key
          Z:={$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(mstr);
        end;
        with Levels[JLevel] do
        begin
          Obj:=NewFuncObj(Func);
          TJSONObj(Obj).{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,Z);
          JP.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(mstr,Obj);
          if Func='' then
          begin
            OpCh:=OpCh_None;
            Rank:=0;
          end
          else if AddMode=amOperator then
          begin
            OpCh:=Func[1];
            Rank:=MyRank;
          end
          else begin
            OpCh:=OpCh_Func;
            Rank:=OpRank_Func;
          end;
        end;
      end
      else begin
        nn:=JLevel-1;
        if Levels[nn].OpCh=OpCh_None then
        begin
          if Func<>'' then
          begin
            //将修饰符转化为 perfix （数组）  2011-09-18
            //如 'public static function' -> {op:"",p1:"public",p2:"static",p3:"function"}
            //转化为: {op:" function",pf:["public","static"], ....... }
            with TJSONObj(Levels[nn].Obj) do
            begin
              //最后一个是类型指示符
              Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[nn].Obj){$ELSE}Length{$ENDIF};
              if Len>1 then
              begin
                mstr:=' '+{$IFDEF SUPEROBJECT}S[JEP_ParamHeader+IntToStr(Len-1)]{$ELSE}ValByIndex[Len-1]{$ENDIF};
                {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,mstr);
              {$IFDEF SUPEROBJECT}
                O[JEP_ParamHeader+IntToStr(JSONParamCount(Levels[nn].Obj)-1)]:=nil;
              {$ELSE}
                Delete(length-1);
              {$ENDIF}
              end;
              Len:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[nn].Obj){$ELSE}Length{$ENDIF};
              if Len>1 then
              begin
                Z:={$IFDEF SUPEROBJECT}SO('[]'){$ELSE}JSONArray.Create{$ENDIF};
                for idx:=1 to Len-1 do
                begin
                  Z2:={$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(JEP_ParamHeader+IntToStr(idx));
                  if Z2<>nil then
                    {$IFDEF SUPEROBJECT}Z.AsArray.Add{$ELSE}JSONArray(Z).put{$ENDIF}(Z2);
                end;
                {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Perfix,Z);
              end;
              Levels[JLevel].Obj:=NewFuncObj(Func);
              {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,Levels[JLevel].Obj);
            end;
            with Levels[nn] do
            begin
              OpCh:=OpCh_Define;
              Rank:=OpRank[' '];
            end;
            with Levels[JLevel] do
            begin
              OpCh:=OpCh_Func;
              Rank:=OpRank_Func;
            end;
            exit;
          end;
          with Levels[nn] do
          begin
            TJSONObj(Obj).{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,Func);
            if Func='' then
            begin
              OpCh:=OpCh_None;
              Rank:=0;
            end
            else if AddMode=amOperator then
            begin
              OpCh:=Func[1]; //AddMode;
              Rank:=MyRank;
            end
            else begin
              OpCh:=OpCh_Func;
              Rank:=OpRank_Func;
            end;
          end;
        end;
        Dec(JLevel);
      end;
      exit;
    end
    else if UsePiror then
    begin
      if Func[1]<>'[' then
        JLevel:=InBlockLevel(OpRank[Func[1]])
      else
        JLevel:=ExprLevel;  //2011-09-01  A[1][2]
    end;
  CommonCase:
    J:=TJSONObj(Levels[JLevel].Obj);
    if AddMode=amFunc then
      IsEmpty:=({$IFDEF SUPEROBJECT}JSONParamCount(J){$ELSE}J.Length{$ENDIF}<=1) and (Levels[JLevel].OpCh=OpCh_None) //(OptString(BIOS_Operator)='')
    else
      IsEmpty:=false;
    if ((AddMode=amOperator) or IsEmpty) and (Levels[JLevel].OpCh=OpCh_None) then
    begin
      J.{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,Func);
      with Levels[JLevel] do
        if AddMode=amOperator then
        begin
          OpCh:=Func[1];
          Rank:=MyRank;
        end
        else if AddMode=amFunc then
        begin
          OpCh:=OpCh_Func;
          Rank:=OpRank_Func;
        end
        else begin
          OpCh:=OpCh_None;
          Rank:=0;
        end;
    end
    else begin
      JN:=NewFuncObj(Func);
      Levels[JLevel+1].Obj:=JN;
      //按照左侧优先结合的规律进行重新组合——考虑运算符的优先级
      // not X  =>  (not X) or Y
      // X + Y  =>  ( X + Y ) - Z
      if UsePiror then
      begin
        //当前Level的所有者
        if JLevel>0 then
        begin
          JP:=TJSONObj(Levels[JLevel-1].Obj);
          nn:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}JP.Length{$ENDIF};
          //在含有前缀的情况下，最后一个Key不是 P + (n-1)  2011-09-20
          {TODO: when SUPEROBJECT enabled... }
          mstr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(nn-1){$ELSE}JP.KeyByIndex[nn-1]{$ENDIF};
          JN.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,JP.{$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(mstr));
          JP.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(mstr,JN);
        end
        else
          JN.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,J);
        with Levels[JLevel] do
        begin
          Obj:=JN;
          if Func='' then
          begin
            OpCh:=OpCh_None;
            Rank:=0;
          end
          else if AddMode in [amOperator,amBlockOp] then
          begin
            OpCh:=Func[1];
            Rank:=MyRank;
          end
          else begin
            OpCh:=OpCh_Func;
            Rank:=OpRank_Func;
          end;
        end;
        exit;
      end;
      with J do
        {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_ParamHeader+IntToStr({$IFDEF SUPEROBJECT}JSONParamCount(J){$ELSE}Length{$ENDIF}),Levels[JLevel+1].Obj);
      Inc(JLevel);
      with Levels[JLevel] do
      begin
        BC:=BlockCnt;
        if Func='' then
        begin
          OpCh:=OpCh_None;
          Rank:=0;
        end
        else if AddMode in [amOperator,amBlock,amBlockOp] then
        begin
          OpCh:=Func[1];
          Rank:=MyRank;
        end
        else begin
          OpCh:=OpCh_Func;
          Rank:=OpRank_Func;
        end;
      end;
    end;
  end;
  procedure AddLineDiv;
  var
    nn,OriLv:Integer;
    Z:TJSONLeaf;
    J,JP,JN:TJSONObj;
    mstr:String;
    label CommonCase;
  begin
    LastIsVar:=false;
    //最后一个Symbol是简单值或变量的情况 -- 应n当将其提升，嵌入到Func表达式内
    if Levels[JLevel].Obj.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>TJSONObj{$ENDIF} then
    begin
      //简单变量后跟双目操作符
      nn:=JLevel-1;
      JP:=TJSONObj(Levels[nn].Obj); //表达式JSON对象
      //没有操作符的表达式 -- 填入Func
      if Levels[nn].OpCh=OpCh_None then
      begin
        //处理 ... var A:... 的情况  2011-09-18
        JP.{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,OpCh_Sentence);
        with Levels[nn] do
        begin
          OpCh:=OpCh_Sentence;
          Rank:=RK_Sentence;
        end;
        Dec(JLevel);
        exit;
      end;
      with Levels[nn] do
      begin
        if (BC>=BlockCnt) and (OpCh<>'[') then  //当前双目操作符和前一个表达式位于相同的括号层次
        begin
          if (OpCh<>OpCh_None) and (RK_Sentence<=Rank) then
          begin
            JLevel:=InBlockLevel(RK_Sentence); //InBlockLevel; //n;
            goto CommonCase;
          end;
        end;
      end;
      with JP do
      begin
        mstr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(JSONParamCount(JP)-1){$ELSE}KeyByIndex[Length-1]{$ENDIF};  //最后一个Key
        Z:={$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(mstr);
      end;
      with Levels[JLevel] do
      begin
        Obj:=NewFuncObj(OpCh_Sentence);
        TJSONObj(Obj).{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,Z);
        JP.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(mstr,Obj);
        OpCh:=OpCh_Sentence; //AddMode;
        Rank:=RK_Sentence;
      end;
      exit;
    end
    else begin
      OriLv:=JLevel;
      JLevel:=InBlockLevel(RK_Sentence);
      if JLevel>OriLv then
      begin
        //分隔符前无语句 if( a, ; b ....
        JN:=NewFuncObj(OpCh_Sentence);
        JLevel:=OriLv;
        with TJSONObj(Levels[JLevel].Obj) do
        begin
          nn:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[JLevel].Obj){$ELSE}Length{$ENDIF};
          {$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_ParamHeader+IntToStr(nn),JN);
        end;
        Inc(JLevel);
        with Levels[JLevel] do
        begin
          Obj:=JN;
          OpCh:=OpCh_Sentence;
          Rank:=RK_Sentence;
          BC:=BlockCnt;
        end;
        exit;
      end;
    end;
  CommonCase:
    J:=TJSONObj(Levels[JLevel].Obj);
    if (Levels[JLevel].OpCh=OpCh_None) then
    begin
      J.{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,OpCh_Sentence);
      with Levels[JLevel] do
      begin
        OpCh:=OpCh_Sentence;
        Rank:=RK_Sentence;
      end;
    end
    else if (Levels[JLevel].OpCh=OpCh_Sentence) then  //平级的 ; 号   2010-06-28
      exit
    else begin
      JN:=NewFuncObj(OpCh_Sentence);
      Levels[JLevel+1].Obj:=JN;
      //按照左侧优先结合的规律进行重新组合——考虑运算符的优先级
      // not X  =>  (not X) or Y
      // X + Y  =>  ( X + Y ) - Z
      //当前Level的所有者
      if JLevel>0 then
      begin
        JP:=TJSONObj(Levels[JLevel-1].Obj);
        nn:={$IFDEF SUPEROBJECT}JSONParamCount(JP){$ELSE}JP.Length{$ENDIF};
        //在含有前缀的情况下，最后一个Key不是 P + (n-1)  2011-09-20
        {TODO: when SUPEROBJECT enabled... }
        mstr:={$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(nn-1){$ELSE}JP.KeyByIndex[nn-1]{$ENDIF};
        JN.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,JP.{$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(mstr));
        JP.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(mstr,JN);
      end
      else
        JN.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(JEP_Param1,J);
      with Levels[JLevel] do
      begin
        Obj:=JN;
        OpCh:=OpCh_Sentence;
        Rank:=RK_Sentence;
      end;
    end;
  end;
  procedure WriteStr(const AStr: String);
  var
    e,nn:Integer;
    mstr:String;
    J:TJSONLeaf;
  begin
    //将两个前后连续定义、且没有其它操作符夹杂的的字符串合并为一个 eg:   'ABC' 'abc'  => 'ABCabc'
    //可以实现用字符串加长变量名的效果  eg:  X2' Old'  will be var name "X2 Old"
    J:=Levels[JLevel].Obj;
    if (J.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF}) then
    begin
      mstr:={$IFDEF SUPEROBJECT}J.AsString{$ELSE}_String(J).toString{$ENDIF};
      if mstr<>'' then
      begin
        if mstr[1]=JEP_StrParamHeader then // ... 'ABC' 'abcd' ...
          {$IFDEF SUPEROBJECT}{TODO: AsString:=...}{$ELSE}_String(J).AsString:=mstr+AStr{$ENDIF}
        else begin // ... XXXXX 'abc' ...
          AddOp(' ',amOperator,true);
          WriteStr(AStr);
        end;
      end;
      exit;
    end;
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      nn:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      mstr:=JEP_ParamHeader+IntToStr(nn);
      {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(mstr,JEP_StrParamHeader+AStr);
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[mstr]{$ELSE}ValObjByIndex[nn]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
      end;
    end;
  end;
  procedure WriteValVar(v: Variant);
  begin
    if VarIsNull(v) then
      WriteNull
    else
      case VarType(v) of
        varBoolean: WriteBool(Boolean(v));
        varString: WriteStr(String(v));
        else
          WriteFloat(StrToFloat(VarToStr(v)));  //2011-09-02
      end;
  end;
  function WriteVar(VarName: String):Boolean;
  var
    e,nn:Integer;
    v:Variant;
    mstr:String;
  begin
    e:=ExprLevel;
    with TJSONObj(Levels[e].Obj) do
    begin
      nn:={$IFDEF SUPEROBJECT}JSONParamCount(Levels[e].Obj){$ELSE}Length{$ENDIF};
      if nn>1 then
      begin
        mstr:={$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
        if (mstr='') or (mstr=' ') or ((nn>2) and (mstr=';') and LastIsVar) then  //已经有了一个参数，但还缺少操作符
        begin
          Result:=false;
          exit;
        end;
        //  Foo( const VarName )
        if (BlockCnt>0) and (JLevel>=1) and (Levels[e].BC<Levels[JLevel].BC) then
        begin
          Result:=false;
          exit;
        end;
      end;
      if (VarHelper<>nil) and UseVarHelperOnParse then
      begin
        //尽可能将变量的值直接代入表达式
        if VarHelper.GetVar(VarName,v) then
        begin
          WriteValVar(v);
          Result:=true;
          exit;
        end;
        //对变量名进行规范化
        if not VarHelper.CheckAndTransName(VarName) then
        begin
          Result:=false;
          exit;
        end;
      end;
      mstr:=JEP_ParamHeader+IntToStr(nn);
      {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(mstr,VarName);
      JLevel:=e+1;
      with Levels[JLevel] do
      begin
        Obj:={$IFDEF SUPEROBJECT}O[mstr]{$ELSE}ValObjByIndex[nn]{$ENDIF};
        BC:=BlockCnt;
        OpCh:=OpCh_None;
        Rank:=0;
      end;
    end;
    LastIsVar:=true;
    Result:=true;
  end;
  function NextCh(CurPos: Integer):Char; //automatic inline
  begin
    if CurPos>=EndPos then
      Result:=#0
    else
      Result:=SubString[CurPos+1];
  end;
  function NextOps(CurPos: Integer):String; //automatic inline
  var
    j:Integer;
  begin
    Result:='';
    j:=CurPos+1;
    while j<=EndPos do
    begin
      if not (SubString[j] in MathOp4+CompOp2+[':']) then exit;
      Result:=Result+SubString[j];
      Inc(j);
    end;
  end;
  function IsPerfix(CurPos: Integer):Boolean;
  var
    j:Integer;
  begin
    j:=CurPos;
    while j<=EndPos do
    begin
      if not (SubString[j] in ([#1..' ','.']+VarBegin+VarBody)) then
      begin
        Result:=SubString[j] in [':','(',')','=',';',','];
        exit;
      end;
      Inc(j);
    end;
    Result:=true;
  end;
  function IsPerfixEnd(CurPos: Integer):Boolean;
  var
    j:Integer;
  begin
    j:=CurPos;
    while j<=EndPos do
    begin
      if not (SubString[j] in ([#1..' '])) then
      begin
        Result:=SubString[j] in [';',',',')'];
        exit;
      end;
      Inc(j);
    end;
    Result:=true;
  end;
var
  i,s,OriLv,i0: Integer;
  FuncName: String;
  ExprStart: Boolean;
  StrValue, mstr: string;
  CW,Cnt:Word;
  Ch:Char;
  WCh:WideChar;
begin
  LastExprType:=etEmpty;
  if PStart<>nil then
    s:=PStart^
  else
    s:=1;
  if PEnd<>nil then
  begin
    EndPos:=PEnd^;
    SubString:=Copy(Expr,s,s-EndPos+1);
  end
  else if S<=1 then
    SubString:=Expr
  else
    SubString:=Copy(Expr,s,MaxInt);
  if SubString='' then
  begin
    Result:={$IFDEF SUPEROBJECT}SO('{}'){$ELSE}TJSONObj.Create{$ENDIF};
    if POutExprLen<>nil then
      POutExprLen^:=0;
    exit;
  end;
  JLevel:=0;
  InitLevel;//AddOp('',amNone);
  Levels[0].BC:=0;
  BlockCnt:=0;
  FuncName:='';
  ExprStart:=true;
  EndPos:=Length(SubString);
  i:=1;
  while i<=EndPos do
  begin
    Ch:=SubString[I];
    if Ch<=' ' then
    begin
      Inc(I);
      continue;
    end;
    StrValue:='';
    if (ExprStart and (Ch in ['+','-'])) or (Ch in Digits) then  //2011-09-01  Can start with + or -
    begin
      while true do
      begin
        StrValue:=StrValue + Ch;
        Inc(I);
        Ch:=SubString[i];
        if not (Ch in Digits) then
        begin
          if Ch<>'.' then break;
          if (i<EndPos) and (SubString[i+1]='.') then  //  2..9
            break
          else
            continue;
        end;
      end;
      //Scientific Number. eg:  1.34E-20  9E55
      if (i<EndPos) and (SubString[I] in ['e','E']) then
      begin
        StrValue:=StrValue + SubString[I];
        Inc(I);
        if SubString[I] in ['+','-'] then
        begin
          StrValue:=StrValue + SubString[i];
          Inc(i);
        end;
        if i<=EndPos then
          repeat
            StrValue:=StrValue + SubString[i];
            Inc(I);
          until not (SubString[I] in Digits);      
      end;
      //并不在数字之前的单独的负号  解释为 0 - 
      if StrValue='-' then
      begin
        AddOp(StrValue);
        //WriteFloat(0);
        PushEmptyItem;  //2012-06-12
      end
      else begin
        try
          WriteFloat(StrToFloat(StrValue));
        except
        end;
      end;
      ExprStart:=false;
    end
    else if (Ch in VarBegin) then  //变量或函数
    begin
      ExprStart:=false;
      repeat
        StrValue:=StrValue + SubString[i];
        Inc(i);
      until not (SubString[I] in VarBody);
      FuncName:=UpperCase(StrValue);
      if (FuncName='AND') or (FuncName='OR') or (FuncName='IN') or (FuncName='IS') or (FuncName='XOR') then  //双目运算符
      begin
        if JLevel>=MaxJETreeLevel then break;  //强制跳出
        AddOp(FuncName,amOperator,true);
        FuncName:='';
      end
      else if NextIsBlockBegin(I) or (FuncName='NOT') or (FuncName='NEW')
        or (FuncName='CONTINUE') or (FuncName='BREAK') or (FuncName='EXIT') then
      begin
        if not NextIsBlockBegin(I) then
          AddOp(FuncName,amFunc)
        else if not UpperCaseNormalFuncName then  //还原成未大写的状态  2010-04-02
          FuncName:=StrValue;
      end
      else if (FuncName='TRUE') or (FuncName='FALSE') or (FuncName='NULL') then  //布尔值或NULL
      begin
        if FuncName[1]='N' then
          WriteNull
        else
          WriteBool(FuncName[1]='T');
        FuncName:='';
      end
      else begin  //非关键字
        if not WriteVar(StrValue) then  //将第二个 "变量" 当成操作符
        begin
          if JLevel>=MaxJETreeLevel then break;  //强制跳出
          if IsPerfix(i) then  // "public function Foo()"  OR  "out R : Int"  OR  "const A = 100"
          begin
            WritePerfix(StrValue,(i>EndPos) or IsPerfixEnd(i));
          end  
          else  //  A Like B
            AddOp(FuncName,amOperator,true);
        end;
        FuncName:='';
      end;
    end
    else if (Ch in MathOp2+CompOp2) then  //双目运算符以及比较操作符、赋值运算符
    begin
      if JLevel>=MaxJETreeLevel then break;  //强制跳出
      if Ch=OpCh_Sentence then
      begin
        AddLineDiv;
        FuncName:='';
        ExprStart:=true;
        Inc(I);
        continue;
      end;
      if Ch<>'/' then  //排除单行注释的情况
      begin
        StrValue:=Ch;
        Inc(I);
        while true do  //2011-09-03  Allow op like:  +=  %>  -*  =<>  ::=  -:>>  |=  :<=  ...
        begin
          Ch:=SubString[I];
          if not (Ch in MathOp3+CompOp2+[':','!','.']) then break;
          StrValue:=StrValue+Ch;
          Inc(I);
        end;
        Cnt:=Length(StrValue);
        if (Cnt>=2) and (StrValue[Cnt] in ['+','-','!']) then  // + or - or ! at tail
        begin
          if StrValue[Cnt-1]<>StrValue[Cnt] then  // if not end with ++ or -- or !! , delete last char.
          begin
            StrValue:=Copy(StrValue,1,Cnt-1);
            Dec(i);
          end;
        end;
      end
      else begin //  '/'  2010-08-10
        if (i<EndPos) and (SubString[i+1] in CompOp2) then  //  Allow  /=  />  /<
        begin
          StrValue:=SubString[i]+SubString[i+1];
          Inc(i,2);
        end
        else begin
          while true do
          begin
            if SubString[I]<>'/' then break;
            StrValue:=StrValue+'/';
            Inc(i);
          end;
          if Length(StrValue)>=2 then  //  '//' or more...
          begin
            if Keep_Comment then i0:=i;
            while true do
            begin
              if SubString[I] in [#13,#10,#0] then break;
              Inc(i);
            end;
            if Keep_Comment and (i>i0) then
            begin
              mstr:=Copy(SubString,i0,i-i0);
              if Length(StrValue)>=3 then  //以 /// 开始的注释——当成文档
              begin
               // mstr:=Copy(SubString,i0,i-i0);
                AddOp(JEF_DocRem,amFunc);
              end
              else if mstr[1]='$' then  //以 //$ 开始的注释——当成编译指示
              begin
                AddOp(JEF_DirRem,amFunc);
                Delete(mstr,1,1);
              end
              else
                AddOp(JEF_Rem,amFunc);
              Inc(BlockCnt);
              //FuncName:='';
              Levels[JLevel].BC:=BlockCnt-1;
              WriteStr(mstr);
              Dec(BlockCnt);
              AddLineDiv;
            end;  
            continue;
          end;
        end;
      end;
      AddOp(StrValue,amOperator,true);
      FuncName:='';
      ExprStart:=true;
    end
    else begin
      case Ch of
        ',':
        begin
          OriLv:=JLevel;
          JLevel:=FuncLevel;
          if Levels[JLevel].OpCh=OpCh_None then  //逗号
          begin
            with Levels[JLevel] do
            begin
              TJSONObj(Obj).{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,'(');
              OpCh:='(';
              Rank:=OpRank['('];
              Dec(BC);
            end;
          end
          else if (Levels[JLevel].BC=BlockCnt) and (Levels[JLevel].OpCh<>'[') then  //... ? ( A ? B , ...
          begin  //将纯括号内部的表达式提升到括号的内部
            AddOp('(',amOperator,true);
            Dec(Levels[JLevel].BC);
          end
          else if Levels[JLevel].BC<BlockCnt then
          begin
            //逗号位于括号内部，并且括号外的操作符不是一般函数（无论单目还是双目算子，右侧都不会有多个参数)
            // -- 将括号内的内容做为集合处理
            JLevel:=OriLv;
            JLevel:=ExprLevel;
            with TJSONObj(Levels[JLevel].Obj) do
            begin
              StrValue:={$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
              if StrValue='' then
              begin
                {$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(JEP_Operator,'(');  //括号做为集合标志
              end
              else
                JLevel:=FuncLevel;
            end;
          end;
          Inc(i);
          ExprStart:=true;
        end;
        '(':
        begin
          if JLevel>=MaxJETreeLevel then break;  //强制跳出
          Inc(BlockCnt);
          if FuncName<>'' then  //函数调用
          begin
            AddOp(FuncName,amFunc);
            FuncName:='';
            Levels[JLevel].BC:=BlockCnt-1;
          end
          else
            AddOp('',amBlock);
          Inc(I);
          FuncName:='';
          ExprStart:=true;
        end;
        ')',']':
        begin
          JLevel:=BlockLevel;
          Dec(BlockCnt);
          Inc(I);
          FuncName:='';
          ExprStart:=false;  //2011-09-01   Bug fix for something like:  Now()-1
        end;
        '[':  //数组下标
        begin
          if JLevel>=MaxJETreeLevel then break;  //强制跳出
          Inc(BlockCnt);
          if ExprStart then  // ? := [ ...
          begin
            AddOp(Ch,amBlockOp,false);
            WriteEmpty;  //写入第一个参数（ '[' 函数的第一个参数是左部 ）
          end
          else  // ? [ ...
            AddOp(Ch,amBlockOp,true);
          Levels[JLevel].BC:=BlockCnt-1;
          Inc(I);
          FuncName:='';
          ExprStart:=true;
        end;
        '!','~':
        begin
          AddOp(Ch,amFunc);
          Inc(i);
        end;
        '''':
        begin
          Inc(i);
          while i<=EndPos do
          begin
            if SubString[i]='''' then
            begin
              Inc(i);
              if (i<=EndPos) and (SubString[i]='''') then
              begin
                StrValue:=StrValue+'''';
                Inc(i);
              end
              else
                break;
            end
            else begin
              StrValue:=StrValue+SubString[i];
              Inc(i);
            end;
          end;
          WriteStr(StrValue);
          ExprStart:=false;  //2011-09-01   Bug fix for something like:  Now()-1
        end;
        '"':  //2010-04-02
        begin
          ExprStart:=false;
          Inc(i);
          while i<=EndPos do
          begin
            if SubString[i]='"' then
            begin
              Inc(i);
              if (i<=EndPos) and (SubString[i]='"') then
              begin
                StrValue:=StrValue+'"';
                Inc(i);
              end
              else
                break;
            end
            else begin
              StrValue:=StrValue+SubString[i];
              Inc(i);
            end;
          end;
          if DblQuotationAsString then  //将双引号内的内容当成普通字符串
          begin
            WriteStr(StrValue);
          end
          else begin
            FuncName:=StrValue;
            if NextIsBlockBegin(I) then
            begin
              if not NextIsBlockBegin(I) then
                AddOp(FuncName,amFunc);
            end
            else begin  //非关键字
              if not WriteVar(StrValue) then  //将第二个 "变量" 当成操作符
              begin
                if JLevel>=MaxJETreeLevel then break;  //强制跳出
                AddOp(FuncName,amOperator,true);
              end;
              FuncName:='';
            end;
          end;
        end;
        '#':  //Allow #123 in String
        begin
          CW:=0;
          while true do
          begin
            Inc(i);
            if SubString[i] in Digits then
              CW:=CW*10+(Byte(SubString[i])-Byte('0'))
            else if SubString[i]='#' then
            begin
              if CW<256 then
                StrValue:=StrValue+Char(CW)
              else begin
                PWord(@WCh)^:=CW;
                StrValue:=StrValue+WCh;
              end;
              CW:=0;
            end
            else
              break;
          end;
          if CW<256 then
            StrValue:=StrValue+Char(CW)
          else begin
            PWord(@WCh)^:=CW;
            StrValue:=StrValue+WCh;
          end;
          WriteStr(StrValue);
        end;
        '{':  //Embed TJSONObj
        begin
          while i<=EndPos do
          begin
            StrValue:=StrValue+SubString[i];
            if SubString[i]='}' then
            begin
              Inc(i);
              break;
            end;
            Inc(i);
          end;
          WriteObjStr(StrValue);
        end;
        else
          Inc(I);
      end;
    end;
  end;
  Result:=TJSONObj(Levels[0].Obj);
  if POutExprLen<>nil then
    POutExprLen^:=i;
end;

function TJSONExprParser.ExprToJSONStr(const Expr: String): String;
var
  J:TJSONObj;
begin
  J:=ExprToJSON(Expr);
  if J=nil then
    Result:=''
  else begin
    Result:=J.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
    {$IFNDEF SUPEROBJECT}J.Free{$ENDIF};
  end;
end;

class function TJSONExprParser.GetLastExprType: TExprType;
begin
  Result:=LastExprType;
end;

procedure TJSONExprParser.InitForEval(EnableTimeLimit: Boolean);
begin
  if EnableTimeLimit then
    FStartEvalTime:=Now
  else
    FStartEvalTime:=0;
  FLastConfirmTime:=0;
end;

function TJSONExprParser.JSONToExpr(AObj: TJSONObj; ParentOpRank: Integer): String;
var
  Func:String;
  IsCommonFunc:Boolean;
  OpRk:Integer;
  function J2Str(Z: TJSONLeaf):String;
  var
    v:Variant;
  begin
    if Z=nil then
    begin
      Result:='';
      exit;
    end;
    if Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF} then
    begin
      if IsCommonFunc then  //只有一个参数的普通函数自带括号了
      begin
        if ParentOpRank<0 then
          Result:=JSONToExpr(TJSONObj(Z),ParentOpRank)
        else
          Result:=JSONToExpr(TJSONObj(Z),0)
      end
      else
        Result:=JSONToExpr(TJSONObj(Z),OpRk);
    end
    else begin
      Result:=Z.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
      if (Result<>'') and (Z.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF}) then
      begin
        if Result[1]=JEP_StrParamHeader then  //String
          Result:=QuotedStr(Copy(Result,2,MaxInt))
        else if Result[1]=JEP_TypeHead then
        begin
          if (Length(Result)>1) then
          begin
            case Result[2] of
              JEPT_EchoStr:
              begin
                Result:='?>'+Copy(Result,3,MaxInt)+'<?';
              end;
              JEPT_EmptyItem:
                Result:='';
            end;
          end;
        end
        else begin //Var Name
          if UseVarHelperOnTextGen and (VarHelper<>nil) and VarHelper.GetVar(Result,v) then
          begin
            Result:=VarToExprStr(v);
          end
          else if not IsNormalVarName(Result) then  //2010-04-02
            Result:='"'+StringReplace(Result,'"','""',[rfReplaceAll])+'"';
        end;
      end;
    end;
  end;
var
  idx:Integer;
  C1:Char;
  BodyStr:String;
  Z:TJSONLeaf;
begin
  Result:='';
  if AObj=nil then exit;
  Func:=AObj.{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator);
{$IFNDEF NO_OPTRANSLATE}
  if OpHelper<>nil then  //2011-09-04
    Func:=OpHelper.RestoreOperator(Func,AObj);
{$ENDIF}
  if Func<>'' then
    C1:=Func[1]
  else
    C1:=#0;
  IsCommonFunc:=(Func='') or not ((C1 in MathOp2+CompOp2+['.']+MathOp1) or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR'));
  if IsCommonFunc or (ParentOpRank<0) then
    OpRk:=-1
  else
    OpRk:=GetStdOpRank(C1,Func);
  //Perfix expression like 'public final function Foo(A,B)'
  if C1=' ' then
  begin
    if Length(Func)=1 then  //  include 'aaa.inc'  => {op:" ",p1:xx,p2:yy}
    begin
      with AObj do
      begin
        Result:=J2Str({$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1));
        for idx:=2 to {$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF}-1 do
          Result:=Result+' '+J2Str({$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(idx)));
      end;
      exit;
    end;
    Z:=AObj.{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Perfix);
    if {$IFDEF SUPEROBJECT}Z.AsArray<>nil{$ELSE}Z is JSONArray{$ENDIF} then
    begin
      with {$IFDEF SUPEROBJECT}Z.AsArray{$ELSE}JSONArray(Z){$ENDIF} do
      begin
        for idx:=0 to Pred(length) do
          Result:=Result+{$IFDEF SUPEROBJECT}S[idx]{$ELSE}getString(idx){$ENDIF}+' ';
      end;
    end;
    Result:=Result+Copy(Func,2,MaxInt)+' ';  //去掉空格
    with AObj do
      Z:={$IFDEF SUPEROBJECT}GetO(JEP_ParamHeader+IntToStr(JSONParamCount(AObj)-1)){$ELSE}ValObjByIndex[length-1]{$ENDIF};
    if {$IFDEF SUPEROBJECT}Z.DataType<>stArray{$ELSE}not (Z is JSONArray){$ENDIF} then
      Result:=Result+J2Str(Z);
    exit;
  end;
  Z:=AObj.{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param1);
  Result:=J2Str(Z);
  if Func='' then
  begin
    if {$IFDEF SUPEROBJECT}Z.DataType=stObject{$ELSE}(Z is TJSONObj){$ENDIF} then  //处理括号
      Result:='('+Result+')';
    exit;
  end;
  if (C1 in MathOp2+CompOp2+['.']+MathOp1) or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR') then
  begin
    if C1 in MathOp2+CompOp2+['.'] then
    begin
      Result:=Result+Func;
    end
    else if C1 in MathOp1 then
    begin
      Result:=Func+Result;
      if (ParentOpRank<0) or (OpRk<ParentOpRank) then
        Result:='('+Result+')';
      exit;
    end
    else
      Result:=Result+' '+Func+' ';
    Z:=AObj.{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_Param2);
    if OpRk>=0 then
    begin
      //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
      Inc(OpRk);
      BodyStr:=J2Str(Z);
      Dec(OpRk);
    end
    else
      BodyStr:=J2Str(Z);
    if Func=JEP_BodyDefOp then
    begin
      if Z=nil then
        Result:=Result+'()'
      else if ({$IFDEF SUPEROBJECT}Z.DataType=stObject{$ELSE}Z is TJSONObj{$ENDIF})
        and (TJSONObj(Z).{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(JEP_Operator)=';') then
      begin
        Result:=Result+'('+BodyStr+')'
      end
      else
        Result:=Result+BodyStr;
      exit;
    end;
    Result:=Result+BodyStr;
    if C1=OpCh_Sentence then  //支持多个平行语句  2010-06-28
      with AObj do
        for idx:=3 to {$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF}-1 do
          Result:=Result+Func+J2Str({$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(idx)));
    if (ParentOpRank<0) or (OpRk<ParentOpRank) then
      Result:='('+Result+')';
  end
  else begin
    if C1='[' then   // A[1]  =>  [,A,1
    begin
      if Z<>CNULL then  //2011-09-25
        Result:=Result+'['
      else
        Result:='[';
      with AObj do
        for idx:=2 to Pred({$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF}) do
        begin
          if idx>2 then Result:=Result+',';
          Result:=Result+J2Str({$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(idx)));
        end;
      Result:=Result+']';
      exit;
    end
    else begin
      with AObj do
        for idx:=2 to Pred({$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF}) do
          Result:=Result+','+J2Str({$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(JEP_ParamHeader+IntToStr(idx)));
    end;
    Result:='('+Result+')';
    if Func<>'(' then  //集合以 "(" 做为操作符
      Result:=Func+Result;
  end;
end;

function TJSONExprParser.OptimizeJSON(AObj: TJSONObj): TJSONObj;
var
  SL:TStrings;
  c,idx:Integer;
  Z:TJSONLeaf;
begin
  Result:=nil;
  if AObj=nil then exit;
  SL:=nil;
  c:=VarNeeded(AObj,SL);
  if c>0 then
  begin
    Result:={$IFDEF SUPEROBJECT}SO('{}'){$ELSE}TJSONObj.Create{$ENDIF};
    with AObj do
    begin
      Result.{$IFDEF SUPEROBJECT}PutO(JEP_Operator,O[JEP_Operator].Clone){$ELSE}Put(KeyByIndex[0],ValObjByIndex[0].Clone){$ENDIF};
      for idx:=1 to Pred({$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}Length{$ENDIF}) do
      begin
        Z:=AObj.{$IFDEF SUPEROBJECT}GetO(JEP_ParamHeader+IntToStr(idx)){$ELSE}ValObjByIndex[idx]{$ENDIF};
        if Z.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>TJSONObj{$ENDIF} then
        begin
          Result.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(
            {$IFDEF SUPEROBJECT}JEP_ParamHeader+IntToStr(idx){$ELSE}KeyByIndex[idx]{$ENDIF},Z.Clone);
          continue;
        end;
      end;
    end;
    SL.Free;
  end;
end;

procedure TJSONExprParser.RemoveFuncHelper(AHelper: TJEFuncHelper);
var
  h,LastH:TJEFuncHelper;
begin
  if AHelper=nil then exit;
  h:=FuncHelper;
  LastH:=h;
  while h<>nil do
  begin
    if h=AHelper then
    begin
      if LastH<>h then
        LastH.NextHelper:=h.NextHelper
      else
        FFuncHelper:=h.NextHelper;
      exit;
    end;
    LastH:=h;
    h:=h.NextHelper;
  end;
end;

procedure TJSONExprParser.RemoveVarHelper(AHelper: TJEVarHelper);
var
  h,LastH:TJEVarHelper;
begin
  if AHelper=nil then exit;
  h:=VarHelper;
  LastH:=h;
  while h<>nil do
  begin
    if h=AHelper then
    begin
      if LastH<>h then
        LastH.NextHelper:=h.NextHelper
      else
        FVarHelper:=h.NextHelper;
      exit;
    end;
    LastH:=h;
    h:=h.NextHelper;
  end;
end;

procedure TJSONExprParser.SetConfirmFunc(const Value: TConfirmFunc);
begin
  FConfirmFunc := Value;
end;

procedure TJSONExprParser.SetEchoFunc(const Value: TPrintFunc);
begin
  FEchoFunc := Value;
end;

procedure TJSONExprParser.SetOnLineComplete(const Value: TTraceLineFunc);
begin
  FOnLineComplete := Value;
end;

procedure TJSONExprParser.SetOpHelper(const Value: TJEOpHelper);
begin
  FOpHelper := Value;
end;

procedure TJSONExprParser.SetPrintFunc(const Value: TPrintFunc);
begin
  FPrintFunc := Value;
end;

procedure TJSONExprParser.SetTimeLimitConfirm(const Value: Integer);
begin
  FTimeLimitConfirm := Value;
end;

procedure TJSONExprParser.SetTraceOnLine(const Value: Boolean);
begin
  FTraceOnLine := Value;
end;

procedure TJSONExprParser.SetUseVarHelperOnParse(const Value: Boolean);
begin
  FUseVarHelperOnParse := Value;
end;

procedure TJSONExprParser.SetUseVarHelperOnTextGen(const Value: Boolean);
begin
  FUseVarHelperOnTextGen := Value;
end;

procedure TJSONExprParser.SetVarToStrDefFunc(const Value: TVarToStrDefFunc);
begin
  FVarToStrDefFunc := Value;
end;

function TJSONExprParser.TypeStrToVar(const Str: String): Variant;
var
  mstr:String;
begin
  if (Length(Str)<2) or (Str[1]<>JEP_TypeHead) then
  begin
    Result:=Null;
    exit;
  end;
  mstr:=Copy(Str,3,MaxInt);
  case Str[2] of
    JEPT_Hex:  Result:=HexToInt(mstr);
    JEPT_Oct:  Result:=OctToInt(mstr);
    JEPT_Bin:  Result:=BinToInt(mstr);
    JEPT_Date: Result:=StrToDate(mstr);
    JEPT_Time: Result:=StrToTime(mstr);
    JEPT_DateTime: Result:=StrToDateTime(mstr);
    JEPT_EchoStr:
    begin
      if Assigned(EchoFunc) then EchoFunc(mstr);
      Result:=0;
    end;
    JEPT_EmptyItem: Result:=0;
    else
      Result:=Null;
  end;
end;

class function TJSONExprParser.VarNeeded(AObj: TJSONObj; var Vars: TStrings):Integer;
  procedure Check(Z: TJSONLeaf);
  var
    mstr:String;
  begin
    if Z=nil then exit;
    if Z.{$IFDEF SUPEROBJECT}DataType=stString{$ELSE}ClassType=_String{$ENDIF} then
    begin
      mstr:={$IFDEF SUPEROBJECT}Z.AsString{$ELSE}_String(Z).toString{$ENDIF};
      if (mstr<>'') and (mstr[1]<>JEP_StrParamHeader) then  //变量名
      begin
        Inc(Result);
        if Vars=nil then
          Vars:=TStringList.Create;
        Vars.Add(mstr);
      end;
    end
    else if Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF} then
    begin
      Inc(Result,VarNeeded(TJSONObj(Z),Vars));
    end;
  end;
var
  i:Integer;
begin
  Result:=0;
  if AObj=nil then exit;
  //忽略为首的操作符，检查每个参数成员
  for i:=1 to Pred({$IFDEF SUPEROBJECT}JSONParamCount(AObj){$ELSE}AObj.Length{$ENDIF}) do
    Check(AObj.{$IFDEF SUPEROBJECT}GetO(JEP_ParamHeader+IntToStr(i)){$ELSE}ValObjByIndex[i]{$ENDIF});
end;

class function TJSONExprParser.VarToExprStr(V: Variant): String;
begin
  if VarIsNull(V) then
    Result:=CNULL.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF}
  else if VarType(V)=varString then
    Result:=QuotedStr(String(V))
  else if VarType(V)=varBoolean then
  begin
    if Boolean(V) then
      Result:={$IFDEF SUPEROBJECT}'true'{$ELSE}_Boolean._TRUE.toString{$ENDIF}
    else
      Result:={$IFDEF SUPEROBJECT}'false'{$ELSE}_Boolean._FALSE.toString{$ENDIF};
  end
  else
    Result:=VarToStr(V);
end;

class function TJSONExprParser.Version: ShortString;
begin
  Result:='0.5.4';
end;

function TJSONExprParser.VHEnterObj(const ObjName: String): TJEVarHelper;
begin
  Result:=FVarHelper;
  while not Result.EnterObj(ObjName) do
  begin
    Result:=Result.NextHelper;
    if Result=nil then exit;
  end;  
end;

{ TJEVarHelper }

function TJEVarHelper.CallObjFunc(AObj: TObject; const Func: String;
  Args: array of Variant): Variant;
begin

end;

function TJEVarHelper.CheckAndTransName(var VarName: String): Boolean;
begin
  Result:=true;
end;

procedure TJEVarHelper.Clean;
begin

end;

{$IFNDEF NO_COMPLEXOBJ}
function TJEVarHelper.EnterObj(const ObjName: String): Boolean;
begin
  Result:=false;
end;
{$ENDIF}

function TJEVarHelper.GetAsJSONString: String;
var
  J:TJSONObj;
begin
  J:={$IFDEF SUPEROBJECT}SO('{}'){$ELSE}TJSONObj.Create{$ENDIF};
  ValExport(J);
  Result:=J.{$IFDEF SUPEROBJECT}AsString{$ELSE}toString{$ENDIF};
  {$IFNDEF SUPEROBJECT}J.Free{$ENDIF};
end;

function TJEVarHelper.GetObjAttr(const Attr: String): Variant;
begin

end;

function TJEVarHelper.GetObjElement(AObj: TObject; const Index: Variant): Variant;
begin

end;

function TJEVarHelper.GetTraceOnSet: Boolean;
begin
  Result:=false;
end;

function TJEVarHelper.GetVar(const VarName: String; out Val:Variant):Boolean;
begin
  if NextHelper<>nil then
    Result:=NextHelper.GetVar(VarName,Val)
  else
    Result:=false;
end;

{$IFNDEF NO_RECMEMBER}
function TJSONVarHelper.GetVar2(AObj: TJSONObj; out Val:Variant):Boolean;
var
  str1,str2:String;
begin
  if AObj=nil then
    str1:=''
  else begin
    str1:=AObj.{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(BIOS_Param1);
    str2:=AObj.{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(BIOS_Param2);
    if str2<>'' then
      str1:=str1+'.'+str2;
  end;
  Result:=GetVar(str1,Val);
end;
{$ENDIF}

function TJEVarHelper.GetVarCount: Integer;
begin
  Result:=-1;
end;

function TJEVarHelper.GetVarDef(const VarName: String;
  const Default: Variant): Variant;
begin
  if not GetVar(VarName,Result) then
    Result:=Default;
end;

function TJEVarHelper.GetVarNames(const Idx: Integer): String;
begin
  Result:='';
end;

function TJEVarHelper.GetVarObj(const VarName: String; out Obj: TJSONObj): Boolean;
begin
  Result:=false;
end;

{$IFNDEF NO_COMPLEXOBJ}
function TJEVarHelper.LeaveObj(const ObjName: String): Boolean;
begin
  Result:=false;
end;
{$ENDIF}

procedure TJEVarHelper.SetAsJSONString(const Value: String);
var
  J:TJSONObj;
begin
  try
    J:={$IFDEF SUPEROBJECT}SO(Value){$ELSE}TJSONObj.Create(Value){$ENDIF};
  except
    exit;
  end;
  Clean;
  ValImport(J);
  {$IFNDEF SUPEROBJECT}J.Free{$ENDIF};
end;

procedure TJEVarHelper.SetNextHelper(const Value: TJEVarHelper);
begin
  FNextHelper := Value;
end;

function TJEVarHelper.SetObjAttr(const Attr: String; const Val: Variant): Boolean;
begin
  Result:=false;
end;

function TJEVarHelper.SetObjectVar(const VarName: String; JObj: TJSONObj): Boolean;
begin
  Result:=false;
end;

function TJEVarHelper.SetObjElement(AObj: TObject; const Index: Variant; Val: Variant): Boolean;
begin
  Result:=false;
end;

function TJEVarHelper.SetObjVar(const VarName: String; AObj: TObject): Boolean;
begin
  Result:=false;
end;

procedure TJEVarHelper.SetTraceOnSet(const Value: Boolean);
begin

end;

function TJEVarHelper.SetVar(const VarName: String;
  const Val: Variant): Boolean;
begin
  if NextHelper<>nil then
    Result:=NextHelper.SetVar(VarName,Val)
  else
    Result:=false;
end;

{$IFNDEF NO_RECMEMBER}
function TJSONVarHelper.SetVar2(AObj: TJSONObj;
  const Val: Variant): Boolean;
var
  str1,str2:String;
begin
  if AObj=nil then
    str1:=''
  else begin
    str1:=AObj.{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(BIOS_Param1);
    str2:=AObj.{$IFDEF SUPEROBJECT}GetS{$ELSE}OptString{$ENDIF}(BIOS_Param2);
    if str2<>'' then
      str1:=str1+'.'+str2;
  end;
  Result:=SetVar(str1,Val);
end;
{$ENDIF}

function TJEVarHelper.ValExport(PlainObj: TJSONObj): Integer;
var
  i:Integer;
  mstr:String;
  v:Variant;
  Obj:TJSONObj;
begin
  if PlainObj=nil then
  begin
    Result:=0;
    exit;
  end;
  Result:=VarCount;
  if Result<=0 then exit;
  for i:=0 to Pred(Result) do
  begin
    mstr:=VarNames[i];
    if GetVarObj(mstr,Obj) then
    begin
      PutObjToJSON(PlainObj,mstr,Obj{$IFNDEF SUPEROBJECT}.Clone{$ENDIF});
    end
    else if not GetVar(mstr,v) then
      Dec(Result)
    else
      PutVarToJSON(PlainObj,mstr,v);
  end;
end;

function TJEVarHelper.ValImport(PlainObj: TJSONObj): Integer;
var
  idx:Integer;
  mstr:String;
  v:Variant;
  Z:TJSONLeaf;
  JObj:TJSONObj;
begin
  Result:=0;
  if PlainObj=nil then exit;
  with PlainObj{$IFDEF SUPEROBJECT}.GetEnumerator{$ENDIF} do
  begin
  {$IFDEF SUPEROBJECT}
    while true do
    begin
      Z:=Current;
      {TODO: mstr=?}
  {$ELSE}
    for idx:=0 to Pred(Length) do
    begin
      Z:=ValObjByIndex[idx];
      mstr:=KeyByIndex[idx];
      if mstr='' then continue;
  {$ENDIF}
      if Z.{$IFDEF SUPEROBJECT}DataType=stObject{$ELSE}ClassType=TJSONObj{$ENDIF} then
      begin
        JObj:=TJSONObj(Z.Clone);
        SetObjectVar(mstr,JObj);
        continue;
      end;
      v:=VarFromJSON(Z);
      if SetVar(mstr,v) then
        Inc(Result);
    {$IFDEF SUPEROBJECT}
      if not MoveNext then exit;
    {$ENDIF}
    end;
  end;
end;

function TJEVarHelper.VarIsObj(const VarName: String): Boolean;
begin
  Result:=false;
end;

{ TJEFuncHelper }

function TJEFuncHelper.GetValue(Sender: TJSONExprParser; const Func: String;
  var Params: array of Variant; out Val: Variant; out OutParamIdx: TParamSet): Boolean;
begin
  Result:=false;
end;

function TJEFuncHelper.GetValue2(Sender: TJSONExprParser; FuncObj: TJSONObj;
  var Params: array of Variant; out Val: Variant; out OutParamIdx: TParamSet): Boolean;
begin
  Result:=false;
end;

procedure TJEFuncHelper.SetNextHelper(const Value: TJEFuncHelper);
begin
  FNextHelper := Value;
end;

{ TSimpleVarHelper }

procedure TSimpleVarHelper.Clean;
begin
{$IFDEF NO_COMPLEXOBJ}
  FValueHolder.Clean;
{$ELSE}
  FRootHolder.{$IFDEF SUPEROBJECT}Clear(true){$ELSE}Clean{$ENDIF};
{$ENDIF}
end;

{$IFNDEF NO_COMPLEXOBJ}
function TSimpleVarHelper.ComplexValObjByName(const VarName: String): TJSONLeaf;
var
  i:Integer;
begin
  with FValObjStatck do
    for i:=Pred(Count) downto 0 do
    begin
      Result:={$IFDEF SUPEROBJECT}TJSONObj(FValItfs[i]).GetO{$ELSE}TJSONObj(Objects[i]).Opt{$ENDIF}(VarName);
      if Result<>nil then exit;
    end;
end;

function TSimpleVarHelper.ComplexValObjByNameEx(const VarName: String;
  out AValHolder: TJSONObj): TJSONLeaf;
var
  i:Integer;
begin
  with FValObjStatck do
  begin
    for i:=Pred(Count) downto 0 do
    begin
      AValHolder:=TJSONObj({$IFDEF SUPEROBJECT}FValItfs{$ELSE}Objects{$ENDIF}[i]);
      Result:=AValHolder.{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(VarName);
      if Result<>nil then exit;
    end;
    AValHolder:=TJSONObj({$IFDEF SUPEROBJECT}FValItfs{$ELSE}Objects{$ENDIF}[Count-1]);  //默认为最近的ValObj
  end;
end;
{$ENDIF}

constructor TSimpleVarHelper.Create;
begin
  FValueHolder:={$IFDEF SUPEROBJECT}SO{$ELSE}TJSONObj.Create{$ENDIF};
{$IFNDEF NO_COMPLEXOBJ}
  FRootHolder:=FValueHolder;
  FValObjStatck:=TStringList.Create; {$IFDEF SUPEROBJECT}FValItfs:=TInterfaceList.Create;{$ENDIF}
  {$IFDEF SUPEROBJECT}
  FValObjStatck.Add('');
  FValItfs.Add(FValueHolder);
  {$ELSE}
  FValObjStatck.AddObject('',TObject(FValueHolder));
  {$ENDIF}
{$ENDIF}
end;

destructor TSimpleVarHelper.Destroy;
begin
{$IFDEF NO_COMPLEXOBJ}
  FValueHolder.Free;
{$ELSE}
  {$IFDEF SUPEROBJECT}FRootHolder:=nil{$ELSE}FRootHolder.Free{$ENDIF};
  {$IFDEF SUPEROBJECT}FValItfs.Free;{$ENDIF}
  FValObjStatck.Free;
{$ENDIF}
  inherited;
end;

{$IFNDEF NO_COMPLEXOBJ}
function TSimpleVarHelper.EnterObj(const ObjName: String): Boolean;
var
  Holder:TJSONObj;
  Z:TJSONLeaf;
begin
  Z:=ComplexValObjByNameEx(ObjName,Holder);
  if Z<>nil then
  begin
    if {$IFDEF SUPEROBJECT}Z.DataType<>stObject{$ELSE}not (Z is TJSONObj){$ENDIF} then
    begin
      Result:=false;
      exit;
    end;
    FValueHolder:=TJSONObj(Z);
  end
  else begin
    FValueHolder:={$IFDEF SUPEROBJECT}SO{$ELSE}TJSONObj.Create{$ENDIF};
    Holder.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(ObjName,FValueHolder);
  end;
{$IFDEF SUPEROBJECT}
  FValObjStatck.Add(ObjName);FValItfs.Add(FValueHolder);
{$ELSE}
  FValObjStatck.AddObject(ObjName,FValueHolder);
{$ENDIF}
  Result:=true;
end;
{$ENDIF}

procedure TSimpleVarHelper.Delete(const VarName: String);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}Delete{$ELSE}Remove{$ENDIF}(VarName){$IFNDEF SUPEROBJECT}.Free{$ENDIF};
end;

function TSimpleVarHelper.GetObjAttr(const Attr: String): Variant;
begin
  GetVar(Attr,Result);
end;

function TSimpleVarHelper.GetTraceOnSet: Boolean;
begin
  Result:=FTraceOnSet;
end;

function TSimpleVarHelper.GetVar(const VarName: String;
  out Val: Variant): Boolean;
var
  Z:TJSONLeaf;
begin
{$IFDEF NO_COMPLEXOBJ}
  Z:=FValueHolder.{$IFDEF SUPEROBJECT}GetO{$ELSE}Opt{$ENDIF}(VarName);
{$ELSE}
  Z:=ComplexValObjByName(VarName);
{$ENDIF}
  if Z=nil then
  begin
    Result:=false;
    exit;
  end;
  Val:=VarFromJSON(Z);
  Result:=true;
end;

function TSimpleVarHelper.GetVarCount: Integer;
begin
{$IFDEF NO_COMPLEXOBJ}
  Result:=FValueHolder.Length;
{$ELSE}
  Result:={$IFDEF SUPEROBJECT}JSONParamCount(FRootHolder){$ELSE}FRootHolder.Length{$ENDIF};
{$ENDIF}
end;

function TSimpleVarHelper.GetVarNames(const Idx: Integer): String;
begin
{$IFDEF NO_COMPLEXOBJ}
  Result:=FValueHolder.KeyByIndex[Idx];
{$ELSE}
  {$IFDEF SUPEROBJECT}{TODO: KeyByIndex here.}
    Result:='';
  {$ELSE}
  Result:=FRootHolder.KeyByIndex[Idx];
  {$ENDIF}
{$ENDIF}
end;

function TSimpleVarHelper.GetVarObj(const VarName: String; out Obj: TJSONObj): Boolean;
begin
{$IFDEF NO_COMPLEXOBJ}
  Obj:=FValueHolder.OptJSONObject(VarName);
{$ELSE}
  Obj:=FRootHolder.{$IFDEF SUPEROBJECT}GetO{$ELSE}OptJSONObject{$ENDIF}(VarName);
{$ENDIF}
  Result:=Obj<>nil;
end;

{$IFNDEF NO_COMPLEXOBJ}
function TSimpleVarHelper.LeaveObj(const ObjName: String): Boolean;
begin
  with FValObjStatck do
  begin
    if Count=1 then
    begin
      Result:=false;
      exit;
    end;
    if (ObjName='') or (ObjName=Strings[Count-1]) then
    begin
      {$IFDEF SUPEROBJECT}FValItfs.Delete(Count-1);{$ENDIF}
      Delete(Count-1);
      FValueHolder:=TJSONObj({$IFDEF SUPEROBJECT}FValItfs.Items{$ELSE}Objects{$ENDIF}[Count-1]);
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;
{$ENDIF}

procedure TSimpleVarHelper.Put(const VarName: String; V: Double);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutD{$ELSE}Put{$ENDIF}(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName: String; V: Boolean);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutB{$ELSE}Put{$ENDIF}(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName, V: String);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutS{$ELSE}Put{$ENDIF}(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName: String; V: Integer);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutI{$ELSE}Put{$ENDIF}(VarName,V);
end;

procedure TSimpleVarHelper.PutNull(const VarName: String);
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(VarName,CNULL);
end;

function TSimpleVarHelper.SetObjAttr(const Attr: String;
  const Val: Variant): Boolean;
begin
  Result:=SetVar(Attr,Val);
end;

function TSimpleVarHelper.SetObjectVar(const VarName: String; JObj: TJSONObj): Boolean;
begin
  FValueHolder.{$IFDEF SUPEROBJECT}PutO{$ELSE}Put{$ENDIF}(VarName,JObj);
end;

procedure TSimpleVarHelper.SetOnTrace(const Value: TTraceValueFunc);
begin
  FOnTrace := Value;
end;

procedure TSimpleVarHelper.SetTraceOnSet(const Value: Boolean);
begin
  FTraceOnSet := Value;
end;

function TSimpleVarHelper.SetVar(const VarName: String;
  const Val: Variant): Boolean;
begin
  if FTraceOnSet then
    if Assigned(FOnTrace) then
      FOnTrace(Self,VarName,Val);
  PutVarToJSON(FValueHolder,VarName,Val);
  Result:=true;
end;

function TSimpleVarHelper.VarIsObj(const VarName: String): Boolean;
begin
  Result:=FValueHolder.{$IFDEF SUPEROBJECT}GetO(VarName).DataType=stObject{$ELSE}Opt(VarName) is TJSONObj{$ENDIF};
end;

{ TMemVarHelper }

procedure TMemVarHelper.Clean;
begin
  FVals.Clear;
  FreeAndNil(FTypes);
end;

constructor TMemVarHelper.Create;
begin
  FVals:=TStringList.Create;
  FVals.Duplicates:=dupIgnore;
end;

destructor TMemVarHelper.Destroy;
begin
  FVals.Free;
  FTypes.Free;
  inherited;
end;

function TMemVarHelper.GetVar(const VarName: String;
  out Val: Variant): Boolean;
var
  i:Integer;
  Ch:Char;
begin
  if FTypes<>nil then
  begin
    i:=FVals.IndexOf(VarName);
    if i<0 then
    begin
      Result:=false;
      exit;
    end;
    Ch:=FTypes[i][1];
  end
  else begin
    i:=FVals.IndexOfName(VarName);
    if i<0 then
    begin
      Result:=false;
      exit;
    end;
    Ch:=FVals.ValueFromIndex[i][1];
  end;
  case Ch of
    'D': Val:=PDouble(FVals.Objects[i])^;
    'B': Val:=PBoolean(FVals.Objects[i])^;
    'C': Val:=PChar(FVals.Objects[i])^;
    'F': Val:=PSingle(FVals.Objects[i])^;
    'L': Val:=PLongWord(FVals.Objects[i])^;
    'W': Val:=PWord(FVals.Objects[i])^;
    'Y': Val:=PByte(FVals.Objects[i])^;
    'i': Val:=PShortInt(FVals.Objects[i])^;
    '6': Val:=PInt64(FVals.Objects[i])^;
    else Val:=PInteger(FVals.Objects[i])^;
  end;
  Result:=true;
end;

function TMemVarHelper.GetVarCount: Integer;
begin
  Result:=FVals.Count;
end;

function TMemVarHelper.GetVarNames(const Idx: Integer): String;
begin
  if FTypes=nil then
    Result:=FVals.Names[Idx]
  else
    Result:=FVals[Idx];
end;

procedure TMemVarHelper.RegBool(const VarName: String; const P: PBoolean);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=B',TObject(P));
end;

procedure TMemVarHelper.RegByte(const VarName: String; const P: PByte);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=Y',TObject(P));
end;

procedure TMemVarHelper.RegChar(const VarName: String; const P: PChar);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=C',TObject(P));
end;

procedure TMemVarHelper.RegDouble(const VarName: String; const P: PDouble);
begin
  if FTypes<>nil then exit;                
  FVals.AddObject(VarName+'=D',TObject(P));
end;

procedure TMemVarHelper.RegInt(const VarName: String; const P: PInteger);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=I',TObject(P));
end;

procedure TMemVarHelper.RegInt64(const VarName: String; const P: PInt64);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=6',TObject(P));
end;

procedure TMemVarHelper.RegLongWord(const VarName: String;
  const P: PLongWord);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=L',TObject(P));
end;

procedure TMemVarHelper.RegShortInt(const VarName: String;
  const P: PShortInt);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=i',TObject(P));
end;

procedure TMemVarHelper.RegSingle(const VarName: String; const P: PSingle);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=F',TObject(P));
end;

procedure TMemVarHelper.RegWord(const VarName: String; const P: PWord);
begin
  if FTypes<>nil then exit;
  FVals.AddObject(VarName+'=W',TObject(P));
end;

function TMemVarHelper.SetVar(const VarName: String;
  const Val: Variant): Boolean;
var
  i:Integer;
  S:String;
  Ch:Char;
begin
  Result:=false;
  if FTypes<>nil then
  begin
    i:=FVals.IndexOf(VarName);
    if i<0 then exit;
    Ch:=FTypes[i][1];
  end
  else begin
    i:=FVals.IndexOfName(VarName);
    if i<0 then exit;
    Ch:=FVals.ValueFromIndex[i][1];
  end;
  try
    case Ch of
      'D': PDouble(FVals.Objects[i])^:=Double(Val);
      'F': PSingle(FVals.Objects[i])^:=Single(Val);
      'B': PBoolean(FVals.Objects[i])^:=Boolean(Val);
      'C':
      begin
        S:=VarToStrDef(Val,'');
        if S='' then S:=#0;
        PChar(FVals.Objects[i])^:=S[1];
      end;
      'L': PLongWord(FVals.Objects[i])^:=LongWord(Val);
      'W': PWord(FVals.Objects[i])^:=Word(Val);
      'Y': PByte(FVals.Objects[i])^:=Byte(Val);
      'i': PShortInt(FVals.Objects[i])^:=Val;
      '6': PInt64(FVals.Objects[i])^:=Val;
      else PInteger(FVals.Objects[i])^:=Integer(Val);
    end;
    Result:=true;
  except
  end;
end;

procedure TMemVarHelper.Sort;
var
  i:Integer;
begin
  if Sorted then exit;
  FTypes:=TStringList.Create;
  FVals.Sorted:=true;
  FVals.Sorted:=false;
  for i:=0 to Pred(FVals.Count) do
  begin
    FTypes.Add(FVals.ValueFromIndex[i]);
    FVals[i]:=FVals.Names[i];
  end;
  FVals.Sorted:=true;
end;

function TMemVarHelper.Sorted: Boolean;
begin
  Result:=FTypes<>nil;
end;

{ TExitException }

constructor TExitException.CreateVal(V: Variant);
begin
  inherited Create('RET');
  ReturnVal:=V;
end;

{ TBreakException }

constructor TBreakException.Create(Lv: Integer);
begin
  Level:=Lv;
end;

{ TContinueException }

constructor TContinueException.Create(Lv: Integer);
begin
  Level:=Lv;
end;

initialization
  InitOpRank;
  LastExprType:=etEmpty;

end.
