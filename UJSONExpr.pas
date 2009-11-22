{
  Copyright 2009  creation_zy
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
}

unit UJSONExpr;

interface

uses
  SysUtils, Classes, Variants, uJSON;

const
  BIOS_Operator='op';
  BIOS_ParamHeader='p';
  BIOS_Param1='p1';
  BIOS_StrParamHeader='''';    //为了将字符串与变量名相区别，所有字符串值均带单引号前缀
type
  TTraceLineFunc=procedure (Sender: TObject; LineData: TZAbstractObject; const LineVal: Variant);
  TTraceValueFunc=procedure (Sender: TObject; const VarName: String; const Val: Variant);
  TExprType=(etEmpty, etNumber, etBool, etString, etMixed);
  TJSONVarHelper=class;
  TJSONFuncHelper=class;
  { Expression Parser/Evaluator/Analyser 表达式解析/计算/分析器
    可以将类似 (X+3)*Y 的文本表达式解析为JSON对象，并可以对这个JSON对象进行计算求值
  }
  TJSONExprParser=class
  private
    FFuncHelper: TJSONFuncHelper;
    FVarHelper: TJSONVarHelper;
    FTraceOnLine: Boolean;
    FOnLineComplete: TTraceLineFunc;
    procedure SetOnLineComplete(const Value: TTraceLineFunc);
    procedure SetTraceOnLine(const Value: Boolean);
  public
    property VarHelper: TJSONVarHelper read FVarHelper;
    property FuncHelper: TJSONFuncHelper read FFuncHelper;
    //是否在一个语句执行完毕时触发
    property TraceOnLine:Boolean read FTraceOnLine write SetTraceOnLine;
    property OnLineComplete:TTraceLineFunc read FOnLineComplete write SetOnLineComplete;
    function Eval(AObj: TZAbstractObject):Variant;
    function EvalNumber(AObj: TZAbstractObject; out Val:Double):Boolean;
    function OptimizeJSON(AObj: JSONObject):JSONObject;
    procedure AddVarHelper(AHelper: TJSONVarHelper);
    procedure AddFuncHelper(AHelper: TJSONFuncHelper);
    //将文本表达式转换为JSON表达式树，允许代入变量的值  2+(X*Sin(Y)) => {op:"+",p1:2,p2:{op:"*",p1:"X",p2:{op:"SIN",p1:"Y"}}}
    class function ExprToJSON(const Expr: String; VarHelper: TJSONVarHelper=nil;
      PStart: PInteger=nil; PEnd: PInteger=nil; POutExprLen: PInteger=nil):JSONObject;
    class function ExprToJSONStr(const Expr: String; VarHelper: TJSONVarHelper=nil):String;
    class function GetLastExprType:TExprType;
    { 将JSON表达式还原为文本表达式
      如果指定了VarHelper，会将变量的值代入结果文本
      如果ParentOpRank<0，会为所有表达式加上括号
      如果ParentOpRank>0，会根据运算符优先级，返回尽量不带括号的结果
    }
    class function JSONToExpr(AObj: JSONObject; ParentOpRank: Integer=-1;
      VarHelper: TJSONVarHelper=nil):String;
    class function VarToExprStr(V: Variant):String;
    class function VarNeeded(AObj: JSONObject; var Vars: TStrings):Integer;
  end;
  { 变量值获取类
    可以通过传入的变量名或JSON格式的变量描述，提取变量的值。
    通过设置NextHelper属性，可以形成搜索链。
  }
  TJSONVarHelper=class
  private
    FNextHelper: TJSONVarHelper;
    procedure SetNextHelper(const Value: TJSONVarHelper);
  protected
    function GetVarNames(const Idx: Integer): String; virtual;
    function GetVarCount: Integer; virtual;
    function GetTraceOnSet: Boolean; virtual;
    procedure SetTraceOnSet(const Value: Boolean); virtual;
  public
    property NextHelper: TJSONVarHelper read FNextHelper write SetNextHelper;
    property TraceOnSet:Boolean read GetTraceOnSet write SetTraceOnSet;
    //对变量名进行规范化
    function CheckAndTransName(var VarName: String):Boolean; virtual;
    //Read Value
    function GetVar(const VarName: String; out Val: Variant):Boolean; virtual;
    function GetVar2(AObj: JSONObject; out Val: Variant):Boolean; virtual;
    function GetVarDef(const VarName: String; const Default: Variant):Variant;
    //Write Value
    function SetVar(const VarName: String; const Val: Variant):Boolean; virtual;
    function SetVar2(AObj: JSONObject; const Val: Variant):Boolean; virtual;
    //
    property VarCount:Integer read GetVarCount;
    property VarNames[const Idx:Integer]:String read GetVarNames;
    //JSON I/O
    function ValImport(PlainObj: JSONObject):Integer; virtual;
    function ValExport(PlainObj: JSONObject):Integer; virtual;
  end;
  { 函数值获取类
    可以通过传入的JSON格式的函数，提取结果值。
    通过设置NextHelper属性，可以形成搜索链。
  }
  TJSONFuncHelper=class
  private
    FNextHelper: TJSONFuncHelper;
    procedure SetNextHelper(const Value: TJSONFuncHelper);
  public
    property NextHelper: TJSONFuncHelper read FNextHelper write SetNextHelper;
    function GetValue(Sender: TJSONExprParser; const Func: String;
      Params: array of Variant; out Val:Variant):Boolean; virtual;
    function GetValue2(Sender: TJSONExprParser; FuncObj: JSONObject;
      Params: array of Variant; out Val:Variant):Boolean; virtual;
  end;
  TSimpleVarHelper=class(TJSONVarHelper)
  private
    FValueHolder:JSONObject;
    FTraceOnSet: Boolean;
    FOnTrace: TTraceValueFunc;
    procedure SetOnTrace(const Value: TTraceValueFunc);
  protected
    function GetVarNames(const Idx: Integer): String; override;
    function GetVarCount: Integer; override;
    function GetTraceOnSet: Boolean; override;
    procedure SetTraceOnSet(const Value: Boolean); override;
  public
    property OnTrace:TTraceValueFunc read FOnTrace write SetOnTrace;
    procedure Put(const VarName: String; V: Boolean); overload;
    procedure Put(const VarName: String; V: Double); overload;
    procedure Put(const VarName: String; V: Integer); overload;
    procedure Put(const VarName: String; const V: String); overload;
    procedure PutNull(const VarName: String);
    procedure Delete(const VarName: String);
    procedure Clean;
    function GetVar(const VarName: String; out Val:Variant):Boolean; override;
    function SetVar(const VarName: String; const Val: Variant):Boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

type
  TVarAy=array of Variant;
var
  OpRank:array [Char] of Byte;  //操作符优先级表（根据操作符的首字母来确定）
threadvar
  LastExprType:TExprType;
const
  Digits: set of Char=['0'..'9'];
  VarBegin: set of Char=['a'..'z', 'A'..'Z', '_', '$', '@', #129..#254];  //允许$,@以及汉字
  VarBody: set of Char=['a'..'z', 'A'..'Z', '_', '$', '@', '0'..'9', #129..#254];
  MathOp1: set of Char=['!', '~'];
  MathOp2: set of Char=['+', '-', '*', '/', '\', '%', '^', '&', '|', '.', ':', ';'];
  CompOp2: set of Char=['=', '>', '<'];


procedure InitOpRank;
var
  r:Byte;
begin
  r:=240;
  OpRank['(']:=r; OpRank['[']:=r; OpRank['.']:=r; Dec(r,20);
  OpRank['!']:=r; OpRank['~']:=r; OpRank['N']:=r; Dec(r,20);  //NOT
  OpRank['*']:=r; OpRank['/']:=r; OpRank['\']:=r; OpRank['%']:=r; Dec(r,20);
  OpRank['+']:=r; OpRank['-']:=r; Dec(r,20);
  OpRank['>']:=r; OpRank['<']:=r; OpRank['=']:=r; Dec(r,20);
  OpRank['&']:=r; OpRank['|']:=r; OpRank['^']:=r; Dec(r,20);
  OpRank['A']:=r; Dec(r,20);  //AND
  OpRank['O']:=r; OpRank['X']:=r; Dec(r,20);  //OR XOR
  OpRank['I']:=r; Dec(r,20);  //IN IS
  OpRank[':']:=r; OpRank[',']:=r; Dec(r,20);  //:=
  OpRank[';']:=r; //Sentence end
end;

procedure PutVarToJSON(JObj: JSONObject; const VarName: String; const v: Variant);
begin   
  case VarType(v) of
    varNull:
      JObj.Put(VarName,CNULL);
    varSmallInt, varInteger, varShortInt,  varWord:
      JObj.Put(VarName,Integer(v));
    varSingle, varDouble, varCurrency:
      JObj.Put(VarName,Double(v));
    varDate:
      JObj.Put(VarName,DateTimeToStr(TDateTime(v)));
    varOleStr:
      JObj.Put(VarName,String(v));
    varBoolean:
      JObj.Put(VarName,Boolean(v));
    else
      JObj.Put(VarName,String(v));
  end;
end;

function VarFromJSON(Z:TZAbstractObject):Variant;
begin
  if Z=nil then exit;
  if Z.ClassType=_String then
    Result:=Z.toString
  else if Z.ClassType=_Boolean then
    Result:=_Boolean(Z).boolValue
  else if Z.ClassType=_Double then
    Result:=_Double(Z).doubleValue
  else if Z.ClassType=_Integer then
    Result:=_Integer(Z).intValue
  else
    Result:=Null;
end;

{ TJSONExprParser }

procedure TJSONExprParser.AddFuncHelper(AHelper: TJSONFuncHelper);
begin
  if AHelper=nil then exit;
  if VarHelper<>nil then
    AHelper.NextHelper:=FuncHelper;
  FFuncHelper:=AHelper;
end;

procedure TJSONExprParser.AddVarHelper(AHelper: TJSONVarHelper);
begin
  if AHelper=nil then exit;
  if VarHelper<>nil then
    AHelper.NextHelper:=VarHelper;
  FVarHelper:=AHelper;
end;

function TJSONExprParser.Eval(AObj: TZAbstractObject): Variant;
  function GetP1:Variant;
  begin
    Result:=Eval(JSONObject(AObj).Opt(BIOS_Param1));
  end;
  function GetP2:Variant;
  begin
    Result:=Eval(JSONObject(AObj).Opt(BIOS_ParamHeader+'2'));
  end;
  function GetP3:Variant;
  begin
    Result:=Eval(JSONObject(AObj).Opt(BIOS_ParamHeader+'3'));
  end;
  function GetPN(n:Integer):Variant;
  begin
    Result:=Eval(JSONObject(AObj).Opt(BIOS_ParamHeader+IntToStr(n)));
  end;
  function GetParams(JObj: JSONObject):TVarAy;
  var
    i:Integer;
  begin
    with JSONObject(AObj) do
    begin
      if Length<=1 then exit;
      SetLength(Result,Length-1);
      for i:=0 to High(Result) do
        Result[i]:=GetPN(i);
    end;
  end;
  function Func_IN:Boolean;
  var
    i:Integer;
    NullVal:Boolean;
    Z:TZAbstractObject;
    v1,v2:Variant;
  begin
    Result:=false;
    v1:=GetP1;
    NullVal:=VarIsNull(v1);
    Z:=JSONObject(AObj).Opt(BIOS_ParamHeader+'2');
    if Z=nil then exit;
    if (Z.ClassType=JSONObject) and (JSONObject(Z).OptString(BIOS_Operator)='(') then
    begin
      for i:=1 to Pred(JSONObject(Z).Length) do
      begin
        v2:=Eval(JSONObject(Z).Opt(BIOS_ParamHeader+IntToStr(i)));
        if NullVal then
        begin
          if VarIsNull(v2) then
            Result:=true;
        end
        else if not VarIsNull(v2) then
          Result:=v1=v2;
        if Result then break;
      end;
    end
    else begin
      v2:=GetP2;
      if NullVal then
      begin
        if VarIsNull(v2) then
          Result:=true;
      end
      else if not VarIsNull(v2) then
        Result:=v1=v2;
    end;
  end;
  procedure SetValue;
  var
    Z:TZAbstractObject;
    mstr:String;
  begin
    Result:=GetP2;  //将右侧表达式的值做为整个赋值过程的值
    Z:=JSONObject(AObj).Opt(BIOS_Param1);
    begin
      if Z.ClassType=_String then
      begin
        mstr:=Z.toString;
        if mstr='' then exit;
        if mstr[1] in VarBegin then
          VarHelper.SetVar(mstr,Result);
      end;
    end;
  end;
var
  Func,mstr:String;
  v1,v2:Variant;
  Done:Boolean;
  Z:TZAbstractObject;
begin
  Result:=Null;
  if AObj=nil then exit;
  if AObj.ClassType<>JSONObject then
  begin
    if AObj.ClassType=_String then
    begin
      mstr:=_String(AObj).toString;
      if (mstr<>'') and (mstr[1]<>BIOS_StrParamHeader) then  //变量名
      begin
        if VarHelper<>nil then
          VarHelper.GetVar(mstr,Result);
      end
      else  //加了前缀的字符串，此时应当去掉前缀
        Result:=Copy(mstr,2,MaxInt);
    end
    else if AObj.ClassType=_Boolean then
      Result:=_Boolean(AObj).boolValue
    else if AObj is _Number then
      Result:=_Number(AObj).doubleValue;
    exit;
  end;
  Z:=JSONObject(AObj).ValObjByIndex[0];  //第一个成员，应当是 BIOS_Operator
  if (Z<>nil) and (Z.ClassType=JSONObject) then  //操作符是JSON对象
  begin
    if FuncHelper<>nil then
    begin
      case JSONObject(AObj).Length of
        2: FuncHelper.GetValue2(Self,JSONObject(Z),[GetP1],Result);
        3: FuncHelper.GetValue2(Self,JSONObject(Z),[GetP2,GetP2],Result);
        else
          FuncHelper.GetValue2(Self,JSONObject(Z),[GetP2,GetP2],Result);
      end;
    end;
    exit;
  end;
  if Z=nil then
    Func:=''
  else
    Func:=Z.toString;
  if Func='' then  //如果操作符为空，就用第一个参数的值
  begin
    Result:=GetP1;
    exit;
  end;
  try
    Done:=true;
    case Length(Func) of
      1:
      begin
        if Func[1]='.' then  //something like:  Plan.Max
        begin
          if VarHelper<>nil then
            VarHelper.GetVar2(JSONObject(AObj),Result);
          exit;
        end
        else if Func[1] in (MathOp2+CompOp2) then
        begin
          if Func[1]=';' then
          begin
            //尽量获取后面那个表达式的值，如果只有一个表达式，那就获取第一个表达式的值
            //eg:  X:=10; Y:=3; X*Y-9   => 21
            if JSONObject(AObj).Length>2 then
            begin
              Result:=GetP1;
              if TraceOnLine then
              begin
                Z:=JSONObject(AObj).Opt(BIOS_Param1);
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,JSONObject(Z),Result);
              end;
              Result:=GetP2;
              if TraceOnLine then
              begin
                Z:=JSONObject(AObj).Opt(BIOS_ParamHeader+'2');
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,JSONObject(Z),Result);
              end;
            end
            else begin
              Result:=GetP1;
              if TraceOnLine then
              begin
                Z:=JSONObject(AObj).Opt(BIOS_Param1);
                if Assigned(FOnLineComplete) then
                  FOnLineComplete(Self,JSONObject(Z),Result);
              end;
            end;
            exit;
          end;
          v1:=GetP1;
          v2:=GetP2;
          if (v1=Null) or (v2=Null) then  //两个表达式有一个为空的情况
          begin
            if Func[1]='=' then
            begin
              if (v1=Null) and (v2=Null) then
                Result:=true
              else
                Result:=false;
              exit;
            end;
            Result:=Null;
            exit;
          end;
          case Func[1] of
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
        else if Func[1]='!' then
        begin
          Result:=not Integer(GetP1);
        end
        else
          Done:=false;
      end;
      2:
      begin
        if Func[1] in CompOp2 then
        begin
          if Func='>=' then
            Result:=GetP1>=GetP2
          else if Func='<=' then
            Result:=GetP1<=GetP2
          else if Func='<>' then
            Result:=GetP1<>GetP2
          else if Func='>>' then
            Result:=Integer(GetP1) shr Integer(GetP2)
          else if Func='<<' then
            Result:=Integer(GetP1) shl Integer(GetP2)
          else
            Done:=false;
        end
        else begin
          case Func[1] of
            ':':
            begin
              if Func[2]='=' then  //:=  Set variable value
              begin
                if VarHelper<>nil then
                  SetValue;
              end
              else
                Done:=false;
            end;
            'I':
            begin
              case Func[2] of
                'F': //IF
                begin
                  v1:=GetP1;
                  if VarType(v1)=varBoolean then
                    if Boolean(v1) then
                      Result:=GetP2
                    else
                      Result:=GetP3;
                end;
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
        case Func[1] of
          'A':
            if Func='AND' then
              Result:=Boolean(GetP1) and Boolean(GetP2)
            else
              Done:=false;
          'L':
            if Func='LEN' then
              Result:=Length(String(GetP1))
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
                Result:=Boolean(Result);
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
      else
        Done:=false;
    end;
    if Done then exit;
    if FuncHelper<>nil then
      case JSONObject(AObj).Length of
        2: FuncHelper.GetValue(Self,Func,[GetP1],Result);
        3: FuncHelper.GetValue(Self,Func,[GetP2,GetP2],Result);
        else
          FuncHelper.GetValue(Self,Func,GetParams(JSONObject(AObj)),Result);
      end;
  except
    Result:=Null;
  end;
end;

function TJSONExprParser.EvalNumber(AObj: TZAbstractObject; out Val: Double):Boolean;
  function GetP1(var OK: Boolean):Double;
  begin
    OK:=EvalNumber(JSONObject(AObj).Opt(BIOS_Param1),Result);
  end;
  function GetP2(var OK: Boolean):Double;
  begin
    OK:=EvalNumber(JSONObject(AObj).Opt(BIOS_ParamHeader+'2'),Result);
  end;
  function GetPN(n:Integer; var OK: Boolean):Double;
  begin
    OK:=EvalNumber(JSONObject(AObj).Opt(BIOS_ParamHeader+IntToStr(n)),Result);
  end;
  function GetParams(JObj: JSONObject; var OK: Boolean):TVarAy;
  var
    i:Integer;
    f:Double;
  begin
    with JSONObject(AObj) do
    begin
      if Length<=1 then exit;
      SetLength(Result,Length-1);
      for i:=0 to High(Result) do
      begin
        f:=GetPN(i,OK);
        if not OK then exit;
        Result[i]:=f;
      end;
    end;
  end;
  procedure SetValue;
  var
    Z:TZAbstractObject;
    mstr:String;
  begin
    Val:=GetP2(Result);  //将右侧表达式的值做为整个赋值过程的值
    if not Result then exit;
    Z:=JSONObject(AObj).Opt(BIOS_Param1);
    if Z<>nil then
    begin
      if Z.ClassType=_String then
      begin
        mstr:=Z.toString;
        if mstr='' then exit;
        if mstr[1] in VarBegin then
          VarHelper.SetVar(mstr,Val);
      end;
    end;
  end;
var
  Func,mstr:String;
  v:Variant;
  v1,v2:Double;
  Done:Boolean;
  Z:TZAbstractObject;
begin
  Result:=false;
  if AObj=nil then exit;
  if AObj.ClassType<>JSONObject then
  begin
    if AObj.ClassType=_String then
    begin
      mstr:=_String(AObj).toString;
      if (mstr<>'') and (mstr[1]<>BIOS_StrParamHeader) then  //变量名
      begin
        if VarHelper<>nil then
          VarHelper.GetVar(mstr,v);
        Val:=Double(v);
        Result:=true;
      end;
    end
    else if AObj is _Number then
    begin
      Val:=_Number(AObj).doubleValue;
      Result:=true;
    end;
    exit;
  end;
  Z:=JSONObject(AObj).ValObjByIndex[0];  //第一个成员，应当是 BIOS_Operator
  if (Z<>nil) and (Z.ClassType=JSONObject) then  //操作符是JSON对象
    exit;
  if Z=nil then
    Func:=''
  else
    Func:=Z.toString;
  if Func='' then  //如果操作符为空，就用第一个参数的值
  begin
    Val:=GetP1(Result);
    exit;
  end;
  Done:=true;
  case Length(Func) of
    1:
    begin
      if Func[1] in (MathOp2+CompOp2) then
      begin
        if Func[1]=';' then
        begin
          //同上
          if JSONObject(AObj).Length>2 then
          begin
            GetP1(Result);
            Val:=GetP2(Result);
          end
          else
            Val:=GetP1(Result);
          exit;
        end;
        Result:=true;
        v1:=GetP1(Result);
        v2:=GetP2(Result);
        if not Result then exit;
        case Func[1] of
          '+': Val:=v1+v2;  //String? Number?
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
        case Func[1] of
          '.':  //something like:  Plan.Max
          begin
            if VarHelper<>nil then
            begin
              Result:=VarHelper.GetVar2(JSONObject(AObj),v);
              if Result then
                Val:=Double(v);
            end;
            exit;
          end;
          else if Func[1]='!' then
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
      if Func[1] in CompOp2 then
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
    end;
    {3:
    begin
      case Func[1] of
        'S':
        begin
          Result:=true;
          if Func='SHL' then
            Val:=Trunc(GetP1(Result)) shl Trunc(GetP2(Result))
          else if Func='SHR' then
            Val:=Trunc(GetP1(Result)) shr Trunc(GetP2(Result))
          else
            Done:=false;
          if not Result then exit;
        end;
        else
          Done:=false;
      end;
    end;}
    else
      Done:=false;
  end;
  if Done then exit;
  {
  if FuncHelper<>nil then
    case JSONObject(AObj).Length of
      2: FuncHelper.GetValue(Self,Func,[GetP1],Result);
      3: FuncHelper.GetValue(Self,Func,[GetP2,GetP2],Result);
      else
        FuncHelper.GetValue(Self,Func,GetParams(JSONObject(AObj)),Result);
    end;
  }
end;

class function TJSONExprParser.ExprToJSON(const Expr: String;
  VarHelper: TJSONVarHelper; PStart, PEnd, POutExprLen: PInteger): JSONObject;
type
  TAddMode=(amNone, amBlock, amFunc, amOperator);
var
  SubString: String;
  //Use this function to check whether the name is a var or a function.
  function NextIsBlockBegin(StartPos: Integer):Boolean;
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
  JObjs:array[0..64] of TZAbstractObject;
  LevelBC:array[0..64] of Integer;  //Block depth in eath level.  括号层级
  LevelOpKind:array[0..64] of TAddMode;  //各个层次的操作符类型
  JLevel,BlockCnt:Integer;
  function ExprLevel:Integer;
  begin
    if (JObjs[JLevel]=nil) or (JObjs[JLevel].ClassType=JSONObject) then
      Result:=JLevel
    else
      Result:=JLevel-1;
  end;
  function FuncLevel:Integer;
  begin
    Result:=ExprLevel;
    while LevelBC[Result]>=BlockCnt do
    begin
      Dec(Result);
      if Result<0 then break;
    end;
  end;
  function BlockLevel:Integer;
  begin
    Result:=ExprLevel;
    while (LevelBC[Result]>0) and ((Result>0) and (LevelBC[Result]=LevelBC[Result-1])) do
    begin
      Dec(Result);
      if Result<0 then break;
    end;
  end;
  function InBlockLevel(Rank: Byte=1):Integer;  // Func( X+Y*2    1+X.Next.Val
  var
    Op:String;
  begin
    Result:=ExprLevel;
    //If the expression is inside a block or not...
    if LevelBC[Result]>BlockCnt then  // ... A ? (B ? C) / ...  or  ... A ?  Func( B ? C ) / ...
    begin
      repeat
        Dec(Result);
        if Result<0 then break;
        if LevelOpKind[Result]=amFunc then continue; //函数优先级最高，无需比较
        Op:=JSONObject(JObjs[Result]).OptString(BIOS_Operator);
        if Op<>'' then
          if OpRank[Op[1]]<Rank then break;
      until LevelBC[Result]<BlockCnt;
    end
    else  // ... A ? B / ...
      while LevelBC[Result]=BlockCnt do
      begin
        Dec(Result);
        if Result<0 then break;
        Op:=JSONObject(JObjs[Result]).OptString(BIOS_Operator);
        if Op<>'' then
          if OpRank[Op[1]]<Rank then break;
      end;
    Inc(Result);
  end;
  procedure WriteFloat(F: Double);
  var
    e,n:Integer;
  begin
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      Put(BIOS_ParamHeader+IntToStr(n),F);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
    end;
  end;
  procedure WriteStr(const S: String);
  var
    e,n:Integer;
  begin
    //将两个前后连续定义、且没有其它操作符夹杂的的字符串合并为一个 eg:   'ABC' 'abc'  => 'ABCabc'
    //可以实现用字符串加长变量名的效果  eg:  X2' Old'  will be var name "X2 Old"
    if (JObjs[JLevel].ClassType=_String) then
    begin
      with _String(JObjs[JLevel]) do
        AsString:=AsString+S;
      exit;
    end;
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      Put(BIOS_ParamHeader+IntToStr(n),BIOS_StrParamHeader+S);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
    end;
  end;
  procedure WriteObjStr(const S: String);
  var
    e,n:Integer;
    J:JSONObject;
  begin
    try
      J:=JSONObject.Create(S);
    except
      exit;
    end;
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      Put(BIOS_ParamHeader+IntToStr(n),J);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
    end;
  end;
  procedure WriteBool(B: Boolean);
  var
    e,n:Integer;
  begin
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      Put(BIOS_ParamHeader+IntToStr(n),B);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
    end;
  end;
  procedure WriteNull;
  var
    e,n:Integer;
  begin
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      Put(BIOS_ParamHeader+IntToStr(n),CNULL);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
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
          WriteFloat(StrToFloatDef(VarToStr(v),0));
      end;
  end;
  function WriteVar(VarName: String):Boolean;
  var
    e,n:Integer;
    v:Variant;
  begin
    e:=ExprLevel;
    with JSONObject(JObjs[e]) do
    begin
      n:=Length;
      if (n>1) and (OptString(BIOS_Operator)='') then  //已经有了一个参数，但还缺少操作符
      begin
        Result:=false;
        exit;
      end;
      if VarHelper<>nil then
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
      Put(BIOS_ParamHeader+IntToStr(n),VarName);
      JLevel:=e+1;
      JObjs[JLevel]:=ValObjByIndex[n];
      LevelBC[JLevel]:=BlockCnt;
      LevelOpKind[JLevel]:=amNone;
    end;
    Result:=true;
  end;
  procedure AddOp(const Func: String; AddMode: TAddMode=amOperator; UsePiror: Boolean=false);
    function NewFuncObj:JSONObject;
    begin
      Result:=JSONObject.Create;
      Result.Put(BIOS_Operator,Func);
    end;
  var
    IsEmpty:Boolean;
    n:Integer;
    Z:TZAbstractObject;
    J,JP,JN:JSONObject;
    mstr:String;
    label CommonCase;
  begin
    //初始的情况
    if (JObjs[JLevel]=nil) then
    begin
      JObjs[JLevel]:=NewFuncObj;
      LevelOpKind[JLevel]:=AddMode;
      exit;
    end;
    //最后一个Symbol是简单值或变量的情况 -- 应当将其提升，嵌入到Func表达式内
    if JObjs[JLevel].ClassType<>JSONObject then
    begin
      //简单变量后跟双目操作符
      if UsePiror then
      begin
        n:=JLevel-1;
        JP:=JSONObject(JObjs[n]); //表达式JSON对象
        //没有操作符的表达式 -- 填入Func
        if LevelOpKind[n]=amNone {JP.OptString(BIOS_Operator)=''} then
        begin
          JP.Put(BIOS_Operator,Func);
          LevelOpKind[n]:=AddMode;
          Dec(JLevel);
          exit;
        end;
        if LevelBC[n]>=BlockCnt then  //当前双目操作符和前一个表达式位于相同的括号层次
        begin
          if (AddMode=amOperator) and (Func<>'') then
          begin
            mstr:=JSONObject(JObjs[n]).OptString(BIOS_Operator);
            if (mstr<>'') and (OpRank[Func[1]]<=OpRank[mstr[1]]) then
            begin
              JLevel:=InBlockLevel(OpRank[Func[1]]); //InBlockLevel; //n;
              goto CommonCase;
            end;
          end;
        end;
        with JP do
        begin
          mstr:=KeyByIndex[Length-1];  //最后一个Key
          Z:=Remove(mstr);
        end;
        JObjs[JLevel]:=NewFuncObj;
        JSONObject(JObjs[JLevel]).Put(BIOS_Param1,Z);
        JP.Put(mstr,JObjs[JLevel]);
        if Func='' then
          LevelOpKind[JLevel]:=amNone
        else
          LevelOpKind[JLevel]:=AddMode;
      end
      else begin
        n:=JLevel-1;
        if LevelOpKind[n]=amNone {J.OptString(BIOS_Operator)=''} then
        begin
          JSONObject(JObjs[n]).Put(BIOS_Operator,Func);
          if Func='' then
            LevelOpKind[n]:=amNone
          else
            LevelOpKind[n]:=AddMode;
        end;
        Dec(JLevel);
      end;
      exit;
    end
    else if UsePiror then
    begin
      JLevel:=InBlockLevel(OpRank[Func[1]]);
    end;
  CommonCase:
    J:=JSONObject(JObjs[JLevel]);
    if AddMode=amFunc then
      with J do
        IsEmpty:=(Length<=1) and (LevelOpKind[JLevel]=amNone) //(OptString(BIOS_Operator)='')
    else
      IsEmpty:=false;
    if ((AddMode=amOperator) or IsEmpty) and (LevelOpKind[JLevel]=amNone){(J.OptString(BIOS_Operator)='')} then
    begin
      J.Put(BIOS_Operator,Func);
      LevelOpKind[JLevel]:=AddMode;
    end
    else begin
      JN:=NewFuncObj;
      JObjs[JLevel+1]:=JN;
      //按照左侧优先结合的规律进行重新组合——考虑运算符的优先级
      // not X  =>  (not X) or Y
      // X + Y  =>  ( X + Y ) - Z
      if UsePiror then
      begin
        //当前Level的所有者
        if JLevel>0 then
        begin
          if LevelBC[JLevel]>=BlockCnt then
            JP:=JSONObject(JObjs[JLevel-1])
          else begin
            with J do
            begin
              mstr:=BIOS_ParamHeader+IntToStr(Length-1);
              JN.Put(BIOS_ParamHeader+'1',Remove(mstr));
              Put(mstr,JN);
            end;
            Inc(JLevel);
            LevelBC[JLevel]:=BlockCnt;
            exit;
          end;
        end
        else
          JP:=nil;
        if JP<>nil then
        begin
          n:=JP.Length;
          mstr:=BIOS_ParamHeader+IntToStr(n-1);
          J:=JSONObject(JP.Remove(mstr));
          JN.Put(BIOS_Param1,J);
          JP.Put(mstr,JN);
        end
        else
          JN.Put(BIOS_Param1,J);
        JObjs[JLevel]:=JN;
        exit;
      end
      else
        with J do
          Put(BIOS_ParamHeader+IntToStr(Length),JObjs[JLevel+1]);
      LevelBC[JLevel+1]:=BlockCnt;
      Inc(JLevel);
      if Func='' then
        LevelOpKind[JLevel]:=amNone
      else
        LevelOpKind[JLevel]:=AddMode;
    end;
  end;
var
  i,s,e: Integer;
  FuncName: String;
  ExprStart: Boolean;
  StrValue: string;
  CW:Word;
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
    e:=PEnd^;
    SubString:=Copy(Expr,s,s-e+1);
  end
  else if S<=1 then
    SubString:=Expr
  else
    SubString:=Copy(Expr,s,MaxInt);
  if SubString='' then
  begin
    Result:=JSONObject.Create;
    if POutExprLen<>nil then
      POutExprLen^:=0;
    exit;
  end;
  JLevel:=0;
  AddOp('',amNone);
  LevelBC[0]:=0;
  BlockCnt:=0;
  FuncName:='';
  ExprStart:=true;
  e:=Length(SubString);
  i:=1;
  while i<=e do
  begin
    Ch:=SubString[I];
    if Ch<=' ' then
    begin
      Inc(I);
      continue;
    end;
    StrValue:='';
    if (ExprStart and (Ch='-')) or (Ch in Digits) then
    begin
      repeat
        StrValue:=StrValue + SubString[i];
        Inc(I);
      until not (SubString[I] in Digits+['.']);
      //并不在数字之前的单独的负号  解释为 0 - 
      if StrValue='-' then
      begin
        AddOp(StrValue);
        WriteFloat(0);
      end
      else
        WriteFloat(StrToFloatDef(StrValue,0));
      ExprStart:=false;
    end
    else if (Ch in VarBegin) then  //变量或函数
    begin
      ExprStart:=false;
      repeat
        StrValue:=StrValue + SubString[i];
        Inc(i);
      until (SubString[I] in VarBody)=false;
      FuncName:=UpperCase(StrValue);
      if (FuncName='AND') or (FuncName='OR') or (FuncName='IN') or (FuncName='IS') then  //双目运算符
      begin
        if JLevel>=High(JObjs) then break;  //强制跳出
        AddOp(FuncName,amOperator,true);
        FuncName:='';
      end
      else if NextIsBlockBegin(I) or (FuncName='NOT') then
      begin
        if not NextIsBlockBegin(I) then
          AddOp(FuncName,amFunc);
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
          if JLevel>=High(JObjs) then break;  //强制跳出
          AddOp(FuncName,amOperator,true);
        end;
        FuncName:='';
      end;
    end
    else if (Ch in MathOp2+CompOp2) then  //双目运算符以及比较操作符、赋值运算符
    begin
      if JLevel>=High(JObjs) then break;  //强制跳出
      if Ch in MathOp2-[':'] then  //排除赋值的情况
      begin
        StrValue:=Ch;
        Inc(I);
      end
      else
        repeat
          StrValue:=StrValue + SubString[i];
          Inc(i);
        until (SubString[I] in CompOp2)=false;
      AddOp(StrValue,amOperator,true);
      FuncName:='';
      ExprStart:=true;
    end
    else begin
      case Ch of
        ',':
        begin
          JLevel:=FuncLevel;
          if JLevel<0 then JLevel:=0;
          //逗号位于括号内部，并且括号外的操作符不是一般函数（无论单目还是双目算子，右侧都不会有多个参数)
          // -- 将括号内的内容做为集合处理
          if LevelBC[JLevel]<BlockCnt then
            with JSONObject(JObjs[JLevel]) do
            begin
              StrValue:=OptString(BIOS_Operator);
              if StrValue='' then
              begin
                Put(BIOS_Operator,'(');  //括号做为集合标志
              end
              else if not (StrValue[1] in VarBegin+['(']) or (StrValue='IN') then
              begin
                Inc(JLevel);
                AddOp('(',amOperator,true);
                Dec(LevelBC[JLevel]);
              end;
            end;
          Inc(i);
          ExprStart:=true;
        end;
        '(':
        begin
          if JLevel>=High(JObjs) then break;  //强制跳出
          Inc(BlockCnt);
          if FuncName<>'' then  //函数调用
          begin
            AddOp(FuncName,amFunc);
            FuncName:='';
            LevelBC[JLevel]:=BlockCnt-1;
          end
          else
            AddOp('',amBlock);
          Inc(I);
          FuncName:='';
          ExprStart:=true;
        end;
        ')':
        begin
          JLevel:=BlockLevel; //FuncLevel;
          if JLevel<0 then JLevel:=0;
          Dec(BlockCnt);
          Inc(I);
          FuncName:='';
        end;
        '[':  //数组下标
        begin
          AddOp(Ch,amFunc);
          Inc(BlockCnt);
          Inc(I);
          FuncName:='';
          ExprStart:=true;
        end;
        ']':
        begin
          JLevel:=FuncLevel;
          if JLevel<0 then
            JLevel:=0
          else if JLevel>0 then
          begin

          end;
          Dec(BlockCnt);
          Inc(I);
          FuncName:='';
        end;
        '!','~':
        begin
          AddOp(Ch,amFunc);
          Inc(i);
        end;
        '''':
        begin
          Inc(i);
          while i<=e do
          begin
            if SubString[i]='''' then
            begin
              Inc(i);
              if (i<=e) and (SubString[i]='''') then
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
        '{':  //Embed JSONObject
        begin
          while i<=Length(SubString) do
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
  Result:=JSONObject(JObjs[0]);
  if POutExprLen<>nil then
    POutExprLen^:=i;
end;

class function TJSONExprParser.ExprToJSONStr(const Expr: String;
  VarHelper: TJSONVarHelper): String;
var
  J:JSONObject;
begin
  J:=ExprToJSON(Expr,VarHelper);
  if J=nil then
    Result:=''
  else begin
    Result:=J.ToString;
    J.Free;
  end;
end;

class function TJSONExprParser.GetLastExprType: TExprType;
begin
  Result:=LastExprType;
end;

class function TJSONExprParser.JSONToExpr(AObj: JSONObject; ParentOpRank: Integer;
  VarHelper: TJSONVarHelper): String;
var
  Func:String;
  IsCommonFunc:Boolean;
  OpRk:Integer;
  function J2Str(Z: TZAbstractObject):String;
  var
    v:Variant;
  begin
    if Z=nil then
    begin
      Result:='';
      exit;
    end;
    if Z.ClassType=JSONObject then
    begin
      if IsCommonFunc then  //只有一个参数的普通函数自带括号了
      begin
        if ParentOpRank<0 then
          Result:=JSONToExpr(JSONObject(Z),ParentOpRank,VarHelper)
        else
          Result:=JSONToExpr(JSONObject(Z),0,VarHelper)
      end
      else
        Result:=JSONToExpr(JSONObject(Z),OpRk,VarHelper);
    end
    else begin
      Result:=Z.toString;
      if (Result<>'') and (Z.ClassType=_String) then
      begin
        if Result[1]=BIOS_StrParamHeader then  //String
          Result:=QuotedStr(Copy(Result,2,MaxInt))
        else begin //Var Name
          if (VarHelper<>nil) and VarHelper.GetVar(Result,v) then
          begin
            Result:=VarToExprStr(v);
          end;
        end;
      end;
    end;
  end;
var
  i:Integer;
begin
  Result:='';
  if AObj=nil then exit;
  Func:=AObj.OptString(BIOS_Operator);
  IsCommonFunc:=(Func='') or not ((Func[1] in MathOp2+CompOp2+['.']+MathOp1) or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS'));
  if IsCommonFunc or (ParentOpRank<0) then
    OpRk:=-1
  else
    OpRk:=OpRank[Func[1]];
  Result:=J2Str(AObj.Opt(BIOS_Param1));
  if Func='' then exit;
  if (Func[1] in MathOp2+CompOp2+['.']+MathOp1) or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') then
  begin
    if Func[1] in MathOp2+CompOp2+['.'] then
      Result:=Result+Func
    else if Func[1] in MathOp1 then
    begin
      Result:=Func+Result;
      if (ParentOpRank<0) or (OpRk<ParentOpRank) then
        Result:='('+Result+')';
      exit;
    end
    else
      Result:=Result+' '+Func+' ';
    Result:=Result+J2Str(AObj.Opt(BIOS_ParamHeader+'2'));
    if (ParentOpRank<0) or (OpRk<ParentOpRank) then
      Result:='('+Result+')';
  end
  else begin
    for i:=2 to Pred(AObj.Length) do
      Result:=Result+','+J2Str(AObj.Opt(BIOS_ParamHeader+IntToStr(i)));
    Result:='('+Result+')';
    if Func<>'(' then  //集合以 "(" 做为操作符
      Result:=Func+Result;
  end;
end;

function TJSONExprParser.OptimizeJSON(AObj: JSONObject): JSONObject;
var
  SL:TStrings;
  c,i:Integer;
  Z:TZAbstractObject;
begin
  Result:=nil;
  if AObj=nil then exit;
  SL:=nil;
  c:=VarNeeded(AObj,SL);
  if c>0 then
  begin
    Result:=JSONObject.Create;
    with AObj do
    begin
      Result.Put(KeyByIndex[0],ValObjByIndex[0].Clone);
      for i:=1 to Pred(Length) do
      begin
        Z:=AObj.ValObjByIndex[i];
        if Z.ClassType<>JSONObject then
        begin
          Result.Put(KeyByIndex[i],Z.Clone);
          continue;
        end;
      end;
    end;
    SL.Free;
  end;
end;

procedure TJSONExprParser.SetOnLineComplete(const Value: TTraceLineFunc);
begin
  FOnLineComplete := Value;
end;

procedure TJSONExprParser.SetTraceOnLine(const Value: Boolean);
begin
  FTraceOnLine := Value;
end;

class function TJSONExprParser.VarNeeded(AObj: JSONObject; var Vars: TStrings):Integer;
  procedure Check(Z: TZAbstractObject);
  var
    mstr:String;
  begin
    if Z=nil then exit;
    if Z.ClassType=_String then
    begin
      mstr:=_String(Z).toString;
      if (mstr<>'') and (mstr[1]<>BIOS_StrParamHeader) then  //变量名
      begin
        Inc(Result);
        if Vars=nil then
          Vars:=TStringList.Create;
        Vars.Add(mstr);
      end;
    end
    else if Z.ClassType=JSONObject then
    begin
      Inc(Result,VarNeeded(JSONObject(Z),Vars));
    end;
  end;
var
  i:Integer;
begin
  Result:=0;
  if AObj=nil then exit;
  //忽略为首的操作符，检查每个参数成员
  for i:=1 to Pred(AObj.Length) do
    Check(AObj.ValObjByIndex[i]);
end;

class function TJSONExprParser.VarToExprStr(V: Variant): String;
begin
  if VarIsNull(V) then
    Result:=CNULL.toString
  else if VarType(V)=varString then
    Result:=QuotedStr(String(V))
  else if VarType(V)=varBoolean then
  begin
    if Boolean(V) then
      Result:=_Boolean._TRUE.toString
    else
      Result:=_Boolean._FALSE.toString;
  end
  else
    Result:=VarToStr(V);
end;

{ TJSONVarHelper }

function TJSONVarHelper.CheckAndTransName(var VarName: String): Boolean;
begin
  Result:=true;
end;

function TJSONVarHelper.GetTraceOnSet: Boolean;
begin
  Result:=false;
end;

function TJSONVarHelper.GetVar(const VarName: String; out Val:Variant):Boolean;
begin
  if NextHelper<>nil then
    Result:=NextHelper.GetVar(VarName,Val)
  else
    Result:=false;
end;

function TJSONVarHelper.GetVar2(AObj: JSONObject; out Val:Variant):Boolean;
begin
  if AObj=nil then
    Result:=GetVar('',Val)
  else
    Result:=GetVar(AObj.OptString(BIOS_Param1),Val);
end;

function TJSONVarHelper.GetVarCount: Integer;
begin
  Result:=-1;
end;

function TJSONVarHelper.GetVarDef(const VarName: String;
  const Default: Variant): Variant;
begin
  if not GetVar(VarName,Result) then
    Result:=Default;
end;

function TJSONVarHelper.GetVarNames(const Idx: Integer): String;
begin
  Result:='';
end;

procedure TJSONVarHelper.SetNextHelper(const Value: TJSONVarHelper);
begin
  FNextHelper := Value;
end;

procedure TJSONVarHelper.SetTraceOnSet(const Value: Boolean);
begin

end;

function TJSONVarHelper.SetVar(const VarName: String;
  const Val: Variant): Boolean;
begin
  Result:=false;
end;

function TJSONVarHelper.SetVar2(AObj: JSONObject;
  const Val: Variant): Boolean;
begin
  Result:=false;
end;

function TJSONVarHelper.ValExport(PlainObj: JSONObject): Integer;
var
  i:Integer;
  mstr:String;
  v:Variant;
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
    if not GetVar(mstr,v) then
      Dec(Result)
    else
      PutVarToJSON(PlainObj,mstr,v);
  end;
end;

function TJSONVarHelper.ValImport(PlainObj: JSONObject): Integer;
var
  i:Integer;
  mstr:String;
  v:Variant;
  Z:TZAbstractObject;
begin
  Result:=0;
  if PlainObj=nil then exit;
  with PlainObj do
    for i:=0 to Pred(Length) do
    begin
      Z:=ValObjByIndex[i];
      if Z.ClassType=JSONObject then continue;
      mstr:=KeyByIndex[i];
      if mstr='' then continue;
      v:=VarFromJSON(Z);
      if SetVar(mstr,v) then
        Inc(Result);
    end;
end;

{ TJSONFuncHelper }

function TJSONFuncHelper.GetValue(Sender: TJSONExprParser; const Func: String;
  Params: array of Variant; out Val: Variant): Boolean;
begin
  Result:=false;
end;

function TJSONFuncHelper.GetValue2(Sender: TJSONExprParser; FuncObj: JSONObject;
  Params: array of Variant; out Val: Variant): Boolean;
begin
  Result:=false;
end;

procedure TJSONFuncHelper.SetNextHelper(const Value: TJSONFuncHelper);
begin
  FNextHelper := Value;
end;

{ TSimpleVarHelper }

procedure TSimpleVarHelper.Clean;
begin
  FValueHolder.Clean;
end;

constructor TSimpleVarHelper.Create;
begin
  FValueHolder:=JSONObject.Create;
end;

destructor TSimpleVarHelper.Destroy;
begin
  FValueHolder.Free;
  inherited;
end;

procedure TSimpleVarHelper.Delete(const VarName: String);
begin
  FValueHolder.Remove(VarName).Free;
end;

function TSimpleVarHelper.GetTraceOnSet: Boolean;
begin
  Result:=FTraceOnSet;
end;

function TSimpleVarHelper.GetVar(const VarName: String;
  out Val: Variant): Boolean;
var
  Z:TZAbstractObject;
begin
  Z:=FValueHolder.Opt(VarName);
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
  Result:=FValueHolder.Length;
end;

function TSimpleVarHelper.GetVarNames(const Idx: Integer): String;
begin
  Result:=FValueHolder.KeyByIndex[Idx];
end;

procedure TSimpleVarHelper.Put(const VarName: String; V: Double);
begin
  FValueHolder.Put(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName: String; V: Boolean);
begin
  FValueHolder.Put(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName, V: String);
begin
  FValueHolder.Put(VarName,V);
end;

procedure TSimpleVarHelper.Put(const VarName: String; V: Integer);
begin
  FValueHolder.Put(VarName,V);
end;

procedure TSimpleVarHelper.PutNull(const VarName: String);
begin
  FValueHolder.Put(VarName,CNULL);
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

initialization
  InitOpRank;
  LastExprType:=etEmpty;

end.
