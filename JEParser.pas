unit JEParser;

interface

uses
  uJSON, SysUtils, UJSONExpr;

type
  TJEOperators=(
    jeopStEnd,    // ;
    jeopStDiv,    // :  (VB)
    jeopSetValue, // :=
    jeopEqual,    // =
    jeopNE,       // <>
    jeopBT,       // >
    jeopLT,       // <
    jeopBE,       // >=
    jeopSE,       // <=
    jeopStB,      // {  (C)
    jeopStE,      // }  (C)
    jeopBracketB, // (
    jeopBracketE, // )
    jeopArrayB,   // [
    jeopArrayE,   // ]
    jeopVarTp,    // AS  (VB)
    jeopFuncTp,   // :
    jeopParamDiv, // ,
    jeopStrQuote, // '
    jeopStrLink,  // +
    jeopMod,      // %
    jeopDiv,      // \
    jeopBoolAnd,  // AND
    jeopBoolOr,   // OR
    jeopBoolXor,  // XOR
    jeopBoolNot,  // NOT
    jeopAnd,      // &
    jeopOr,       // |
    jeopXor,      // ^
    jeopNot,      // !
    jeopShr,      // >>
    jeopShl,      // <<
    jeopMember,   // .
    jeopRange     // ..
  );
const
  JOP_SetValue=':=';
  StdJEOps:array [TJEOperators] of String=(
    ';',
    '',
    JOP_SetValue,
    '=',
    '<>',
    '>',
    '<',
    '>=',
    '<=',
    '(',
    ')',
    '(',
    ')',
    '[',
    ']',
    ':',
    ':',
    ',',
    '''',
    '+',
    '%',
    '\',
    'AND',
    'OR',
    'XOR',
    'NOT',
    '&',
    '|',
    '^',
    '!',
    '>>',
    '<<',
    '.',
    '..'
  );
type
  TJETypes=(
    jetVoid,jetAny,jetString,jetInt,jetBool,
    jetLong,jetShort,jetSmall,jetFloat,jetSingle,
    jetDate,jetTime,jetDateTime,jetChar,jetBin,
    jetPointer,jetArray,jetObject,jetInterface,jetUnknown);
const
  StdJETypes:array [TJETypes] of String=(
    'VOID', 'ANY', 'STRING', 'INT', 'BOOL',
    'LONG', 'SHORT', 'SMALL', 'FLOAT', 'SINGLE',
    'DATE', 'TIME', 'DATETIME', 'CHAR', 'BIN',
    'POINTER', 'ARRAY', 'OBJECT', 'INTERFACE', 'UNKNOWN'
  );
type
  TJENode=TZAbstractObject;
  TJEBranch=JSONObject;
  TJEParser=class

  end;
  TJETranslaterClass=class of TJETranslater;
  TNodeData=record
    Node: TJENode;
    Op: String;
    NeedVal: Boolean;
    NoLeftOutput: Boolean; //当右部完成利用LeftStr后，左部不必再使用
    LeftStr,               //用于赋值左部的嵌入  X:=IF(A>0,B,C) => IF A>0 THEN X:=B ELSE X:=C
    PerfixStr,             //用于表达式中条件转移的提前实现  XXX := YY + IF(...);
    PostfixStr: String;
  end;
  PNodeData=^TNodeData;
  TJETranslater=class
  private
    FIdent: String;
    FNodeAy: array[0..MaxJETreeLevel] of TNodeData;
    FCurNodeIdx: Integer;
    FTmpVarNum: Integer;
    FNextNeedValue: Boolean;
    procedure SetIdent(const Value: String);
    function GetIdent: String;
  protected
    function PushNode(ANode: TJENode; const AOp: String):Boolean;
    function PopupNode:TJENode;
    function CurNode:TJENode;
    function ParentNode:TJENode;
    function ParentOp:String;
    function ParentNodePtr:PNodeData;
    function NodeNeedVal:Boolean;
    function OwnerNeedVal:Boolean;
    procedure SetCurNeedVal(B: Boolean);
    procedure SetNextNeedVal(B: Boolean);
    procedure SetCurLeftStr(const Str: String);
    procedure SetParentNoLeft;
    procedure Set_Before(const Str: String);
    procedure Set_After(const Str: String);
    function Get_Before:String;
    function Get_After:String;
    procedure Gather_BeforeAfter;
    procedure Combine_BeforeAfter(var Str: String);
    procedure Combine_SubBeforeAfter(var Str: String);
    procedure Clear_BeforeAfter;
    procedure InitNodeAy;
    //不同语言的运算符优先级不同
    function GetOpRank(const Op: String):Byte; virtual;
    function GenTempVar:String; virtual;
    function Node2Str(Z: TJENode; IsCommonFunc: Boolean;
      OpRank, PrnRank: Integer):String;
    function LineDivStr:String; virtual;
    function TransSetValue(JObj: TJEBranch):String; virtual;
    function TransIF(JObj: TJEBranch):String; virtual;
    function TransIFELSE(JObj: TJEBranch):String; virtual;
    function TransIS(JObj: TJEBranch):String; virtual;
    function TransDEC(JObj: TJEBranch):String; virtual;
    function TransINC(JObj: TJEBranch):String; virtual;
    function TransFOR(JObj: TJEBranch):String; virtual;
    function TransCASE(JObj: TJEBranch):String; virtual;
    function TransEVAL(JObj: TJEBranch):String; virtual;
    function TransEXIT(JObj: TJEBranch):String; virtual;
    function TransPRED(JObj: TJEBranch):String; virtual;
    function TransSUCC(JObj: TJEBranch):String; virtual;
    function TransWAIT(JObj: TJEBranch):String; virtual;
    function TransBREAK(JObj: TJEBranch):String; virtual;
    function TransWHILE(JObj: TJEBranch):String; virtual;
    function TransREPEAT(JObj: TJEBranch):String; virtual;
    function TransRETURN(JObj: TJEBranch):String; virtual;
    function TransTIMES(JObj: TJEBranch):String; virtual;
    function TransISNULL(JObj: TJEBranch):String; virtual;
  public
    class function Language:ShortString; virtual; abstract;
    class function DefaultIdent:String; virtual;
    property Ident:String read GetIdent write SetIdent;
    function GetOp(JOp: TJEOperators):String; virtual;
    function IsStdType(Str: String; out JT: TJETypes):Boolean; virtual;
    class function RegisterTranslater(AClass: TJETranslaterClass):Boolean;
    class function GetTranslaterForLanguage(ALan: String):TJETranslaterClass;
    function TranslateJETree(JObj: TJENode):String;
    function TransANode(JObj: TJENode; PrnRank: Integer=-1):String; virtual;
    function TransNodeOp(JObj: TJEBranch; const Op: String; PrnRank: Integer):String; virtual;
    function TransCommonOp(JObj: TJEBranch; const Op: String; PrnRank: Integer=-1):String; virtual;
    function TransFuncOp(JObj: TJEBranch; const Op: String):String; virtual;
    function TransSpaceOp(JObj: TJEBranch; const Op: String):String; virtual;
    // P1 op P2
    function Trans_Mid(JObj: TJEBranch; const Op: String;
      Rank, PrnRank: Integer):String;
    // op(P1,...)
    function Trans_Func(JObj: TJEBranch; const Op: String):String;
    function AddIdent(const Text: String):String;
  end;

implementation

uses
  Classes;

var
  JETypesOnLen:array [3..9] of array of TJETypes;
  Translaters:TStrings;

procedure InitTypeLenAy;
var
  i,c,k,s:Integer;
  j:TJETypes;
  Ay:array [0..255] of TJETypes;
begin
  s:=0;
  for i:=Low(JETypesOnLen) to High(JETypesOnLen) do
  begin
    c:=0;
    for j:=Low(TJETypes) to High(TJETypes) do
    begin
      if Length(StdJETypes[j])=i then
      begin
        Ay[c]:=j;
        Inc(c);
      end;
    end;
    SetLength(JETypesOnLen[i],c);
    for k:=0 to Pred(c) do
      JETypesOnLen[i][k]:=Ay[k];
    Inc(s,c);
  end;
  if s<>(Integer(High(TJETypes))-Integer(Low(TJETypes))+1) then
    raise Exception.Create('Init TypeLenAy Error!');
end;

procedure FreeTypeLenAy;
var
  i:Integer;
begin
  for i:=Low(JETypesOnLen) to High(JETypesOnLen) do
    SetLength(JETypesOnLen[i],0);
end;

{ TJETranslater }

function TJETranslater.AddIdent(const Text: String): String;
begin
  Result:=Ident+StringReplace(Text,#13#10,#13#10+Ident,[rfReplaceAll]);
end;

procedure TJETranslater.Clear_BeforeAfter;
begin
  with FNodeAy[FCurNodeIdx] do
  begin
    PerfixStr:='';
    PostfixStr:='';
  end;
end;

procedure TJETranslater.Combine_BeforeAfter(var Str: String);
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  if Idx<0 then Idx:=0;
  with FNodeAy[Idx] do
  begin
    if PerfixStr<>'' then
    begin
      Str:=PerfixStr+LineDivStr+Str;
    end;
    if PostfixStr<>'' then
    begin
      Str:=Str+LineDivStr+PostfixStr;
    end;
  end;
end;

procedure TJETranslater.Combine_SubBeforeAfter(var Str: String);
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  if Idx>MaxJETreeLevel then exit;
  with FNodeAy[Idx+1] do
  begin
    if PerfixStr<>'' then
    begin
      Str:=PerfixStr+LineDivStr+Str;
    end;
    if PostfixStr<>'' then
    begin
      Str:=Str+LineDivStr+PostfixStr;
    end;
  end;
end;

function TJETranslater.CurNode: TJENode;
begin
  if FCurNodeIdx>=0 then
    Result:=FNodeAy[FCurNodeIdx].Node
  else
    Result:=nil;
end;

class function TJETranslater.DefaultIdent: String;
begin
  Result:='  ';
end;

procedure TJETranslater.Gather_BeforeAfter;
var
  Idx:Integer;
  mstr,mstr2:String;
begin
  Idx:=FCurNodeIdx;
  if Idx>=MaxJETreeLevel then exit;
  mstr:=FNodeAy[Idx+1].PostfixStr;
  mstr2:=FNodeAy[Idx+1].PerfixStr;
  if (mstr='') and (mstr2='') then exit;
  with FNodeAy[Idx] do
  begin
    if mstr<>'' then
      if PostfixStr<>'' then
        PostfixStr:=PostfixStr+LineDivStr+mstr
      else
        PostfixStr:=mstr;
    if mstr2<>'' then
      if PerfixStr<>'' then
        PerfixStr:=PerfixStr+LineDivStr+mstr2
      else
        PerfixStr:=mstr2;
  end;
end;

function TJETranslater.GenTempVar: String;
begin
  Inc(FTmpVarNum);
  Result:='tv_'+IntToStr(FCurNodeIdx)+'_'+IntToStr(FTmpVarNum);
end;

function TJETranslater.GetIdent: String;
begin
  if FIdent='' then
    Result:=DefaultIdent
  else
    Result:=FIdent;
end;

function TJETranslater.GetOp(JOp: TJEOperators): String;
begin
  Result:=StdJEOps[JOp];
end;

function TJETranslater.GetOpRank(const Op: String): Byte;
begin
  if Op='' then
    Result:=0
  else
    Result:=GetStdOpRank(Op[1],Op);
end;

class function TJETranslater.GetTranslaterForLanguage(ALan: String): TJETranslaterClass;
var
  i:Integer;
begin
  ALan:=UpperCase(ALan);
  for i:=0 to Pred(Translaters.Count) do
  begin
    Result:=TJETranslaterClass(Translaters.Objects[i]);
    if UpperCase(Result.Language)=ALan then exit;
  end;
  Result:=nil;
end;

function TJETranslater.Get_After: String;
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
  begin
    Result:=PostfixStr;
  end;
end;

function TJETranslater.Get_Before: String;
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
  begin
    Result:=PerfixStr;
  end;
end;

procedure TJETranslater.InitNodeAy;
begin
  with FNodeAy[0] do
  begin
    Node:=nil;
    Op:='';
  end;
  FCurNodeIdx:=-1;
  FTmpVarNum:=0;
end;

function TJETranslater.IsStdType(Str: String; out JT: TJETypes): Boolean;
var
  i,n:Integer;
begin
  n:=Length(Str);
  if (n<3) or (n>9) then
  begin
    Result:=false;
    exit;
  end;
  Str:=UpperCase(Str);
  for i:=Low(JETypesOnLen[n]) to High(JETypesOnLen[n]) do
  begin
    JT:=JETypesOnLen[n][i];
    if StdJETypes[JT]=Str then
    begin
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

function TJETranslater.LineDivStr: String;
begin
  Result:=';'#13#10;
end;

function TJETranslater.Node2Str(Z: TJENode; IsCommonFunc: Boolean;
  OpRank, PrnRank: Integer): String;
var
  v:Variant;
begin
  if Z=nil then
  begin
    Result:='';
    exit;
  end;
  if Z.ClassType=TJEBranch then
  begin
    if IsCommonFunc then  //只有一个参数的普通函数自带括号了
    begin
      if PrnRank<0 then
        Result:=TransANode(TJEBranch(Z),PrnRank)
      else
        Result:=TransANode(TJEBranch(Z),0)
    end
    else
      Result:=TransANode(TJEBranch(Z),OpRank);
    Gather_BeforeAfter;
  end
  else begin
    Result:=Z.toString;
    if (Result<>'') and (Z.ClassType=_String) then
    begin
      if Result[1]=JEP_StrParamHeader then  //String
        Result:=QuotedStr(Copy(Result,2,MaxInt))
      else begin //Var Name
        if not IsNormalVarName(Result) then  //2010-04-02
          Result:='"'+StringReplace(Result,'"','""',[rfReplaceAll])+'"';
      end;
    end;
  end;
end;

function TJETranslater.NodeNeedVal: Boolean;
begin
  if FCurNodeIdx>=0 then
    Result:=FNodeAy[FCurNodeIdx].NeedVal
  else
    Result:=false;
end;

function TJETranslater.OwnerNeedVal: Boolean;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].NeedVal
  else
    Result:=false;
end;

function TJETranslater.ParentNode: TJENode;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].Node
  else
    Result:=nil;
end;

function TJETranslater.ParentNodePtr: PNodeData;
begin
  if FCurNodeIdx>0 then
    Result:=@FNodeAy[FCurNodeIdx-1]
  else
    Result:=nil;
end;

function TJETranslater.ParentOp: String;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].Op
  else
    Result:=#0;
end;

function TJETranslater.PopupNode: TJENode;
begin
  if FCurNodeIdx>=0 then
  begin
    Result:=FNodeAy[FCurNodeIdx].Node;
    Dec(FCurNodeIdx);
  end
  else
    Result:=nil;
end;

function TJETranslater.PushNode(ANode: TJENode; const AOp: String): Boolean;
begin
  Result:=FCurNodeIdx<MaxJETreeLevel;
  if Result then
  begin
    Inc(FCurNodeIdx);
    with FNodeAy[FCurNodeIdx] do
    begin
      Node:=ANode;
      Op:=AOp;
      NeedVal:=(FCurNodeIdx=0) or FNextNeedValue;
      LeftStr:='';
      PerfixStr:='';
      PostfixStr:='';
    end;
    FNextNeedValue:=false;
  end;
end;

class function TJETranslater.RegisterTranslater(AClass: TJETranslaterClass): Boolean;
var
  mstr:String;
begin
  mstr:=UpperCase(AClass.Language);
  Result:=Translaters.IndexOf(mstr)<0;
  if Result then
    Translaters.AddObject(mstr,TObject(AClass));
end;

procedure TJETranslater.SetCurLeftStr(const Str: String);
begin
  if FCurNodeIdx<0 then exit;
  FNodeAy[FCurNodeIdx].LeftStr:=Str;
end;

procedure TJETranslater.SetCurNeedVal(B: Boolean);
begin
  if FCurNodeIdx<0 then exit;
  FNodeAy[FCurNodeIdx].NeedVal:=B;
end;

procedure TJETranslater.SetIdent(const Value: String);
begin
  FIdent := Value;
end;

procedure TJETranslater.SetNextNeedVal(B: Boolean);
begin
  FNextNeedValue:=B;
end;

procedure TJETranslater.SetParentNoLeft;
begin
  if FCurNodeIdx<1 then exit;
  FNodeAy[FCurNodeIdx-1].NoLeftOutput:=true;
end;

procedure TJETranslater.Set_After(const Str: String);
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
  begin
    if PostfixStr<>'' then
      PostfixStr:=PostfixStr+';'#13#10+Str
    else
      PostfixStr:=Str;
  end;
end;

procedure TJETranslater.Set_Before(const Str: String);
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
  begin
    if PerfixStr<>'' then
      PerfixStr:=PerfixStr+';'#13#10+Str
    else
      PerfixStr:=Str;
  end;
end;

function TJETranslater.TransANode(JObj: TJENode; PrnRank: Integer): String;
begin
  if JObj=nil then exit;
  if JObj.ClassType=TJEBranch then
    Result:=TransNodeOp(TJEBranch(JObj),TJEBranch(JObj).OptString(JEP_Operator),PrnRank)
  else
    Result:=JObj.toString;
end;

function TJETranslater.TransBREAK(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Break);
end;

function TJETranslater.TransDEC(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Dec);
end;

function TJETranslater.TransEVAL(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Eval);
end;

function TJETranslater.TransEXIT(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Exit);
end;

function TJETranslater.TransFOR(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_For);
end;

function TJETranslater.TransIF(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_If);
end;

function TJETranslater.TransIFELSE(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_IfElse);
end;

function TJETranslater.TransINC(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Inc);
end;

function TJETranslater.TransIS(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Is);
end;

function TJETranslater.TransISNULL(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_IsNull);
end;

function TJETranslater.TranslateJETree(JObj: TJENode): String;
begin
  InitNodeAy;
  Result:=TransANode(JObj,0);
  Combine_BeforeAfter(Result);
end;

function TJETranslater.TransFuncOp(JObj: TJEBranch; const Op: String): String;
var
  Func:String;
  IsCommonFunc:Boolean;
  OpRk:Integer;
  i:Integer;
  C1:Char;
  BodyStr:String;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  if Func<>'' then
    C1:=Func[1]
  else
    C1:=#0;
  IsCommonFunc:=(Func='') or not ({(C1 in MathOp2+CompOp2+['.']+MathOp1) or }(Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR'));
  if IsCommonFunc {or (PrnRank<0)} then
    OpRk:=-1
  else
    OpRk:=GetOpRank(Func);
  Z:=JObj.Opt(JEP_Param1);
  Result:=Node2Str(Z,IsCommonFunc,OpRk,0);
  if Func='' then
  begin
    if (Z is TJEBranch) then  //处理括号
      Result:='('+Result+')';
    exit;
  end;
  if (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR') then
  begin
    Result:=Result+' '+Func+' ';
    Z:=JObj.Opt(JEP_Param2);
    if OpRk>=0 then
    begin
      //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
      Inc(OpRk);
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,0);
      Dec(OpRk);
    end
    else
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,0);
    if Func=JEP_BodyDefOp then
    begin
      if Z=nil then
        Result:=Result+'()'
      else if (Z is TJEBranch) and (TJEBranch(Z).OptString(JEP_Operator)=';') then
      begin
        Result:=Result+'('+BodyStr+')'
      end
      else
        Result:=Result+BodyStr;
      exit;
    end;
    Result:=Result+BodyStr;
    if OpRk<0 then
      Result:='('+Result+')';
  end
  else begin
    with JObj do
      for i:=2 to Pred(Length) do
        Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,0);
    Result:='('+Result+')';
    Result:=Func+Result;
  end;
end;

function TJETranslater.TransNodeOp(JObj: TJEBranch; const Op: String; PrnRank: Integer): String;
var
  n:Integer;
  Ch:Char;
begin
  n:=Length(Op);
  if n=0 then
  begin
    Result:=TransCommonOp(JObj,Op,PrnRank);
    exit;
  end;
  Ch:=Op[1];
  PushNode(JObj,Op);
  try
    if Ch in MathOp1+MathOp2+CompOp2+['(','['] then
    begin
      if (n=2) and (Op=JOP_SetValue) then
        Result:=TransSetValue(JObj)
      else
        Result:=TransCommonOp(JObj,Op,PrnRank);
      exit;
    end
    else if Ch=' ' then
    begin
      Result:=TransSpaceOp(JObj,Op);
      exit;
    end;
    if Ch in ['A'..'Z'] then
    begin
      case n of
        2:
        begin
          case Ch of
            'I':
              if Op=JEF_If then
              begin Result:=TransIF(JObj); exit; end
              else if Op=JEF_Is then
              begin Result:=TransIS(JObj); exit; end;
          end;
        end;
        3:
        begin
          case Ch of
            'D':
              if Op=JEF_Dec then begin Result:=TransDEC(JObj); exit; end;
            'F':
              if Op=JEF_For then begin Result:=TransFOR(JObj); exit; end;
            'I':
              if Op=JEF_Inc then begin Result:=TransINC(JObj); exit; end;
          end;
        end;
        4:
        begin
          case Ch of
            'C':
              if Op=JEF_Case then begin Result:=TransCASE(JObj); exit; end;
            'E':
              if Op=JEF_Exit then
              begin Result:=TransEXIT(JObj); exit; end
              else if Op=JEF_Eval then
              begin Result:=TransEVAL(JObj); exit; end;
            'P':
              if Op=JEF_Pred then begin Result:=TransPRED(JObj); exit; end;
            'S':
              if Op=JEF_Succ then begin Result:=TransSUCC(JObj); exit; end;
            'W':
              if Op=JEF_Wait then begin Result:=TransWAIT(JObj); exit; end;
          end;
        end;
        5:
        begin
          case Ch of
            'B':
              if Op=JEF_Break then begin Result:=TransBREAK(JObj); exit; end;
            'T':
              if Op=JEF_Times then begin Result:=TransTIMES(JObj); exit; end;
            'W':
              if Op=JEF_While then begin Result:=TransWHILE(JObj); exit; end;
          end;
        end;
        6:
        begin
          case Ch of
            'I':
              if Op=JEF_IfElse then
              begin Result:=TransIFELSE(JObj); exit; end
              else if Op=JEF_IsNull then
              begin Result:=TransISNULL(JObj); exit; end;
            'R':
              if Op=JEF_Repeat then begin Result:=TransREPEAT(JObj); exit; end
              else if Op=JEF_Return then begin Result:=TransRETURN(JObj); exit; end;
            'W':
              if Op=JEF_While then begin Result:=TransWHILE(JObj); exit; end;
          end;
        end;
      end;
    end;
    Result:=TransFuncOp(JObj,Op);
  finally
    PopupNode;
  end;
end;

function TJETranslater.TransPRED(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Pred);
end;

function TJETranslater.TransREPEAT(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Repeat);
end;

function TJETranslater.TransRETURN(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Return);
end;

function TJETranslater.TransSetValue(JObj: TJEBranch): String;
var
  Func:String;
  OpRk:Integer;
  BodyStr:String;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=JOP_SetValue;
  OpRk:=GetOpRank(Func);
  Z:=JObj.Opt(JEP_Param1);
  Result:=Node2Str(Z,false,OpRk,0);
  if not (Z is TJEBranch)  then
    SetCurLeftStr(Result);
  Result:=Result+Func;
  Z:=JObj.Opt(JEP_Param2);
  //当第二个语句与父节点优先级相同时
  Inc(OpRk);
  SetNextNeedVal(true);
  BodyStr:=Node2Str(Z,false,OpRk,0);
  if FNodeAy[FCurNodeIdx].NoLeftOutput then
    Result:=BodyStr
  else
    Result:=Result+BodyStr;
end;

function TJETranslater.TransSpaceOp(JObj: TJEBranch; const Op: String): String;
var
  Func:String;
  OpRk:Integer;
  i:Integer;
  C1:Char;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  OpRk:=-1;
  //Perfix expression like 'public final function Foo(A,B)'
  if Length(Func)=1 then  //  include 'aaa.inc'  => {op:" ",p1:xx,p2:yy}
  begin
    with JObj do
    begin
      Result:=Node2Str(Opt(JEP_Param1),true,OpRk,0);
      for i:=2 to Length-1 do
        Result:=Result+' '+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),true,OpRk,0);
    end;
    exit;
  end;
  Z:=JObj.Opt(JEP_Perfix);
  if Z is JSONArray then
  begin
    with JSONArray(Z) do
    begin
      for i:=0 to Pred(length) do
        Result:=Result+getString(i)+' ';
    end;
  end;
  Result:=Result+Copy(Func,2,MaxInt)+' ';  //去掉空格
  with JObj do
    Z:=ValObjByIndex[length-1];
  if not (Z is JSONArray) then
    Result:=Result+Node2Str(Z,true,OpRk,0);
end;

function TJETranslater.TransCASE(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Case);
end;

function TJETranslater.TransSUCC(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Succ);
end;

function TJETranslater.TransCommonOp(JObj: TJEBranch; const Op: String; PrnRank: Integer): String;
var
  Func:String;
  IsCommonFunc:Boolean;
  OpRk:Integer;
  i:Integer;
  C1:Char;
  BodyStr:String;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  if Func<>'' then
    C1:=Func[1]
  else
    C1:=#0;
  IsCommonFunc:=(Func='') or not ((C1 in MathOp2+CompOp2+['.']+MathOp1) {or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR')});
  if IsCommonFunc or (PrnRank<0) then
    OpRk:=-1
  else
    OpRk:=GetOpRank(Func);
  Z:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  Result:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank);
  if Func='' then
  begin
    if (Z is TJEBranch) then  //处理括号
      Result:='('+Result+')';
    exit;
  end;
  if (C1 in MathOp2+CompOp2+['.']+MathOp1) {or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR')} then
  begin
    if C1 in MathOp2+CompOp2+['.'] then
    begin
      Result:=Result+Func;
    end
    else //if C1 in MathOp1 then
    begin
      Result:=Func+Result;
      if (PrnRank<0) or (OpRk<PrnRank) then
        Result:='('+Result+')';
      exit;
    end;
    Z:=JObj.Opt(JEP_Param2);
    SetNextNeedVal(true);
    if OpRk>=0 then
    begin
      //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
      Inc(OpRk);
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank);
      Dec(OpRk);
    end
    else
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank);
    if Func=JEP_BodyDefOp then
    begin
      if Z=nil then
        Result:=Result+'()'
      else if (Z is TJEBranch) and (TJEBranch(Z).OptString(JEP_Operator)=';') then
      begin
        Result:=Result+'('+BodyStr+')'
      end
      else
        Result:=Result+BodyStr;
      exit;
    end;
    if C1=OpCh_Sentence then  //语句
    begin
      Result:=Result+#13#10+BodyStr;
      with JObj do
        for i:=3 to Length-1 do
          Result:=Result+Func+#13#10+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank);
    end
    else
      Result:=Result+BodyStr;
    if (PrnRank<0) or (OpRk<PrnRank) then
      Result:='('+Result+')';
  end
  else begin
    if C1='[' then   // A[1]  =>  [,A,1
    begin
      if Z<>CNULL then  //2011-09-25
        Result:=Result+'['
      else
        Result:='[';
      with JObj do
        for i:=2 to Pred(Length) do
        begin
          if i>2 then Result:=Result+',';
          SetNextNeedVal(true);
          Result:=Result+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank);
        end;
      Result:=Result+']';
      exit;
    end
    else begin
      with JObj do
        for i:=2 to Pred(Length) do
        begin
          SetNextNeedVal(true);
          Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank);
        end;
    end;
    Result:='('+Result+')';
    if Func<>'(' then  //集合以 "(" 做为操作符
      Result:=Func+Result;
  end;
end;

function TJETranslater.TransTIMES(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Times);
end;

function TJETranslater.TransWAIT(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Wait);
end;

function TJETranslater.TransWHILE(JObj: TJEBranch): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_While);
end;

function TJETranslater.Trans_Func(JObj: TJEBranch; const Op: String): String;
var
  Func:String;
  OpRk:Integer;
  i:Integer;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  OpRk:=-1;
  Result:=Node2Str(JObj.Opt(JEP_Param1),true,OpRk,0);
  with JObj do
    for i:=2 to Pred(Length) do
      Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),true,OpRk,0);
  Result:=Func+'('+Result+')';
end;

function TJETranslater.Trans_Mid(JObj: TJEBranch; const Op: String;
  Rank, PrnRank: Integer): String;
var
  Func:String;
  i:Integer;
  C1:Char;
  BodyStr:String;
  Z:TJENode;
begin
  Result:='';
  Func:=Op;
  Z:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  Result:=Node2Str(Z,false,Rank,PrnRank);
  if C1 in MathOp2+CompOp2+['.'] then  //like:  P1/P2
    Result:=Result+Func
  else   //like:  P1 mod P2
    Result:=Result+' '+Func+' ';
  Z:=JObj.Opt(JEP_Param2);
  SetNextNeedVal(true);
  if Rank>=0 then
  begin
    //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
    Inc(Rank);
    BodyStr:=Node2Str(Z,false,Rank,PrnRank);
    Dec(Rank);
  end
  else
    BodyStr:=Node2Str(Z,false,Rank,PrnRank);
  Result:=Result+BodyStr;
  if (PrnRank<0) or (Rank<PrnRank) then
    Result:='('+Result+')';
end;

initialization
  InitTypeLenAy;
  Translaters:=TStringList.Create;

finalization
  FreeTypeLenAy;
  Translaters.Free;

end.
