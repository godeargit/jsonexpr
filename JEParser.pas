unit JEParser;

interface

uses
  Classes, uJSON, SysUtils, UJSONExpr;

type
  TTokenKind=(
    tkSPACE,
    tkEOLN,
    tkLINEDIV,
    tkSTRING,
    tkNUMERIC,
    tkOPERATOR,
    tkDELIMITER,
    tkBRACKET,
    tkWORD,
    tkKEYWORD,
    tkVAR,
    tkCONST,
    tkFUNC,
    tkTYPE,
    tkOBJ,
    tkCOMMENT,
    tkStart,
    tkEND
  );
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
  LanFuncHead='=';
  LanFuncDiv='::';  //语言相关的语法或功能如 Basic::SET
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
  JETAG_Keyword=$1000000;
  JETAG_LanFunc=$2000000;
  JETAG_LanObj=$3000000;
  JETAG_LanVal=$4000000;
  JETAG_LanType=$5000000;
  JETAG_LanOp2=$6000000;
  JETAG_LanOp1=$7000000;
  JETAG_MASK=$FF000000;
  JETAG_BODYMASK=$00FFFFFF;
type
  TObjProc=function : Integer of object;
  TJETypes=(
    jetNone, jetVoid,jetAny,jetString,jetInt,jetBool,
    jetLong,jetShort,jetSmall,jetFloat,jetSingle,
    jetDate,jetTime,jetDateTime,jetChar,jetBin,
    jetPointer,jetArray,jetSet,jetObject,jetInterface,
    jetFunction,jetClass,jetRecord,jetUserDef,jetUnknown);
const
  StdJETypes:array [TJETypes] of String=(
    '',
    'VOID', 'ANY', 'STRING', 'INT', 'BOOL',
    'LONG', 'SHORT', 'SMALL', 'FLOAT', 'SINGLE',
    'DATE', 'TIME', 'DATETIME', 'CHAR', 'BIN',
    'POINTER', 'ARRAY', 'SET', 'OBJECT', 'INTERFACE',
    'FUNCTION', 'CLASS', 'RECORD', 'USERDEF', 'UNKNOWN'
  );
type
  TJEVisiblity=(jevPrivate, jevProtected, jevPackage, jevPublic, jevPublished);
  TJEVarMode=(jemVar, jemStatic, jemConst, jemDefine);
  TJETypeRec=record
    BasicType:TJETypes;
    ItemType:TJETypes;
    Visiblity:TJEVisiblity;
    Mode:TJEVarMode;
    SelfIndex:Integer;
    OwnerEnv:Integer;
    RefTo:Integer;
    ItemRefTo:Integer;
  end;
  PJETypeRec=^TJETypeRec;
  TBool3=(b3False, b3True, b3None);
type
  TJEChar=Char;
  TJECharSet=set of TJEChar;
  TJENode=TZAbstractObject;
  TJEBranch=JSONObject;
  TTokenRec=record
    Token:String;
    CaseOKToken:String;
    StartPos:Integer;
    Kind:TTokenKind;
    KWIdx1:Integer;
  end;
  //操作符类型
  TJEOpKind=(
    jokNone,
    jokVal,          //值（常量，变量，字符串，数字等...）
    jokFunc,         //普通函数   alert(msg)
    jokBracketBegin, //括号开始
    jokBracketEnd,   //括号结束
    jokOp,           //运算符（单目、双目等）
    jokStatement,    //语言的语法结构   if A then B else C
    jokSentenceDiv,  //语句分隔
    jokItems         //子项  Class定义中，允许多个成员
  );
  TJEPLevelNode=record
    Obj:TJENode;
    BC:Integer;  //Block depth in eath level.  括号层级
    SC:Integer;  //Statement Count
    Op:String;   //各个层次的操作符  如果是普通函数或者没有操作符，为空
    OpKind: TJEOpKind;
    Rank:Byte;   //操作符优先级
  end;
  TKeywordTag=(ktStatementHead, ktPerfix, ktPostfix, ktRetVal);
  TKeywordTags=set of TKeywordTag;
const
  KeywordTagMasks:array [TKeywordTag] of Integer=($010000,$020000,$040000,$080000);
  KeywordIdxMask=$0000FFFF;
  KeywordTagMask=$00FF0000;
type
  TExtractOption=(eoIncObj, eoIncObjMember);
  TExtractOptions=set of TExtractOption;
  TJEParserClass=class of TJEParser;
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
  TJEParser=class
  private
    procedure SetSource(const Value: String);
    procedure ResizeSPAy(NewSize: Integer);
  protected
    FBlanks:TJECharSet;  //空白
    FLineBreakOp, FLineJoinOp:TJECharSet;  //起分行及连接行的符号  Basic中的 : 及 _
    FVBegin:TJECharSet;  //变量起始符
    FVBody:TJECharSet;
    FCBegin:TJECharSet;  //常量起始符
    FFBegin:TJECharSet;  //函数起始符
    FStrCh:TJECharSet;
    FBrackets:TJECharSet;
    FMathOps:TJECharSet;
    FCompOps:TJECharSet;
    FCommetHC:TJECharSet;   //注释符号的前导字符
    FSpecialHCs:TJECharSet; //可能需要进行特殊处理的前导字符  如Basic的 &H ，C的 0x 等
    FMathStrs:TStrings;
    FCompStrs:TStrings;
    FLineCommet:TStrings;
    FBlockCommet:TStrings;  //Name=Value  Start with name, end with value.
    FKeywords, FHeadKeywords, FLanFuncs, FLanObjs, FLanVals,
    FLanTypes, FLanOp2s, FLanOp1s, FPerfixKWs, FPostfixKWs: TStringList;
    FLanWordsAy: array of TStringList;
    FStatementProcs: array of TObjProc;
    FLineBreakSentence:Boolean;
    FLineBreakCh, FLineJoinCh: TJEChar;
    FSetValOp, FEqualOp, FNotEqualOp: String;
    FSEEqual: Boolean;
    FArrayUseFuncBracket:Boolean;     //是否使用 () 进行访问数组
    FMathOpWithEqualPostfix:Boolean;  //数学运算符可以以等号结尾
    FSingleMathOp:Boolean;            //数学运算符只有1位
    FAddAsStrJoin:Boolean;            //加号可用于字符串连接
    FFuncNameAsResult:Boolean;        //函数的名称可用作返回值
    FCTypeStr,FPascalTypeStr:Boolean;
    FVCBeginEqual,FVFBeginEqual:Boolean;
    FWordNoCase:Boolean;
    FSource: String;
    FCurPos: Integer;
    FCurToken, FLastToken: TTokenRec;
    FLevelNodes: array[0..MaxJETreeLevel] of TJEPLevelNode;
    FDefLevels: TStrings;  //Strings:类型  Objects:Level
    FCurNodeLevel, FCurBlockCount, FStatementLevel: Integer;
    FFuncWithoutBracket: Boolean;
    FUserSymbols: TStringList;
    function RegKeyword(const AWord: String; KWTag: TKeywordTags=[]):Integer;
    { 无返回值的头关键字 }
    function RegHeadKeywordMethod(const AWord: String; AProc: TObjProc):Integer;
    { 有返回值的语句 }
    function RegHeadKeywordRetVal(const AWord: String; AProc: TObjProc):Integer;
    function RegLanFunc(const AWord: String):Integer;
    function RegLanObj(const AWord: String):Integer;
    function RegLanVal(const AWord: String):Integer;
    function RegLanType(const AWord: String):Integer;
    function RegLanOp(const AWord: String; Op2: Boolean=true):Integer;
    procedure InitParser; virtual;
    procedure AfterInitParser; virtual;
    procedure GenLanWordsAy;
    function GetBlanks:TJECharSet; virtual;
    function GetLineBreakOps:TJECharSet; virtual;
    function GetLineJoinOps:TJECharSet; virtual;
    function GetVarBeginCharSet:TJECharSet; virtual;
    function GetVarBodyCharSet:TJECharSet; virtual;
    function GetConstBeginCharSet:TJECharSet; virtual;
    function GetFuncBeginCharSet:TJECharSet; virtual;
    function GetStrChCharSet:TJECharSet; virtual;
    function GetBracketCharSet:TJECharSet; virtual;
    function GetMathOpCharSet:TJECharSet; virtual;
    function GetCompOpCharSet:TJECharSet; virtual;
    function GetSpecialCharSet:TJECharSet; virtual;
    function GetMathStrs:TStrings; virtual;
    function GetCompStrs:TStrings; virtual;
    function GetLineCommetOps:TStrings; virtual;
    function GetBlockCommetStrs:TStrings; virtual;
    function DefaultVisibility:String; virtual;
    procedure SetCurToken(const AStr: String; AKind: TTokenKind; APos: Integer);
    function CheckFuncNoBracket(var ExprLv: Integer):Boolean;
    function CurExprIsArray:Boolean; virtual;
    function CurExprIsNotFunc:Boolean;
    function CurNodeIsFunc:Boolean; virtual;
    function NodeIsNotFunc(ANode: TJENode; BracketLevel: Integer):Boolean; virtual;
    procedure PushString(const Str: String);
    procedure PushNumber(const NumStr: String);
    procedure PushFloat(const F: Double);
    procedure PushInt(const I: Integer);
    procedure PushVar(const VarStr: String);
    procedure PushTypeVal(const ValStr: String; TypeCh: Char=#0);  //十六进制等特殊字符串
    procedure PushOutStrVal(const ValStr: String; AddLnBreak: Boolean=true);  //直接输出的文本串
    procedure PushOp(const Op: String);
    procedure PushOp2(const Op: String); overload;
    procedure PushOp2(const AOp: String; ARank: Byte); overload; //双目运算符
    procedure PushOp1(const Op: String); overload;
    procedure PushOp1(const AOp: String; ARank: Byte); overload; //单目运算符
    procedure PushBracket(const AOp: String);
    procedure PushBracketOp(const Op: String);
    procedure PushNull;
    procedure PushBool(const B: Boolean);
    procedure PushFunc(const AName: String; IsStatement: Boolean=false);
    procedure PushStatement(const S: String);
    procedure PushDefStatement(const S, TypeStr: String);
    { 压入空语素，用于-或++,--等前置算子 }
    procedure PushEmptyItem;
    procedure StatementBegin;
    procedure PushLineBreakOp;
    procedure PushSetValOp;
    procedure PushDefine(const TypeStr: String);
    procedure PopupDefine;
    procedure PushPerfix(const S: String; Lv: Integer=-1);
    { 形成一个 Var := Var + IncVal 的增量算式 }
    procedure MakeVarInc(AVar, IncVal: TJENode);
    procedure MakeVarInc1(AVar: TJENode);
    procedure GoNextParam;
    procedure GoNextStatementParam;
    function MakeArrayParam(const AName: String):Boolean;
    procedure StatementEnd(BreakLine: Boolean=true);
    procedure StatementWithDefEnd;
    function GetOpInfo(var Op: String; out IsOp2: Boolean; out Rank: Byte):Boolean; virtual;
    function GetOpRank(const Op: String):Byte; virtual;
    function GetSetValOpRank:Byte; virtual;
    function GetExprLevel:Integer;
    function GetFuncLevel:Integer;
    function GetBlockLevel:Integer;
    function GetStatementLevel:Integer;
    function InBlockLevel(ARank: Byte=1):Integer;
    function InBlockLevel2(ARank: Byte):Integer;
    procedure AfterPushItem(ItemObj: TJENode);
    procedure AfterPushTypeVal(ItemObj: TJENode; TypeCh: Char);
    function ExpectHeadKW(out TagInt: Integer):Integer;
    function ExpectKW:Integer;
    function ExpectPerfix:Integer;
    { 用于单目运算的关键字 }
    function ExpectOp1KW:Integer;
    function OnSpecialHeadChar(ACh: Char; APos: Integer):Boolean; virtual;
    function OnKeyword(const Str: String; KWIdx: Integer):Integer; virtual;
    function OnStatementHKW(const Str: String; KWIdx: Integer):Integer; virtual;
    function OnLanConst(const Str: String; KWIdx: Integer):Boolean; virtual;
    function OnLanVar(const Str: String; KWIdx: Integer):Boolean; virtual;
    function OnPerfix(const Str: String; KWIdx: Integer):Integer; virtual;
    procedure PrintErr(const Msg: String; TokenLevel: Integer=-1);
    function MakeLanFunc(const AName: String):String;  //语言相关的语法或功能如  "=Basic::SET"
    function MakeBuildInFunc(const AName: String):String;  //语言的内建函数  =::MID
    function VarIsArray:Boolean; virtual;
    function WordIsFunc:Boolean; virtual;
    function WordIsBuildInFunc(const AName: String):Boolean; virtual; abstract;
    { 修改函数名称 }
    function ModifyFuncName(Lv: Integer; const NewName: String):Boolean;
    { 从特定层次析取出赋值表达式的左部 }
    function ExtractVarInSetValueLv(Lv: Integer):TJENode;
    procedure RenameLastNodeAsBody;
    function ParseExpr:Integer; virtual;
    function ParseVar:Integer; virtual;
    function ParseIdentifier:String; virtual;
    function ParseStatements(InOneLine: Boolean=false):Integer; virtual;
    { 尝试解析语句，若无有效语句，可以使用NULL做为占位符（以避免参数表的次序混乱） }
    function TryParseStatements(InOneLine: Boolean=false; ForceFill: Boolean=true):Boolean;
    function ParseDefines:Integer; virtual;
    function ParseParameterDef:Integer; virtual;
    function ParseCBlock:Integer; virtual;  //  {...}
    procedure JumpOverLineBreaks;
    { 将表达式中的指定符号转化为另一个符号 }
    function ReplaceOpInExpr(ANode: TJENode; const SrcOp, DestOp: String):Integer;
    { 如果可能，将表达式中的加号转化为字符串连接符 }
    procedure CheckAndTransAddToJoin(ExprLv: Integer);
    { 提取语句内部的所有变量名,可以限定仅取出首部为特定串的字符 }
    function StatementVarExtract(ANode: TJENode; const VarHead: String='';
      Options: TExtractOptions=[]):TStrings;
    { 替换语句内部指定的变量名——用于Basic这类可以将函数名当成返回值运算的情况 }
    function StatementVarReplace(ANode: TJENode; const SrcVar, DestVar: String;
      Options: TExtractOptions=[]):Integer;
    { 将变量替换为Result，返回值为新名称（如果已存在名为Result的变量，则用已rR开头、JER结尾的名字，并保证为r打头的变量中最长，如rRRRRJER） }
    function ReplaceVarToResult(ANode: TJENode; const VarName: String):String;
    function NodeValIsString(ANode: TJENode):TBool3;
    function StrNodeIsString(const StrVal: String):TBool3;
    { 函数是否返回字符串值 }
    function StrOpResultIsString(const StrVal: String; BranchNode: TJEBranch):TBool3; virtual;
    { 操作符是否是表达式内操作符（如四则运算、逻辑运算符等） }
    function IsExprOp(const OpStr: String; AfterCastToStd: Boolean=true):Boolean; virtual;
    { 对于只有一个标识符的语句，将其转化为方法调用，而不是任其以变量的形式存在 }
    procedure TryTransOneWordStatement;
  private
    FIdent: String;
    FTreeBaseLan: String;
    FTreeBaseLanClass: TJEParserClass;
    FNodeAy: array[0..MaxJETreeLevel] of TNodeData;
    FCurNodeIdx: Integer;
    FTmpVarNum: Integer;
    FNextNeedValue: Boolean;
    FRootNeedValue: Boolean;
    procedure SetIdent(const Value: String);
    function GetIdent: String;
    procedure SetRootNeedValue(const Value: Boolean);
  protected
    property TreeBaseLan:String read FTreeBaseLan;
    property TreeBaseLanClass: TJEParserClass read FTreeBaseLanClass;
    function ParamDefDiv:String; virtual;
    function PushNode(ANode: TJENode; const AOp: String):Boolean;
    function PopupNode:TJENode;
    function CurNode:TJENode;
    function CurNodePtr:PNodeData;
    function ParentNode:TJENode;
    function ParentOp:String;
    function ParentNodePtr:PNodeData;
    function GetCycleLevelNode(LevelCount: Integer=1):TJENode;
    function GetProcLevelNode:TJENode;
    function GetClassLevelNode(AllowProc: Boolean=true):TJENode;
    { 当前定义节点是否正好处于Class的内部(CLASS的MEMBERS数组中) }
    function DefInClass:Boolean;
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
    procedure Gather_BeforeAfter(Ident: Integer);
    procedure Combine_BeforeAfter(var Str: String);
    procedure Combine_SubBeforeAfter(var Str: String; Ident: Integer=0);
    procedure Clear_BeforeAfter;
    procedure InitNodeAy;
    procedure BeforeTransTree; virtual;
    function GetParentLeftExpr(out LStr, VarName: String):PNodeData;
    function GetLanOp(const StdOp: String): String; virtual;
    function GenTempVar:String; virtual;
    function Node2Str(Z: TJENode; IsCommonFunc: Boolean;
      OpRank, PrnRank: Integer; Ident: Integer):String;
    function SingleNode2Str(Z: TJENode; Ident: Integer):String;
    { 返回所在的定义声明，如类、函数、过程等 }
    function GetParentDefStatement(out NodeIdx: Integer):String;
    function SetValOp:String; virtual;
    function LineEndStr:String; virtual;
    function LineDivStr:String; virtual;
    function AddLineEnd(const Str: String):String; virtual;
    function GetJETreePerfix:String; virtual;
    function GetJETreePostfix:String; virtual;
    function GetPerfixStr(JObj: TJEBranch):String; virtual;
    function TransLanStatmentFunc(JObj: TJEBranch; const Lan, Op: String;
      Ident: Integer):String; virtual;
    function TransBuildInFunc(JObj: TJEBranch; const Op: String; Ident: Integer):String; virtual;
    function TransOtherLanBuildInFunc(JObj: TJEBranch; const Op, ALan: String;
      Ident: Integer):String; virtual;
    function TransTypeVal(const ValStr: String; Ident: Integer):String; virtual;
    function TransSetValue(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransINCLUDE(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransIF(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransIIF(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransIS(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransDEC(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransINC(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransFOR(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransFOREACH(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransFORTO(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransCASE(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransECHO(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransEVAL(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransEXIT(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransPRED(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransSUCC(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransWAIT(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransBREAK(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransCONTINUE(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransLOOP(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransNEW(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransWHILE(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransWHILENOT(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransREPEAT(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransRETURN(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransTIMES(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransISNULL(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransPROCEDURE(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransFUNCTION(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransCLASS(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransVAR(JObj: TJEBranch; Ident: Integer):String; virtual;
    function TransCONST(JObj: TJEBranch; Ident: Integer):String; virtual;
    function Trans_OneParamDef(JObj: TJENode):String; virtual;
    function Trans_BodyDef(JObj: TJENode; Ident: Integer):String; virtual;
    function StdMakeBlock(const Str, IdentStr:String; NoHeadIdent: Boolean):String; virtual;
    function StdMakeSetVal(const VarStr, ExprStr: String):String; virtual;
    function StdMakeIfThen(const Expr, IdentStr: String):String; virtual;
    function StdMakeElseIf(const Expr, IdentStr: String):String; virtual;
    function StdMakeElseEnd(IsElse: Boolean; const IdentStr: String):String; virtual;
    function StdTransIFEx(JObj: TJEBranch; Ident: Integer):String;
    function MakeReturn(const ValExpr: String):String; virtual;
    { 获取一维或多维数组的定义文本 如 ay(2,1)  array [0..2,0..1] of Variant }
    function GetArrayDefStr(ANode: TJEBranch; CurDimIdx: Integer=1):String; virtual;
    { 文件名后缀，如 bas,asp,php,cs,js,sql 等 }
    class function GetFilePostfixs:TStrings; virtual;
  public
    class function DefaultIdent:String; virtual;
    { 函数内是否有表示返回值的变量  如Pascal的Result，Basic的函数名 }
    class function HasResultVar:Boolean; virtual;
    { 是否使用上界作为数组元素个数声明 （如Basic）}
    class function ArrayUseUBound:Boolean; virtual;
    { 是否使用逗号分隔多维数组的维度，如  A[10][2][3] }
    class function CommaDivArrayDim:Boolean; virtual;
    property LanOp[const StdOp: String]:String read GetLanOp;
    property Ident:String read GetIdent write SetIdent;
    property RootNeedValue:Boolean read FRootNeedValue write SetRootNeedValue;
    function GetOp(JOp: TJEOperators):String; virtual;
    function IsStdType(Str: String; out JT: TJETypes):Boolean; virtual;
    function StrForLan(const Str: String):String; virtual;
    function VarForLan(const VarName: String):String; virtual;
    function TranslateJETree(JObj: TJENode):String;
    function TransANode(JObj: TJENode; Ident: Integer; PrnRank: Integer=-1):String; virtual;
    { 对节点的值-1后输出 }
    function TransANode_Dec1(JObj: TJENode; Ident: Integer; PrnRank: Integer=-1):String;
    { 对节点的值+1后输出 }
    function TransANode_Inc1(JObj: TJENode; Ident: Integer; PrnRank: Integer=-1):String;
    function TransStatementNode(JObj: TJENode; Ident: Integer; PrnRank: Integer=-1):String;
    function TransNodeOp(JObj: TJEBranch; const Op: String; PrnRank, Ident: Integer):String; virtual;
    function TransCommonOp(JObj: TJEBranch; const Op: String; Ident: Integer; PrnRank: Integer=-1):String; virtual;
    function TransFuncOp(JObj: TJEBranch; const Op: String; Ident: Integer; PrnRank: Integer=-1):String; virtual;
    function TransSpaceOp(JObj: TJEBranch; const Op: String; Ident: Integer):String; virtual;
    // P1 op P2
    function Trans_Mid(JObj: TJEBranch; const Op: String;
      Rank, PrnRank: Integer):String;
    // op(P1,...)
    function Trans_Func(JObj: TJEBranch; const Op: String; Ident: Integer):String;
    // P1 ?= P2
    function Trans_CalcAndSet(JObj: TJEBranch; const CalcOp: String;
      OpRank: Byte):String;
    function Trans_ParamDefs(JObj: TJENode):String;
    function IdentLen(const Ident: Integer):String;
    function AddIdent(const Text: String):String; overload;
    function AddIdent(const Text: String; Ident: Integer):String; overload;
    { 生成带加减值的表达式文本，如 Age+1 }
    function MakeIncDecStr(JObj: TJENode; IncNum: Integer; PrnRank: Integer):String; virtual;
    //Parser...
    property Source:String read FSource write SetSource;
    property CurToken:TTokenRec read FCurToken;
    procedure DoInit; virtual;
    function LastToken:TTokenRec;
    function NextToken:String; //virtual;
    function NextTokenKW:Integer;
    function NextTo(const EndStr: String; ToEnd: Boolean=false; PtrBack: Boolean=false):String;
    class function Lan:ShortString; virtual; abstract;
    class function RegisterParser(AClass: TJEParserClass):Boolean;
    class function GetParserForLan(ALan: String):TJEParserClass;
    class function LanCount:Integer;
    class function Lans(const Index: Integer):TJEParserClass;
    class function StrIsVar(const AString: String):Boolean; {$IFDEF INLINE_OPT}inline;{$ENDIF}
    class function LanHasResultVar(const ALan: String):Boolean;
    function DoParse:TJEBranch; virtual;
    procedure RegGlobalEnv;
    function RegUserSymbol(const AName: String):PJETypeRec;
    function RegUserVar(const AName: String):PJETypeRec;
    function RegUserConst(const AName: String):PJETypeRec;
    function RegUserVarArray(const AName: String):PJETypeRec;
    function RegUserConstArray(const AName: String):PJETypeRec;
    function RegUserFunc(const AName: String):PJETypeRec;
    function RegUserClass(const AName: String):PJETypeRec;
    function SymbolIsArray(const AName: String):Boolean;
    function SymbolIsFunc(const AName: String):Boolean;
    { 判断 XXX 或者 XXX(A)(B) 是否是函数 }
    function SymbolIsNotFunc(const AName: String; BracketCnt: Integer=0):Boolean;
    function SymbolBasicType(const AName: String):TJETypes;
    procedure ClearUserSymbols;
    { 将文本中语言相关的后缀转化为本语言的后缀 }
    procedure TransPostfixInStr(var Text: String; OriLanClass: TJEParserClass;
      DestPostfix: String='');
    function PackSrc(const Source: String; out WordCount: Integer):String; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

const
  Alpha_Number=['0'..'9','A'..'Z','a'..'z','_'];
var
  ErrPrint:TPrintFunc;

function DblQuotedStr(const S: String): String;
function FastPos(const aSourceString,aFindString:String;const aSourceLen,aFindLen,StartPos:Integer):Integer;
function ScanStrParam(const Text: String; S, E: Integer; out ParamVal: String):Integer;
function MakeZeroVal:TJENode;
function NewFuncObj(const AFunc: String): TJEBranch;
function ExtractVarInSetValue(AObj: TJEBranch): TJENode;
function IsConstNumber(ANode: TJENode; out Val: Double):Boolean;
function MakeOpExpr(const OpStr: String; Var1, Var2: TJENode):TJEBranch;

implementation

var
  JETypesOnLen:array [3..9] of array of TJETypes;
  Parsers:TStrings;

function DblQuotedStr(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '"' then Insert('"', Result, I);
  Result := '"' + Result + '"';
end;

function FastPos(const aSourceString,aFindString:String;const aSourceLen,aFindLen,StartPos:Integer):Integer;
var
  SourceLen : integer;
begin
  // Next, we determine how many bytes we need to
  // scan to find the "start" of aFindString.
  SourceLen := aSourceLen;
  SourceLen := SourceLen - aFindLen;
  if (StartPos-1) > SourceLen then begin
    Result := 0;
    Exit;
  end;
  SourceLen := SourceLen - StartPos;
  SourceLen := SourceLen +2;
  asm
    push ESI
    push EDI
    push EBX
    mov EDI, aSourceString
    add EDI, StartPos
    Dec EDI
    mov ESI, aFindString
    mov ECX, SourceLen
    Mov  Al, [ESI]
    @ScaSB:
    Mov  Ah, [EDI]
    cmp  Ah,Al
    jne  @NextChar
    @CompareStrings:
    mov  EBX, aFindLen
    dec  EBX
    Jz @EndOfMatch
    @CompareNext:
    mov  Al, [ESI+EBX]
    mov  Ah, [EDI+EBX]
    cmp  Al, Ah
    Jz   @Matches
    Mov  Al, [ESI]
    Jmp  @NextChar
    @Matches:
    Dec  EBX
    Jnz  @CompareNext
    @EndOfMatch:
    mov  EAX, EDI
    sub  EAX, aSourceString
    inc  EAX
    mov  Result, EAX
    jmp  @TheEnd
    @NextChar:
    Inc  EDI
    dec  ECX
    jnz  @ScaSB
    mov  Result,0
    @TheEnd:
    pop  EBX
    pop  EDI
    pop  ESI
  end;
end;

{
  "con  n.asp  ' "
}
function ScanStrParam(const Text: String; S, E: Integer; out ParamVal: String):Integer;
var
  i:Integer;
  QCh:Char;
  HasQCh:Boolean;
begin
  ParamVal:='';
  Result:=0;
  i:=S;
  while Text[i]=' ' do
  begin
    Inc(i);
    S:=i;
    if i>=E then break;
  end;
  if i>E then exit;
  QCh:=Text[i];
  HasQCh:=QCh in ['''','"'];
  if HasQCh then
  begin
    Inc(i);
    S:=i;
    while i<=E do
    begin
      if Text[i]=QCh then break;
      if Text[i] in [#13,#10] then break;
      if i=E then break;
      Inc(i);
    end;
    ParamVal:=Copy(Text,S,i-S);
    Result:=i;
  end
  else begin
    while i<=E do
    begin
      if Text[i] in [' ',#13,#10] then break;
      if i=E then break;
      Inc(i);
    end;
  end;
  if i<=E then
  begin
    ParamVal:=Copy(Text,S,i-S);
    Result:=i;
  end;
end;

//搜索字符串中最后一个非空白字符
function ScanLastNoneBlankCh(const Text: String):Char;
var
  i:Integer;
begin
  for i:=Length(Text) downto 1 do
  begin
    if Text[i]>' ' then
    begin
      Result:=Text[i];
      exit;
    end;
  end;
  Result:=#0;
end;

function KeywordTagsToMask(Tags: TKeywordTags):Integer;
var
  t:TKeywordTag;
begin
  Result:=0;
  for t:=Low(TKeywordTag) to High(TKeywordTag) do
    if t in Tags then 
      Result:=Result or KeywordTagMasks[t];
end;

function MakeZeroVal:TJENode;
begin
  Result:=_Integer.Create(0);
end;

function MakeOpExpr(const OpStr: String; Var1, Var2: TJENode):TJEBranch;
begin
  if (Var1=nil) or (Var2=nil) then exit;
  Result:=NewFuncObj(OpStr);
  Result.Put(JEP_Param1,Var1);
  Result.Put(JEP_Param2,Var2);
end;

function NewFuncObj(const AFunc: String): TJEBranch;
begin
  Result:=TJEBranch.Create.Put(JEP_Operator,AFunc);
end;

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
  s:=(Integer(High(TJETypes))-Integer(Low(TJETypes))+1)-s;
  if StdJETypes[jetNone]='' then
  begin
    if s<>1 then
      raise Exception.Create('Init TypeLenAy Error!')
  end
  else if s<>0 then
    raise Exception.Create('Init TypeLenAy Error!');
end;

procedure FreeTypeLenAy;
var
  i:Integer;
begin
  for i:=Low(JETypesOnLen) to High(JETypesOnLen) do
    SetLength(JETypesOnLen[i],0);
end;

function ExtractVarInSetValue(AObj: TJEBranch): TJENode;
begin
  Result:=nil;
  with AObj do
  begin
    if OptString(JEP_Operator)<>JOP_SetValue then exit;
    Result:=Opt(JEP_Param1);
  end;  
end;

function IsConstNumber(ANode: TJENode; out Val: Double): Boolean;
begin
  if (ANode is _Integer) then
  begin
    Val:=_Integer(ANode).intValue;
    Result:=true;
  end
  else if (ANode is _Double) then
  begin
    Val:=_Double(ANode).doubleValue;
    Result:=true;
  end
  else
    Result:=false;
end;

function StrMatch(const Str:String; StartPos: Integer; SL: TStrings):Integer;
var
  n:Integer;
begin
  with SL do
    for Result:=0 to Pred(Count) do
    begin
      n:=Length(Strings[Result]);
      if Copy(Str,StartPos,n)=Strings[Result] then exit;      
    end;
end;

function StrMatchName(const Str:String; StartPos: Integer; NameVals: TStrings):Integer;
var
  n:Integer;
  mstr:String;
begin
  with NameVals do
    for Result:=0 to Pred(Count) do
    begin
      mstr:=Names[Result];
      n:=Length(mstr);
      if Copy(Str,StartPos,n)=mstr then exit;      
    end;
  Result:=-1;
end;

function StrMatchValue(const Str:String; StartPos: Integer; NameVals: TStrings):Integer;
var
  n:Integer;
  mstr:String;
begin
  with NameVals do
    for Result:=0 to Pred(Count) do
    begin
      mstr:=ValueFromIndex[Result];
      n:=Length(mstr);
      if Copy(Str,StartPos,n)=mstr then exit;      
    end;
  Result:=-1;
end;

{ TJEParser }

function TJEParser.AddIdent(const Text: String): String;
begin
  Result:=Ident+StringReplace(Text,#13#10,#13#10+Ident,[rfReplaceAll]);
end;

function TJEParser.AddLineEnd(const Str: String): String;
begin
  Result:=LineEndStr;
  if (Str='') or (Result='') then
    Result:=Str
  else if Str[Length(Str)]<>Result then
    Result:=Str+Result
  else
    Result:=Str;
end;

function TJEParser.AddIdent(const Text: String; Ident: Integer): String;
begin
  Result:=IdentLen(Ident)+StringReplace(Text,#13#10,#13#10+IdentLen(Ident),[rfReplaceAll]);
end;

procedure TJEParser.BeforeTransTree;
begin
  FTreeBaseLan:='';
  FTreeBaseLanClass:=nil;
end;

procedure TJEParser.ClearUserSymbols;
var
  i:Integer;
  P:PJETypeRec;
begin
  with FUserSymbols do
  begin
    for i:=0 to Pred(Count) do
    begin
      P:=PJETypeRec(Objects[i]);
      Dispose(P);
    end;
    Clear;
  end;
end;

procedure TJEParser.Clear_BeforeAfter;
begin
  with FNodeAy[FCurNodeIdx] do
  begin
    PerfixStr:='';
    PostfixStr:='';
  end;
end;

procedure TJEParser.Combine_BeforeAfter(var Str: String);
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

procedure TJEParser.Combine_SubBeforeAfter(var Str: String; Ident: Integer);
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  if Idx>MaxJETreeLevel then exit;
  with FNodeAy[Idx+1] do
  begin
    if PerfixStr<>'' then
    begin
      if Ident>0 then
        Str:=AddIdent(PerfixStr,Ident)+LineDivStr+Str
      else
        Str:=PerfixStr+LineDivStr+Str;
    end;
    if PostfixStr<>'' then
    begin
      if Ident>0 then
        Str:=Str+LineDivStr+AddIdent(PostfixStr,Ident)
      else
        Str:=Str+LineDivStr+PostfixStr;
    end;
  end;
end;

class function TJEParser.CommaDivArrayDim: Boolean;
begin
  Result:=true;
end;

constructor TJEParser.Create;
begin
  inherited Create;
  FKeywords:=TStringList.Create;
  FHeadKeywords:=TStringList.Create;
  FLanFuncs:=TStringList.Create;
  FLanObjs:=TStringList.Create;
  FLanVals:=TStringList.Create;
  FLanTypes:=TStringList.Create;
  FLanOp2s:=TStringList.Create;
  FLanOp1s:=TStringList.Create;
  FPerfixKWs:=TStringList.Create;
  FPostfixKWs:=TStringList.Create;
  FUserSymbols:=TStringList.Create;
  InitParser;
  AfterInitParser;
  FRootNeedValue:=true;
end;

function TJEParser.CurExprIsArray: Boolean;
begin
  with FLevelNodes[FCurNodeLevel] do
    Result:=SymbolIsArray(Obj.toString);
end;

function TJEParser.CurExprIsNotFunc: Boolean;
begin
  with FLevelNodes[FCurNodeLevel] do
  begin
    if not (OpKind in [jokNone, jokVal]) then
      Result:=NodeIsNotFunc(Obj,0)
    else
      Result:=SymbolIsNotFunc(Obj.toString);
  end;
end;

function TJEParser.CurNode: TJENode;
begin
  if FCurNodeIdx>=0 then
    Result:=FNodeAy[FCurNodeIdx].Node
  else
    Result:=nil;
end;

function TJEParser.CurNodeIsFunc: Boolean;
begin
  with FLevelNodes[FCurNodeLevel] do
  begin
    if not (Obj is _String) then
      Result:=false
    else
      Result:=SymbolIsFunc(Obj.toString);
  end;
end;

function TJEParser.CurNodePtr: PNodeData;
begin
  if FCurNodeIdx>=0 then
    Result:=@FNodeAy[FCurNodeIdx]
  else
    Result:=nil;
end;

class function TJEParser.DefaultIdent: String;
begin
  Result:='  ';
end;

procedure TJEParser.Gather_BeforeAfter(Ident: Integer);
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
        PostfixStr:=IdentLen(Ident)+PostfixStr+LineDivStr+mstr
      else
        PostfixStr:=mstr;
    if mstr2<>'' then
      if PerfixStr<>'' then
        PerfixStr:=IdentLen(Ident)+PerfixStr+LineDivStr+mstr2
      else
        PerfixStr:=mstr2;
  end;
end;

function TJEParser.GenTempVar: String;
begin
  Inc(FTmpVarNum);
  Result:='tv_'+IntToStr(FCurNodeIdx)+'_'+IntToStr(FTmpVarNum);
end;

function TJEParser.GetClassLevelNode(AllowProc: Boolean): TJENode;
var
  i:Integer;
  mstr:String;
begin
  Result:=nil;
  for i:=FCurNodeIdx downto 0 do
  begin
    mstr:=FNodeAy[i].Op;
    if (mstr='') or (mstr[1]<>' ') then continue;
    if (mstr=' '+JED_Class) or (AllowProc and (mstr=' '+JED_Proc)) then
    begin
      Result:=FNodeAy[i].Node;
      exit;
    end;
  end;
end;

function TJEParser.GetCycleLevelNode(LevelCount: Integer): TJENode;
var
  i:Integer;
  mstr:String;
begin
  Result:=nil;
  for i:=FCurNodeIdx downto 0 do
  begin
    mstr:=FNodeAy[i].Op;
    if (mstr='') then continue;
    if (mstr=JEF_For) or (mstr=JEF_ForTo) or (mstr=JEF_While) or (mstr=JEF_WhileNot)
      or (mstr=JEF_Repeat) or (mstr=JEF_Loop) then
    begin
      Dec(LevelCount);
      if LevelCount=0 then
      begin
        Result:=FNodeAy[i].Node;
        exit;
      end;
    end;
  end;
end;

function TJEParser.GetIdent: String;
begin
  if FIdent='' then
    Result:=DefaultIdent
  else
    Result:=FIdent;
end;

function TJEParser.GetJETreePerfix: String;
begin
  Result:='';
end;

function TJEParser.GetJETreePostfix: String;
begin
  Result:='';
end;

function TJEParser.GetLanOp(const StdOp: String): String;
begin
  Result:=StdOp;
end;

function TJEParser.GetOp(JOp: TJEOperators): String;
begin
  Result:=StdJEOps[JOp];
end;

function TJEParser.GetOpRank(const Op: String): Byte;
begin
  if Op='' then
    Result:=0
  else
    Result:=GetStdOpRank(Op[1],Op);
end;

function TJEParser.GetParentDefStatement(out NodeIdx: Integer): String;
var
  mstr:String;
begin
  NodeIdx:=FCurNodeIdx;
  while NodeIdx>=0 do
  begin
    Result:=FNodeAy[NodeIdx].Op;
    if Copy(Result,1,1)=' ' then exit;  //定义以空格开头
    Dec(NodeIdx);
  end;
  Result:='';
end;

function TJEParser.GetParentLeftExpr(out LStr, VarName: String): PNodeData;
begin
  LStr:='';
  VarName:='';
  Result:=ParentNodePtr;
  if Result<>nil then
  begin
    LStr:=Result^.LeftStr;
    if LStr<>'' then
      SetParentNoLeft
    else begin
      VarName:=GenTempVar;
      LStr:=VarName;
    end;
  end;
end;

function TJEParser.GetPerfixStr(JObj: TJEBranch): String;
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
        mstr:=TransANode(get(i),0);
        if i=0 then
          Result:=mstr
        else
          Result:=Result+' '+mstr;
      end;
  end
  else
    Result:=TransANode(Z,0);
end;

function TJEParser.GetProcLevelNode: TJENode;
var
  i:Integer;
  mstr:String;
begin
  Result:=nil;
  for i:=FCurNodeIdx downto 0 do
  begin
    mstr:=FNodeAy[i].Op;
    if (mstr='') or (mstr[1]<>' ') then continue;
    if (mstr=' '+JED_Func) or (mstr=' '+JED_Proc) then
    begin
      Result:=FNodeAy[i].Node;
      exit;
    end;
  end;
end;

function TJEParser.Get_After: String;
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
    Result:=PostfixStr;
end;

function TJEParser.Get_Before: String;
var
  Idx:Integer;
begin
  Idx:=FCurNodeIdx;
  with FNodeAy[Idx] do
    Result:=PerfixStr;
end;

function TJEParser.IdentLen(const Ident: Integer): String;
begin
  Result:=SpaceStr(Ident*2);
end;

procedure TJEParser.InitNodeAy;
var
  i:Integer;
begin
  with FNodeAy[0] do
  begin
    Node:=nil;
    Op:='';
    NeedVal:=RootNeedValue;
  end;
  FNextNeedValue:=false;
  FCurNodeIdx:=-1;
  FTmpVarNum:=0;
  for i:=1 to High(FNodeAy) do
  begin
    with FNodeAy[i] do
    begin
      if Node=nil then break;
      Node:=nil;
      Op:='';
      NeedVal:=false;
    end;
  end;
end;

function TJEParser.IsExprOp(const OpStr: String; AfterCastToStd: Boolean): Boolean;
var
  n:Integer;
begin
  Result:=false;
  n:=Length(OpStr);
  case n of
    0: exit;
    1:
    begin
      if OpStr=OpCh_Sentence then exit;
      case OpStr[1] of
        '+','-','*','/',
        '%','\',
        '!','~','&','|','^',
        '>','<','=':
          Result:=true;
      end;
    end;
    2:
    begin
      if OpStr=StdJEOps[jeopBoolOr] then
      begin
        Result:=true;
        exit;
      end;
      case OpStr[1] of
        '+','-','*','/',
        '!','~','&','|','^',
        '%','\',
        '>','<','=':
          Result:=true;
      end;
    end;
    3:
    begin
      Result:=(OpStr=StdJEOps[jeopBoolAnd])
        or (OpStr=StdJEOps[jeopBoolXor])
        or (OpStr=StdJEOps[jeopBoolXor]);
      if Result then exit;
    end;
  end;
end;

function TJEParser.IsStdType(Str: String; out JT: TJETypes): Boolean;
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

procedure TJEParser.JumpOverLineBreaks;
begin
  while CurToken.Kind in [tkEOLN,tkLINEDIV] do
    NextToken;
end;

function TJEParser.LineDivStr: String;
begin
  Result:=';'#13#10;
end;

function TJEParser.LineEndStr: String;
begin
  Result:=';';
end;

function TJEParser.Node2Str(Z: TJENode; IsCommonFunc: Boolean;
  OpRank, PrnRank: Integer; Ident: Integer): String;
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
        Result:=TransANode(TJEBranch(Z),Ident,PrnRank)
      else
        Result:=TransANode(TJEBranch(Z),Ident,0)
    end
    else
      Result:=TransANode(TJEBranch(Z),Ident,OpRank);
    Gather_BeforeAfter(Ident);
  end
  else
    Result:=SingleNode2Str(Z,Ident);
end;

function TJEParser.NodeIsNotFunc(ANode: TJENode; BracketLevel: Integer): Boolean;
var
  mstr:String;
begin
  if ANode is TJEBranch then
  begin
    with TJEBranch(ANode) do
    begin
      mstr:=OptString(JEP_Operator);
      if mstr='[' then
        Result:=NodeIsNotFunc(Opt(JEP_Param1),BracketLevel+1)
      else
        Result:=SymbolIsNotFunc(mstr,1); 
    end;
    exit;
  end;
  mstr:=ANode.toString;
  Result:=SymbolIsNotFunc(mstr);
end;

function TJEParser.NodeNeedVal: Boolean;
begin
  if FCurNodeIdx>=0 then
    Result:=FNodeAy[FCurNodeIdx].NeedVal
  else
    Result:=false;
end;

function TJEParser.NodeValIsString(ANode: TJENode): TBool3;
var
  i:Integer;
  ZObj:TJENode;
  OpStr,ValStr:String;
  HasStrVal:Boolean;
begin
  Result:=b3None;
  if ANode is _String then
  begin
    ValStr:=ANode.toString;
    Result:=StrNodeIsString(ValStr);
  end
  else if ANode is TJEBranch then
  begin
    ZObj:=TJEBranch(ANode).Opt(JEP_Operator);
    if ZObj is _String then
    begin
      OpStr:=ZObj.toString;
      if OpStr=JEOP_StrJoin then
      begin
        Result:=b3True;
        exit;
      end;
      if (OpStr<>'+') then
      begin
        Result:=StrOpResultIsString(OpStr,TJEBranch(ANode));
        exit;
      end;
      ZObj:=TJEBranch(ANode).Opt(JEP_Param1);
      Result:=NodeValIsString(ZObj);
      if Result=b3False then exit;
      HasStrVal:=Result=b3True;
      ZObj:=TJEBranch(ANode).Opt(JEP_Param2);
      if ZObj<>nil then
      begin
        Result:=NodeValIsString(ZObj);
        if Result=b3False then exit;
      end;
      if HasStrVal then      
        Result:=b3True;
    end
    else
      exit;
  end;
end;

function TJEParser.OwnerNeedVal: Boolean;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].NeedVal
  else
    Result:=false;
end;

function TJEParser.PackSrc(const Source: String; out WordCount: Integer): String;
begin

end;

function TJEParser.ParamDefDiv: String;
begin
  Result:=';';
end;

function TJEParser.ParentNode: TJENode;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].Node
  else
    Result:=nil;
end;

function TJEParser.ParentNodePtr: PNodeData;
begin
  if FCurNodeIdx>0 then
    Result:=@FNodeAy[FCurNodeIdx-1]
  else
    Result:=nil;
end;

function TJEParser.ParentOp: String;
begin
  if FCurNodeIdx>0 then
    Result:=FNodeAy[FCurNodeIdx-1].Op
  else
    Result:=#0;
end;

function TJEParser.PopupNode: TJENode;
begin
  if FCurNodeIdx>=0 then
  begin
    Result:=FNodeAy[FCurNodeIdx].Node;
    Dec(FCurNodeIdx);
  end
  else
    Result:=nil;
end;

function TJEParser.PushNode(ANode: TJENode; const AOp: String): Boolean;
begin
  Result:=FCurNodeIdx<MaxJETreeLevel;
  if Result then
  begin
    Inc(FCurNodeIdx);
    with FNodeAy[FCurNodeIdx] do
    begin
      Node:=ANode;
      Op:=AOp;
      NeedVal:=((FCurNodeIdx=0) and RootNeedValue) or FNextNeedValue;
      LeftStr:='';
      PerfixStr:='';
      PostfixStr:='';
    end;
    FNextNeedValue:=false;
  end;
end;

procedure TJEParser.SetCurLeftStr(const Str: String);
begin
  if FCurNodeIdx<0 then exit;
  FNodeAy[FCurNodeIdx].LeftStr:=Str;
end;

procedure TJEParser.SetCurNeedVal(B: Boolean);
begin
  if FCurNodeIdx<0 then exit;
  FNodeAy[FCurNodeIdx].NeedVal:=B;
end;

procedure TJEParser.SetIdent(const Value: String);
begin
  FIdent := Value;
end;

procedure TJEParser.SetNextNeedVal(B: Boolean);
begin
  FNextNeedValue:=B;
end;

procedure TJEParser.SetParentNoLeft;
begin
  if FCurNodeIdx<1 then exit;
  FNodeAy[FCurNodeIdx-1].NoLeftOutput:=true;
end;

procedure TJEParser.SetRootNeedValue(const Value: Boolean);
begin
  FRootNeedValue := Value;
end;

function TJEParser.SetValOp: String;
begin
  Result:=JOP_SetValue;
end;

procedure TJEParser.Set_After(const Str: String);
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

procedure TJEParser.Set_Before(const Str: String);
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

function TJEParser.SingleNode2Str(Z: TJENode; Ident: Integer): String;
begin
  Result:=Z.toString;
  if (Result<>'') and (Z.ClassType=_String) then
  begin
    case Result[1] of
      JEP_StrParamHeader:  //String
        Result:=StrForLan(Copy(Result,2,MaxInt));
      JEP_TypeHead:
        Result:=TransTypeVal(Result,0);
      LanFuncHead:  //2012-06-13  考虑内建无参函数的情况，如  =::NOW
      begin
        if Copy(Result,2,Length(LanFuncDiv))=LanFuncDiv then
        begin
          Result:=TransOtherLanBuildInFunc(nil,Copy(Result,Length(LanFuncDiv)+2,MaxInt),FTreeBaseLan,Ident);
        end
        else
          Result:=VarForLan(Result);
      end
      else  //Var Name
        Result:=VarForLan(Result);
    end;
  end;
  if Ident>0 then
    Result:=IdentLen(Ident)+Result;
end;

function TJEParser.StdMakeBlock(const Str, IdentStr: String;
  NoHeadIdent: Boolean): String;
begin
  Result:=Str;
end;

function TJEParser.StdMakeElseEnd(IsElse: Boolean;
  const IdentStr: String): String;
begin
  if IsElse then
    Result:=IdentStr+'else'
  else
    Result:='';
end;

function TJEParser.StdMakeElseIf(const Expr, IdentStr: String): String;
begin
  Result:=IdentStr+'elseif';
end;

function TJEParser.StdMakeIfThen(const Expr, IdentStr: String): String;
begin
  Result:=IdentStr+'if '+Expr+' then';
end;

function TJEParser.StdMakeSetVal(const VarStr, ExprStr: String): String;
begin
  Result:=VarStr+SetValOp+ExprStr+LineEndStr;
end;

function TJEParser.StdTransIFEx(JObj: TJEBranch; Ident: Integer): String;
var
  Str0,Str1,LStr,TmpVar,IdentStr:String;
  MyObj:TJENode;
  P:PNodeData;
  NVal:Boolean;
  i,n,InnerIdent:Integer;
  procedure ProcBody(var Str: String);
  begin
    if Str='' then exit;
    if not (ScanLastNoneBlankCh(Str) in [';','}',#0]) then
      Str:=Str+';';
    if P<>nil then
      Str:=IdentLen(Ident+1)+StdMakeSetVal(LStr,Str);
    Combine_SubBeforeAfter(Str,Ident+1);
    Clear_BeforeAfter;
  end;
begin
  n:=JObj.length-1;
  MyObj:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  Str0:=TransANode(MyObj,0);
  if Str0='' then Str0:='true';
  Gather_BeforeAfter(Ident);
  NVal:=NodeNeedVal;
  //需要IF表达式的返回值  X := IF( ?, A, B )    Y:=IF(?,X+IF(?,1,2),3)+100
  if NVal then
  begin
    P:=GetParentLeftExpr(LStr,TmpVar);
    InnerIdent:=0;  //Ident
  end
  else begin
    P:=nil;
    InnerIdent:=Ident+1;
  end;
  IdentStr:=IdentLen(Ident);
  Result:='';
  i:=1;
  while i<=n do
  begin
    if (i<n) or (i=1) then  //考虑完全没有body的情况: if A then end if
    begin
      SetNextNeedVal(true);
      Str0:=TransANode(JObj.Opt(JEP_ParamHeader+IntToStr(i)),0,0);
      if i=1 then
        Result:=StdMakeIfThen(Str0,IdentStr)
      else
        Result:=Result+StdMakeElseIf(Str0,IdentStr); //#13#10+IdentStr+'}'#13#10+IdentStr+'elseif('+Str0+'){';
      Inc(i);
    end;
    MyObj:=JObj.Opt(JEP_ParamHeader+IntToStr(i));
    SetNextNeedVal(NVal);
    Str1:=TransStatementNode(MyObj,InnerIdent,0);  //2012-06-06  考虑空语句，使用TransStatementNode
    ProcBody(Str1);
    if Str1<>'' then
    begin
      if (i=n) and (i mod 2=1) then
        Result:=Result+StdMakeElseEnd(true,IdentStr)+StdMakeBlock(Str1,IdentStr,true) //#13#10+IdentStr+'}else{'#13#10+Str1
      else
        Result:=Result+StdMakeBlock(Str1,IdentStr,false); //#13#10+Str1;
    end
    else if (i<n) or (i=2) then  //2012-06-08  空Body的情况
    begin
      Result:=Result+StdMakeBlock('',IdentStr,false);
    end;
    Inc(i);
  end;
  Result:=Result+StdMakeElseEnd(false,IdentStr);
  if TmpVar<>'' then
  begin
    Set_Before(Result);
    Result:=TmpVar;
  end;
end;

function TJEParser.StrForLan(const Str: String): String;
begin
  Result:=QuotedStr(Str);
end;

class function TJEParser.StrIsVar(const AString: String): Boolean;
begin
  Result:=not ((AString='') or (AString[1] in [JEP_StrParamHeader,JEP_TypeHead]));
end;

function TJEParser.StrNodeIsString(const StrVal: String): TBool3;
begin
  Result:=b3False;
  if StrVal='' then exit;
  if StrVal[1]=JEP_StrParamHeader then
  begin
    Result:=b3True;
    exit;
  end;
  if StrVal[1]=JEP_TypeHead then exit;
  Result:=b3None;
end;

function TJEParser.StrOpResultIsString(const StrVal: String; BranchNode: TJEBranch): TBool3;
begin
  Result:=b3False;
  case Length(StrVal) of
    1:
    begin
      if StrVal='+' then
        Result:=b3None;
    end;
    2:
    begin
      if StrVal=JEOP_StrJoin then
        Result:=b3True;
    end;
  end;
end;

function TJEParser.SymbolBasicType(const AName: String): TJETypes;
var
  i:Integer;
begin
  Result:=jetNone;
  i:=FUserSymbols.IndexOf(AName);
  if i<0 then exit;
  with PJETypeRec(FUserSymbols.Objects[i])^ do
    Result:=BasicType;
end;

function TJEParser.SymbolIsArray(const AName: String): Boolean;
begin
  Result:=SymbolBasicType(AName)=jetArray;
end;

function TJEParser.SymbolIsFunc(const AName: String): Boolean;
begin
  Result:=Copy(AName,1,Length(LanFuncHead))=LanFuncHead;  //2012-05-28
  if Result then exit;
  Result:=SymbolBasicType(AName)=jetFunction;
end;

function TJEParser.SymbolIsNotFunc(const AName: String; BracketCnt: Integer): Boolean;
begin
  Result:=Copy(AName,1,Length(LanFuncHead))<>LanFuncHead;
  if not Result then exit;  
  Result:=not(SymbolBasicType(AName) in [jetNone,jetFunction]);  
end;

function TJEParser.TransANode(JObj: TJENode; Ident, PrnRank: Integer): String;
var
  i:Integer;
begin
  Result:='';
  if JObj=nil then exit;
  if JObj.ClassType=TJEBranch then
    Result:=TransNodeOp(TJEBranch(JObj),TJEBranch(JObj).OptString(JEP_Operator),PrnRank,Ident)
  else if JObj is JSONArray then
  begin
    Result:='';
    with JSONArray(JObj) do
      for i:=0 to Pred(length) do
      begin
        if i>0 then
          Result:=Result+#13#10;
        Result:=Result+TransANode(get(i),Ident);
      end;
  end
  else
    Result:=SingleNode2Str(JObj,Ident);
end;

function TJEParser.TransANode_Dec1(JObj: TJENode; Ident,
  PrnRank: Integer): String;
begin
  if JObj=nil then
  begin
    Result:='';
    exit;
  end;
  if JObj is _Number then
  begin
    Result:=JObj.toString;
    try
      Result:=IntToStr(StrToInt(Result)-1);
    except
      Result:=FloatToStr(StrToFloat(Result)-1);
    end;
  end
  else
    Result:=MakeIncDecStr(JObj,-1,PrnRank);
  if Ident>0 then
    Result:=IdentLen(Ident)+Result;
end;

function TJEParser.TransANode_Inc1(JObj: TJENode; Ident,
  PrnRank: Integer): String;
begin
  if JObj=nil then
  begin
    Result:='';
    exit;
  end;
  if JObj is _Number then
  begin
    Result:=JObj.toString;
    try
      Result:=IntToStr(StrToInt(Result)+1);
    except
      Result:=FloatToStr(StrToFloat(Result)+1);
    end;
  end
  else
    Result:=MakeIncDecStr(JObj,1,PrnRank);
  if Ident>0 then
    Result:=IdentLen(Ident)+Result;
end;

function TJEParser.TransBREAK(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Break,Ident);
end;

function TJEParser.TransBuildInFunc(JObj: TJEBranch; const Op: String; Ident: Integer): String;
begin
  if (FTreeBaseLan='') or (FTreeBaseLan=Self.Lan) then
    Result:=TransFuncOp(TJEBranch(JObj),Op,Ident)
  else
    Result:=TransOtherLanBuildInFunc(TJEBranch(JObj),Op,FTreeBaseLan,Ident);//TransFuncOp(TJEBranch(JObj),FTreeBaseLan+'__'+Op,Ident)
end;

function TJEParser.TransDEC(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Dec,Ident);
end;

function TJEParser.TransECHO(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Echo,Ident);
end;

function TJEParser.TransEVAL(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Eval,Ident);
end;

function TJEParser.TransEXIT(JObj: TJEBranch; Ident: Integer): String;
var
  lv:Integer;
begin
  //有返回值变量的函数内部exit需要特殊处理
  if FTreeBaseLanClass.HasResultVar and not Self.HasResultVar and (GetParentDefStatement(Lv)=' '+JED_Func) then
  begin
    Result:=IdentLen(Ident)+MakeReturn(VarForLan(JEV_ResultRep));
  end
  else
    Result:=TransFuncOp(TJEBranch(JObj),JEF_Exit,Ident);
end;

function TJEParser.TransFOR(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_For,Ident);
end;

function TJEParser.TransFOREACH(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_ForEach,Ident);
end;

function TJEParser.TransFORTO(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_ForTo,Ident);
end;

function TJEParser.TransIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_If,Ident);
end;

function TJEParser.TransIIF(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_IIf,Ident);
end;

function TJEParser.TransINC(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Inc,Ident);
end;

function TJEParser.TransINCLUDE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Include,Ident);
end;

function TJEParser.TransIS(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Is,Ident);
end;

function TJEParser.TransISNULL(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_IsNull,Ident);
end;

function TJEParser.TransLanStatmentFunc(JObj: TJEBranch; const Lan, Op: String;
  Ident: Integer): String;
begin
  Result:=IdentLen(Ident)+Op+' '+Node2Str(JObj.Opt(JEP_Param1),false,-1,0,0);
end;

function TJEParser.TranslateJETree(JObj: TJENode): String;
var
  mstr:String;
  LanNode:TJENode;
begin
  InitNodeAy;
  BeforeTransTree;
  if JObj is TJEBranch then
  begin
    LanNode:=TJEBranch(JObj).Remove(JEP_LanDef);
    if LanNode<>nil then
    begin
      FTreeBaseLan:=LanNode.toString;
      FTreeBaseLanClass:=GetParserForLan(FTreeBaseLan);
      LanNode.Free;
    end;
  end;
  if FTreeBaseLanClass=nil then FTreeBaseLanClass:=TJEParserClass(Self.ClassType);
  Result:=TransANode(JObj,0,0);
  Combine_BeforeAfter(Result);
  mstr:=GetJETreePerfix;
  if mstr<>'' then
    Result:=mstr+Result;
  mstr:=GetJETreePostfix;
  if mstr<>'' then
    Result:=Result+mstr;
end;

function TJEParser.TransLOOP(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_LOOP,Ident);
end;

function TJEParser.TransFuncOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
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
  IsCommonFunc:=(Func='') or not ((Func='AND') or (Func='OR') or (Func='IN') or (Func='IS')
    or (Func='XOR') or (Func='NOT'));
  if IsCommonFunc {or (PrnRank<0)} then
    OpRk:=-1
  else
    OpRk:=GetOpRank(Func);
  Z:=JObj.Opt(JEP_Param1);
  Result:=Node2Str(Z,IsCommonFunc,OpRk,0,0);
  if Func='' then
  begin
    if (Z is TJEBranch) then  //处理括号
      Result:=IdentLen(Ident)+'('+Result+')';
    exit;
  end;
  if (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR') then
  begin
    Result:=Result+' '+LanOp[Func]+' ';
    Z:=JObj.Opt(JEP_Param2);
    if OpRk>=0 then
    begin
      //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
      Inc(OpRk);
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,0,Ident);
      Dec(OpRk);
    end
    else
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,0,Ident);
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
    if OpRk<PrnRank then
      Result:='('+Result+')';
    if Ident>0 then
      Result:=IdentLen(Ident)+Result;
  end
  else if Func='NOT' then
  begin
    Result:=LanOp[Func]+' '+Result;
    if Ident>0 then
      Result:=IdentLen(Ident)+Result;
  end
  else begin
    with JObj do
      for i:=2 to Pred(Length) do
        Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,0,0);
    Result:='('+Result+')';
    Result:=LanOp[Func]+Result;
    if Ident>0 then
      Result:=IdentLen(Ident)+Result;
  end;
end;

function TJEParser.TransFUNCTION(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JED_Func,Ident);
end;

function TJEParser.TransNEW(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=IdentLen(Ident)+JEF_New+' '+TransANode(JObj.Opt(JEP_Param1),0);
end;

function TJEParser.TransNodeOp(JObj: TJEBranch; const Op: String;
  PrnRank, Ident: Integer): String;
var
  n,i:Integer;
  Ch:Char;
  DefStr:String;
begin
  n:=Length(Op);
  if n=0 then
  begin
    Result:=TransCommonOp(JObj,Op,Ident,PrnRank);
    exit;
  end;
  Ch:=Op[1];
  PushNode(JObj,Op);
  try
    if Ch in MathOp1+MathOp2+CompOp2+['(','['] then
    begin
      if (Ch=LanFuncHead) and (n>=4) then
      begin
        //eg:  =Basic::SET  =::MID
        i:=Pos(LanFuncDiv,Op);
        if i>0 then
        begin
          if i=2 then
            Result:=TransBuildInFunc(JObj,Copy(Op,i+2,MaxInt),Ident)
          else
            Result:=TransLanStatmentFunc(JObj,Copy(Op,2,i-2),Copy(Op,i+2,MaxInt),Ident);
          exit;
        end;
      end;
      if (n=2) and (Op=JOP_SetValue) then
        Result:=TransSetValue(JObj,Ident)
      else
        Result:=TransCommonOp(JObj,Op,Ident,PrnRank);
      exit;
    end
    else if Ch=JED_HeadChar then
    begin
      if n>2 then
      begin
        DefStr:=Copy(Op,2,MaxInt);
        Ch:=DefStr[1];
        Dec(n);
        case n of
          3:
          begin
            case Ch of
              'V':
                if DefStr=JED_Var then begin Result:=TransVAR(JObj,Ident); exit; end;
            end;
          end;
          5:
          begin
            case Ch of
              'C':
                if DefStr=JED_Const then begin Result:=TransCONST(JObj,Ident); exit; end
                else if DefStr=JED_Class then begin Result:=TransCLASS(JObj,Ident); exit; end
            end;
          end;
          8:
          begin
            case Ch of
              'F':
                if DefStr=JED_Func then begin Result:=TransFUNCTION(JObj,Ident); exit; end;
            end;
          end;
          9:
          begin
            case Ch of
              'P':
                if DefStr=JED_Proc then begin Result:=TransPROCEDURE(JObj,Ident); exit; end;
            end;
          end;
        end;
      end;
      Result:=TransSpaceOp(JObj,Op,Ident);
      exit;
    end;
    if Ch in ['A'..'Z'] then
    begin
      if n<=4 then
      begin
        case n of
          2:
          begin
            case Ch of
              'I':
                if Op=JEF_If then
                begin Result:=TransIF(JObj,Ident); exit; end
                else if Op=JEF_Is then
                begin Result:=TransIS(JObj,Ident); exit; end;
            end;
          end;
          3:
          begin
            case Ch of
              'D':
                if Op=JEF_Dec then begin Result:=TransDEC(JObj,Ident); exit; end;
              'F':
                if Op=JEF_For then begin Result:=TransFOR(JObj,Ident); exit; end;
              'I':
                if Op=JEF_Inc then begin Result:=TransINC(JObj,Ident); exit; end
                else if Op=JEF_IIF then begin Result:=TransIIF(JObj,Ident); exit; end;
              'N':
                if Op=JEF_New then begin Result:=TransNEW(JObj,Ident); exit; end;
            end;
          end;
          4:
          begin
            case Ch of
              'C':
                if Op=JEF_Case then begin Result:=TransCASE(JObj,Ident); exit; end;
              'E':
                if Op=JEF_Exit then
                begin Result:=TransEXIT(JObj,Ident); exit; end
                else if Op=JEF_Echo then
                begin Result:=TransECHO(JObj,Ident); exit; end
                else if Op=JEF_Eval then
                begin Result:=TransEVAL(JObj,Ident); exit; end;
              'L':
                if Op=JEF_Loop then begin Result:=TransLOOP(JObj,Ident); exit; end;
              'P':
                if Op=JEF_Pred then begin Result:=TransPRED(JObj,Ident); exit; end;
              'S':
                if Op=JEF_Succ then begin Result:=TransSUCC(JObj,Ident); exit; end;
              'W':
                if Op=JEF_Wait then begin Result:=TransWAIT(JObj,Ident); exit; end;
            end;
          end;
        end;
      end
      else begin
        case n of
          5:
          begin
            case Ch of
              'B':
                if Op=JEF_Break then begin Result:=TransBREAK(JObj,Ident); exit; end;
              'F':
                if Op=JEF_ForTo then begin Result:=TransFORTO(JObj,Ident); exit; end;
              'T':
                if Op=JEF_Times then begin Result:=TransTIMES(JObj,Ident); exit; end;
              'W':
                if Op=JEF_While then begin Result:=TransWHILE(JObj,Ident); exit; end;
            end;
          end;
          6:
          begin
            case Ch of
              'I':
                {if Op=JEF_IfElse then
                begin Result:=TransIFELSE(JObj,Ident); exit; end
                else }
                if Op=JEF_IsNull then
                begin Result:=TransISNULL(JObj,Ident); exit; end;
              'R':
                if Op=JEF_Repeat then begin Result:=TransREPEAT(JObj,Ident); exit; end
                else if Op=JEF_Return then begin Result:=TransRETURN(JObj,Ident); exit; end;
              'W':
                if Op=JEF_While then begin Result:=TransWHILE(JObj,Ident); exit; end;
            end;
          end;
          7:
          begin
            case Ch of
              'F':
                if Op=JEF_ForEach then begin Result:=TransFOREACH(JObj,Ident); exit; end;
              'I':
                if Op=JEF_Include then begin Result:=TransINCLUDE(JObj,Ident); exit; end;
            end;
          end;
          8:
          begin
            case Ch of
              'W':
                if Op=JEF_WhileNot then begin Result:=TransWHILENOT(JObj,Ident); exit; end;
            end;
          end;
        end;
      end;
    end;
    Result:=TransFuncOp(JObj,Op,Ident,PrnRank);
  finally
    PopupNode;
  end;
end;

function TJEParser.TransOtherLanBuildInFunc(JObj: TJEBranch; const Op,
  ALan: String; Ident: Integer): String;
begin
  if JObj=nil then
    Result:=IdentLen(Ident)+ALan+'__'+Op+'()'
  else
    Result:=TransFuncOp(TJEBranch(JObj),ALan+'__'+Op,Ident);
end;

procedure TJEParser.TransPostfixInStr(var Text: String;
  OriLanClass: TJEParserClass; DestPostfix: String);
var
  Oris:TStrings;
  i:Integer;
begin
  if OriLanClass=nil then exit;
  if DestPostfix='' then
  begin
    //默认取第一个postfix
    with GetFilePostfixs do
    begin
      if Count>0 then
        DestPostfix:=Strings[0];
      Free;
    end;
    if DestPostfix='' then exit;
  end;
  Oris:=OriLanClass.GetFilePostfixs;
  for i:=0 to Pred(Oris.Count) do
  begin
    Text:=StringReplace(Text,'.'+Oris[i],'.'+DestPostfix,[rfIgnoreCase,rfReplaceAll]);
  end;
  Oris.Free;  
end;

function TJEParser.TransPRED(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Pred,Ident);
end;

function TJEParser.TransPROCEDURE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JED_Proc,Ident);
end;

function TJEParser.TransREPEAT(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Repeat,Ident);
end;

function TJEParser.TransRETURN(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Return,Ident);
end;

function TJEParser.TransSetValue(JObj: TJEBranch; Ident: Integer): String;
var
  Func:String;
  OpRk:Integer;
  VarStr,ExprStr:String;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=SetValOp;
  OpRk:=GetOpRank(Func);
  Z:=JObj.Opt(JEP_Param1);
  VarStr:=Node2Str(Z,false,OpRk,0,Ident);
  if not (Z is TJEBranch)  then
    SetCurLeftStr(VarStr);
  Z:=JObj.Opt(JEP_Param2);
  //当第二个语句与父节点优先级相同时
  Inc(OpRk);
  SetNextNeedVal(true);
  ExprStr:=Node2Str(Z,false,OpRk,0,0);
  if FNodeAy[FCurNodeIdx].NoLeftOutput then
    Result:=AddLineEnd(ExprStr)
  else
    Result:=StdMakeSetVal(VarStr,ExprStr);
end;

function TJEParser.TransSpaceOp(JObj: TJEBranch; const Op: String; Ident: Integer): String;
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
      Result:=Node2Str(Opt(JEP_Param1),true,OpRk,0,Ident);
      for i:=2 to Length-1 do
        Result:=Result+' '+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),true,OpRk,0,Ident);
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
    Result:=Result+Node2Str(Z,true,OpRk,0,Ident);
end;

function TJEParser.TransStatementNode(JObj: TJENode; Ident, PrnRank: Integer): String;
begin
  if (JObj=nil) or (JObj=CNULL) then  //2012-06-06  CNULL可以代表空语句
    Result:=''
  else
    Result:=TransANode(JObj,Ident,PrnRank);
end;

function TJEParser.TransCASE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Case,Ident);
end;

function TJEParser.TransCLASS(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JED_Class,Ident);
end;

function TJEParser.TransSUCC(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Succ,Ident);
end;

function TJEParser.TransCommonOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
var
  Func:String;
  IsCommonFunc,IsSentence:Boolean;
  OpRk:Integer;
  i,n:Integer;
  C1:Char;
  BodyStr,ADivStr:String;
  Z:TJENode;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  if Func<>'' then
  begin
    C1:=Func[1];
    IsSentence:=C1=OpCh_Sentence;
  end
  else begin
    C1:=#0;
    IsSentence:=false;
  end;
  IsCommonFunc:=(Func='') or not ((C1 in MathOp2+CompOp2+['.']+MathOp1));
  if IsCommonFunc or (PrnRank<0) then
    OpRk:=-1
  else
    OpRk:=GetOpRank(Func);
  if IsSentence then  //串行语句  A; B; ...
  begin
    with JObj do
    begin
      n:=Length-1;
      for i:=1 to n do
      begin
        SetNextNeedVal((i=n) and NodeNeedVal);
        BodyStr:=Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank,Ident);
        Combine_BeforeAfter(BodyStr);
        Clear_BeforeAfter;
        if LineEndStr<>'' then
          BodyStr:=AddLineEnd(BodyStr);
        if i=1 then
          Result:=BodyStr
        else
          Result:=Result+LineDivStr+BodyStr;
      end;
    end;
    exit;
  end;
  Z:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  Result:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank,0);
  if C1=#0 then
  begin
    if (Z is TJEBranch) then  //处理括号
      Result:=IdentLen(Ident)+'('+Result+')';
    exit;
  end;
  if (C1 in MathOp2+CompOp2+['.']+MathOp1) {or (Func='AND') or (Func='OR') or (Func='IN') or (Func='IS') or (Func='XOR')} then
  begin
    if C1 in MathOp2+CompOp2+['.'] then
    begin
      Result:=Result+LanOp[Func];
    end
    else //if C1 in MathOp1 then
    begin
      Result:=LanOp[Func]+Result;
      if (PrnRank<0) or (OpRk<PrnRank) then
        Result:=IdentLen(Ident)+'('+Result+')';
      exit;
    end;
    Z:=JObj.Opt(JEP_Param2);
    SetNextNeedVal(true);
    if OpRk>=0 then
    begin
      //当第二个语句与父节点优先级相同时，应当使用括号 -- 主动提高父节点优先级
      Inc(OpRk);
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank,0);
      Dec(OpRk);
    end
    else
      BodyStr:=Node2Str(Z,IsCommonFunc,OpRk,PrnRank,0);
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
      Result:=IdentLen(Ident)+Result;
      exit;
    end;
    Result:=Result+BodyStr;
    if OpRk<PrnRank then
      Result:='('+Result+')';
    if Ident>0 then
      Result:=IdentLen(Ident)+Result;
  end
  else begin
    if C1='[' then   // A[1]  =>  [,A,1
    begin
      if Z<>CNULL then  //2011-09-25
      begin
        if FArrayUseFuncBracket then
          Result:=Result+'('
        else
          Result:=Result+'[';
      end
      else if FArrayUseFuncBracket then
        Result:='('
      else
        Result:='[';
      //2012-06-11  多维数组的分隔符
      if CommaDivArrayDim then
        ADivStr:=','
      else begin
        if FArrayUseFuncBracket then
          ADivStr:=')('
        else
          ADivStr:=']['
      end;
      with JObj do
        for i:=2 to Pred(Length) do
        begin
          if i>2 then Result:=Result+ADivStr;
          SetNextNeedVal(true);
          Result:=Result+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank,0);
        end;
      if FArrayUseFuncBracket then
        Result:=Result+')'
      else
        Result:=Result+']';
      if Ident>0 then Result:=IdentLen(Ident)+Result;
      exit;
    end
    else begin
      with JObj do
        for i:=2 to Pred(Length) do
        begin
          SetNextNeedVal(true);
          Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),IsCommonFunc,OpRk,PrnRank,0);
        end;
    end;
    Result:='('+Result+')';
    if Func<>'(' then  //集合以 "(" 做为操作符
      Result:=LanOp[Func]+Result;
    if Ident>0 then Result:=IdentLen(Ident)+Result;
  end;
end;

function TJEParser.TransCONST(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JED_Const,Ident);
end;

function TJEParser.TransCONTINUE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Continue,Ident);
end;

function TJEParser.TransTIMES(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Times,Ident);
end;

function TJEParser.TransTypeVal(const ValStr: String; Ident: Integer): String;
begin
  Result:=ValStr;
end;

function TJEParser.TransVAR(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JED_Var,Ident);
end;

function TJEParser.TransWAIT(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_Wait,Ident);
end;

function TJEParser.TransWHILE(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_While,Ident);
end;

function TJEParser.TransWHILENOT(JObj: TJEBranch; Ident: Integer): String;
begin
  Result:=TransFuncOp(TJEBranch(JObj),JEF_WhileNot,Ident);
end;

function TJEParser.Trans_BodyDef(JObj: TJENode; Ident: Integer): String;
begin
  Result:=TransANode(JObj,Ident);
end;

function TJEParser.Trans_CalcAndSet(JObj: TJEBranch;
  const CalcOp: String; OpRank: Byte): String;
var
  Func:String;
  i:Integer;
  C1:Char;
  VarStr,BodyStr:String;
  Z:TJENode;
begin
  Result:='';
  Func:=CalcOp;
  Z:=JObj.Opt(JEP_Param1);
  SetNextNeedVal(true);
  VarStr:=Node2Str(Z,false,0,0,0);
  if (Func='') or (Func[1] in MathOp2+CompOp2+['.']) then  //like:  P1/P2
    Result:=Func
  else   //like:  P1 mod P2
    Result:=' '+Func+' ';
  Z:=JObj.Opt(JEP_Param2);
  SetNextNeedVal(true);
  BodyStr:=Node2Str(Z,false,OpRank+1,OpRank,0);
  Result:=VarStr+GetOp(jeopSetValue)+VarStr+Result+BodyStr;
end;

function TJEParser.Trans_Func(JObj: TJEBranch; const Op: String;
  Ident: Integer): String;
var
  Func:String;
  OpRk:Integer;
  i:Integer;
begin
  Result:='';
  if JObj=nil then exit;
  Func:=Op;
  OpRk:=-1;
  Result:=Node2Str(JObj.Opt(JEP_Param1),true,OpRk,0,0);
  with JObj do
    for i:=2 to Pred(Length) do
      Result:=Result+','+Node2Str(Opt(JEP_ParamHeader+IntToStr(i)),true,OpRk,0,0);
  Result:=Func+'('+Result+')';
end;

function TJEParser.Trans_Mid(JObj: TJEBranch; const Op: String;
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
  Result:=Node2Str(Z,false,Rank,PrnRank,0);
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
    BodyStr:=Node2Str(Z,false,Rank,PrnRank,0);
    Dec(Rank);
  end
  else
    BodyStr:=Node2Str(Z,false,Rank,PrnRank,0);
  Result:=Result+BodyStr;
  if (PrnRank<0) or (Rank<PrnRank) then
    Result:='('+Result+')';
end;

function TJEParser.Trans_OneParamDef(JObj: TJENode): String;
begin
  Result:=TransANode(JObj,0);
end;

function TJEParser.Trans_ParamDefs(JObj: TJENode): String;
var
  i:Integer;
begin
  if JObj is JSONArray then
  begin
    Result:='';
    with JSONArray(JObj) do
      for i:=0 to Pred(length) do
      begin
        if i=0 then
          Result:=Trans_OneParamDef(get(i))
        else
          Result:=Result+ParamDefDiv+Trans_OneParamDef(get(i));
      end;
  end
  else
    Result:=Trans_OneParamDef(JObj);
end;

function TJEParser.TryParseStatements(InOneLine, ForceFill: Boolean): Boolean;
begin
  Result:=ParseStatements(InOneLine)>=0;
  if Result then exit;
  PushNull;
end;

procedure TJEParser.TryTransOneWordStatement;
var
  JN,JP:TJEBranch;
  Z:TJENode;
  mstr:String;
  PrnOPK:TJEOpKind;
begin
  if FCurNodeLevel<=0 then exit;  
  //if A then B  -- B should be B()
  //  ,pN:"B" => ,pN:{op:"B"}
  with FLevelNodes[FCurNodeLevel] do
  begin
    if not (OpKind in [jokNone,jokVal]) then exit;
    PrnOPK:=FLevelNodes[FCurNodeLevel-1].OpKind;
    if PrnOPK in [jokOp,jokFunc] then exit;
    if PrnOPK=jokStatement then
    begin
      //对var,const等定义型的Statement，不必转换
      if Copy(FLevelNodes[FCurNodeLevel-1].Op,1,1)=JED_HeadChar then
      begin
        mstr:=Copy(FLevelNodes[FCurNodeLevel-1].Op,2,MaxInt);
        if (mstr=JED_Var) or (mstr=JED_Const) then
          exit;
      end;
      //2012-06-08 空语句的情况，不做处理
      if Obj=CNull then exit;      
    end;
    JN:=TJEBranch.Create;
    JP:=TJEBranch(FLevelNodes[FCurNodeLevel-1].Obj);
    with JP do
    begin
      mstr:=JEP_ParamHeader+IntToStr(length-1);
      Z:=Remove(mstr);
      JN.Put(JEP_Operator,Z);
      Put(mstr,JN);
      //2012-08-03  处理无外层操作符的简单语句
      if OptString(JEP_Operator)='' then
      begin
        Put(JEP_Operator,OpCh_Sentence);
        with FLevelNodes[FCurNodeLevel-1] do
        begin
          Op:=OpCh_Sentence;
          OpKind:=jokSentenceDiv;
        end;
      end;
    end;
    Obj:=JN;
    Op:=Z.toString;
    OpKind:=jokFunc;
  end;
end;

function TJEParser.VarForLan(const VarName: String): String;
begin
  if not IsNormalVarName(VarName) then  //2010-04-02
    Result:='"'+StringReplace(VarName,'"','""',[rfReplaceAll])+'"'
  else
    Result:=VarName;
end;

procedure TJEParser.AfterInitParser;
var
  i:Integer;
  ch:TJEChar;
begin
  FCommetHC:=[];
  with FLineCommet do
    for i:=0 to Pred(Count) do
      Include(FCommetHC,Strings[i][1]);
  with FBlockCommet do
    for i:=0 to Pred(Count) do
      Include(FCommetHC,Strings[i][1]);
  FHeadKeywords.CaseSensitive:=not FWordNoCase;
  FHeadKeywords.Sorted:=true;
  FSEEqual:=(FSetValOp=FEqualOp);
  GenLanWordsAy;
end;

procedure TJEParser.AfterPushItem(ItemObj: TJENode);
begin
  with FLevelNodes[FCurNodeLevel] do
  begin
    Obj:=ItemObj;
    BC:=FCurBlockCount;
    SC:=FStatementLevel;
    OpKind:=jokNone;
    Rank:=0;
  end;
end;

procedure TJEParser.AfterPushTypeVal(ItemObj: TJENode; TypeCh: Char);
begin
  with FLevelNodes[FCurNodeLevel] do
  begin
    Obj:=ItemObj;
    BC:=FCurBlockCount;
    SC:=FStatementLevel;
    if TypeCh=JEPT_EchoStr then
      OpKind:=jokFunc
    else
      OpKind:=jokVal;
    Rank:=0;
  end;
end;

class function TJEParser.ArrayUseUBound: Boolean;
begin
  Result:=false;
end;

procedure TJEParser.CheckAndTransAddToJoin(ExprLv: Integer);
var
  ZObj:TJENode;
begin
  if not FAddAsStrJoin or (ExprLv<0) then exit;
  ZObj:=FLevelNodes[ExprLv].Obj;
  if Self.NodeValIsString(ZObj)<>b3True then exit;
  ReplaceOpInExpr(ZObj,'+',JEOP_StrJoin);
end;

function TJEParser.CheckFuncNoBracket(var ExprLv: Integer): Boolean;
var
  Node:TJENode;
  mstr,FuncStr:String;
  n:Integer;
  JN:TJEBranch;
begin
  with FLevelNodes[ExprLv] do
  begin
    if OpKind=jokOp then
    begin
      if Op='[' then exit;  //2012-06-11  考虑多维数组的情况  Ay[1,2,3]    
      //双目操作符参数已满后，继续有值需要加入，如：  Obj.Func "ab"
      with TJEBranch(Obj) do
      begin
        n:=length;
        if n<3 then exit;        
        mstr:=KeyByIndex[n-1];
        Node:=Remove(mstr);
        FuncStr:=Node.toString;
        JN:=NewFuncObj(FuncStr);
        Put(mstr,JN);
      end;
      Node.Free;
      Inc(ExprLv);
      with FLevelNodes[ExprLv] do
      begin
        Op:=FuncStr;
        Obj:=JN;
        BC:=FCurBlockCount;
        SC:=FStatementLevel;
        OpKind:=jokFunc;
        Rank:=OpRank_Func;
      end;
      //进入虚拟的括号
      Inc(FCurBlockCount);
      FFuncWithoutBracket:=true;
    end;
  end;
end;

function TJEParser.DefaultVisibility: String;
begin
  Result:='PUBLIC';
end;

function TJEParser.DefInClass: Boolean;
var
  i:Integer;
  mstr:String;
begin
  (*
  {
    "op": " CLASS",
    "p1": "TMSH",
    "MEMBERS": [
      {
        "op": " VAR",
        "p1": "Name",
        "p0": ["PUBLIC"]
      }
    ]
  }
  MEMBERS数组在LEVELNODE数组中可能被忽略
  *)
  Result:=false;
  if FCurNodeIdx<1 then exit;
  if not (FNodeAy[FCurNodeIdx-1].Node is JSONArray) then  //应为MEMBERS数组或CLASS结构
  begin
    if not (FNodeAy[FCurNodeIdx-1].Node is TJEBranch) then exit;
    i:=FCurNodeIdx-1;
  end
  else if FCurNodeIdx>1 then
    i:=FCurNodeIdx-2
  else
    exit;
  if not (FNodeAy[i].Node is TJEBranch) then exit;  //应为CLASS结构
  Result:=TJEBranch(FNodeAy[i].Node).OptString(JEP_Operator)=' '+JED_Class;
end;

destructor TJEParser.Destroy;
begin
  ClearUserSymbols;
  FUserSymbols.Free;
  FDefLevels.Free;
  FLineCommet.Free;
  FBlockCommet.Free;
  FKeywords.Free;
  FHeadKeywords.Free;
  FLanFuncs.Free;
  FLanObjs.Free;
  FLanVals.Free;
  FLanTypes.Free;
  FLanOp2s.Free;
  FLanOp1s.Free;
  FPerfixKWs.Free;
  FPostfixKWs.Free;
  inherited;
end;

procedure TJEParser.DoInit;
var
  i:Integer;
begin
  FDefLevels.Free;
  FDefLevels:=TStringList.Create;
  FDefLevels.Capacity:=16;
  FCurPos:=1;
  FCurToken.Token:='';
  FLastToken.Token:='';
  FLastToken.Kind:=tkStart;
  FCurToken.StartPos:=0;
  FLastToken.StartPos:=0;
  FCurNodeLevel:=0;
  FCurBlockCount:=0;
  FStatementLevel:=0;
  FFuncWithoutBracket:=false;
  ClearUserSymbols;
  RegGlobalEnv;
  with FLevelNodes[0] do
  begin
    Obj:=NewFuncObj('');
    Op:='';
    OpKind:=jokNone;
    Rank:=0;
  end;
  for i:=1 to High(FLevelNodes) do
  begin
    with FLevelNodes[i] do
    begin
      if Obj=nil then break;
      Obj:=nil;
      Op:='';
      OpKind:=jokNone;
    end;
  end;
end;

function TJEParser.DoParse: JSONObject;
var
  kwidx,LastPos:Integer;
begin
  LastPos:=0;
  DoInit;
  //if FCurPos=0 then
    NextToken;
  while FCurToken.Kind<>tkEND do
  begin
    LastPos:=FCurPos;
    ParseStatements;
    if (FCurPos=LastPos) and (FCurToken.Kind<>tkEND) then
    begin
      PrintErr('Cycle on ['+IntToStr(LastPos)+'] "'+FCurToken.Token+'" after "'+FLastToken.Token+'".');
      NextToken;
    end;
  end;
  Result:=TJEBranch(FLevelNodes[0].Obj);
  if Result<>nil then
    Result.Put(JEP_LanDef,Self.Lan);  //2012-02-17  
end;

function TJEParser.ExpectHeadKW(out TagInt: Integer): Integer;
begin
  with FCurToken do
  begin
    if not (Kind in [tkWORD,tkKEYWORD]) then
    begin
      Result:=-1;
      exit;
    end;
    Result:=FHeadKeywords.IndexOf(CaseOKToken);
    if Result<0 then exit;
    Result:=Integer(FHeadKeywords.Objects[Result]) and JETAG_BODYMASK;
    TagInt:=Result and KeywordTagMask;
    Result:=Result and KeywordIdxMask;
    KWIdx1:=Result+1;
  end;
end;

function TJEParser.ExpectKW: Integer;
var
  n:Integer;
begin
  Result:=-1;
  with FCurToken do
  begin
    if KWIdx1>0 then
    begin
      Result:=KWIdx1-1;
      exit;
    end;
    if not (Kind in [tkWORD,tkKEYWORD]) then exit;
    n:=Length(CaseOKToken);
    if n<=High(FLanWordsAy) then
    begin
      if FLanWordsAy[n]<>nil then
      begin
        Result:=FLanWordsAy[n].IndexOf(CaseOKToken);
        if Result<0 then exit;
        Result:=Integer(FLanWordsAy[n].Objects[Result]);
        KWIdx1:=1+(Result and JETAG_BODYMASK);
        case Result and JETAG_MASK of
          JETAG_Keyword: Kind:=tkKEYWORD;
          JETAG_LanFunc: Kind:=tkFunc;
          JETAG_LanObj:  Kind:=tkObj;
          JETAG_LanVal:  Kind:=tkVar;
          JETAG_LanType: Kind:=tkType;
          JETAG_LanOp2:  Kind:=tkOPERATOR;
          JETAG_LanOp1:  Kind:=tkOPERATOR;
        end;
      end;
    end;
  end;
end;

function TJEParser.ExpectOp1KW: Integer;
begin
  Result:=FLanOp1s.IndexOf(FCurToken.CaseOKToken);
  if Result<0 then exit;
  Result:=Integer(FLanOp1s.Objects[Result]) and JETAG_BODYMASK;
  FCurToken.Kind:=tkOPERATOR;
end;

function TJEParser.ExpectPerfix: Integer;
begin
  Result:=FPerfixKWs.IndexOf(FCurToken.CaseOKToken);
  if Result<0 then exit;
  Result:=Integer(FPerfixKWs.Objects[Result]);
end;

function TJEParser.ExtractVarInSetValueLv(Lv: Integer): TJENode;
begin
  Result:=nil;
  with FLevelNodes[Lv] do
  begin
    if Op<>JOP_SetValue then exit;    
    Result:=TJEBranch(Obj).Opt(JEP_Param1);
  end;  
end;

procedure TJEParser.GenLanWordsAy;
var
  i,n,m:Integer;
  SL:TStringList;
begin
  ResizeSPAy(FKeywords.Count);
  m:=0;
  with FKeywords do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanFuncs do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanObjs do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanVals do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanTypes do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanOp2s do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  with FLanOp1s do
    for i:=0 to Pred(Count) do
    begin
      n:=Length(Strings[i]);
      if n>m then m:=n;
    end;
  SetLength(FLanWordsAy,m+1);
  for n:=1 to m do
  begin
    SL:=TStringList.Create;
    with FKeywords do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_Keyword or i));
      end;
    with FLanFuncs do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanFunc or i));
      end;
    with FLanObjs do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanObj or i));
      end;
    with FLanVals do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanVal or i));
      end;
    with FLanTypes do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanType or i));
      end;
    with FLanOp2s do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanOp2 or i));
      end;
    with FLanOp1s do
      for i:=0 to Pred(Count) do
      begin
        if Length(Strings[i])<>n then continue;
        SL.AddObject(Strings[i],TObject(JETAG_LanOp1 or i));
      end;
    if SL.Count>0 then
    begin
      SL.CaseSensitive:=true;
      SL.Sorted:=true;
      FLanWordsAy[n]:=SL;
    end
    else begin
      SL.Free;
      FLanWordsAy[n]:=nil;
    end;
  end;
end;

function TJEParser.GetArrayDefStr(ANode: TJEBranch; CurDimIdx: Integer): String;
begin
  if CurDimIdx=1 then
  begin
    Result:=TransANode(ANode.Opt(JEP_Param1),0);
    Result:=Result+GetArrayDefStr(ANode,CurDimIdx+1);
    if FArrayUseFuncBracket then
      Result:='('+Result+')'
    else
      Result:='['+Result+']';
  end
  else if CurDimIdx>0 then
  begin
    Result:=TransANode(ANode.Opt(JEP_ParamHeader+IntToStr(CurDimIdx)),0);
    if Result<>'' then
      Result:=','+Result;
  end
  else
    Result:='';
end;

function TJEParser.GetBlanks: TJECharSet;
begin
  Result:=[#9,' '];
end;

function TJEParser.GetBlockCommetStrs: TStrings;
begin
  Result:=TStringList.Create;
  Result.Values['{']:='}';
end;

function TJEParser.GetBlockLevel: Integer;
begin
  Result:=GetExprLevel;
  while (Result>0) and (FLevelNodes[Result].BC>=FLevelNodes[Result-1].BC) do
  begin
    with FLevelNodes[Result] do
    begin
      if BC<FCurBlockCount then break;
      if SC<FStatementLevel then break;  //2012-06-07
    end;
    Dec(Result);
    with FLevelNodes[Result] do
    begin
      //2012-06-07 一步跨出两层的情况   if (A=B) or ... then ...
      if (BC<FCurBlockCount) and (SC<FStatementLevel) then
      begin
        Inc(Result);
        break;
      end;
    end;
  end;
end;

function TJEParser.GetBracketCharSet: TJECharSet;
begin
  Result:=['(',')','[',']','{','}'];
end;

function TJEParser.GetCompOpCharSet: TJECharSet;
begin
  Result:=['=','>','<'];
end;

function TJEParser.GetCompStrs: TStrings;
begin
  Result:=TStringList.Create;
  Result.Add('=');
  Result.Add('>');
  Result.Add('<');
  Result.Add('>=');
  Result.Add('<=');
  Result.Add('<>');
end;

function TJEParser.GetConstBeginCharSet: TJECharSet;
begin
  Result:=VarBegin;
end;

function TJEParser.GetExprLevel: Integer;
begin
  Result:=FCurNodeLevel;
  with FLevelNodes[FCurNodeLevel] do
    if (Obj=nil) or (Obj.ClassType=JSONObject) or (Obj.ClassType=JSONArray) then exit;
  if Result>0 then Dec(Result);
end;

class function TJEParser.GetFilePostfixs: TStrings;
begin
  Result:=TStringList.Create;
end;

function TJEParser.GetFuncBeginCharSet: TJECharSet;
begin
  Result:=VarBegin;
end;

function TJEParser.GetFuncLevel: Integer;
begin
  Result:=GetExprLevel;
  while (FLevelNodes[Result].BC>=FCurBlockCount) //and (FLevelNodes[Result].opKind<>jokStatement) do
    and (FLevelNodes[Result].SC>=FStatementLevel) do
  begin
    Dec(Result);
    if Result<0 then
    begin
      Inc(Result);
      break;
    end;
    if ((FLevelNodes[Result].BC<FCurBlockCount)
      or (FLevelNodes[Result].SC<FStatementLevel)) and not (FLevelNodes[Result].OpKind in [jokFunc, jokBracketBegin]) then
    begin
      Inc(Result);
      exit;
    end;
  end;
end;

function TJEParser.GetLineBreakOps: TJECharSet;
begin
  Result:=[';'];
end;

function TJEParser.GetLineCommetOps: TStrings;
begin
  Result:=TStringList.Create;
  Result.Add('//');
end;

function TJEParser.GetLineJoinOps: TJECharSet;
begin
  Result:=[];
end;

function TJEParser.GetMathOpCharSet: TJECharSet;
begin
  Result:=['+', '-', '*', '/', '\', '%', '^', '&', '|', '.'];
end;

function TJEParser.GetMathStrs: TStrings;
begin
  Result:=nil;
end;

function TJEParser.GetOpInfo(var Op: String; out IsOp2: Boolean; out Rank: Byte): Boolean;
begin
  IsOp2:=true;
  Result:=true;
  case Length(Op) of
    0:
    begin
      Result:=false;
      Rank:=0;
      exit;
    end;
    1:
    begin
      case Op[1] of
        '!','~': IsOp2:=false;
      end;
      Rank:=GetStdOpRank(Op[1],Op);
    end;
    2:
    begin
      Rank:=GetStdOpRank(Op[1],Op);
    end;
  end;
end;

class function TJEParser.GetParserForLan(ALan: String): TJEParserClass;
var
  mstr:String;
  i:Integer;
begin
  mstr:=UpperCase(ALan);
  i:=Parsers.IndexOf(mstr);
  if i>=0 then
    Result:=TJEParserClass(Parsers.Objects[i])
  else
    Result:=nil;
end;

function TJEParser.GetSetValOpRank: Byte;
begin
  Result:=GetOpRank(FSetValOp);
end;

function TJEParser.GetSpecialCharSet: TJECharSet;
begin
  Result:=[];
end;

function TJEParser.GetStatementLevel: Integer;
begin
  Result:=FCurNodeLevel;
  while (FLevelNodes[Result].SC>=FStatementLevel) do
  begin
    Dec(Result);
    if Result<0 then
    begin
      Inc(Result);
      break;
    end;
  end;
end;

function TJEParser.GetStrChCharSet: TJECharSet;
begin
  Result:=['''','"'];
end;

function TJEParser.GetVarBeginCharSet: TJECharSet;
begin
  Result:=VarBegin;
end;

function TJEParser.GetVarBodyCharSet: TJECharSet;
begin
  Result:=VarBody;
end;

procedure TJEParser.GoNextParam;
var
  OriLv:Integer;
  StrVal:String;
begin
  OriLv:=FCurNodeLevel;
  FCurNodeLevel:=GetFuncLevel;
  with FLevelNodes[FCurNodeLevel] do
  begin
    if OpKind=jokNone then  //逗号
    begin
      JSONObject(Obj).Put(JEP_Operator,'(');
      Op:='(';
      Rank:=OpRank['('];
      Dec(BC);
    end
    else if (BC=FCurBlockCount) and (SC=FStatementLevel) and (Op<>'[') {and (OpKind<>jokStatement)} then  //... ? ( A ? B , ...
    begin  //将纯括号内部的表达式提升到括号的内部
      // 参数表(A,B,...) => MEMBERS: [ "A", "B", ...] 
      if FLevelNodes[FCurNodeLevel].OpKind=jokItems then exit;      
      //AddOp('(',amOperator,true);
      PushBracket('(');
      Dec(FLevelNodes[FCurNodeLevel].BC);
    end
    else if BC<FCurBlockCount then
    begin
      //逗号位于括号内部，并且括号外的操作符不是一般函数（无论单目还是双目算子，右侧都不会有多个参数)
      // -- 将括号内的内容做为集合处理
      FCurNodeLevel:=OriLv;
      FCurNodeLevel:=GetExprLevel;
      with JSONObject(FLevelNodes[FCurNodeLevel].Obj) do
      begin
        StrVal:=OptString(JEP_Operator);
        if StrVal='' then
        begin
          Put(JEP_Operator,'(');  //括号做为集合标志
        end
        else
          FCurNodeLevel:=GetFuncLevel;
      end;
    end;
  end;
  //ExprStart:=true;
end;

procedure TJEParser.GoNextStatementParam;
begin
  FCurNodeLevel:=GetStatementLevel;
end;

class function TJEParser.HasResultVar: Boolean;
begin
  Result:=false;
end;

function TJEParser.InBlockLevel(ARank: Byte=1): Integer;
var
  Op:String;
  Ch:Char;
  n:Byte;
begin  // Func( X+Y*2    1+X.Next.Val
  Result:=GetExprLevel;
  //If the expression is inside a block or not...
  n:=FLevelNodes[Result].BC;
  if n>FCurBlockCount then  // ... A ? (B ? C) / ...  or  ... A ?  Func( B ? C ) / ...
  begin
    repeat
      Dec(Result);
      if Result<0 then break;
      with FLevelNodes[Result] do
      begin
        if SC=FStatementLevel-1 then break;
        if OpKind=jokFunc then continue; //函数优先级最高，无需比较
        Op:=JSONObject(FLevelNodes[Result].Obj).OptString(JEP_Operator);
        if Op<>'' then
        begin
          Ch:=Op[1];
          if FLevelNodes[Result].Rank<ARank then break;
          if (Ch in ['(','[','{']) and (Result>0) and (FLevelNodes[Result-1].BC<FCurBlockCount) then break
        end;
      end;
    until FLevelNodes[Result].BC<FCurBlockCount;
  end
  else  // ... A ? B / ...
    while FLevelNodes[Result].BC=FCurBlockCount do
    begin  
      Dec(Result);
      if Result<0 then break;
      with FLevelNodes[Result] do
      begin
        if SC<=FStatementLevel-1 then break;
        if Rank<ARank then break;
      end;
    end;
  Inc(Result);
end;

function TJEParser.InBlockLevel2(ARank: Byte): Integer;
var
  Op:String;
  Ch:Char;
  n:Byte;
begin  // Func( X+Y*2    1+X.Next.Val
  Result:=GetExprLevel;
  //If the expression is inside a block or not...
  n:=FLevelNodes[Result].BC;
  if n<FCurBlockCount then
  begin
    if FLevelNodes[Result+1].BC<FCurBlockCount then
    begin
      // x := ( - (...) )
      Inc(Result);
      exit;
    end;
  end;
  if n>FCurBlockCount then  // ... A ? (B ? C) / ...  or  ... A ?  Func( B ? C ) / ...
  begin
    repeat
      Dec(Result);
      if Result<0 then break;
      with FLevelNodes[Result] do
      begin
        if SC=FStatementLevel-1 then break;
        if OpKind=jokFunc then continue; //函数优先级最高，无需比较
        Op:=JSONObject(FLevelNodes[Result].Obj).OptString(JEP_Operator);
        if Op<>'' then
        begin
          Ch:=Op[1];
          if FLevelNodes[Result].Rank<ARank then break;
          if (Ch in ['(','[','{']) and (Result>0) and (FLevelNodes[Result-1].BC<FCurBlockCount) then break
        end;
      end;
    until FLevelNodes[Result].BC<FCurBlockCount;
  end
  else  // ... A ? B / ...
    while FLevelNodes[Result].BC=FCurBlockCount do
    begin
      if FLevelNodes[Result].Rank<ARank then break;
      Dec(Result);
      if Result<0 then break;
      with FLevelNodes[Result] do
      begin
        if SC<=FStatementLevel-1 then break;
        if Rank<ARank then break;
      end;
    end;
  Inc(Result);
end;

procedure TJEParser.InitParser;
begin
  FMathOpWithEqualPostfix:=false;
  FSingleMathOp:=false;
  FCTypeStr:=false;
  FPascalTypeStr:=false;
  FBlanks:=GetBlanks;
  FVBegin:=GetVarBeginCharSet;
  FVBody:=GetVarBodyCharSet;
  FCBegin:=GetConstBeginCharSet;
  FStrCh:=GetStrChCharSet;
  FBrackets:=GetBracketCharSet;
  FMathOps:=GetMathOpCharSet;
  FCompOps:=GetCompOpCharSet;
  FSpecialHCs:=GetSpecialCharSet;
  FMathStrs:=GetMathStrs;
  FCompStrs:=GetCompStrs;
  FLineCommet:=GetLineCommetOps;
  FBlockCommet:=GetBlockCommetStrs;
  FVCBeginEqual:=FVBegin=FCBegin;
  FVFBeginEqual:=FVBegin=FFBegin;
  FLineBreakCh:=OpCh_Sentence;
  FSetValOp:=':=';
  FEqualOp:='=';
  FNotEqualOp:='<>';
end;

class function TJEParser.LanCount: Integer;
begin
  Result:=Parsers.Count;
end;

class function TJEParser.LanHasResultVar(const ALan: String): Boolean;
var
  i:Integer;
begin
  i:=Parsers.IndexOf(ALan);
  if i<0 then Result:=false;
  Result:=TJEParserClass(Parsers.Objects[i]).HasResultVar;
end;

class function TJEParser.Lans(const Index: Integer): TJEParserClass;
begin
  if (Index<Parsers.Count) and (Index>=0) then
    Result:=TJEParserClass(Parsers.Objects[Index])
  else
    Result:=nil;
end;

function TJEParser.LastToken: TTokenRec;
begin
  Result:=FLastToken;
end;

function TJEParser.MakeArrayParam(const AName: String):Boolean;
var
  Z:TJENode;
begin
  with FLevelNodes[FCurNodeLevel] do
  begin
    if not (Obj is TJEBranch) then
    begin
      Result:=false;
      exit;
    end;
    Z:=JSONArray.Create;
    TJEBranch(Obj).Put(AName,Z);
    Inc(FCurNodeLevel);
    Result:=true;
  end;
  with FLevelNodes[FCurNodeLevel] do
  begin
    Obj:=Z;
    Op:=AName;
    OpKind:=jokItems;
    BC:=FCurBlockCount;
    SC:=FStatementLevel;
  end;
end;

function TJEParser.MakeBuildInFunc(const AName: String): String;
begin
  Result:=LanFuncHead+LanFuncDiv+AName;
end;

function TJEParser.MakeIncDecStr(JObj: TJENode; IncNum,
  PrnRank: Integer): String;
var
  OpStr,mstr:String;
  rk,n:Byte;
begin
  if IncNum>0 then
  begin
    OpStr:='+';
    mstr:='-'+IntToStr(IncNum);
  end
  else if IncNum<0 then
  begin
    OpStr:='-';
    IncNum:=-IncNum;
    mstr:='+'+IntToStr(IncNum);
  end
  else begin  //IncNum=0
    Result:=TransANode(JObj,0,PrnRank);
    exit;
  end;
  rk:=GetOpRank(OpStr);
  Result:=TransANode(JObj,0,rk);
  n:=Length(mstr);
  //检查是否可以抵消表达式中的最后部分 如 N-1 => N
  if Copy(Result,Length(Result)-n+1,n)=mstr then
  begin
    Result:=Copy(Result,1,Length(Result)-n);
    exit;
  end;
  Result:=Result+OpStr+IntToStr(IncNum);
  if PrnRank<rk then
    Result:='('+Result+')';
end;

function TJEParser.MakeLanFunc(const AName: String): String;
begin
  Result:=LanFuncHead+Self.Lan+LanFuncDiv+AName;
end;

function TJEParser.MakeReturn(const ValExpr: String): String;
begin
  Result:='return('+VarForLan(JEV_ResultRep)+');';
end;

procedure TJEParser.MakeVarInc(AVar, IncVal: TJENode);
var
  J1:TJEBranch;
begin
  if (AVar=nil) or (IncVal=nil) then exit;
  J1:=NewFuncObj('+=');
  J1.Put(JEP_Param1,AVar);
  J1.Put(JEP_Param2,IncVal);
  with TJEBranch(FLevelNodes[FCurNodeLevel].Obj) do
    Put(JEP_ParamHeader+IntToStr(length),J1);
end;

procedure TJEParser.MakeVarInc1(AVar: TJENode);
var
  Node1:TJENode;
begin
  Node1:=_Integer.create(1);
  MakeVarInc(AVar,Node1);
end;

function TJEParser.ModifyFuncName(Lv: Integer; const NewName: String): Boolean;
begin
  with FLevelNodes[Lv] do
  begin
    if OpKind in [jokFunc,jokStatement] then
    begin
      TJEBranch(Obj).Put(JEP_Operator,NewName);
      Op:=NewName;
    end;
  end;
  Result:=false;
end;

function TJEParser.NextTo(const EndStr: String; ToEnd: Boolean; PtrBack: Boolean): String;
var
  i,j,n,n2:Integer;
  Ch1,Ch2:Char;
begin
  Result:='';
  n2:=Length(EndStr);
  if n2=0 then exit;
  Ch1:=EndStr[1];
  if n2>=2 then Ch2:=EndStr[2];
  n:=Length(FSource);
  if FCurPos>=n-n2+1 then
  begin
    if ToEnd then
    begin
      Result:=Copy(FSource,FCurPos,n-FCurPos-n2+1);
      FCurPos:=n+1;
    end;
    exit;
  end;
  if n2=1 then
  begin
    for i:=FCurPos to n do
      if FSource[i]=Ch1 then
      begin
        Result:=Copy(FSource,FCurPos,i-FCurPos);
        if PtrBack then
          FCurPos:=i
        else
          FCurPos:=i+1;
        exit;
      end
  end
  else if n2=2 then
  begin
    for i:=FCurPos to n-1 do
      if FSource[i]=Ch1 then
      begin
        if FSource[i+1]<>Ch2 then continue;
        Result:=Copy(FSource,FCurPos,i-FCurPos);
        if PtrBack then
          FCurPos:=i
        else
          FCurPos:=i+2;
        exit;
      end;
  end
  else begin
    for i:=FCurPos to n-n2+1 do
    begin
      if (FSource[i]=Ch1) and (FSource[i+1]=Ch2) then
      begin
        for j:=2 to n2 do
        begin
          if FSource[i+j]<>EndStr[j] then break;
          if j=n2 then
          begin
            Result:=Copy(FSource,FCurPos,i-FCurPos-n2+1);
            if PtrBack then
              FCurPos:=i
            else
              FCurPos:=i+n2;
            exit;
          end;
        end;
      end;
    end;
  end;
  if ToEnd then
  begin
    Result:=Copy(FSource,FCurPos,n-FCurPos+1);
    FCurPos:=n+1;
  end;
end;

function TJEParser.NextToken: String;
var
  i,j,n,SLen:Integer;
  Ch,C2:Char;
  StrVal,mstr:String;
  WCh:WideChar;
  function ExprStart:Boolean; 
  begin
    Result:=CurToken.Kind in [tkOPERATOR,tkDELIMITER,tkBRACKET,tkKEYWORD];
    if Result and ((CurToken.Token=')') or (CurToken.Token=']')) then
      Result:=false;    
  end; 
begin
  n:=Length(FSource);
  Result:='';
  if FCurPos>n then
  begin
    SetCurToken('',tkEND,FCurPos);
    exit;
  end;
  i:=FCurPos;
  StrVal:='';
  while i<=n do
  begin
    Ch:=FSource[i];
    if Ch in FSpecialHCs then
      if OnSpecialHeadChar(Ch,i) then exit;
    // Is commet ?
    if Ch in FCommetHC then
    begin
      if StrMatch(FSource,i,FLineCommet)>=0 then
      begin
        repeat
          Inc(i);
          if i>n then break;
        until (FSource[i] in [#13,#10]);
        if i<=n then
        begin
          while FSource[i] in [#13,#10] do
          begin
            Inc(i);
            if i>n then break;
          end;
          if FLineBreakSentence then
          begin
            SetCurToken('',tkEOLN,FCurPos);
            FCurPos:=i;
          end;
        end
        else begin
          FCurPos:=i;
          SetCurToken('',tkEND,FCurPos);
          exit;
        end;
        break;
      end;
      j:=StrMatchName(FSource,i,FBlockCommet);
      if j>=0 then
      begin
        mstr:=FBlockCommet.ValueFromIndex[j];
        SLen:=Length(mstr);
        C2:=mstr[1];
        repeat
          Inc(i);
          if i>n then break;
        until (FSource[i]=C2) and (Copy(FSource,i,SLen)=mstr);
        Inc(i,SLen);
        break;
      end
    end;
    if Ch in FBlanks then
    begin
      repeat
        Inc(i);
        if i>n then break;        
      until not (FSource[i] in FBlanks);
      FCurPos:=i;
      if i>n then
      begin
        SetCurToken('',tkEND,FCurPos);
        exit;
      end;
      continue;
    end
    else if Ch in [#13,#10] then
    begin
      repeat
        Inc(i);
        if i>n then break;
      until not (FSource[i] in [#13,#10]);
      if FLineBreakSentence then
      begin
        SetCurToken('',tkEOLN,FCurPos);
        FCurPos:=i;
        exit;
      end;
      FCurPos:=i;
      continue;
    end
    else if Ch in FVBegin then
    begin
      repeat
        StrVal:=StrVal+FSource[i];
        Inc(i);
        if i>n then break;
      until not (FSource[i] in FVBody);
      SetCurToken(StrVal,tkWORD,FCurPos);
      Result:=StrVal;
      break;
    end
    else if not FVCBeginEqual and (Ch in FCBegin) then
    begin
      repeat
        StrVal:=StrVal+FSource[i];
        Inc(i);
        if i>n then break;
      until not (FSource[i] in FVBody);
      SetCurToken(StrVal,tkWORD,FCurPos);
      Result:=StrVal;
      break;
    end
    else if not FVFBeginEqual and (Ch in FFBegin) then
    begin
      repeat
        StrVal:=StrVal+FSource[i];
        Inc(i);
        if i>n then break;
      until not (FSource[i] in FVBody);
      SetCurToken(StrVal,tkWORD,FCurPos);
      Result:=StrVal;
      break;
    end
    else if (Ch in Digits) or (ExprStart and (Ch in ['+','-'])) then
    begin
      while true do
      begin
        StrVal:=StrVal + Ch;
        Inc(I);
        Ch:=FSource[i];
        if not (Ch in Digits) then
        begin
          if Ch<>'.' then break;
          if (i<n) and (FSource[i+1]='.') then  //  2..9
            break
          else
            continue;
        end;
      end;
      if StrVal='-' then  //2012-06-11  非数字前的 - 号
        SetCurToken(StrVal,tkOPERATOR,FCurPos)
      else begin
        //Scientific Number. eg:  1.34E-20  9E55
        if (i<n) and (FSource[i] in ['e','E']) then
        begin
          StrVal:=StrVal + FSource[i];
          Inc(I);
          if FSource[i] in ['+','-'] then
          begin
            StrVal:=StrVal + FSource[i];
            Inc(i);
          end;
          if i<=n then
            repeat
              StrVal:=StrVal + FSource[i];
              Inc(I);
            until not (FSource[i] in Digits);
        end;
        SetCurToken(StrVal,tkNUMERIC,FCurPos);
      end;
      Result:=StrVal;
      break;
    end
    else if Ch in FMathOps then
    begin
      if FSingleMathOp then
      begin
        StrVal:=Ch;
      end
      else begin
        repeat
          StrVal:=StrVal+FSource[i];
          Inc(i);
          if i>n then break;
        until not (FSource[i] in FMathOps);
      end;
      SetCurToken(StrVal,tkOPERATOR,FCurPos);
      Result:=StrVal;
      break;
    end
    else if Ch in FCompOps then
    begin
      if FSingleMathOp then
      begin
        StrVal:=Ch;
      end
      else begin
        repeat
          StrVal:=StrVal+FSource[i];
          Inc(i);
          if i>n then break;
        until not (FSource[i] in FCompOps);
      end;
      SetCurToken(StrVal,tkOPERATOR,FCurPos);
      Result:=StrVal;
      break;
    end
    else if Ch in FStrCh then
    begin
      if FPascalTypeStr then
      begin
        Inc(i);
        while i<=n do
        begin
          if FSource[i]=Ch then
          begin
            Inc(i);
            if (i<=n) and (FSource[i]=Ch) then
            begin
              StrVal:=StrVal+Ch;
              Inc(i);
            end
            else
              break;
          end
          else begin
            StrVal:=StrVal+FSource[i];
            Inc(i);
          end;
        end;
      end
      else if FCTypeStr then
      begin
        Inc(i);
        while true do
        begin
          C2:=FSource[i];
          case C2 of
            #0, #10, #13:
            begin
              //Ignore #10 and #13 inside a string.  By creation_zy  2009-11-22
              if C2=#0 then
                break  //raise syntaxError('Unterminated string')
              else
                continue;
            end;
            '\':
            begin
              Inc(i);
              C2:=FSource[i];
              case C2 of
                'b':  //By creation_zy  2009-08-20
                  StrVal:=StrVal + #8;
                't':
                  StrVal:=StrVal + #9;
                'n':
                  StrVal:=StrVal + #10;
                'f':
                  StrVal:=StrVal + #12;
                'r':
                  StrVal:=StrVal + #13;
                {case 'u':
                  sb.append((char)Integer.parseInt(next(4), 16));
                  break;
                case 'x':  \cx  	The control character corresponding to x
                  sb.append((char) Integer.parseInt(next(2), 16));
                  break;}
                'u':  //By creation_zy  2009-08-20
                begin
                  PWord(@WCh)^:=Word(HexToInt(Copy(FSource,i,4)));
                  Inc(i,4);
                  StrVal:=StrVal+WCh;
                end;
                else
                  StrVal:=StrVal + C2
              end;
            end
            else  begin
              if (C2 = Ch) then
              begin
                Result:=StrVal;
                exit;
              end;
              StrVal:=StrVal + C2;
            end;
          end;
        end;
      end;
      SetCurToken(StrVal,tkSTRING,FCurPos);
      Result:=StrVal;
      break;
    end
    else if Ch=FLineBreakCh then
    begin
      SetCurToken(Ch,tkLINEDIV,FCurPos);
      Result:=Ch;
      FCurPos:=i+1;
      exit;
    end
    else if Ch in FBrackets then
    begin
      SetCurToken(Ch,tkBRACKET,FCurPos);
      Result:=Ch;
      FCurPos:=i+1;
      exit;
    end
    else begin
      SetCurToken(Ch,tkDELIMITER,FCurPos);
      Result:=Ch;
      FCurPos:=i+1;
      exit;
    end;
    Inc(i);
  end;
  FCurPos:=i;
end;

function TJEParser.NextTokenKW: Integer;
begin
  NextToken;
  ExpectKW;
  Result:=CurToken.KWIdx1-1;
end;

function TJEParser.OnKeyword(const Str: String; KWIdx: Integer):Integer;
begin
  Result:=-1;
end;

function TJEParser.OnLanConst(const Str: String; KWIdx: Integer):Boolean;
begin
  if Str='TRUE' then
  begin
    PushBool(true);
    Result:=true;
  end
  else if Str='FALSE' then
  begin
    PushBool(false);
    Result:=true;
  end
  else if Str='NULL' then  //2012-06-07
  begin
    PushNull;
    Result:=true;
  end
  else
    Result:=false;
end;

function TJEParser.OnLanVar(const Str: String; KWIdx: Integer):Boolean;
begin
  Result:=OnLanConst(Str,KWIdx);
end;

function TJEParser.OnPerfix(const Str: String; KWIdx: Integer): Integer;
begin
  Result:=-1;
end;

function TJEParser.OnSpecialHeadChar(ACh: Char; APos: Integer): Boolean;
begin
  Result:=false;
end;

function TJEParser.OnStatementHKW(const Str: String; KWIdx: Integer):Integer;
begin
  Result:=-1;
end;

function TJEParser.ParseCBlock: Integer;
begin
  if CurToken.Token='{' then
  begin
    while (CurToken.Kind in [tkEOLN,tkLINEDIV]) do
      NextToken;
    GoNextStatementParam;
    ParseStatements;
    if CurToken.Token<>'}' then
      PrintErr('Expect "}" here.');
  end;
end;

function TJEParser.ParseDefines: Integer;
begin
  Result:=ParseStatements(false);
end;

function TJEParser.ParseExpr: Integer;
var
  StrVal:String;
  idx,OriBC,MinLv:Integer;
begin
  Result:=-1;
  OriBC:=FCurBlockCount;
  MinLv:=-1;
  while CurToken.Kind<>tkEnd do
  begin
    StrVal:=FCurToken.Token;
    case FCurToken.Kind of
      tkEOLN, tkLINEDIV:
        if FLineBreakSentence then break;
      tkSTRING:
      begin
        PushString(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkNUMERIC:
      begin
        PushNumber(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkDELIMITER:
      begin
        if (StrVal=',') and (FCurBlockCount=OriBC) then break;       
        GoNextParam;
      end;
      tkOPERATOR:
      begin
        if FSEEqual and (StrVal=FEqualOp) then
        begin
          PushOp2('=',GetOpRank(StrVal));
        end
        else
          PushOp(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkWORD:
      begin
        with LastToken do
          if not ((Kind in [tkOPERATOR]) and (Token<>FEqualOp)) then
            idx:=ExpectKW
          else
            idx:=ExpectOp1KW; //仅检查单目操作关键字
        if idx>=0 then
        begin
          StrVal:=FCurToken.CaseOKToken;
          case FCurToken.Kind of
            tkKEYWORD:
            begin
              if (Integer(FKeywords.Objects[idx and KeywordIdxMask]) and KeywordTagMasks[ktRetVal])<>0 then
              begin
                ParseStatements(true);
                continue;
              end;
              break;
            end;
            tkOPERATOR:
            begin
              PushOp(StrVal);
              if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
              NextToken;
              continue;
            end;
            tkCONST:
            begin
              if OnLanConst(StrVal,idx) then
              begin
                if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
                NextToken;
                continue;
              end;
            end;
            tkVAR:
            begin
              if OnLanVar(StrVal,idx) then
              begin
                if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
                NextToken;
                continue;
              end;
            end;
          end;
        end;
        if WordIsBuildInFunc(StrVal) then
        begin
          PushVar(MakeBuildInFunc(FCurToken.CaseOKToken));
        end
        else
          PushVar(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkBRACKET:
      begin
        //终止括号超越了表达式的初始层级
        if (StrVal[1] in [')',']','}']) and (OriBC>=FCurBlockCount) then break;
        PushBracketOp(StrVal);
      end;
      else break;
    {tkDELIMITER,
    tkWORD,
    tkID,
    tkFUNC,}
    end;
    if (MinLv<0) or (FCurNodeLevel<MinLv) then
      MinLv:=FCurNodeLevel;
    NextToken;
  end;
  if FAddAsStrJoin and (MinLv>=0) then
    CheckAndTransAddToJoin(MinLv);
end;

function TJEParser.ParseIdentifier: String;
begin
  Result:='';
  if FCurToken.Kind=tkWORD then
  begin
    if ExpectKW>=0 then exit;
    Result:=FCurToken.Token;
    PushVar(Result);
    NextToken;
  end
end;

function TJEParser.ParseParameterDef: Integer;
begin
  Result:=ParseStatements;
end;

function TJEParser.ParseStatements(InOneLine: Boolean): Integer;
var
  StrVal:String;
  idx,tg,lv:Integer;
begin
  Result:=-1;
  while CurToken.Kind<>tkEnd do
  begin
    StrVal:=CurToken.Token;
    case CurToken.Kind of
      tkNUMERIC:
      begin
        PushNumber(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkString:
      begin
        PushString(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkOPERATOR:
      begin
        if FSEEqual and (StrVal=FEqualOp) then
        begin
          PushOp2(':=',GetOpRank(StrVal));
          NextToken;
          //增加括号层次，以处理形如 b = b1 and b2 = b3 的表达式
          Inc(FCurBlockCount);
          ParseExpr;
          Dec(FCurBlockCount);
          continue;
        end
        else
          PushOp(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkWORD:
      begin
        if ExpectKW>=0 then
        begin
          //部分语言允许关键字名称的函数，如 Response.End 因此必须在双目操作符后避免关键字检测
          if (FCurToken.Kind=tkKEYWORD) and (LastToken.Kind<>tkOPERATOR) then
          begin
            idx:=ExpectHeadKW(tg);
            if idx>=0 then
            begin
              if (High(FStatementProcs)>=0) and Assigned(FStatementProcs[idx]) then
                lv:=FStatementProcs[idx]
              else
                lv:=OnStatementHKW(FCurToken.CaseOKToken,idx);
              if (Result<0) or (Result>FCurNodeLevel) then Result:=lv;
              if CurToken.Kind=tkWORD then
              begin
                //一个语法结构结束后，跟着关键字
                if (ExpectKW>=0) and (CurToken.Kind=tkKEYWORD) then
                begin
                  if ExpectHeadKW(tg)>=0 then continue;              
                  exit;
                end
                else
                  continue;
              end
              else if CurToken.Kind in [tkEOLN,tkLINEDIV] then
                continue;
            end
            else if ExpectPerfix>=0 then
            begin
              lv:=OnPerfix(CurToken.CaseOKToken,CurToken.KWIdx1-1);
              if (Result<0) or (Result>FCurNodeLevel) then Result:=lv;
              continue;              
            end;
            exit;
          end;
          if FCurToken.Kind=tkOPERATOR then
          begin
            PushOp(FCurToken.CaseOKToken);
            NextToken;
            continue;
          end;
        end;
        PushVar(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkKEYWORD:
      begin
        idx:=ExpectHeadKW(tg);
        if idx>=0 then
        begin
          if (High(FStatementProcs)>=0) and Assigned(FStatementProcs[idx]) then
            lv:=FStatementProcs[idx]
          else
            lv:=OnStatementHKW(FCurToken.CaseOKToken,idx);
          if (Result<0) or (Result>lv) then Result:=lv;
          if CurToken.Kind=tkWORD then
          begin
            //一个语法结构结束后，跟着关键字
            if (ExpectKW>=0) and (CurToken.Kind=tkKEYWORD) then
            begin
              if ExpectHeadKW(tg)>=0 then continue;              
              exit;
            end
            else
              continue;
          end
          else if CurToken.Kind in [tkEOLN,tkLINEDIV] then
            continue;               
        end;
        exit;
      end;
      tkDELIMITER:
      begin     
        GoNextParam;
      end;
      tkBRACKET:
      begin
        PushBracketOp(StrVal);
      end;
      tkEOLN:
      begin
        if InOneLine then exit;
        if FLineBreakSentence then TryTransOneWordStatement;
        PushLineBreakOp;
      end;
      tkLINEDIV:
      begin
        if FLineBreakSentence then TryTransOneWordStatement;
        PushLineBreakOp;
      end;
    end;
    NextToken;
    //2012-08-03  考虑最后一行
    if CurToken.Kind=tkEnd then
      if FLineBreakSentence then TryTransOneWordStatement;
  end;
end;

function TJEParser.ParseVar: Integer;
var
  StrVal:String;
begin
  Result:=-1;
  while CurToken.Kind<>tkEnd do
  begin
    StrVal:=FCurToken.Token;
    case FCurToken.Kind of
      tkEOLN, tkLINEDIV:
        if FLineBreakSentence then break;
      tkSTRING:
      begin
        PushString(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkNUMERIC:
      begin
        PushNumber(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkOPERATOR:
      begin
        if StrVal=FEqualOp then exit;        
        PushOp2(StrVal); 
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkWORD:
      begin
        if ExpectKW>=0 then
        begin
          if FCurToken.Kind=tkKEYWORD then exit;
        end;
        PushVar(StrVal);
        if (Result<0) or (Result>FCurNodeLevel) then Result:=FCurNodeLevel;
      end;
      tkBRACKET:
      begin
        PushBracketOp(StrVal);
      end;
    {tkDELIMITER,
    tkWORD,
    tkID,
    tkVAR,
    tkCONST,
    tkFUNC,}
    end;
    NextToken;
  end;
end;

procedure TJEParser.PopupDefine;
begin
  with FDefLevels do
    Delete(Count-1);
end;

procedure TJEParser.PrintErr(const Msg: String; TokenLevel: Integer);
var
  mstr:String;
begin
  if TokenLevel<0 then
    mstr:=Format('Pos: %d  [%s]  ',[FCurToken.StartPos,FCurToken.Token])
  else
    mstr:=Format('Pos: %d  [%s]  ',[FCurToken.StartPos-1,FLevelNodes[TokenLevel].Obj.toString]);
  if Assigned(ErrPrint) then
  begin
    ErrPrint(mstr+Msg);
  end;
end;

procedure TJEParser.PushBool(const B: Boolean);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    Put(JEP_ParamHeader+IntToStr(n),B);
    FCurNodeLevel:=e+1;
    AfterPushItem(ValObjByIndex[n]);
  end;
end;

procedure TJEParser.PushBracket(const AOp: String);
var
  BracketBegin,IsEmpty,UsePiror:Boolean;
  OpCh:Char;
  Z:TZAbstractObject;
  J,JN,JP:TJEBranch;
  n:Integer;
  mstr,Func:String;
begin
  OpCh:=AOp[1];
  BracketBegin:=OpCh in ['(','[','{'];
  if BracketBegin then
  begin
    if AOp='[' then
    begin
      if FLevelNodes[FCurNodeLevel].Obj.ClassType<>JSONObject then
      begin
        n:=FCurNodeLevel-1;
        JP:=JSONObject(FLevelNodes[n].Obj); //表达式JSON对象
        //没有操作符的表达式 -- 填入Func
        if FLevelNodes[n].OpKind=jokNone then
        begin
          JP.Put(JEP_Operator,AOp);
          with FLevelNodes[n] do
          begin
            Op:='[';
            Rank:=GetOpRank('[');
            OpKind:=jokOp;
          end;
          Dec(FCurNodeLevel);
        end
        else //if FLevelNodes[n].OpKind=jokStatement then  // Dim A[9]
        begin
          JN:=NewFuncObj(AOp);
          with JP do
          begin
            mstr:=JEP_ParamHeader+IntToStr(length-1);
            Z:=Remove(mstr);
            JN.Put(JEP_Param1,Z);
            Put(mstr,JN);
          end;
          with FLevelNodes[FCurNodeLevel] do
          begin
            Obj:=JN;
            Op:='[';
            Rank:=GetOpRank('[');
            OpKind:=jokOp;
          end;
        end;
        Inc(FCurBlockCount);
        exit;
      end;
      FCurNodeLevel:=GetExprLevel;
      J:=JSONObject(FLevelNodes[FCurNodeLevel].Obj);
      JN:=NewFuncObj(AOp);
      FLevelNodes[FCurNodeLevel+1].Obj:=JN;
      //当前Level的所有者
      if FCurNodeLevel>0 then
      begin
        JP:=JSONObject(FLevelNodes[FCurNodeLevel-1].Obj);
        n:=JP.Length;
        //在含有前缀的情况下，最后一个Key不是 P + (n-1)  2011-09-20
        mstr:=JP.KeyByIndex[n-1];
        JN.Put(JEP_Param1,JP.Remove(mstr));
        JP.Put(mstr,JN);
      end
      else
        JN.Put(JEP_Param1,J);
      with FLevelNodes[FCurNodeLevel] do
      begin
        Obj:=JN;
        OpCh:='[';
        Rank:=GetOpRank('[');
      end;
      Inc(FCurBlockCount);
      exit;
    end;
    // ... SomeWord ( ...  =>  { op: SomeWord }
    if FLevelNodes[FCurNodeLevel].Obj.ClassType<>TJEBranch then
    begin
      Dec(FCurNodeLevel);
      with TJEBranch(FLevelNodes[FCurNodeLevel].Obj) do
      begin
        mstr:=JEP_ParamHeader+IntToStr(length-1);
        Z:=Remove(mstr);
        Func:=Z.toString;
        Z.Free;
      end;
    end
    else begin
      if LastToken.Kind in [tkWORD,tkVAR,tkCONST] then
        Func:=LastToken.Token
      else
        Func:='';
    end;
    Inc(FCurBlockCount);
    J:=JSONObject(FLevelNodes[FCurNodeLevel].Obj);
    if Func<>'' then
      with J do
        IsEmpty:=(Length<=1) and (FLevelNodes[FCurNodeLevel].OpKind=jokNone)
    else
      IsEmpty:=false;
    if IsEmpty and (FLevelNodes[FCurNodeLevel].OpKind=jokNone) then
    begin
      J.Put(JEP_Operator,Func);
      //FLevelNodes[FCurNodeLevel].OpCh:=AddMode;
      with FLevelNodes[FCurNodeLevel] do
      begin
        OpKind:=jokFunc;
        Rank:=OpRank_Func;
      end;
    end
    else begin
      JN:=NewFuncObj(Func);
      FLevelNodes[FCurNodeLevel+1].Obj:=JN;
      with J do
        Put(JEP_ParamHeader+IntToStr(Length),FLevelNodes[FCurNodeLevel+1].Obj);
      Inc(FCurNodeLevel);
      with FLevelNodes[FCurNodeLevel] do
      begin
        if Func='' then
        begin
          Op:=OpCh;
          OpKind:=jokNone;
          Rank:=0;
          BC:=FCurBlockCount;
        end
        else begin
          Op:=Func;
          OpKind:=jokFunc;
          Rank:=OpRank_Func;
          BC:=FCurBlockCount-1;
        end;
        SC:=FStatementLevel;  //2012-06-13
      end;
    end;
  end
  else begin
    FCurNodeLevel:=GetBlockLevel;
    Dec(FCurBlockCount);
  end;
end;

procedure TJEParser.PushBracketOp(const Op: String);
begin
  if FArrayUseFuncBracket and (Op='(') then
  begin
    //2012-06-07  考虑数组中的括号表达式 Persons((A-1)*2)
    if FCurBlockCount>FLevelNodes[FCurNodeLevel].BC then
    begin
      PushBracket(Op);
      exit;
    end;
    if CurExprIsArray then
      PushBracket('[')
    else if CurNodeIsFunc then
      PushBracket(Op)
    else if CurExprIsNotFunc then  //2012-06-07
      PushBracket('[')
    else
      PushBracket(Op);
  end
  else
    PushBracket(Op);
end;

procedure TJEParser.PushDefine(const TypeStr: String);
begin
  FDefLevels.AddObject(TypeStr,TObject(FCurNodeLevel));
end;

procedure TJEParser.PushDefStatement(const S, TypeStr: String);
begin
  PushStatement(S);
  PushDefine(TypeStr);
end;

procedure TJEParser.PushEmptyItem;
begin
  PushTypeVal('',JEPT_EmptyItem);
end;

procedure TJEParser.PushFloat(const F: Double);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    Put(JEP_ParamHeader+IntToStr(n),F);
    FCurNodeLevel:=e+1;
    AfterPushItem(ValObjByIndex[n]);
  end;
end;

procedure TJEParser.PushFunc(const AName: String; IsStatement: Boolean);
var
  IsEmpty:Boolean;
  i,n:Integer;
  Z,Z2:TZAbstractObject;
  J,JP,JN:JSONObject;
  mstr:String;
  AClass:TClass;
  label CommonCase;
begin
  //LastIsVar:=false;
  //最后一个Symbol是简单值或变量的情况
  AClass:=FLevelNodes[FCurNodeLevel].Obj.ClassType;
  if AClass<>TJEBranch then
  begin
    if AClass=JSONArray then
    begin
      JN:=NewFuncObj(AName);
      with JSONArray(FLevelNodes[FCurNodeLevel].Obj) do
      begin
        Put(JN);
      end;
      Inc(FCurNodeLevel);
      with FLevelNodes[FCurNodeLevel] do
      begin
        Obj:=JN;
        BC:=FCurBlockCount;
        SC:=FStatementLevel;
        if IsStatement then
          OpKind:=jokStatement
        else
          OpKind:=jokFunc;
        Rank:=OpRank_Func;
        Op:=AName;
      end;
      exit;
    end;
    n:=FCurNodeLevel-1;
    if FLevelNodes[n].OpKind=jokNone then
    begin
      if AName<>'' then
      begin
        //将修饰符转化为 perfix （数组）  2011-09-18
        //如 'public static function' -> {op:"",p1:"public",p2:"static",p3:"function"}
        //转化为: {op:" function",pf:["public","static"], ....... }
        with TJEBranch(FLevelNodes[n].Obj) do
        begin
          //最后一个是类型指示符
          if Length>1 then
          begin
            mstr:=' '+ValByIndex[Length-1];
            Put(JEP_Operator,mstr);
            Delete(length-1);
          end;
          if Length>1 then
          begin
            Z:=JSONArray.Create;
            for i:=1 to length-1 do
            begin
              Z2:=Remove(JEP_ParamHeader+IntToStr(i));
              if Z2<>nil then
                JSONArray(Z).put(Z2);
            end;
            Put(JEP_Perfix,Z);
          end;
          FLevelNodes[FCurNodeLevel].Obj:=NewFuncObj(AName);
          Put(JEP_Param1,FLevelNodes[FCurNodeLevel].Obj);
        end;
        with FLevelNodes[n] do
        begin
          //OpCh:=OpCh_Define;
          Rank:=OpRank[' '];
        end;
        with FLevelNodes[FCurNodeLevel] do
        begin
          Op:=AName;
          if IsStatement then
            OpKind:=jokStatement
          else
            OpKind:=jokFunc;
          Rank:=OpRank_Func;
        end;
        exit;
      end;
      with FLevelNodes[n] do
      begin
        JSONObject(Obj).Put(JEP_Operator,AName);
        if AName='' then
        begin
          OpKind:=jokNone;
          Rank:=0;
        end
        else begin
          if IsStatement then
            OpKind:=jokStatement
          else
            OpKind:=jokFunc;
          Rank:=OpRank_Func;
        end;
      end;
    end;
    Dec(FCurNodeLevel);
    exit;
  end;
  J:=JSONObject(FLevelNodes[FCurNodeLevel].Obj);
  with J do
    IsEmpty:=(Length<=1) and (FLevelNodes[FCurNodeLevel].OpKind=jokNone);
  if IsEmpty and (FLevelNodes[FCurNodeLevel].OpKind=jokNone) then
  begin
    J.Put(JEP_Operator,AName);
    //FLevelNodes[FCurNodeLevel].OpCh:=AddMode;
    with FLevelNodes[FCurNodeLevel] do
    begin
      if IsStatement then
        OpKind:=jokStatement
      else
        OpKind:=jokFunc;
      Rank:=OpRank_Func;
      Op:=AName;
      SC:=FStatementLevel;
    end;
  end
  else begin
    JN:=NewFuncObj(AName);
    FLevelNodes[FCurNodeLevel+1].Obj:=JN;
    with J do
      Put(JEP_ParamHeader+IntToStr(Length),FLevelNodes[FCurNodeLevel+1].Obj);
    Inc(FCurNodeLevel);
    with FLevelNodes[FCurNodeLevel] do
    begin
      BC:=FCurBlockCount;
      SC:=FStatementLevel;
      if AName='' then
      begin
        OpKind:=jokNone;
        Rank:=0;
      end
      else begin
        if IsStatement then
          OpKind:=jokStatement
        else
          OpKind:=jokFunc;
        Rank:=OpRank_Func;
        Op:=AName;
      end;
    end;
  end;
end;

procedure TJEParser.PushInt(const I: Integer);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    Put(JEP_ParamHeader+IntToStr(n),I);
    FCurNodeLevel:=e+1;
    AfterPushItem(ValObjByIndex[n]);
  end;
end;

procedure TJEParser.PushLineBreakOp;
var
  IsEmpty:Boolean;
  i,n:Integer;
  Z,Z2:TZAbstractObject;
  J,JP,JN:JSONObject;
  mstr:String;
  AClass:TClass;
  label CommonCase;
begin
  if FFuncWithoutBracket then  //离开虚拟的括号
  begin
    Dec(FCurBlockCount);
    FFuncWithoutBracket:=false;
  end;
  AClass:=FLevelNodes[FCurNodeLevel].Obj.ClassType;
  //最后一个Symbol是简单值或变量的情况 -- 应n当将其提升，嵌入到Func表达式内
  if AClass<>TJEBranch then
  begin
    n:=FCurNodeLevel-1;
    with FLevelNodes[n] do
    begin
      if OpKind=jokItems then exit;      
      JP:=JSONObject(Obj); //表达式JSON对象
    end;
    //没有操作符的表达式 -- 填入Func
    if FLevelNodes[n].OpKind=jokNone then
    begin
      JP.Put(JEP_Operator,OpCh_Sentence);
      with FLevelNodes[n] do
      begin
        OpKind:=jokSentenceDiv;
        Rank:=RK_Sentence;
      end;
      Dec(FCurNodeLevel);
      exit;
    end;
    with FLevelNodes[n] do
    begin
      if (BC>=FCurBlockCount) and (Op<>'[') and (OpKind<>jokStatement) then
      begin
        if (OpKind<>jokNone) and (RK_Sentence<=Rank) then
        begin
          FCurNodeLevel:=InBlockLevel(RK_Sentence);
          goto CommonCase;
        end;
      end;
    end;
    with JP do
    begin
      mstr:=KeyByIndex[Length-1];  //最后一个Key
      Z:=Remove(mstr);
    end;
    with FLevelNodes[FCurNodeLevel] do
    begin
      Obj:=NewFuncObj(OpCh_Sentence);
      JSONObject(Obj).Put(JEP_Param1,Z);
      JP.Put(mstr,Obj);
      Op:=OpCh_Sentence; //Func[1]; //AddMode;
      Rank:=RK_Sentence;
    end;
    exit;
  end;
  FCurNodeLevel:=InBlockLevel(RK_Sentence);
  // Set x=new Car  --对于含有statement的表达式，换行应跳出表达式  2012-05-31
  if FCurNodeLevel>0 then
  begin
    with FLevelNodes[FCurNodeLevel] do
    begin
      if (OpKind=jokStatement) and (FLevelNodes[FCurNodeLevel-1].SC=FStatementLevel) then
      begin
        Dec(FCurNodeLevel);
      end;
    end;
  end;
CommonCase:
  //2012-06-05  考虑class内部 MEMBERS 数组的情况
  if FLevelNodes[FCurNodeLevel].OpKind=jokItems then exit;  
  J:=JSONObject(FLevelNodes[FCurNodeLevel].Obj);
  if J=nil then exit; 
  IsEmpty:=false;
  if (FLevelNodes[FCurNodeLevel].OpKind=jokNone) then
  begin
    J.Put(JEP_Operator,OpCh_Sentence);
    //FLevelNodes[FCurNodeLevel].OpCh:=AddMode;
    with FLevelNodes[FCurNodeLevel] do
    begin
      Op:=OpCh_Sentence;
      Rank:=RK_Sentence;
      OpKind:=jokSentenceDiv;
    end;
  end
  else if (FLevelNodes[FCurNodeLevel].OpKind=jokSentenceDiv){ and (Func=';')} then  //平级的 ; 号   2010-06-28
    exit
  else begin
    if FCurNodeLevel>0 then
      with FLevelNodes[FCurNodeLevel-1] do
        if (OpKind=jokItems) and (SC=FStatementLevel) then  //Class定义内部，一个定义语句结束后的断句
        begin
          Dec(FCurNodeLevel);
          exit;
        end;
    if FLevelNodes[FCurNodeLevel].SC<FStatementLevel then exit;  //Statement内部的分行，无视  
    JN:=NewFuncObj(OpCh_Sentence);
    FLevelNodes[FCurNodeLevel+1].Obj:=JN;
    //按照左侧优先结合的规律进行重新组合——考虑运算符的优先级
    // not X  =>  (not X) or Y
    //当前Level的所有者
    if FCurNodeLevel>0 then
      JP:=JSONObject(FLevelNodes[FCurNodeLevel-1].Obj)
    else
      JP:=nil;
    if JP<>nil then
    begin
      n:=JP.Length;
      //在含有前缀的情况下，最后一个Key不是 P + (n-1)  2011-09-20
      mstr:=JP.KeyByIndex[n-1];
      JN.Put(JEP_Param1,JP.Remove(mstr));
      JP.Put(mstr,JN);
    end
    else begin
      if FStatementLevel>FLevelNodes[FCurNodeLevel].SC then
      begin
        // Statement内部的 语句分隔符 —— 不能跑到Statement外面去
        with J do
          Put(JEP_ParamHeader+IntToStr(length),JN);
        Inc(FCurNodeLevel);
      end
      else
        JN.Put(JEP_Param1,J);
    end;
    with FLevelNodes[FCurNodeLevel] do
    begin
      Obj:=JN;
      Op:=OpCh_Sentence;
      Rank:=RK_Sentence;
      OpKind:=jokSentenceDiv;
    end;
  end;
end;

procedure TJEParser.PushNull;
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    Put(JEP_ParamHeader+IntToStr(n),CNULL);
    FCurNodeLevel:=e+1;
    AfterPushItem(ValObjByIndex[n]);
  end;
end;

procedure TJEParser.PushNumber(const NumStr: String);
var
  e,n:Integer;
  f:Double;
begin
  if NumStr='' then exit;  
  try
    if NumStr[1]=JEP_TypeHead then
      PushTypeVal(NumStr)
    else
      PushFloat(StrToFloat(NumStr));
  except
    e:=GetExprLevel;
    CheckFuncNoBracket(e);
    with TJEBranch(FLevelNodes[e].Obj) do
    begin
      n:=Length;
      Put(JEP_ParamHeader+IntToStr(n),NumStr);
      FCurNodeLevel:=e+1;
      AfterPushItem(ValObjByIndex[n]);
    end;
  end;
end;

procedure TJEParser.PushOp(const Op: String);
var
  IsOp2:Boolean;
  R:Byte;
  OpStr:String;
begin
  if Op='' then exit;
  if Op[1] in ['(',')','[',']','{','}'] then
  begin
    PushBracketOp(Op);
    exit;
  end;
  OpStr:=Op;
  if not GetOpInfo(OpStr,IsOp2,R) then exit;
  if IsOp2 then
    PushOp2(OpStr,R)
  else
    PushOp1(OpStr,R);
end;

procedure TJEParser.PushOp1(const AOp: String; ARank: Byte);
var
  IsEmpty:Boolean;
  i,n:Integer;
  Z,Z2:TZAbstractObject;
  J,JP,JN:JSONObject;
  mstr:String;
  label CommonCase;
begin
  //最后一个Symbol是简单值或变量的情况 -- 应n当将其提升，嵌入到Func表达式内
  if FLevelNodes[FCurNodeLevel].Obj.ClassType<>JSONObject then
  begin
    {n:=FCurNodeLevel-1;
    if FLevelNodes[n].OpKind=jokNone then
    begin
      if AOp<>'' then
      begin
        //将修饰符转化为 perfix （数组）  2011-09-18
        with JSONObject(FLevelNodes[n].Obj) do
        begin
          //最后一个是类型指示符
          if Length>1 then
          begin
            mstr:=' '+ValByIndex[Length-1];
            Put(JEP_Operator,mstr);
            Delete(length-1);
          end;
          if Length>1 then
          begin
            Z:=JSONArray.Create;
            for i:=1 to length-1 do
            begin
              Z2:=Remove(JEP_ParamHeader+IntToStr(i));
              if Z2<>nil then
                JSONArray(Z).put(Z2);
            end;
            Put(JEP_Perfix,Z);
          end;
          FLevelNodes[FCurNodeLevel].Obj:=NewFuncObj(AOp);
          Put(JEP_Param1,FLevelNodes[FCurNodeLevel].Obj);
        end;
        with FLevelNodes[n] do
        begin
          OpCh:=OpCh_Define;
          Rank:=OpRank[' '];
        end;
        with FLevelNodes[FCurNodeLevel] do
        begin
          OpCh:=OpCh_Func;
          Rank:=OpRank[OpCh_Func];
        end;
        exit;
      end;
      with FLevelNodes[n] do
      begin
        JSONObject(Obj).Put(JEP_Operator,AOp);
        if AOp='' then
        begin
          OpCh:=OpCh_None;
          Rank:=0;
        end
        else if AddMode=amOperator then
        begin
          OpCh:=Func[1]; //AddMode;
          Rank:=GetStdOpRank(OpCh,Func);
        end
        else begin
          OpCh:=OpCh_Func;
          Rank:=OpRank[OpCh_Func];
        end;
      end;
    end;
    Dec(FCurNodeLevel);}
    exit;
  end;
CommonCase:
  J:=JSONObject(FLevelNodes[FCurNodeLevel].Obj);
  IsEmpty:=false;
  if (FLevelNodes[FCurNodeLevel].OpKind=jokNone) then
  begin
    J.Put(JEP_Operator,AOp);
    FLevelNodes[FCurNodeLevel].OpKind:=jokOp;
  end
  else begin
    JN:=NewFuncObj(AOp);
    FLevelNodes[FCurNodeLevel+1].Obj:=JN;
    with J do
      Put(JEP_ParamHeader+IntToStr(Length),FLevelNodes[FCurNodeLevel+1].Obj);
    Inc(FCurNodeLevel);
    with FLevelNodes[FCurNodeLevel] do
    begin
      if AOp='' then
      begin
        OpKind:=jokNone;
        //Rank:=0;
      end
      else begin
        OpKind:=jokOp;
      end;
    end;
  end;
  with FLevelNodes[FCurNodeLevel] do
  begin
    Op:=AOp;
    Rank:=ARank;
    BC:=FCurBlockCount;
    SC:=FStatementLevel;
  end;
end;

procedure TJEParser.PushOp1(const Op: String);
begin
  PushOp1(Op,GetOpRank(Op));
end;

procedure TJEParser.PushOp2(const Op: String);
begin
  PushOp2(Op,GetOpRank(Op));
end;

procedure TJEParser.PushOp2(const AOp: String; ARank: Byte);
var
  IsEmpty:Boolean;
  i,n:Integer;
  Z,Z2:TZAbstractObject;
  J,JP,JN:JSONObject;
  mstr:String;
  label CommonCase;
begin
  //是否简单变量 ?
  if FLevelNodes[FCurNodeLevel].Obj.ClassType<>TJEBranch then
  begin
    //简单变量后跟双目操作符
    n:=FCurNodeLevel-1;
    JP:=JSONObject(FLevelNodes[n].Obj); //表达式JSON对象
    //没有操作符的表达式 -- 填入Func
    if FLevelNodes[n].OpKind=jokNone then
    begin
      JP.Put(JEP_Operator,AOp);
      with FLevelNodes[n] do
      begin
        Op:=AOp;
        OpKind:=jokOp;
        Rank:=ARank;
      end;
      //2012-06-13  清除 FCurNodeLevel 层的信息
      with FLevelNodes[FCurNodeLevel] do
      begin
        Op:='';
        OpKind:=jokNone;
        Rank:=0;
        Obj:=nil;
        BC:=0;
        SC:=0;
      end;
      Dec(FCurNodeLevel);
      exit;
    end;
    with FLevelNodes[n] do
    begin
      if (BC>=FCurBlockCount) and (AOp<>'[') and (OpKind<>jokStatement) then  //当前双目操作符和前一个表达式位于相同的括号层次
      begin
        if (OpKind<>jokNone) and (ARank<=Rank) then
        begin
          FCurNodeLevel:=InBlockLevel(ARank); //InBlockLevel; //n;
          goto CommonCase;
        end;
      end;
    end;
    with JP do
    begin
      mstr:=KeyByIndex[Length-1];  //最后一个Key
      Z:=Remove(mstr);
    end;
    with FLevelNodes[FCurNodeLevel] do
    begin
      Obj:=NewFuncObj(AOp);
      Op:=AOp;
      JSONObject(Obj).Put(JEP_Param1,Z);
      JP.Put(mstr,Obj);
      if AOp='' then
      begin
        OpKind:=jokNone;
        Rank:=0;
      end
      else begin
        OpKind:=jokOp;
        Rank:=ARank;
      end;
      BC:=FCurBlockCount;
      SC:=FStatementLevel;
    end;
    exit;
  end
  else begin
    if AOp[1]<>'[' then
    begin
      FCurNodeLevel:=InBlockLevel2(ARank);
      if FLevelNodes[FCurNodeLevel].Obj=nil then  //2012-06-13  空语素后跟双目算子
      begin
        Dec(FCurNodeLevel);
        PushEmptyItem;
        PushOp2(AOp,ARank);
        exit;
      end;
    end
    else
      FCurNodeLevel:=GetExprLevel;  //2011-09-01  A[1][2]
  end;
CommonCase:
  with FLevelNodes[FCurNodeLevel] do
  begin
    //考虑被前置的双目运算符，如 A = -B
    if Obj.ClassType<>TJEBranch then
    begin
      PushEmptyItem;
      PushOp2(AOp,ARank);
      exit;
    end;
    J:=JSONObject(Obj);
  end;
  IsEmpty:=false;
  if FLevelNodes[FCurNodeLevel].OpKind=jokNone then
  begin
    J.Put(JEP_Operator,AOp);
    //FLevelNodes[FCurNodeLevel].OpCh:=AddMode;
    with FLevelNodes[FCurNodeLevel] do
    begin
      Op:=AOp;
      OpKind:=jokOp;
      Rank:=ARank;
      BC:=FCurBlockCount;
      SC:=FStatementLevel;
    end;
  end
  else begin
    JN:=NewFuncObj(AOp);
    FLevelNodes[FCurNodeLevel+1].Obj:=JN;
    //按照左侧优先结合的规律进行重新组合——考虑运算符的优先级
    // not X  =>  (not X) or Y
    // X + Y  =>  ( X + Y ) - Z
    //当前Level的所有者
    if FCurNodeLevel>0 then
      JP:=JSONObject(FLevelNodes[FCurNodeLevel-1].Obj)
    else
      JP:=nil;
    if JP<>nil then
    begin
      n:=JP.Length;
      //在含有前缀的情况下，最后一个Key不是 P + (n-1)  2011-09-20
      mstr:=JP.KeyByIndex[n-1];
      JN.Put(JEP_Param1,JP.Remove(mstr));
      JP.Put(mstr,JN);
    end
    else
      JN.Put(JEP_Param1,J);
    with FLevelNodes[FCurNodeLevel] do
    begin
      Obj:=JN;
      if AOp='' then
      begin
        OpKind:=jokNone;
        Rank:=0;
      end
      else begin
        Op:=AOp;
        OpKind:=jokOp;
        Rank:=ARank;
      end;
      BC:=FCurBlockCount;
      SC:=FStatementLevel;
    end;
  end;
end;

procedure TJEParser.PushOutStrVal(const ValStr: String; AddLnBreak: Boolean);
begin
  if ValStr<>'' then
    PushTypeVal(ValStr,JEPT_EchoStr);
  if AddLnBreak then SetCurToken(':',tkLINEDIV,FCurPos);
end;

procedure TJEParser.PushPerfix(const S: String; Lv: Integer);
var
  Z:JSONArray;
  i:Integer;
begin
  if Lv<0 then Lv:=FCurNodeLevel;
  with TJEBranch(FLevelNodes[Lv].Obj) do
  begin
    Z:=OptJSONArray(JEP_Perfix);
    if Z=nil then    
      Z:=JSONArray.Create
    else
      with Z do
        for i:=0 to length-1 do
          if getString(i)=S then exit;
    Z.put(S);
    Put(JEP_Perfix,Z);
  end;
end;

procedure TJEParser.PushSetValOp;
begin
  PushOp2(JOP_SetValue,GetSetValOpRank);
end;

procedure TJEParser.PushStatement(const S: String);
begin
  PushFunc(S,true);
  StatementBegin;
end;

procedure TJEParser.PushString(const Str: String);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    Put(JEP_ParamHeader+IntToStr(n),JEP_StrParamHeader+Str);
    FCurNodeLevel:=e+1;
    AfterPushItem(ValObjByIndex[n]);
  end;
end;

procedure TJEParser.PushTypeVal(const ValStr: String; TypeCh: Char);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with TJEBranch(FLevelNodes[e].Obj) do
  begin
    n:=Length;
    if TypeCh=#0 then
      Put(JEP_ParamHeader+IntToStr(n),ValStr)
    else
      Put(JEP_ParamHeader+IntToStr(n),JEP_TypeHead+TypeCh+ValStr);
    FCurNodeLevel:=e+1;
    AfterPushTypeVal(ValObjByIndex[n],TypeCh);
  end;
end;

procedure TJEParser.PushVar(const VarStr: String);
var
  e,n:Integer;
begin
  e:=GetExprLevel;
  CheckFuncNoBracket(e);
  with FLevelNodes[e] do
  begin
    if OpKind=jokItems then
    begin
      FCurNodeLevel:=e+1;
      with JSONArray(Obj) do
      begin
        Put(VarStr);
        AfterPushItem(LastItem);
      end;
      exit;
    end;
    with TJEBranch(Obj) do
    begin
      n:=Length;
      Put(JEP_ParamHeader+IntToStr(n),VarStr);
      FCurNodeLevel:=e+1;
      AfterPushItem(ValObjByIndex[n]);
    end;
  end;
end;

procedure TJEParser.RegGlobalEnv;
begin
  if FUserSymbols.IndexOf('\')<0 then
    FUserSymbols.Add('\');
end;

function TJEParser.RegHeadKeywordMethod(const AWord: String; AProc: TObjProc): Integer;
begin
  Result:=RegKeyword(AWord,[ktStatementHead]);
  if Result<0 then exit;
  if High(FStatementProcs)<Result then
    ResizeSPAy(4+Result+Result shr 2);
  FStatementProcs[Result]:=AProc;
end;

function TJEParser.RegHeadKeywordRetVal(const AWord: String;
  AProc: TObjProc): Integer;
begin
  Result:=RegKeyword(AWord,[ktStatementHead,ktRetVal]);
  if Result<0 then exit;
  if High(FStatementProcs)<Result then
    ResizeSPAy(4+Result+Result shr 2);
  FStatementProcs[Result]:=AProc;
end;

class function TJEParser.RegisterParser(AClass: TJEParserClass): Boolean;
var
  mstr:String;
begin
  mstr:=UpperCase(AClass.Lan);
  Result:=Parsers.IndexOf(mstr)<0;
  if Result then
    Parsers.AddObject(mstr,TObject(AClass));
end;

function TJEParser.RegKeyword(const AWord: String; KWTag: TKeywordTags): Integer;
var
  TagObj:TObject;
  TagInt:Integer;
begin
  TagInt:=KeywordTagsToMask(KWTag);
  Result:=FKeywords.AddObject(AWord,TObject(TagInt));
  TagObj:=TObject(TagInt or Result);
  if ktStatementHead in KWTag then
    FHeadKeywords.AddObject(AWord,TagObj);
  if ktPerfix in KWTag then
    FPerfixKWs.AddObject(AWord,TagObj);
  if ktPostfix in KWTag then
    FPostfixKWs.AddObject(AWord,TagObj);
end;

function TJEParser.RegLanFunc(const AWord: String): Integer;
begin
  Result:=FLanFuncs.Add(AWord);
end;

function TJEParser.RegLanObj(const AWord: String): Integer;
begin
  Result:=FLanObjs.Add(AWord);
end;

function TJEParser.RegLanOp(const AWord: String; Op2: Boolean): Integer;
begin
  if Op2 then
    Result:=FLanOp2s.Add(AWord)
  else
    Result:=FLanOp1s.Add(AWord);
end;

function TJEParser.RegLanType(const AWord: String): Integer;
begin
  Result:=FLanTypes.Add(AWord);
end;

function TJEParser.RegLanVal(const AWord: String): Integer;
begin
  Result:=FLanVals.Add(AWord);
end;

function TJEParser.RegUserClass(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetClass;
  Result.Mode:=jemDefine;
end;

function TJEParser.RegUserConst(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetUnknown;
  Result.Mode:=jemConst;
end;

function TJEParser.RegUserConstArray(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetArray;
  Result.ItemType:=jetUnknown;
  Result.Mode:=jemConst;
end;

function TJEParser.RegUserFunc(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetFunction;
  Result.Mode:=jemDefine;
end;

function TJEParser.RegUserSymbol(const AName: String): PJETypeRec;
var
  i:Integer;
begin
  i:=FUserSymbols.IndexOf(AName);
  if i>=0 then
    Result:=PJETypeRec(FUserSymbols.Objects[i])
  else begin
    New(Result);
    Result.OwnerEnv:=0;
    Result.SelfIndex:=FUserSymbols.AddObject(AName,TObject(Result));
  end;
end;

function TJEParser.RegUserVar(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetUnknown;
  Result.Mode:=jemVar;
end;

function TJEParser.RegUserVarArray(const AName: String):PJETypeRec;
begin
  Result:=RegUserSymbol(AName);
  if Result=nil then exit;  
  Result.BasicType:=jetArray;
  Result.ItemType:=jetUnknown;
  Result.Mode:=jemVar;
end;

procedure TJEParser.RenameLastNodeAsBody;
var
  n:Integer;
  mstr:String;
begin
  if FCurNodeLevel<0 then exit;
  if not (FLevelNodes[FCurNodeLevel].Obj is TJEBranch) then exit;
  with TJEBranch(FLevelNodes[FCurNodeLevel].Obj) do
  begin
    n:=length();
    while n>0 do
    begin
      Dec(n);
      mstr:=KeyByIndex[n];
      if (mstr>'p1') and (mstr<'pa') then
      begin
        SetKey(n,JEDN_Body);
        exit;
      end;
    end;
  end;
end;

function TJEParser.ReplaceOpInExpr(ANode: TJENode; const SrcOp, DestOp: String): Integer;
var
  i:Integer;
  ZObj:TJENode;
  OpStr,ValStr:String;
  HasStrVal:Boolean;
begin
  Result:=0;
  if not (ANode is TJEBranch) then exit;
  ZObj:=TJEBranch(ANode).Opt(JEP_Operator);
  if not (ZObj is _String) then exit;
  OpStr:=ZObj.toString;
  if OpStr=SrcOp then
    _String(ZObj).AsString:=DestOp
  else
    if not IsExprOp(OpStr) then exit;
  ZObj:=TJEBranch(ANode).Opt(JEP_Param1);
  Inc(Result,ReplaceOpInExpr(ZObj,SrcOp,DestOp));
  ZObj:=TJEBranch(ANode).Opt(JEP_Param2);
  Inc(Result,ReplaceOpInExpr(ZObj,SrcOp,DestOp));
end;

function TJEParser.ReplaceVarToResult(ANode: TJENode; const VarName: String): String;
var
  SL:TStrings;
  i,n:Integer;
begin
  Result:='';
  if ANode=nil then exit;
  SL:=StatementVarExtract(ANode,'R');
  if SL=nil then exit;
  if SL.IndexOf(JEV_Result)>=0 then
  begin
    n:=0;
    for i:=0 to Pred(SL.Count) do
    begin
      if Length(SL[i])>n then
        n:=Length(SL[i]);
    end;
    Result:='rR'+StringOfChar('R',n-4)+'JER';  // rRRRRJER
  end
  else
    Result:=JEV_ResultRep;
  SL.Free;
  StatementVarReplace(ANode,VarName,Result);  
end;

procedure TJEParser.ResizeSPAy(NewSize: Integer);
var
  Procs:array of TMethod;
  n,m:Integer;
begin
  n:=High(FStatementProcs)+1;
  if NewSize=0 then
  begin
    SetLength(FStatementProcs,NewSize);
    exit;
  end
  else if NewSize=n then
    exit
  else if NewSize<n then
    m:=NewSize
  else
    m:=n;
  SetLength(Procs,m);
  Move(FStatementProcs[0],Procs[0],SizeOf(Procs[0])*m);
  SetLength(FStatementProcs,NewSize);
  Move(Procs[0],FStatementProcs[0],SizeOf(Procs[0])*m);
  SetLength(Procs,0);
end;

procedure TJEParser.SetCurToken(const AStr: String; AKind: TTokenKind; APos: Integer);
begin
  FLastToken:=FCurToken;
  with FCurToken do
  begin
    Token:=AStr;
    if (AKind=tkWORD) and FWordNoCase then
      CaseOKToken:=UpperCase(Token)
    else
      CaseOKToken:=Token;
    StartPos:=APos;
    Kind:=AKind;
    KWIdx1:=0;
  end;
end;

procedure TJEParser.SetSource(const Value: String);
begin
  FSource := Value;
end;

procedure TJEParser.StatementBegin;
begin
  Inc(FStatementLevel);
end;

procedure TJEParser.StatementEnd(BreakLine: Boolean);
begin
  TryTransOneWordStatement;  //2012-05-31
  FCurNodeLevel:=GetStatementLevel;
  Dec(FStatementLevel);
  if BreakLine then
    PushLineBreakOp;
end;

function TJEParser.StatementVarExtract(ANode: TJENode; const VarHead: String;
  Options: TExtractOptions): TStrings;
var
  CaseOKVarHead:String;
  VHLen:Integer;
  procedure ProcOneNode(Node: TJENode);
  var
    i:Integer;
    ZObj:TJENode;
    OpStr,ValStr:String;
    HasStrVal:Boolean;
  begin
    if not (Node is TJEBranch) then
    begin
      if Node is _String then
      begin
        ValStr:=ANode.toString;
        if ValStr='' then exit;
        if not StrIsVar(ValStr) then exit;
        if VarHead<>'' then
        begin
          if FWordNoCase then
          begin
            if UpperCase(Copy(ValStr,1,VHLen))<>CaseOKVarHead then exit
          end
          else
            if Copy(ValStr,1,VHLen)<>CaseOKVarHead then exit;
        end;
        Result.Add(ValStr);
      end
      else if Node is JSONArray then
      begin
        with JSONArray(Node) do
          for i:=0 to Pred(length) do
            ProcOneNode(get(i));
      end;
      exit;
    end;
    ZObj:=TJEBranch(Node).Opt(JEP_Operator);
    if not (ZObj is _String) then exit;
    OpStr:=ZObj.toString;
    if OpStr='.' then
    begin
      if Options=[] then exit;
      if eoIncObj in Options then
        ProcOneNode(TJEBranch(Node).Opt(JEP_Param1));
      if eoIncObjMember in Options then
        ProcOneNode(TJEBranch(Node).Opt(JEP_Param2));
      exit;
    end;
    i:=1; 
    while true do
    begin
      ZObj:=TJEBranch(Node).Opt(JEP_ParamHeader+IntToStr(i));
      if ZObj=nil then break;      
      ProcOneNode(ZObj);
      Inc(i);
    end;
  end;
begin
  Result:=TStringList.Create;
  with TStringList(Result) do
  begin
    Duplicates:=dupIgnore;
    Sorted:=true;
  end;
  if FWordNoCase then
    CaseOKVarHead:=UpperCase(Varhead)
  else
    CaseOKVarHead:=Varhead;
  VHLen:=Length(CaseOKVarHead);
  ProcOneNode(ANode);
end;

function TJEParser.StatementVarReplace(ANode: TJENode; const SrcVar, DestVar: String;
  Options: TExtractOptions): Integer;
var
  CaseOKSrcVar:String;
  procedure ProcOneNode(Node: TJENode);
  var
    i:Integer;
    ZObj:TJENode;
    OpStr,ValStr:String;
    HasStrVal:Boolean;
  begin
    if not (Node is TJEBranch) then
    begin
      if Node is _String then
      begin
        ValStr:=Node.toString;
        if ValStr='' then exit;
        if not StrIsVar(ValStr) then exit;
        if FWordNoCase then
        begin
          if UpperCase(ValStr)<>CaseOKSrcVar then exit
        end
        else
          if ValStr<>CaseOKSrcVar then exit;
        _String(Node).AsString:=DestVar;
        Inc(Result);
      end
      else if Node is JSONArray then
      begin
        with JSONArray(Node) do
          for i:=0 to Pred(length) do
            ProcOneNode(get(i));
      end;
      exit;
    end;
    ZObj:=TJEBranch(Node).Opt(JEP_Operator);
    if not (ZObj is _String) then exit;
    OpStr:=ZObj.toString;
    if OpStr='.' then
    begin
      if Options=[] then exit;
      if eoIncObj in Options then
        ProcOneNode(TJEBranch(Node).Opt(JEP_Param1));
      if eoIncObjMember in Options then
        ProcOneNode(TJEBranch(Node).Opt(JEP_Param2));
      exit;
    end
    else if Copy(OpStr,1,1)=' ' then  //Class,Function等定义操作符
    begin
      ZObj:=TJEBranch(Node).Opt(JEDN_Members);
      ProcOneNode(ZObj);
      ZObj:=TJEBranch(Node).Opt(JEDN_Params);
      ProcOneNode(ZObj);
      ZObj:=TJEBranch(Node).Opt(JEDN_Body);
      ProcOneNode(ZObj);
      exit;
    end;
    i:=1;
    while true do
    begin
      ZObj:=TJEBranch(Node).Opt(JEP_ParamHeader+IntToStr(i));
      if ZObj=nil then break;      
      ProcOneNode(ZObj);
      Inc(i);
    end;
  end;
begin
  Result:=0;
  if (SrcVar=DestVar) or (SrcVar='') then exit;  
  if FWordNoCase then
    CaseOKSrcVar:=UpperCase(SrcVar)
  else
    CaseOKSrcVar:=SrcVar;
  ProcOneNode(ANode);
end;

procedure TJEParser.StatementWithDefEnd;
begin
  PopupDefine;
  StatementEnd;
end;

function TJEParser.VarIsArray: Boolean;
begin
  Result:=false;
end;

function TJEParser.WordIsFunc: Boolean;
begin
  Result:=false;
end;

initialization
  InitTypeLenAy;
  Parsers:=TStringList.Create;

finalization
  FreeTypeLenAy;
  Parsers.Free;

end.
