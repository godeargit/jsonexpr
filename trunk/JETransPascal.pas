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
  JPascal_SHR='SHR';
  JPascal_SHL='SHL';
type
  TJETransPascal=class(TJETranslater)
  protected
    function GetOpRank(const Op: String):Byte; override;
    function TransIF(JObj: TJEBranch):String; override;
  public
    class function Language:ShortString; override;
    function TransCommonOp(JObj: TJEBranch; const Op: String; PrnRank: Integer=-1):String; override;
  end;

implementation

{
PASCAL��������ȼ�
���ļ�

��Ŀ����� (������ȼ�)
@ ȡ���������ĵ�ַ(����һ��ָ��)
not �߼�ȡ����λȡ��

�˳�����λ�����
* ��˻򼯺Ͻ���
/ �������
div �������
mod ȡģ (�������������)
as �������н׶�����ת�� (RTTI�����)
and �߼���λ���
shl ��λ����
shr ��λ����

�Ӽ������
+ ��ӡ����ϲ������ַ������ӻ�ָ������һ��ƫ����
- ��������ϲ��ָ�����һ��ƫ����
or �߼���λ������
xor �߼���λ�������

��ϵ���Ƚ������(������ȼ�)
= �ж��Ƿ����
<> �ж��Ƿ����
< �ж��Ƿ�С��
> �ж��Ƿ����
<= �ж��Ƿ�С�ڻ����,���Ƿ���һ�����ϵ��Ӽ�
>= �ж��Ƿ���ڻ����,���Ƿ���һ�����ϵĸ���
in �ж��Ƿ��Ǽ��ϳ�Ա
is �ж϶����Ƿ����ͼ��� (��һ��RTTI�����)
}

{ TJETransPascal }

function TJETransPascal.GetOpRank(const Op: String): Byte;
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

class function TJETransPascal.Language: ShortString;
begin
  Result:='Pascal';
end;

function TJETransPascal.TransCommonOp(JObj: TJEBranch; const Op: String;
  PrnRank: Integer): String;
var
  Ch:Char;
begin
  case Length(Op) of
    1:
    begin
      if Op=StdJEOps[jeopMod] then
      begin
        Result:=Trans_Mid(JObj,JPascal_Mod,GetOpRank(Op),PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopDiv] then
      begin
        Result:=Trans_Mid(JObj,JPascal_DIV,GetOpRank(Op),PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopAnd] then
      begin
        Result:=Trans_Mid(JObj,JPascal_AND,GetOpRank(Op),PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopOr] then
      begin
        Result:=Trans_Mid(JObj,JPascal_OR,GetOpRank(Op),PrnRank);
        exit;
      end
      else if Op=StdJEOps[jeopXor] then
      begin
        Result:=Trans_Mid(JObj,JPascal_XOR,GetOpRank(Op),PrnRank);
        exit;
      end
    end;
    2:
    begin
      if Op=StdJEOps[jeopShl] then
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
  end;
  Result:=inherited TransCommonOp(JObj,Op,PrnRank);
end;

{
test:
  1+IF(A<B,IF(B,100,0.5)*1.5,IF(A<>C,2-3,4+5))
}
function TJETransPascal.TransIF(JObj: TJEBranch): String;
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
  Gather_BeforeAfter;
  Result:='if '+Str0+' then';
  NVal:=NodeNeedVal;
  //��ҪIF���ʽ�ķ���ֵ  X := IF( ?, A, B )    Y:=IF(?,X+IF(?,1,2),3)+100
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
  SetNextNeedVal(true);
  Str1:=TransANode(MyObj,0);
  if Str1<>'' then
  begin
    if P<>nil then
    begin
      Str1:=LStr+JOP_SetValue+Str1;
    end;
    Combine_SubBeforeAfter(Str1);
    Clear_BeforeAfter;
    Result:=Result+#13#10'begin'#13#10+AddIdent(Str1)+';'#13#10'end';
  end;
  MyObj:=JObj.Opt(JEP_Param3);
  SetNextNeedVal(true);
  Str1:=TransANode(MyObj,0);
  if Str1<>'' then
  begin
    if P<>nil then
    begin
      Str1:=LStr+JOP_SetValue+Str1;
    end;
    Combine_SubBeforeAfter(Str1);
    Result:=Result+#13#10'else begin'#13#10+AddIdent(Str1)+';'#13#10'end';
  end;
  if TmpVar<>'' then
  begin
    Set_Before(Result);
    Result:=TmpVar;
  end;
end;

initialization
  TJETranslater.RegisterTranslater(TJETransPascal);

end.
