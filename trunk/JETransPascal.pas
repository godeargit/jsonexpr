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
    function TransIF(JObj: TJEBranch; Ident: Integer):String; override;
  public
    class function Lan:ShortString; override;
    function TransCommonOp(JObj: TJEBranch; const Op: String;
      Ident: Integer; PrnRank: Integer=-1):String; override;
    function PackSrc(const Source: String; out WordCount: Integer):String; override;
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

class function TJEPascalParser.Lan: ShortString;
begin
  Result:='Pascal';
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

function TJEPascalParser.TransCommonOp(JObj: TJEBranch; const Op: String;
  Ident, PrnRank: Integer): String;
var
  Ch:Char;
begin
  case Length(Op) of
    1:
    begin
      if Op[1] in ['%','\','&','|','^','!'] then
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
  Result:=inherited TransCommonOp(JObj,Op,PrnRank);
end;

{
test:
  1+IF(A<B,IF(B,100,0.5)*1.5,IF(A<>C,2-3,4+5))
}
function TJEPascalParser.TransIF(JObj: TJEBranch; Ident: Integer): String;
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
    Result:=Result+#13#10'begin'#13#10+AddIdent(Str1)+';'#13#10'end';
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
    Result:=Result+#13#10'else begin'#13#10+AddIdent(Str1)+';'#13#10'end';
  end;
  if TmpVar<>'' then
  begin
    Set_Before(Result);
    Result:=TmpVar;
  end;
end;

initialization
  InitSingleOp2Func;
  TJEParser.RegisterParser(TJEPascalParser);

end.
