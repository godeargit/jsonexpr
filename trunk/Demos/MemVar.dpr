program MemVar;

{$APPTYPE CONSOLE}

{$D+}

uses
  SysUtils,
  Classes,
  Variants,
  uJSON in '..\uJSON.pas',
  UJSONExpr in '..\UJSONExpr.pas';

//Global Variable
var
  A,B:Integer;
  C:Char;
  F1,F2:Double;
  B1,B2:Boolean;
  V:Variant;

procedure TestMemVarExpr;
var
  Parser:TJSONExprParser;
  VHelper:TMemVarHelper;
  I,J:Integer;
  X,Y:Double;
  JObj,JExpr:JSONObject;
  ExprStr:String;
  v:Variant;
begin
  I:=10;
  X:=108000;
  Y:=84000;
  Parser:=TJSONExprParser.Create;
  VHelper:=TMemVarHelper.Create;
  Parser.AddVarHelper(VHelper);
  VHelper.RegInt('A',@A);
  VHelper.RegInt('B',@B);
  VHelper.RegChar('C',@C);
  VHelper.RegInt('I',@I);
  VHelper.RegInt('J',@J);
  VHelper.RegDouble('F1',@F1);
  VHelper.RegDouble('F2',@F2);
  VHelper.RegDouble('X',@X);
  VHelper.RegDouble('Y',@Y);
  VHelper.RegBool('B1',@B1);
  VHelper.RegBool('B2',@B2);
  VHelper.Sort;
  JObj:=JSONObject.Create;
  VHelper.ValExport(JObj);
  Writeln('Init Memory Variables:');
  Writeln(JObj.toString2(2));
  ExprStr:='F1:=F1*(X-Y); C:='')'';B1:=True; B2:=b2 xor not B1; A:=A+B; IF(B1 xor B2,X:=Y+J,(A:=B|I)); IF((B=A),(F1:=(F2-X)*100;I:=I+100;J:=J-I),IF(B1 and (X>F2+3),A:=1-X,F2:=1+X))';
  Writeln('Expression: '#13#10+ExprStr);
  repeat
    JExpr:=nil;
    try
      JExpr:=Parser.ExprToJSON(ExprStr);
      Writeln('JSONToExpr:'#13#10+Parser.JSONToExpr(JExpr,0));
      try
        v:=Parser.Eval(JExpr);
        Writeln('Result value is:'#13#10+VarToStrDef(v,'N/A'));
      except
        Writeln('Expression eval failed!');
      end;
      VHelper.ValExport(JObj);
      Writeln('Variables now:');
      Writeln(JObj.toString2(2));
      Writeln('');
    except
      Writeln('Expression parse failed!');
    end;
    JExpr.Free;
    Writeln('Please enter an expression to evaluate(eg: X:=X-J*365 ), or nothing to exit :');
    Readln(ExprStr);
  until ExprStr='';
  Parser.Free;
  VHelper.Free;
  JObj.Free;
end;

begin
  A:=10; B:=123;
  C:='!';
  F1:=-9; F2:=7.875;
  B1:=true;
  TestMemVarExpr;
  Writeln(Format('Final global vars:  A=%d, B=%d, F1:=%f, F2=%f, B1=%s, B2=%s',
    [A,B,F1,F2,BoolToStr(B1,true),BoolToStr(B2,true)]));
  Writeln('Press Enter key to exit.');
  Readln;
end.
