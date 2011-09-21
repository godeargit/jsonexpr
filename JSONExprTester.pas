{
  Copyright 2009  creation_zy
  creation_zy@sina.com

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation.

  JSON Expression Parser Tester

  Author: Yi Zhang
  Date: 2009-11-21
}

unit JSONExprTester;

interface

uses
  SysUtils, Variants, uJSON, UJSONExpr;

function TestJSONExprParser(out Msg: String): Boolean;

implementation

var
  DebugMsg:String;

procedure TraceLine(Sender: TObject; LineData: TZAbstractObject; const LineVal: Variant);
var
  mstr:String;
begin
  if LineData=nil then exit;
  if LineData.ClassType<>JSONObject then
  begin
    mstr:='('+LineData.toString+')';
  end
  else begin
    mstr:=JSONObject(LineData).OptString(JEP_Operator);
    if mstr=';' then exit;
    mstr:='"'+mstr+'"';
  end;
  DebugMsg:=DebugMsg+'[L]'+mstr+'=>'+VarToStrDef(LineVal,'N/A')+#13#10;
end;

procedure TraceValue(Sender: TObject; const VarName: String; const Val: Variant);
begin
  DebugMsg:=DebugMsg+'[V]'+VarName+':='+VarToStrDef(Val,'N/A')+#13#10;
end;

function TestJSONExprParser(out Msg: String): Boolean;
var
  VHelper:TSimpleVarHelper;
  J:JSONObject;
  mstr,mstr2,s1,s2:String;
  f,f2:Double;
  v:Variant;
  b:Boolean;
begin
  Msg:='';
  Result:=true;
  VHelper:=TSimpleVarHelper.Create;
  with TJSONExprParser.Create do
  begin
    AddVarHelper(VHelper);
    //Expr Parse Test
    mstr:='';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='-X+2.5';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"+",p1:{op:"-",p1:0,p2:"X"},p2:2.5}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='Sin(Y)';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"SIN",p1:"Y"}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='2 + X * Sin(Y)';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"+",p1:2,p2:{op:"*",p1:"X",p2:{op:"SIN",p1:"Y"}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      if (JSONToExpr(J)<>'(2+(X*SIN(Y)))') or (JSONToExpr(J,0)<>'2+X*SIN(Y)') then
      begin
        Result:=false;
        Msg:=Msg+#13#10+JSONToExpr(J)+#13#10+JSONToExpr(J,0);
      end;
      Free;
    end;
    mstr:='Z+X * 2-Y';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"-",p1:{op:"+",p1:"Z",p2:{op:"*",p1:"X",p2:2}},p2:"Y"}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      if (JSONToExpr(J)<>'((Z+(X*2))-Y)') or (JSONToExpr(J,MaxInt)<>'(Z+X*2-Y)') then
      begin
        Result:=false;
        Msg:=Msg+#13#10+JSONToExpr(J)+#13#10+JSONToExpr(J,MaxInt);
      end;
      Free;
    end;
    mstr:='2+Z+X*2/Y'; mstr2:='2+Z+(X*2)/Y';
    s1:=ExprToJSONStr(mstr);
    s2:=ExprToJSONStr(mstr2);
    if s1<>s2 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+mstr+' =>'#13#10+s1+#13#10+mstr2+' =>'+#13#10+s2;
    end;
    mstr:='2+Z/(X*2)/Y'; mstr2:='2+(Z/(X*2))/Y';
    s1:=ExprToJSONStr(mstr);
    s2:=ExprToJSONStr(mstr2);
    if s1<>s2 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+mstr+' =>'#13#10+s1+#13#10+mstr2+' =>'+#13#10+s2;
    end;
    mstr:='Z*(X.Max+0.5)-dbo.Y.Add(2)';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"-",p1:{op:"*",p1:"Z",p2:{op:"+",p1:{op:".",p1:"X",p2:"Max"},p2:0.5}},p2:{op:".",p1:{op:".",p1:"dbo",p2:"Y"},p2:{op:"ADD",p1:2}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      if JSONToExpr(J)<>'((Z*((X.Max)+0.5))-((dbo.Y).ADD(2)))' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
      end;
      if JSONToExpr(J,0)<>'Z*(X.Max+0.5)-dbo.Y.ADD(2)' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
      end;
      Free;
    end;
    mstr:='A IS NOT NULL';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"IS",p1:"A",p2:{op:"NOT",p1:null}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='Fn_1(-A)+(-X)';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"+",p1:{op:"FN_1",p1:{op:"-",p1:0,p2:"A"}},p2:{op:"-",p1:0,p2:"X"}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString);
      end;
      Free;
    end;
    mstr:='IF(X>=0,X,Y-X+Power(Ln(Z),3))-100';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"-",p1:{op:"IF",p1:{op:">=",p1:"X",p2:0},p2:"X",p3:{op:"+",p1:{op:"-",p1:"Y",p2:"X"},p2:{op:"POWER",p1:{op:"LN",p1:"Z"},p2:3}}},p2:100}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      s1:=JSONToExpr(J);
      if s1<>'(IF((X>=0),X,((Y-X)+POWER(LN(Z),3)))-100)' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      Free;
    end;
    mstr:='(Round(PI*$v1)^~36*2)-100';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"-",p1:{op:"^",p1:{op:"ROUND",p1:{op:"*",p1:"PI",p2:"$v1"}},p2:{op:"*",p1:{op:"~",p1:36},p2:2}},p2:100}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString());
      end;
      s1:=JSONToExpr(J,0);
      if s1<>'(ROUND(PI*$v1)^~36*2)-100' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      Free;
    end;
    mstr:='Name1 in ( ''M''+''ike'' , '''', ''Nike'' , @@Name )';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"IN",p1:"Name1",p2:{op:"(",p1:{op:"+",p1:"''M",p2:"''ike"},p2:"''",p3:"''Nike",p4:"@@Name"}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      s1:=JSONToExpr(J,0);
      if s1<>'Name1 IN (''M''+''ike'','''',''Nike'',@@Name)' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      Free;
    end;
    mstr:='-9*(Sqrt(36+ABS(Sin(PI*0.5)*2))-(300*Money))';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"*",p1:-9,p2:{op:"-",p1:{op:"SQRT",p1:{op:"+",p1:36,p2:{op:"ABS",p1:{op:"*",p1:{op:"SIN",p1:{op:"*",p1:"PI",p2:0.5}},p2:2}}}},p2:{op:"*",p1:300,p2:"Money"}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    VHelper.Put('Z',3.5);
    VHelper.PutNull('Y');
    mstr:='(X>Y) or (Z>(X+Y)) or not (Z<-1)';
    UseVarHelperOnParse:=true;
    with ExprToJSON(mstr{,VHelper}) do
    begin
      if toString<>'{op:"OR",p1:{op:"OR",p1:{op:">",p1:"X",p2:null},p2:{op:">",p1:3.5,p2:{op:"+",p1:"X",p2:null}}},p2:{op:"NOT",p1:{op:"<",p1:3.5,p2:-1}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    UseVarHelperOnParse:=false;
    mstr:='(100*{op:"AVG",p1:"Qty*Price"})/{op:"MAX",p1:"Income",at:"Year"}';
    with ExprToJSON(mstr) do
    begin
      if System.Length(toString)<40 then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='Left(X,4)=''YEAR'' OR ((Y Like ''%2009%'') AND (X IS NULL))';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"OR",p1:{op:"=",p1:{op:"LEFT",p1:"X",p2:4},p2:"''YEAR"},p2:{op:"AND",p1:{op:"LIKE",p1:"Y",p2:"''%2009%"},p2:{op:"IS",p1:"X",p2:null}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      if JSONToExpr(J)<>'((LEFT(X,4)=''YEAR'') OR (LIKE(Y,''%2009%'') AND (X IS null)))' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
      end;
      Free;
    end;
    mstr:='())X T^!15,Y-3 999*not False-Z >';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if System.Length(toString)<40 then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
    mstr:='DatePart(day,getdate())';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"DATEPART",p1:"day",p2:{op:"GETDATE"}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      s1:=JSONToExpr(J);
      if s1<>'DATEPART(day,GETDATE())' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      Free;
    end;
    mstr:='A[1][2+3*4,B[5].C(6,D)]';
    J:=ExprToJSON(mstr);
    if JSONToExpr(J,0)<>mstr then
    begin
      Result:=false;
      Msg:=Msg+#13#10+mstr+' => '+JSONToExpr(J,0);
    end;
    J.Free;
    mstr:='PUBLIC VIRTUAL FUNCTION ADD2(A,B):=(A+B)';
    J:=ExprToJSON(mstr);
    with J do
    begin
      s1:=JSONToExpr(J);
      if s1<>mstr then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      Free;
    end;
    Free;
  end;
  //Eval Test
  VHelper.Clean;
  VHelper.Put('X',3);
  VHelper.Put('PI',3.14159265359);
  with TJSONExprParser.Create do
  begin
    AddVarHelper(VHelper);
    mstr:='(X*16 ^ 32)+0.5';
    J:=ExprToJSON(mstr);
    if not EvalNumber(J,f) then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'EvalNumber failed! =>'#9+J.toString);
    end
    else begin
      f2:=((3*16) xor 32)+0.5;
      if f<>f2 then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(Format('%s'#13#10'=>'#13#10'%s'#13#10'=> %f  !=  (%f)',[mstr,J.toString,f,f2]));
      end;
    end;
    J.Free;
    mstr:='X+2 in (null,2<>3,2+3*1)';
    UseVarHelperOnParse:=true;
    J:=ExprToJSON(mstr{,VHelper});
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    mstr:='(X=3) in (null,2<>3,2+3*1)';
    J:=ExprToJSON(mstr{,VHelper});
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    UseVarHelperOnParse:=false;
    //Cycle test
    mstr:='n:=0; For(i:=1, Inc(i), i<=100, n:=n+i); n';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>5050 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    mstr:='n:=1; i:=2; While(i<10, n:=n*i; i:=i+2); n';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>384 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    VHelper.PutNull('Y');
    mstr:='IF(Y IS not NULL, 3*0.5+Y, 5.875-(9<<2)*X)';
    UseVarHelperOnParse:=true;
    J:=ExprToJSON(mstr{,VHelper});
    v:=Eval(J);
    if v<>-102.125 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J,0)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    FreeAndNil(J);
    UseVarHelperOnParse:=false;
    mstr:='X:=X^X; Y:=(12|9)^(15%6); X:=(8+X)\5;'+mstr;
    J:=ExprToJSON(mstr);  //此处不应当用变量代换
    v:=Eval(J);
    if v<>15.5 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //无参数函数跟加减表达式解析测试
    mstr:='(Now()+1)-(Now()-1)';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>2 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    // 以 + 开头的表达式
    mstr:='+1.25';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>1.25 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    // BETWEEN 表达式
    mstr:='BETWEEN(''B'',''A1'',''X'') AND BETWEEN(100,100,1e+3)';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>true then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    // in 不同类型表达式
    mstr:='12 in (''B''+''A'', false, Z, 18-6.0)';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>true then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //访问数组元素
    mstr:='A:=(1,2,4,8); B:=A[2];';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>4 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    mstr:='A:=((1,10),2,4,8); B:=A[3-2-1,1];';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>10 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //collection and array test
    mstr:='C:=(1,1+2); a:=(20,1+1); PRINT(1+a[1]); IF((1+a[1]) in C, a[0]-(C[1]*2), -a[1])';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>14 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //Eval test
    mstr:='A:=''99'';Eval(''10''+A+''.0-100+2'');';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>1001 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //  ?= test
    mstr:='A:=33;B:=(A/=3);B+=100';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>111 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    //Debug example
    DebugMsg:='';
    TraceOnLine:=true;
    OnLineComplete:=TraceLine;
    VHelper.TraceOnSet:=true;
    VHelper.OnTrace:=TraceValue;
    mstr:='X1:=1; Y:=IF(X IS NULL,0,X1^5);Z:=9*IF(X1+2<=(3+Y),X2:=X1<<3,(Y:=X1*10; X2:=Y^100));(Y+Z)*X2';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>608 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+J.ToString2(2)+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    if DebugMsg<>'' then
      Msg:=Msg+#13#10'Debug Message:'#13#10+DebugMsg;
    J.Free;
    DebugMsg:='';
    //String expression, Special variable name.
    mstr:='Str  ''~1'':=#13#10''Hello World!''#9; 字符串2:=''你好'' #32+''世界'' ''!''; Result:=Len(Str''~1'')>Len(字符串2);';
    J:=ExprToJSON(mstr);
    try
      v:=Eval(J);
      b:=(v<>True) or (v<>VHelper.GetVarDef('Result',false));
    except
      b:=true;
    end;
    if b then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+J.ToString2(2)+#13#10+JSONToExpr(J)+#13#10+JSONToExpr(J,0)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    if DebugMsg<>'' then
      Msg:=Msg+#13#10'Debug Message:'#13#10+DebugMsg;
    J.Free;
    DebugMsg:='';
    Free;
  end;
  J:=JSONObject.Create;
  VHelper.ValExport(J);
  Msg:=Msg+#13#10+J.ToString;
  J.Free;
  VHelper.Free;
  DebugMsg:='';
end;

end.
