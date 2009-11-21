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

function TestJSONExprParser(out Msg: String): Boolean;
var
  VHelper:TSimpleVarHelper;
  J:JSONObject;
  mstr:String;
  f,f2:Double;
  v:Variant;
begin
  Msg:='';
  Result:=true;
  VHelper:=TSimpleVarHelper.Create;
  with TJSONExprParser do
  begin
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
      if JSONToExpr(J)<>'2+(X*SIN(Y))' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
      end;
      Free;
    end;
    mstr:='Z+X * 2-Y';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"-",p1:{op:"+",p1:"Z",p2:{op:"*",p1:"X",p2:2}},p2:"Y"}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
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
      if JSONToExpr(J)<>'(Z*((X.Max)+0.5))-((dbo.Y).ADD(2))' then
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
    mstr:='Fn_1(-A)-(-X)';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"-",p1:{op:"FN_1",p1:{op:"-",p1:0,p2:"A"}},p2:{op:"-",p1:0,p2:"X"}}' then
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
      if JSONToExpr(J)<>'IF((X>=0),X,((Y-X)+POWER(LN(Z),3)))-100' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
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
      if JSONToExpr(J)<>'(ROUND(PI*$v1)^(~(36)*2))-100' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
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
      if JSONToExpr(J)<>'Name1 IN ((''M''+''ike''),'''',''Nike'',@@Name)' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
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
    with ExprToJSON(mstr,VHelper) do
    begin
      if toString<>'{op:"OR",p1:{op:"OR",p1:{op:">",p1:"X",p2:null},p2:{op:">",p1:3.5,p2:{op:"+",p1:"X",p2:null}}},p2:{op:"NOT",p1:{op:"<",p1:3.5,p2:-1}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      Free;
    end;
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
      if JSONToExpr(J)<>'(LEFT(X,4)=''YEAR'') OR (LIKE(Y,''%2009%'') AND (X IS null))' then
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
      if JSONToExpr(J)<>'DATEPART(day,GETDATE())' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(JSONToExpr(J));
      end;
      Free;
    end;
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
    J:=ExprToJSON(mstr,VHelper);
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    mstr:='(X=3) in (null,2<>3,2+3*1)';
    J:=ExprToJSON(mstr,VHelper);
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    VHelper.PutNull('Y');
    mstr:='IF(Y IS not NULL, 3*0.5+Y, 5.875-(9<<2)*X)';
    J:=ExprToJSON(mstr,VHelper);
    v:=Eval(J);
    if v<>-90.375 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    mstr:='X:=X^X; Y:=(12|9)^(15%6); X:=(8+X)\5;'+mstr;
    J:=ExprToJSON(mstr);  //�˴���Ӧ���ñ�������
    v:=Eval(J);
    if v<>14.5 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    J.Free;
    Free;
  end;
  J:=JSONObject.Create;
  VHelper.ValExport(J);
  Msg:=Msg+#13#10+J.ToString;
  J.Free;
  VHelper.Free;
end;

end.