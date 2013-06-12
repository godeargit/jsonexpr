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

//{$DEFINE SUPEROBJECT}

uses
  SysUtils, Variants, {$IFDEF SUPEROBJECT}SuperObject{$ELSE}uJSON{$ENDIF}, UJSONExpr, JEParser;

function TestJSONExprParser(out Msg: String): Boolean;

implementation

var
  DebugMsg:String;

procedure TraceLine(Sender: TObject; LineData: TJSONLeaf; const LineVal: Variant);
var
  mstr:String;
begin
  if LineData=nil then exit;
  if LineData.{$IFDEF SUPEROBJECT}DataType<>stObject{$ELSE}ClassType<>JSONObject{$ENDIF} then
  begin
    mstr:='('+LineData.toString+')';
  end
  else begin
    mstr:=TJSONObj(LineData).OptString(JEP_Operator);
    if mstr=';' then exit;
    mstr:='"'+mstr+'"';
  end;
  DebugMsg:=DebugMsg+'[L]'+mstr+'=>'+VarToStrDef(LineVal,'N/A')+#13#10;
end;

procedure TraceValue(Sender: TObject; const VarName: String; const Val: Variant);
begin
  DebugMsg:=DebugMsg+'[V]'+VarName+':='+VarToStrDef(Val,'N/A')+#13#10;
end;

function CheckParseText(const Text: String; var Msg: String;
  AParser: TJSONExprParser):Boolean;
var
  J:TJSONObj;
  s1:String;
begin
  Result:=true;
  J:=AParser.ExprToJSON(Text);
  with J do
  begin
    s1:=AParser.JSONToExpr(J,0);
    if s1<>Text then
    begin
      Result:=false;
      Msg:=Msg+#13#10+Text+' => '+s1;
    end;
    {$IFNDEF SUPEROBJECT}Free{$ENDIF};
  end;
end;

function TestJSONExprParser(out Msg: String): Boolean;
var
  AParser: TJSONExprParser;
  ABasicParser, APhpJEParser, APascalJEParser: TJEParser;
  BasicJEPC, PhpJEPC, PascalJEPC: TJEParserClass;
  VHelper:TSimpleVarHelper;
  J:TJSONObj;
  mstr,mstr2,s1,s2:String;
  f,f2:Double;
  v:Variant;
  b:Boolean;
  Old_KeepComment:Boolean;
  procedure TestScriptVal(const Src: String; AVal: Variant);
  var
    J:TJSONObj;
  begin
    with AParser do
    begin
      J:=ExprToJSON(Src);
      v:=Eval(J);
    end;
    if v<>AVal then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(Src+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
  end;
  procedure TestLanScript(const Src: String; AVal: Variant; VName: String='');
  var
    b:Boolean;
    J:TJSONObj;
  begin
    ABasicParser.Source:=Src;
    J:=ABasicParser.DoParse;
    try
      v:=AParser.Eval(J);
      if VName<>'' then
        VHelper.GetVar(VName,v);
      b:=(v=AVal);
    except
      b:=false;
    end;
    if not b then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(Src+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
  end;
  procedure CheckLanTrans(SrcLan, DestLan: TJEParser; const Src, Dest: String);
  var
    b:Boolean;
    J:TJSONObj;
    OutTxt,mstr,mstr2:String;
    n:Integer;
  begin
    if (SrcLan=nil) or (DestLan=nil) then exit;
    SrcLan.Source:=Src;
    J:=nil;
    try
      J:=SrcLan.DoParse;
      b:=true;
    except
      b:=false;
    end;
    if not b then
    begin
      Result:=false;
      Msg:=Msg+#13#10'CheckLanTrans'#13#10+Src+#13#10'Parse failed.';
    end
    else begin
      try
        OutTxt:=DestLan.TranslateJETree(J);
      except
        OutTxt:='';
      end;
      if OutTxt<>Dest then
      begin
        mstr:=DestLan.PackSrc(OutTxt,n);
        if mstr<>Dest then
        begin
          mstr2:=DestLan.PackSrc(Dest,n);
          if mstr<>mstr2 then
          begin
            Result:=false;
            Msg:=Msg+#13#10'CheckLanTrans:'#13#10+Src+#13#10'==>'#13#10+mstr+#13#10'<>'#13#10+Dest;
          end;
        end;        
      end;
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
  end;
begin
  Msg:='';
  Result:=true;
  SimpleJSON:=true; //2012-08-23
  VHelper:=TSimpleVarHelper.Create;
  AParser:=TJSONExprParser.Create;
  with AParser do
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
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
    end;
    mstr:='-X+2.5';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"+",p1:{op:"-",p1:"'+JE_EmptyItemStr+'",p2:"X"},p2:2.5}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
    end;
    mstr:='Sin(Y)';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"SIN",p1:"Y"}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
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
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
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
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
    end;
    mstr:='A IS NOT NULL';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"IS",p1:"A",p2:{op:"NOT",p1:null}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
    end;
    mstr:='Fn_1(-A)+(-X)';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"+",p1:{op:"FN_1",p1:{op:"-",p1:"'+JE_EmptyItemStr+'",p2:"A"}},p2:{op:"-",p1:"'+JE_EmptyItemStr+'",p2:"X"}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString);
      end;
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
    end;
    mstr:='IIF(X>=0,X,Y-X+Power(Ln(Z),3))-100';
    J:=ExprToJSON(mstr);
    with J do
    begin
      if toString<>'{op:"-",p1:{op:"IIF",p1:{op:">=",p1:"X",p2:0},p2:"X",p3:{op:"+",p1:{op:"-",p1:"Y",p2:"X"},p2:{op:"POWER",p1:{op:"LN",p1:"Z"},p2:3}}},p2:100}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      s1:=JSONToExpr(J);
      if s1<>'(IIF((X>=0),X,((Y-X)+POWER(LN(Z),3)))-100)' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+mstr+' => '+s1;
      end;
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
    end;
    mstr:='-9*(Sqrt(36+ABS(Sin(PI*0.5)*2))-(300*Money))';
    with ExprToJSON(mstr) do
    begin
      if toString<>'{op:"*",p1:-9,p2:{op:"-",p1:{op:"SQRT",p1:{op:"+",p1:36,p2:{op:"ABS",p1:{op:"*",p1:{op:"SIN",p1:{op:"*",p1:"PI",p2:0.5}},p2:2}}}},p2:{op:"*",p1:300,p2:"Money"}}}' then
      begin
        Result:=false;
        Msg:=Msg+#13#10+(mstr+#13#10'=>'#9+toString2(2));
      end;
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free{$ENDIF};
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
      {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
    end;
    mstr:='include ''AAA.asp'';include ''const.asp'';DIM i';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='1..2.34';   //Range
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='A(1,(B,C))';  //参数中的集合  2011-09-23
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='A:=[1,(2+3)*4,5+B[6],7]';  //2011-09-24
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='DEAL(Map[1][2])';  //2011-09-25
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='LOG(FLY();A[1]:=2)';  //2011-09-25
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='(A,B,C).FUNC1(D[1],E.F)';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='B-=A++';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='A[1][2+3*4,B[5].C(6,D)]';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='PUBLIC VIRTUAL FUNCTION ADD2(A,B):=A+B;';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='PROCEDURE TPlane.FLY(CONST Aim:OBJECT,OUT Cost)';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='FOO(X;OUT VAR Y)';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    mstr:='A:=-10;--A;A++;-A';
    if not CheckParseText(mstr,Msg,AParser) then Result:=false;
    {$IFNDEF SUPEROBJECT}Free;{$ENDIF}
    if Result then
      Msg:=Msg+#13#10'Parse test OK.';
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
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    mstr:='X+2 in (null,2<>3,2+3*1)';
    UseVarHelperOnParse:=true;
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    UseVarHelperOnParse:=false;
    mstr:='i:=0;c:=0;n:=Times(10,i+=4;if(i%3=0,continue,i>30,break);c++;i++));c*=n';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>21 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    mstr:='A:=3;B:=A++;B-=(A++);B--';
    UseVarHelperOnParse:=false;
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>-2 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    mstr:='(X=3) in (null,2<>3,2+3*1)';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if not v then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    UseVarHelperOnParse:=false;
    //Cycle test
    TestScriptVal('n:=0; For(i:=1, i<=100, Inc(i), n:=n+i); n',5050);
    TestScriptVal('n:=0; ForTo(i:=10, 7, -1, n+=i); n',34);
    TestScriptVal('n:=1; i:=2; While(i<10, n:=n*i; i:=i+2); n',384);
    TestScriptVal('n:=1; i:=2; WhileNot(i>=10, n:=n*i; i:=i+2); n',384);
    TestScriptVal('n:=1; i:=2; Loop(n:=n*i; i:=i+2, i<5); n',8);
    VHelper.PutNull('Y');
    mstr:='IIF(Y IS not NULL, 3*0.5+Y, 5.875-(9<<2)*X)';
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
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    // ";" 前无语句
    TestScriptVal('z:=2;if(1>0,;a:=3,b:=4;z:=5);a*z=6',true);
    //无参数函数跟加减表达式解析测试
    TestScriptVal('(Now()+1)-(Now()-1)',2);
    // 以 + 开头的表达式
    TestScriptVal('+1.25',1.25);
    // BETWEEN 表达式
    TestScriptVal('BETWEEN(''B'',''A1'',''X'') AND BETWEEN(100,100,1e+3)',true);
    // in 不同类型表达式
    TestScriptVal('12 in (''B''+''A'', false, Z, 18-6.0)',true);
    //访问数组元素
    TestScriptVal('A:=(1,2,4,8); B:=A[2];',4);
    TestScriptVal('A:=((1,10),2,4,8); B:=A[3-2-1,1];',10);
    //collection and array test
    TestScriptVal('C:=(1,1+2); a:=(20,1+1); PRINT(1+a[1]); IIF((1+a[1]) in C, a[0]-(C[1]*2), -a[1])',14);
    //2013-04-22  Prop test.
    TestScriptVal('P.X:=10;P.Y:=9-P.X/2; S:=P.((X+2)*Y)',48);
    //2013-04-23  中文变量
    TestScriptVal('上证:=12345679; 深成:=上证*9',111111111);
    //Eval test
    TestScriptVal('A:=''99'';Eval(''10''+A+''.0-100+2'');',1001);
    // ?= test
    TestScriptVal('A:=33;B:=(A/=3);B+=100',111);
    //2012-06-22  Test expr with multi block end.
    TestScriptVal('(3-(6))/2',-1.5);
    TestScriptVal('(5/4+(2-(3-(6+4/5))))/(3*(6-2)*(2-7))',-0.1175);
    //Debug example
    DebugMsg:='';
    TraceOnLine:=true;
    OnLineComplete:=TraceLine;
    VHelper.TraceOnSet:=true;
    VHelper.OnTrace:=TraceValue;
    mstr:='X1:=1; Y:=IIF(X IS NULL,0,X1^5);Z:=9*IIF(X1+2<=(3+Y),X2:=X1<<3,(Y:=X1*10; X2:=Y^100));(Y+Z)*X2';
    J:=ExprToJSON(mstr);
    v:=Eval(J);
    if v<>608 then
    begin
      Result:=false;
      Msg:=Msg+#13#10+(mstr+#13#10+J.ToString2(2)+#13#10+JSONToExpr(J)+#13#10'Eval =>'#9+VarToStrDef(v,'N/A'));
    end;
    if DebugMsg<>'' then
      Msg:=Msg+#13#10'Debug Message:'#13#10+DebugMsg;
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    DebugMsg:='';
    //String expression, Special variable name.
    mstr:='"Str~1":=#13#10''Hello World!''#9; 字符串2:=''你好'' #32+''世界'' ''!''; Result:=Len("Str~1")>Len(字符串2);';
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
    {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
    if Result then
      Msg:=Msg+#13#10'Eval test OK.';
    DebugMsg:='';
    BasicJEPC:=TJEParser.GetParserForLan('Basic');
    if BasicJEPC<>nil then
    begin
      ABasicParser:=BasicJEPC.Create;
      //Test If then
      TestLanScript('a=10 : b=2'#13#10'if a>b and (a<=b*5) then'#13#10
        +'b=a mod 3'#13#10
        +'end if'#13#10
        +'x=(a-1)*b',9,'x');
      //Test For to step then
      TestLanScript('n=0'#13#10'a=3'#13#10
        +'for i=1 to 10 step a'#13#10
        +'  n=n+i'#13#10
        +'next'#13#10
        +'x=a*n',66,'x');
      //Test logic
      TestLanScript('b1=false'#13#10'b=b1=false and not (1<0)',true,'b');
      TestLanScript('if (2<=1 or 1<2) and not (1=0) then a=10 else a=1',10,'a');
      TestLanScript('if (1>2) or (2>=1 and 1<>0) then a=100 else a=20',100,'a');
      TestLanScript('a=10 : if -a<1 then a=100 else a=20',100,'a');   //负号对语句内表达式的影响
      TestLanScript('i=1 : n=0 : do while i<10 : n=n+i : i=i+1 : loop',45,'n');  //do while ... loop 语句
      TestLanScript('a="123"'#13#10'b=a+"abc"&a','123abc123','b');
      if Result then
        Msg:=Msg+#13#10'Basic OK!';
      PhpJEPC:=TJEParser.GetParserForLan('PHP');
      if PhpJEPC<>nil then
      begin
        APhpJEParser:=PhpJEPC.Create;
        CheckLanTrans(ABasicParser,APhpJEParser,'if A then'#13#10'  B'#13#10'  C'#13#10'end if','<?php if($A){B();C();} ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'if A then:B:C:end if','<?php if($A){B();C();} ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'if a then:a=new C:set b=new xx(123):xxx:end if',
          '<?php if($a){ $a=new C(); $b=new xx(123); xxx();} ?>');
        Old_KeepComment:=Keep_Comment;
        Keep_Comment:=false;
        CheckLanTrans(ABasicParser,APhpJEParser,'if L>0 then'#13#10'  if 1=X then'
          +#13#10'    ''??'#13#10'    A=B'#13#10'  end if'#13#10'end if',
          '<?php if($L>0){if(1==$X){$A=$B;}}?>');
        Keep_Comment:=Old_KeepComment;
        CheckLanTrans(ABasicParser,APhpJEParser,'if A>B then X=A','<?php if($A>$B){$X=$A;} ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'redim a(10*N,2,c-1)',
          '<?php $a=array_fill(0,(10*$N+1),array_fill(0,3,array_fill(0,$c,null))); ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'Dim Fds:A=Fds(i+3)((8-9)/X)',
          '<?php $Fds=null;$A=$Fds[$i+3][(8-9)/$X]; ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'ReDim A(100,2):A(88,2)=12','<?php $A=array_fill(0,101,array_fill(0,3,null));$A[88][2]=12; ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'a=-b','<?php $a=-$b; ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'a=-(b+2)','<?php $a=-($b+2); ?>');
        CheckLanTrans(ABasicParser,APhpJEParser,'if a=-(b+2) then c=1','<?php if($a==-($b+2)){$c=1;} ?>');
        if Use_Session then  //2013-03-29
        begin
          CheckLanTrans(ABasicParser,APhpJEParser,'session("id")=123.4','<?php $_SESSION[''id'']=123.4; ?>');
        end;
        if Keep_Comment then  //2013-03-30
        begin
          CheckLanTrans(ABasicParser,APhpJEParser,'A=1 ''A <= 1','<?php $A=1; //A <= 1'#13#10'?>');
        end;
        if Use_Request then  //2013-03-29
        begin
          CheckLanTrans(ABasicParser,APhpJEParser,'id=Request("id")','<?php $id=$_REQUEST[''id'']; ?>');
        end;
        CheckLanTrans(ABasicParser,APhpJEParser,'s.Foo(0):s(1)=2*s(2)','<?php $s->Foo(0);$s[1]=2*$s[2]; ?>');  //2013-06-12
        CheckLanTrans(ABasicParser,APhpJEParser,'App("Name")=3','<?php $App[''Name'']=3; ?>');  //2013-06-12
        if Result then
          Msg:=Msg+#13#10'Basic->PHP OK!';
        APhpJEParser.Free;
      end;
      PascalJEPC:=TJEParser.GetParserForLan('Pascal');
      if PascalJEPC<>nil then
      begin
        APascalJEParser:=PascalJEPC.Create;
        CheckLanTrans(ABasicParser,APascalJEParser,'if A then'#13#10'  B'#13#10'  C'#13#10'end if','if A then begin B(); C(); end;');
        CheckLanTrans(ABasicParser,APascalJEParser,'if -a=2 then:b=2:end if','if -a=2 then begin b:=2; end;');
        CheckLanTrans(ABasicParser,APascalJEParser,'<%if A then%>?<%else%>.<%end if%>',
          'if A then begin Echo(''?''); end else begin Echo(''.''); end;');
        CheckLanTrans(ABasicParser,APascalJEParser,'<%if b then%>@<%else%>*<%end if%>.',
          'if b then begin Echo(''@''); end else begin Echo(''*''); end; Echo(''.'');');
        CheckLanTrans(ABasicParser,APascalJEParser,'<%if 10>2 then a.Wt "1" else%>abc',
          'if 10>2 then begin a.Wt(''1''); end; Echo(''abc'');');
        CheckLanTrans(ABasicParser,APascalJEParser,'if a then Call F(121)',
          'if a then begin {Basic.CALL}F(121); end;');
        if Result then
          Msg:=Msg+#13#10'Basic->Pascal OK!';
        APascalJEParser.Free;
      end;
      ABasicParser.Free;
    end;
    Free;
  end;
  J:=JSONObject.Create;
  VHelper.ValExport(J);
  Msg:=Msg+#13#10#13#10+J.ToString;
  {$IFDEF SUPEROBJECT}J:=nil{$ELSE}J.Free{$ENDIF};
  VHelper.Free;
  DebugMsg:='';
end;

end.
