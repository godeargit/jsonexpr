> 表达式的解析和求值是一个应用面非常广的功能。而JSON格式因其简明、易于操作，使用面日渐广泛。本项目将表达式分析的结果存放在JSON对象中，可以非常方便的进行解读及求值。
> 实现了基于JSON表达式的目标语言文本输出功能。

Project has moved to: http://code.taobao.org/p/jsonexpr/src/
New SVN: http://code.taobao.org/svn/jsonexpr/trunk

## Functions: ##
  * ExprToJSON
  * JSONToExpr
  * Eval
  * EvalNumber
  * VarNeeded
  * TraceOnLine
  * ValImport
  * ValExport
  * TJETranslater.TranslateJETree

## Plugins: ##
  * Var Helper
  * Func Helper

## Supports: ##
| Math operation | + - [**] / \ % & | ^ ~ ! >> <<**|
|:---------------|:-----|
| Bool operation | AND OR XOR |
| Compare operation | = > < <> >= <= |
| Object operation | . IS |
| Set value      | X := Y  += -= **= /= ...**|
| Value collection | IN (a,b) |
| String operation | LEN  |
| Condition expr | IF( condition, statement1, statement2) <br> IIF( condition1, value1, condition2, value2, ... valueElse) <br>
<tr><td> Case expr      </td><td> CASE( v, value1, expr1, value2, expr2, ... , exprElse) </td></tr>
<tr><td> Loop expr      </td><td> FOR, WHILE, REPEAT, TIMES </td></tr>
<tr><td> Sentence end tag </td><td> ;    </td></tr>
<tr><td> Variable with " </td><td> "My Var" </td></tr>
<tr><td> Array access   </td><td> A<a href='.md'>B[ C+2 </a>,10 ] </td></tr>
<tr><td> Print output   </td><td> Print(Msg) </td></tr>
<tr><td> Eval a string  </td><td> Eval('A+=20; B:=A/10;') </td></tr>
<tr><td> Funcion/Var define</td><td> public function Max(A,B):=IF(A>B,A,B); </td></tr></tbody></table>

<h2>Parse Example： ##
  * 2 + X / Sin(Y)  =>  {op:"+",p1:2,p2:{op:"/",p1:"X",p2:{op:"SIN",p1:"Y"}}}
  * Z/(X.Max+0.5)  =>  {op:"/",p1:"Z",p2:{op:"+",p1:{op:".",p1:"X",p2:"Max"},p2:0.5}}

## Eval Example： ##
  * IIF( Y.Name IS not NULL, PI, 3 + (9<<2) )
  * X:=3;  Y:=X-5;  IF(2>X+Y, 2, X\*Y)
  * X1:=1; Y:=IIF(X IS NULL,0,X1|5); Z:=9/IF(X1+2<=(3+Y),X2:=X1<<3,(Y:=X1\*10; X2:=Y|100));
  * P1.X:=10; P1.Y:=9-P1.X/2; S:=P1.((X+2)/Y)

## Aim Language Convert Example: ##
  * Convert expression to Pascal: **```
X:=1+IIF(A<B,IIF(B,100,0.5)*1.5,IIF(A<>C,2,4+5))
```
=>
```
if A<B then
begin
  if B then
  begin
    tv_4_2:=100;
  end
  else begin
    tv_4_2:=0.5;
  end;
  tv_2_1:=tv_4_2*1.5;
end
else begin
  if A<>C then
  begin
    tv_3_3:=2;
  end
  else begin
    tv_3_3:=4+5;
  end;
  tv_2_1:=tv_3_3;
end;
X:=1+tv_2_1
```**

## ASP To PHP Example: ##
if a=-(b+2) then c=1
=>
if($a==-($b+2)){$c=1;}

## To do: ##
  * Class/Function Define.
  * Runtime Grammer extend by Self.
  * Parse muti language code (Javascript, SQL, PHP ...).