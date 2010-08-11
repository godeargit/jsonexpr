unit UJsonStrFunc;

interface

uses
  uJSON, UJSONExpr, Variants, SysUtils;

type
  TStrFuncHelper=class(TJSONFuncHelper)
    function GetValue(Sender: TJSONExprParser; const Func: String;
      Params: array of Variant; out Val:Variant):Boolean; override;
  end;

implementation

function StartWithStr(const Str, HeaderStr: String):Boolean;
begin
  Result:=Copy(Str,1,Length(HeaderStr))=HeaderStr;
end;

function EndWithStr(const Str, TailStr: String):Boolean;
begin
  Result:=Copy(Str,Length(Str)-Length(TailStr)+1,Length(TailStr))=TailStr;
end;

function CharCount(const Str: String; const Ch: Char):Integer;
var
  i:Integer;
begin
  Result:=0;
  for i:=1 to Length(Str) do
    if Str[i]=Ch then
      Inc(Result); 
end;

{ TStrFuncHelper }

function TStrFuncHelper.GetValue(Sender: TJSONExprParser; const Func: String;
  Params: array of Variant; out Val: Variant): Boolean;
var
  Func1,Func2,FuncTail:AnsiChar;
  mstr:String;
begin
  Result:=false;
  Val:=Null;
  if Length(Func)<3 then  //×î¶ÌÖ¸Áî
  begin
    if NextHelper<>nil then
      Result:=NextHelper.GetValue(Sender,Func,Params,Val);
    exit;
  end;
  Func1:=Func[1];
  case Func1 of
    'C':
    begin
      if Func='CHARAT' then
      begin
        if High(Params)>=1 then
        begin
          Val:=String(Params[0])[Integer(Params[1])];
        end;
        Result:=true;
      end
      else if Func='CHARCOUNT' then
      begin
        if High(Params)>=1 then
        begin
          mstr:=String(Params[1]);
          if mstr='' then mstr:=#0;
          Val:=CharCount(Params[0],mstr[1]);
        end;
        Result:=true;
      end
      else if Func='COPY' then
      begin
        if High(Params)>=2 then
          Val:=Copy(Params[0],Integer(Params[1]),Integer(Params[2]));
        Result:=true;
      end;
    end;
    'D':
    begin
      if Func='DATETIMETOSTR' then
      begin
        if High(Params)>=0 then
        begin
          Val:=DateTimeToStr(TDateTime(Params[0]));
        end;
        Result:=true;
      end
      else if Func='DATETOSTR' then
      begin
        if High(Params)>=0 then
        begin
          Val:=DateToStr(TDateTime(Params[0]));
        end;
        Result:=true;
      end
      else if Func='DELETE' then
      begin
        if High(Params)>=2 then
        begin
          mstr:=Params[0];
          Delete(mstr,Integer(Params[1]),Integer(Params[2]));
          Val:=mstr;
        end;
        Result:=true;
      end;
    end;
    'E':
    begin
      if Func='ENDWITHSTR' then
      begin
        if High(Params)>=1 then
          Val:=EndWithStr(Params[0],Params[1]);
        Result:=true;
      end;
    end;
    'F':
    begin
      if Func='FLOATTOSTR' then
      begin
        if High(Params)>=0 then
          Val:=FloatToStr(Params[0]);
        Result:=true;
      end;
    end;
    'I':
    begin
      if Func='INTTOSTR' then
      begin
        if High(Params)>=0 then
          Val:=IntToStr(Integer(Params[0]));
        Result:=true;
      end
      else if Func='INTTOHEX' then
      begin
        if High(Params)>=1 then
          Val:=IntToHex(Integer(Params[0]),Integer(Params[1]));
        Result:=true;
      end;
    end;
    'L':
    begin
      if Func='LENGTH' then
      begin
        if High(Params)>=0 then
          Val:=Length(Params[0]);
        Result:=true;
      end
      else if Func='LOWERCASE' then
      begin
        if High(Params)>=0 then
          Val:=LowerCase(Params[0]);
        Result:=true;
      end;
    end;
    'P':
    begin
      if Func='POS' then
      begin
        if High(Params)>=1 then
          Val:=Pos(Params[0],Params[1]);
        Result:=true;
      end;
    end;
    'Q':
    begin
      if Func='QUOTEDSTR' then
      begin
        if High(Params)>=0 then
          Val:=QuotedStr(Params[0]);
        Result:=true;
      end;
    end;
    'R':
    begin
      if Func='REPLACE' then
      begin
        if High(Params)>=2 then
          Val:=StringReplace(Params[0],Params[1],Params[2],[rfReplaceAll]);
        Result:=true;
      end;
    end;
    'S':
    begin
      case Func[Length(Func)] of
        'E':
        begin
          if Func='STRTODATETIME' then
          begin
            if High(Params)>=0 then
              Val:=TDateTime(StrToDateTime(Params[0]));
            Result:=true;
          end
          else if Func='STRTODATE' then
          begin
            if High(Params)>=0 then
              Val:=TDateTime(StrToDate(Params[0]));
            Result:=true;
          end
          else if Func='STRTOTIME' then
          begin
            if High(Params)>=0 then
              Val:=StrToTime(Params[0]);
            Result:=true;
          end;
        end;
        'F':
        begin
          if Func='STRTOINTDEF' then
          begin
            if High(Params)>=1 then
              Val:=StrToIntDef(Params[0],Integer(Params[1]));
            Result:=true;
          end;
        end;
        'R':
        begin
          if Func='STARTWITHSTR' then
          begin
            if High(Params)>=1 then
              Val:=StartWithStr(Params[0],Params[1]);
            Result:=true;
          end;
        end;
        'T':
        begin
          if Func='STRTOINT' then
          begin
            if High(Params)>=0 then
              Val:=StrToInt(Params[0]);
            Result:=true;
          end
          else if Func='STRTOFLOAT' then
          begin
            if High(Params)>=0 then
              Val:=StrToFloat(Params[0]);
            Result:=true;
          end;
        end;
      end;
    end;
    'T':
    begin
      if Func='TRIM' then
      begin
        if High(Params)>=0 then
          Val:=Trim(Params[0]);
        Result:=true;
      end
      else if Func='TIMETOSTR' then
      begin
        if High(Params)>=0 then
          Val:=TimeToStr(Params[0]);
        Result:=true;
      end;
    end;
    'U':
    begin
      if Func='UPPERCASE' then
      begin
        if High(Params)>=0 then
          Val:=UpperCase(Params[0]);
        Result:=true;
      end;
    end;
  end;
  if not Result and (NextHelper<>nil) then
    Result:=NextHelper.GetValue(Sender,Func,Params,Val);  
end;

end.
