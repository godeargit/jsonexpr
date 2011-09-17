unit JSTransC;

interface

uses
  UJSONExpr, uJSON;

type
  TJSCOpHelper=class(TJEOpHelper)
  public
    function TranslateOperator(const Op: ShortString; JNode: TZAbstractObject):ShortString; override;
    function RestoreOperator(const Op: ShortString; JNode: TZAbstractObject):ShortString; override;
  end;

implementation

{ TJSCOpHelper }

function TJSCOpHelper.RestoreOperator(const Op: ShortString; JNode: TZAbstractObject): ShortString;
var
  C1:Char;
begin
  Result:=Op;
  C1:=Op[1];
  case Length(Op) of
    1:
    begin
      case C1 of
        '=': Result:='==';
      end;
    end;
    2:
    begin
      case C1 of
        ':':
          if Op=':=' then
            Result:='=';
        'O':
          if Op='OR' then
            Result:='||';
      end;
    end;
    3:
    begin
      case C1 of
        'A':
          if Op='AND' then
            Result:='&&';
        'N':
          if Op='NOT' then
            Result:='!';
      end;
    end;
  end;
end;

function TJSCOpHelper.TranslateOperator(const Op: ShortString; JNode: TZAbstractObject): ShortString;
var
  C1:Char;
begin
  Result:=Op;
  C1:=Op[1];
  case Length(Op) of
    1:
    begin
      case C1 of
        '=': Result:=':=';
        '!': Result:='NOT';
      end;
    end;
    2:
    begin
      case C1 of
        '=':
          if Op='==' then
            Result:='=';
        '&':
          if Op='&&' then
            Result:='AND';
        '|':
          if Op='||' then
            Result:='OR';
      end;
    end;
  end;
end;

end.
