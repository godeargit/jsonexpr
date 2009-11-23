program JEDemo;

uses
  Forms,
  UJEDemo in 'UJEDemo.pas' {frmMain},
  uJSON in '..\uJSON.pas',
  UJSONExpr in '..\UJSONExpr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
