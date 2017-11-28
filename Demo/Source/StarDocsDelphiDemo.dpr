program StarDocsDelphiDemo;

uses
  Vcl.Forms, Dialogs, SysUtils,
  uStarDocDocumentOperations in 'uStarDocDocumentOperations.pas' {Form1} ,
  UAuthentication in 'UAuthentication.pas' {frmAuthentication} ,
  UInit in 'UInit.pas';

{$R *.res}

var
  LoginSuccess: Boolean;
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);

  LoginSuccess := False;
  if TfrmAuthentication.execute then
  begin
    Form1.gtStarDocsSDK1.ConnectionSettings.ApiServerUri := CURL;
    Form1.gtStarDocsSDK1.ConnectionSettings.ApiKey := CKey;
    Form1.gtStarDocsSDK1.ConnectionSettings.ApiSecret := CSecret;
    Form1.gtStarDocsSDK1.Preferences.DocPassword.ForceFullPermission := True;
    try
      Form1.gtStarDocsSDK1.AuthResponse := Form1.gtStarDocsSDK1.Auth.loginApp;
      LoginSuccess := True;
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;

    if LoginSuccess then
    begin
      Application.Run;
    end;
  end

end.
