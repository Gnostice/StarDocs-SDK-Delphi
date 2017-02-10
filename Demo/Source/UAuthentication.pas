unit UAuthentication;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, UInit, ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmAuthentication = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtKey: TEdit;
    edtSecret: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    LinkLabel1: TLinkLabel;
    Label4: TLabel;
    edtURL: TEdit;

    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
  public
    FKey: string;
    FSecret: string;
    IsOk: Boolean;
    class function Execute: Boolean;
  end;

var
  frmAuthentication: TfrmAuthentication;

implementation

{$R *.dfm}

procedure TfrmAuthentication.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrAbort;
end;

procedure TfrmAuthentication.btnOKClick(Sender: TObject);
begin
  CURL := edtURL.Text;
  CKey := edtKey.Text;
  CSecret := edtSecret.Text;
  IsOk := True;
  ModalResult := mrOK;

end;

class function TfrmAuthentication.Execute: Boolean;
begin
  with TfrmAuthentication.Create(nil) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TfrmAuthentication.LinkLabel1LinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
   ShellExecute(Application.Handle, 'Open', PChar(Link), nil,
        nil, SW_SHOWNORMAL);
end;

end.
