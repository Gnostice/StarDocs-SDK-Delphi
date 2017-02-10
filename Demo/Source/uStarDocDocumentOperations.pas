unit uStarDocDocumentOperations;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CheckLst, ExtCtrls, UInit, gtxStarDocsSDK,
  ShellApi, Generics.Collections, Actions, ActnList, StdActns, TypInfo,
  SHDocVw, OleCtrls, Contnrs;

type
  TForm1 = class(TForm)
    pgeCntrlDocoperation: TPageControl;
    tbshView: TTabSheet;
    tbshSplit: TTabSheet;
    GroupBox9: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    Label9: TLabel;
    edSplitLoad: TEdit;
    edPageRange: TEdit;
    btnSplitload: TButton;
    tbshConvert: TTabSheet;
    tbshEncrypt: TTabSheet;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    tbshRedactText: TTabSheet;
    tbshMerge: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    btnMergeLoad: TButton;
    btnMerge: TButton;
    edMergeLoad: TEdit;
    edMergeoutput: TEdit;
    btnMergeSave: TButton;
    edSplitOutput: TEdit;
    Label5: TLabel;
    btnSplitSave: TButton;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    btnConvertload: TButton;
    btnConvert: TButton;
    edConvertLoad: TEdit;
    edConvertoutput: TEdit;
    btnConvertsave: TButton;
    Label6: TLabel;
    ComboBox1: TComboBox;
    edConvertpagerange: TEdit;
    Label8: TLabel;
    Label10: TLabel;
    GroupBox4: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    edEncryptLoad: TEdit;
    btnEncrypt: TButton;
    btnEncryptLoad: TButton;
    edEncryptOutput: TEdit;
    btnEncryptSave: TButton;
    GroupBox5: TGroupBox;
    Label15: TLabel;
    Label17: TLabel;
    edSearchText: TEdit;
    btnRedact: TButton;
    edRedactOutput: TEdit;
    btnRedactSave: TButton;
    Label16: TLabel;
    edReplaceText: TEdit;
    Label18: TLabel;
    gbEncryption: TGroupBox;
    lblConfirmOwnerPswd: TLabel;
    lblUserPswd: TLabel;
    lblConfirmUserPswd: TLabel;
    lblOwnerPswd: TLabel;
    rgEncryptionLevel: TRadioGroup;
    gbUserPermissions: TGroupBox;
    chklstUserPermissions: TCheckListBox;
    edtOwnerPassword: TEdit;
    edtConfirmOwnerPassword: TEdit;
    edtUserPassword: TEdit;
    edtConfirmUserPassword: TEdit;
    tbshDocInfo: TTabSheet;
    GroupBox6: TGroupBox;
    edFileName: TEdit;
    btnGetInfo: TButton;
    Label29: TLabel;
    btnDocInfoload: TButton;
    edDocInfoLoad: TEdit;
    Label31: TLabel;
    edViewLoad: TEdit;
    btnViewLoad: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    WebBrowser1: TWebBrowser;
    btnSplit: TButton;
    btnRedactLoad: TButton;
    Label7: TLabel;
    edReadctLoad: TEdit;
    ActionList1: TActionList;
    BrowseForFolder1: TBrowseForFolder;
    BrowseForFolder2: TBrowseForFolder;
    BrowseForFolder3: TBrowseForFolder;
    BrowseForFolder4: TBrowseForFolder;
    BrowseForFolder5: TBrowseForFolder;
    Panel3: TPanel;
    btnView: TButton;
    GroupBox3: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    edIsCorrupt: TEdit;
    edFileSize: TEdit;
    edISPasswordProtected: TEdit;
    edIspasswordcorrect: TEdit;
    edPageCount: TEdit;
    edMIMEType: TEdit;
    gtStarDocsSDK1: TgtStarDocsSDK;
    Label11: TLabel;
    ComboBox2: TComboBox;

    procedure btnMergeOpen1Click(Sender: TObject);
    procedure btnViewLoadClick(Sender: TObject);
    procedure btnSplitloadClick(Sender: TObject);
    procedure BrowseForFolder1Accept(Sender: TObject);
    procedure BrowseForFolder5Accept(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
    procedure btnConvertloadClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure BrowseForFolder2Accept(Sender: TObject);
    procedure btnRedactLoadClick(Sender: TObject);
    procedure btnRedactClick(Sender: TObject);
    procedure btnEncryptLoadClick(Sender: TObject);
    procedure BrowseForFolder4Accept(Sender: TObject);
    procedure BrowseForFolder3Accept(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDocInfoloadClick(Sender: TObject);
    procedure btnGetInfoClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure btnSplitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private

  public
    { Public declarations }
    LSplitInFile: TgtFileObject;
    LConverterInFiles: TObjectList<TgtFileObject>;
    LRedactInFile: TgtFileObject;
    LEncryptInFile: TgtFileObject;
    LMergeInFiles: TObjectList<TgtFileObject>;
    LDocInfoInFile: TgtFileObject;
    LLoadedFileList: TObjectList;
  end;

const
  BooleanToStringName: array [False .. True] of string = ('No', 'Yes');

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BrowseForFolder1Accept(Sender: TObject);
begin
  edSplitOutput.Text := BrowseForFolder1.Folder;
end;

procedure TForm1.BrowseForFolder2Accept(Sender: TObject);
begin
  edConvertoutput.Text := BrowseForFolder2.Folder;
end;

procedure TForm1.BrowseForFolder3Accept(Sender: TObject);
begin
  edEncryptOutput.Text := BrowseForFolder3.Folder;
end;

procedure TForm1.BrowseForFolder4Accept(Sender: TObject);
begin
  edRedactOutput.Text := BrowseForFolder4.Folder;
end;

procedure TForm1.BrowseForFolder5Accept(Sender: TObject);
begin
  edMergeoutput.Text := BrowseForFolder5.Folder;
end;

procedure TForm1.btnConvertClick(Sender: TObject);
var
  LPageRanges: TObjectList<TgtPageRangeSettings>;
  LOutFiles: TObjectList<TgtDocObject>;
  LIndex: Integer;
begin
  LOutFiles := nil;
  LPageRanges := nil;
  btnConvert.Enabled := False;
  LPageRanges := TObjectList<TgtPageRangeSettings>.Create;
  LPageRanges.Add(TgtPageRangeSettings.Create(edConvertpagerange.Text));
  try
    case ComboBox1.ItemIndex of
      0:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToPDF(LConverterInFiles,
          nil, LPageRanges);
      1:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToTIFF(LConverterInFiles,
          nil, LPageRanges);
      2:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToMTIFF
          (LConverterInFiles, nil, LPageRanges);
      3:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToJpeg(LConverterInFiles,
          nil, LPageRanges);
      4:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToGIF(LConverterInFiles,
          nil, LPageRanges);
      5:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToPNG(LConverterInFiles,
          nil, LPageRanges);
      6:
        LOutFiles := gtStarDocsSDK1.DocOperations.ConvertToBMP(LConverterInFiles,
          nil, LPageRanges)
    end;

    for LIndex := 0 to LOutFiles.Count - 1 do
      gtStarDocsSDK1.Storage.Download(LOutFiles[LIndex], edConvertoutput.Text);
  finally
    if Assigned(LOutFiles) then
      LOutFiles.Free;
    if Assigned(LPageRanges) then
      LPageRanges.Free;
    btnConvert.Enabled := True;
  end;
end;

procedure TForm1.btnConvertloadClick(Sender: TObject);
var
  LOpnDlg: TOpenDialog;
  Li: Integer;
begin
  LOpnDlg := TOpenDialog.Create(nil);
  LConverterInFiles := TObjectList<TgtFileObject>.Create;

  LOpnDlg.Filter := 'PDF Files (*.pdf)|*.pdf|' + 'DOCX Files(*.docx)|*.docx|' +
    'JPEG File (*.jpeg)|*.jpeg|' + 'PNG Files (*.png)|*.png|' +
    'JPG FIles (*.jpg)|*.jpg|' + 'GIF Files (*.gif)|*.gif|' +
    'TIFF Files (*.tiff)|*.tiff|' + 'TIF Files (*.tif)|*.tif|' +
    'All supported forrmats|*.pdf;*.docx;*.jpeg;*.png;*.jpg;*.gif;*.tiff; *.tif;';
  LOpnDlg.FilterIndex := 9;
  LOpnDlg.Options := [ofAllowMultiSelect];
  if LOpnDlg.Execute then
  begin
    edConvertLoad.Text := LOpnDlg.Files.CommaText;
    edConvertoutput.Text := ExtractFilePath(LOpnDlg.FileName) + 'Converted\';
    btnConvert.Enabled := True;
    for Li := 0 to LOpnDlg.Files.Count - 1 do
    begin
      LConverterInFiles.Add(TgtFileObject.Create(LOpnDlg.Files[Li]));
      LLoadedFileList.Add(LConverterInFiles);
    end;
  end;
  LOpnDlg.Free;
end;

procedure TForm1.btnDocInfoloadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edDocInfoLoad.Text := OpenDialog1.FileName;
    LDocInfoInFile := TgtFileObject.Create(OpenDialog1.FileName);
    LLoadedFileList.Add(LDocInfoInFile);
    GroupBox3.Visible := False;
    btnGetInfo.Enabled := True;
  end;
end;

procedure TForm1.btnEncryptClick(Sender: TObject);
var
  LOutFile: TgtDocObject;
  LUserPassword: String;
  LOwnerPassword: String;
  LEncryptionLevel: TgtPDFEncryptionLevel;
  LUserPermissions: TgtPDFDocPermissions;
begin
  case rgEncryptionLevel.ItemIndex of
    0:
      LEncryptionLevel := pelNone;
    1:
      LEncryptionLevel := pelAES_128bit;
    2:
      LEncryptionLevel := pelRC4_128bit;
    3:
      LEncryptionLevel := pelRC4_40bit;

  end;

  if edtOwnerPassword.Text = edtConfirmOwnerPassword.Text then
    LOwnerPassword := edtOwnerPassword.Text;
  if edtUserPassword.Text = edtConfirmUserPassword.Text then
    LUserPassword := edtUserPassword.Text;

  LUserPermissions := [];

  if chklstUserPermissions.Checked[0] then
    LUserPermissions := LUserPermissions + [pdpAllowPrinting];
  If chklstUserPermissions.Checked[1] then
    LUserPermissions := LUserPermissions + [pdpAllowModifyContents];
  If chklstUserPermissions.Checked[2] then
    LUserPermissions := LUserPermissions + [pdpAllowCopy];
  If chklstUserPermissions.Checked[3] then
    LUserPermissions := LUserPermissions + [pdpAllowModifyAnnotations];
  If chklstUserPermissions.Checked[4] then
    LUserPermissions := LUserPermissions + [pdpAllowFormFill];
  If chklstUserPermissions.Checked[5] then
    LUserPermissions := LUserPermissions + [pdpAllowAccessibility];
  If chklstUserPermissions.Checked[6] then
    LUserPermissions := LUserPermissions + [pdpAllowAssembly];
  If chklstUserPermissions.Checked[7] then
    LUserPermissions := LUserPermissions + [pdpAllowHighResPrint];

  btnEncrypt.Enabled := False;
  LOutFile := nil;
  try
    LOutFile := gtStarDocsSDK1.DocOperations.Encrypt(LEncryptInFile, '',
      LEncryptionLevel, LUserPassword, LOwnerPassword, LUserPermissions);

    // Download the file
    gtStarDocsSDK1.Storage.Download(LOutFile, edEncryptOutput.Text);
  finally
    if Assigned(LOutFile) then
      LOutFile.Free;
    btnEncrypt.Enabled := True;
  end;
end;

procedure TForm1.btnEncryptLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edEncryptLoad.Text := OpenDialog1.FileName;
    LEncryptInFile := TgtFileObject.Create(OpenDialog1.FileName);
    LLoadedFileList.Add(LEncryptInFile);
    edEncryptOutput.Text := ExtractFilePath(OpenDialog1.FileName) +
      'Encrypted\';
    btnEncrypt.Enabled := True;
  end;
end;

procedure TForm1.btnGetInfoClick(Sender: TObject);
var
  LGetDocumentInfoResponse: TgtGetDocumentInfoResponse;
begin
  btnGetInfo.Enabled := False;
  LGetDocumentInfoResponse := nil;
  try
    LGetDocumentInfoResponse := gtStarDocsSDK1.DocOperations.GetDocumentInfo
      (LDocInfoInFile, '');
    GroupBox3.Visible := True;
    edFileName.Text := LGetDocumentInfoResponse.FileName;
    edFileSize.Text := Inttostr(LGetDocumentInfoResponse.FileSize) + ' Bytes';

    edIsCorrupt.Text := BooleanToStringName
      [LGetDocumentInfoResponse.UnsupportedMimeTypeOrCorrupt];
    edISPasswordProtected.Text := BooleanToStringName
      [LGetDocumentInfoResponse.PasswordProtected];
    edIspasswordcorrect.Text := BooleanToStringName
      [LGetDocumentInfoResponse.PasswordCorrect];
    edPageCount.Text := Inttostr(LGetDocumentInfoResponse.PageCount);
    edMIMEType.Text := GetEnumName(TypeInfo(TgtMimeType),
      Integer(LGetDocumentInfoResponse.MimeType));
  finally
    if Assigned(LGetDocumentInfoResponse) then
      LGetDocumentInfoResponse.Free;
    btnGetInfo.Enabled := True;
  end;
end;

procedure TForm1.btnMergeClick(Sender: TObject);
var
  LOutFile: TgtDocObject;
begin
  btnMerge.Enabled := False;
  LOutFile := nil;
  try
    LOutFile := gtStarDocsSDK1.DocOperations.Merge(LMergeInFiles);
    gtStarDocsSDK1.Storage.Download(LOutFile, edMergeoutput.Text);
  finally
    if Assigned(LOutFile) then
      LOutFile.Free;
    btnMerge.Enabled := True;
  end;
end;

procedure TForm1.btnMergeOpen1Click(Sender: TObject);
var
  LOpnDlg: TOpenDialog;
  Li: Integer;
begin
  LOpnDlg := TOpenDialog.Create(nil);
  LMergeInFiles := TObjectList<TgtFileObject>.Create;

  LOpnDlg.Filter := 'PDFFiles(*.pdf)|*.pdf';
  LOpnDlg.Options := [ofAllowMultiSelect];
  if LOpnDlg.Execute then
  begin
    edMergeLoad.Text := LOpnDlg.Files.CommaText;
    edMergeoutput.Text := ExtractFilePath(LOpnDlg.FileName) + 'Merged\';
    btnMerge.Enabled := True;
    for Li := 0 to LOpnDlg.Files.Count - 1 do
    begin
      LMergeInFiles.Add(TgtFileObject.Create(LOpnDlg.Files[Li]));
      LLoadedFileList.Add(LMergeInFiles);
    end;
  end;
  LOpnDlg.Free;
end;

procedure TForm1.btnRedactClick(Sender: TObject);
var
  LOutFile: TgtDocObject;
  LSearchText: TObjectList<TgtSearchText>;
begin
  LOutFile := nil;
  LSearchText := nil;
  btnRedact.Enabled := False;
  LSearchText := TObjectList<TgtSearchText>.Create;
  LSearchText.Add(TgtSearchText.Create(edSearchText.Text, True, False));
  gtStarDocsSDK1.DocOperations.RedactFillSettings.Outline.PenColoringMode :=
    TgtColoringMode.cmUseColor;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.Outline.Pen.Color.Red := 255;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.Outline.Pen.Color.Blue := 0;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.Outline.Pen.Color.Green := 0;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.FillRect.BrushColoringMode :=
    TgtColoringMode.cmUseColor;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.FillRect.Brush.Color.Red := 0;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.FillRect.Brush.Color.
    Green := 255;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.FillRect.Brush.Color.
    Blue := 0;
  gtStarDocsSDK1.DocOperations.RedactFillSettings.FillText.ReplaceText :=
    edReplaceText.Text;

  try
    LOutFile := gtStarDocsSDK1.DocOperations.RedactText(LRedactInFile, '', nil,
      tsmLiteral, LSearchText);
    // Download the file
    gtStarDocsSDK1.Storage.Download(LOutFile, edRedactOutput.Text);
  finally
    if Assigned(LOutFile) then
      LOutFile.Free;
    if Assigned(LSearchText) then
      LSearchText.Free;
    btnRedact.Enabled := True;
  end;
end;

procedure TForm1.btnRedactLoadClick(Sender: TObject);
begin
  btnRedact.Enabled := True;
  if OpenDialog1.Execute then
  begin
    edReadctLoad.Text := OpenDialog1.FileName;
    LRedactInFile := TgtFileObject.Create(OpenDialog1.FileName);
    LLoadedFileList.Add(LRedactInFile);
    edRedactOutput.Text := ExtractFilePath(OpenDialog1.FileName) + 'Redacted\';
  end;
end;

procedure TForm1.btnSplitClick(Sender: TObject);
var
  LPageRanges: TObjectList<TgtPageRangeSettings>;
  LOutFiles: TObjectList<TgtDocObject>;
  LIndex: Integer;
begin
  btnSplit.Enabled := False;
  LOutFiles := nil;
  try
    case ComboBox2.ItemIndex of
      0:
        begin
          LPageRanges := TObjectList<TgtPageRangeSettings>.Create;
          LPageRanges.Add(TgtPageRangeSettings.Create(edPageRange.Text));
          LOutFiles := gtStarDocsSDK1.DocOperations.SplitByPageRange(LSplitInFile,
            '', LPageRanges);
          LPageRanges.Free;
        end;
      1:
        LOutFiles := gtStarDocsSDK1.DocOperations.SplitBySeparatorPage
          (LSplitInFile, '');
    end;
    // Download the files
    for LIndex := 0 to LOutFiles.Count - 1 do
      gtStarDocsSDK1.Storage.Download(LOutFiles[LIndex], edSplitOutput.Text);
  finally
    if Assigned(LOutFiles) Then
      LOutFiles.Free;
    btnSplit.Enabled := True;
  end;
end;

procedure TForm1.btnSplitloadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edSplitLoad.Text := OpenDialog1.FileName;
    LSplitInFile := TgtFileObject.Create(OpenDialog1.FileName);
    edSplitOutput.Text := ExtractFilePath(OpenDialog1.FileName) + 'Splited\';
    btnSplit.Enabled := True;
    LLoadedFileList.Add(LSplitInFile);
  end;
end;

procedure TForm1.btnViewClick(Sender: TObject);
var
  LResponse: TgtViewResponse;
  DocObject: TgtDocObject;
  HighlightColor: TgtColor;
begin
  btnView.Enabled := False;
  DocObject := gtStarDocsSDK1.Storage.Upload(edViewLoad.Text, '');
  gtStarDocsSDK1.Viewer.ViewerSettings.EnableFormFilling := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.FullScreenVisible := False;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleNavigationControls.
    GotoPage := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleColorInversionControls.
    AllPages := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleFileOperationControls.
    Open := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleFileOperationControls.
    Save := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleFileOperationControls.
    Download := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.VisibleFileOperationControls.
    Print := True;
  gtStarDocsSDK1.Viewer.ViewerSettings.SearchControls.EnableQuickSearch := True;
  HighlightColor := TgtColor.Create(255, 255, 0);
  gtStarDocsSDK1.Viewer.ViewerSettings.SearchControls.HighlightColor.Assign
    (HighlightColor);
  HighlightColor.Free;

  LResponse := nil;
  try
    LResponse := gtStarDocsSDK1.Viewer.CreateView(DocObject, '');
    WebBrowser1.Navigate(LResponse.URL);
  finally
    if Assigned(LResponse) then
      LResponse.Free;
    DocObject.Free;
  end;
end;

procedure TForm1.btnViewLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edViewLoad.Text := OpenDialog1.FileName;
    btnView.Enabled := True;
  end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  if ComboBox2.ItemIndex = 0 then
  begin
    Label22.Enabled := True;
    Label9.Enabled := True;
    edPageRange.Enabled := True;
  end;

  if ComboBox2.ItemIndex = 1 then
  begin
    Label22.Enabled := False;
    Label9.Enabled := False;
    edPageRange.Enabled := False;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LLoadedFileList.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LLoadedFileList := TObjectList.Create;
end;

end.
