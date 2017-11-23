unit gtxUniStarDocsViewer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, uniGUIBaseClasses,
  uniGUIClasses, uniURLFrame, Ext, uniGUITypes, UniGUIServer, REST.JSON,
  gtxStarDocsSDK, System.Generics.Collections;

type
  TgtUniStarDocsViewerFormsAPI = class;
  TgtFieldEvent = procedure(ASender: TObject; AFormField: TgtPDFFormField) of object;
  TgtBeforeFormSubmitEvent = procedure(ASender: TObject; AFormData: TgtPDFFormData) of object;
  TgtGetAllFormFieldsCallBackProc = procedure (AFormFields: TList<TgtPDFFormField>) of object;
  TgtGetFormFieldCallBackProc = procedure (AFormField: TgtPDFFormField) of object;
  TgtSubmitFormSuccessCallBackProc = procedure () of object;
  TgtSubmitFormFailureCallBackProc = procedure (ATextStatus: String; AErrorThrown: String) of object;
  TgtGetStringCallBackProc = procedure (AValue: String) of object;
  TgtGetIntegerCallBackProc = procedure (AIndex: Integer) of object;
  TgtGetIntegerArrayCallBackProc = procedure (AIndices: TList<Integer>) of object;
  TgtGetBooleanCallBackProc = procedure (AChecked: Boolean) of object;

  TgtUniStarDocsViewer = class(TUniCustomURLFrame)
  private
    // StarDocs SDK
    FStarDocsSDK: TgtStarDocsSDK;

    // API groups
    FForms: TgtUniStarDocsViewerFormsAPI;
    // Events
    FOnFormLoaded: TNotifyEvent;
    FOnFormModified: TNotifyEvent;
    FBeforeFormSubmit: TgtBeforeFormSubmitEvent;
    FAfterFormSubmit: TNotifyEvent;
    FOnFormFieldClicked: TgtFieldEvent;
    FOnFormFieldEnter: TgtFieldEvent;
    FOnFormFieldExit: TgtFieldEvent;
    FOnFormFieldChanged: TgtFieldEvent;

    // To track JS async callbacks with Delphi callback procedures
    FForms_GetAllFormFields_CallBack: TgtGetAllFormFieldsCallBackProc;
    FForms_GetFormField_CallBack: TgtGetFormFieldCallBackProc;
    FForms_SubmitFormSuccess_CallBack: TgtSubmitFormSuccessCallBackProc;
    FForms_SubmitFormFailure_CallBack: TgtSubmitFormFailureCallBackProc;
    FForms_GetValueAsString_CallBack: TgtGetStringCallBackProc;
    FForms_GetRadioButtonSelectedIndex_CallBack: TgtGetIntegerCallBackProc;
    FForms_ComboListBoxSelectedItemIndices_CallBack: TgtGetIntegerArrayCallBackProc;
    FForms_GetCheckBoxChecked_CallBack: TgtGetBooleanCallBackProc;

    procedure H_OnFormLoaded(AThis: TJSObject; EventName: string; AParams: TUniStrings);
    procedure H_OnFormModified(AThis: TJSObject; EventName: string; AParams: TUniStrings);
    procedure H_BeforeFormSubmit(AThis: TJSObject; EventName: string; AParams: TUniStrings);
    procedure H_AfterFormSubmit(AThis: TJSObject; EventName: string; AParams: TUniStrings);
    procedure H_OnFormFieldEvent(AThis: TJSObject; EventName: string; AParams: TUniStrings);

    // Util methods
    function ConvertJsonToFormField(JsonStr: String): TgtPDFFormField;
    function JoinStrArray(AItems: TList<String>; ASeparator: String): String;
    function SplitToInt(AStr: String; ASeparator: String): TList<Integer>;
    function JoinIntArray(AItems: TList<Integer>; ASeparator: String): String;

  protected
    procedure DOHandleEvent(AEventName: string; AParams: TUniStrings); override;
    procedure ConfigJSClasses(ALoading: Boolean); override;
    procedure SetOnFormLoaded(AValue: TNotifyEvent);
    procedure SetOnFormModified(AValue: TNotifyEvent);
    procedure SetBeforeFormSubmit(AValue: TgtBeforeFormSubmitEvent);
    procedure SetAfterFormSubmit(AValue: TNotifyEvent);
    procedure SetOnFormFieldClicked(AValue: TgtFieldEvent);
    procedure SetOnFormFieldEnter(AValue: TgtFieldEvent);
    procedure SetOnFormFieldExit(AValue: TgtFieldEvent);
    procedure SetOnFormFieldChanged(AValue: TgtFieldEvent);

  public
    constructor Create(AOwner: TComponent); override;

    procedure Load(AFileNameWithPath: string; APassword: string = ''); overload;
    procedure Load(AStream: TStream; AfileName: string; APassword: string = ''); overload;

    property Forms: TgtUniStarDocsViewerFormsAPI read FForms;

    procedure I_Forms_GetAllFormFields(ACallBack: TgtGetAllFormFieldsCallBackProc);
    procedure I_Forms_GetFormField(AFormFieldName: String; ACallBack: TgtGetFormFieldCallBackProc);
    procedure I_Forms_SubmitForm(ASubmitUrl: String; ASubmitMethod: TgtFormSubmitMethod = fsmPOST; AIncludeNoValueFields: Boolean = False;
        ASubmitFields: TList<String> = nil; AIsIncludeList: Boolean = True; ACallBackSuccess: TgtSubmitFormSuccessCallBackProc = nil; ACallBackFailure: TgtSubmitFormFailureCallBackProc = nil);
    procedure I_Forms_ResetForm(AResetFields: TList<String> = nil; AIsIncludeList: Boolean = True);
    procedure I_Forms_FocusForm;
    procedure I_Forms_SetFocus(AFullyQualifiedName: String);
    procedure I_Forms_GetValueAsString(AFullyQualifiedName: String; ACallBack: TgtGetStringCallBackProc);
    procedure I_Forms_SetValueAsString(AFullyQualifiedName: String; AValue: String);
    procedure I_Forms_GetSelectedRadioButtonIndex(AFullyQualifiedName: String; ACallBack: TgtGetIntegerCallBackProc);
    procedure I_Forms_SetSelectedRadioButtonIndex(AFullyQualifiedName: String; AIndex: Integer);
    procedure I_Forms_GetSelectedItemIndices(AFullyQualifiedName: String; ACallBack: TgtGetIntegerArrayCallBackProc);
    procedure I_Forms_SetSelectedItemIndices(AFullyQualifiedName: String; AIndices: TList<Integer>);
    procedure I_Forms_GetChecked(AFullyQualifiedName: String; ACallBack: TgtGetBooleanCallBackProc);
    procedure I_Forms_SetChecked(AFullyQualifiedName: String; AChecked: Boolean);

  published
    property StarDocsSDK: TgtStarDocsSDK read FStarDocsSDK write FStarDocsSDK;

    //property URL;
    property Align;
    property Anchors;
    property LayoutConfig;

    // Events
    property OnFormLoaded: TNotifyEvent read FOnFormLoaded write SetOnFormLoaded;
    property OnFormModified: TNotifyEvent read FOnFormModified write SetOnFormModified;
    property BeforeFormSubmit: TgtBeforeFormSubmitEvent read FBeforeFormSubmit write SetBeforeFormSubmit;
    property AfterFormSubmit: TNotifyEvent read FAfterFormSubmit write SetAfterFormSubmit;
    property OnFormFieldClicked: TgtFieldEvent read FOnFormFieldClicked write SetOnFormFieldClicked;
    property OnFormFieldEnter: TgtFieldEvent read FOnFormFieldEnter write SetOnFormFieldEnter;
    property OnFormFieldExit: TgtFieldEvent read FOnFormFieldExit write SetOnFormFieldExit;
    property OnFormFieldChanged: TgtFieldEvent read FOnFormFieldChanged write SetOnFormFieldChanged;

  end;

  TgtUniStarDocsViewerFormsAPI = class
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    constructor Create(AViewerControl: TgtUniStarDocsViewer);

    { Returns a list of all the form fields present in the currently loaded PDF document }
    procedure GetAllFormFields(ACallBack: TgtGetAllFormFieldsCallBackProc);

    { Returns the form field present in the currently loaded PDF document matching the given name.
    First the passed name is treated as the fully qualified field name and if no field match is found then the search is
    repeated treating the name as the unqualified name. Null is returned if no field is found.
    @param formFieldName name of the form field to return }
    procedure GetFormField(AFormFieldName: String; ACallBack: TgtGetFormFieldCallBackProc);

    {
    Submits the document form fields to the given URL.
    @param submitUrl URL to submit the form.
    @param submitMethod HTTP Method used to submit the fields. Supported values are GET and POST.
    @param includeNoValueFields Whether to submit fields withithout a value.
    @param submitFields List of fields to include.
    @param isIncludeList If true "submitFields" specifies the fields to include for submission. If set to false then "submitFields" specifies which fields to exclude.
    }
    procedure SubmitForm(ASubmitUrl: String; ASubmitMethod: TgtFormSubmitMethod = fsmPOST; AIncludeNoValueFields: Boolean = False;
            ASubmitFields: TList<String> = nil; AIsIncludeList: Boolean = True;
            ACallBackSuccess: TgtSubmitFormSuccessCallBackProc = nil; ACallBackFailure: TgtSubmitFormFailureCallBackProc = nil);

    {
    Resets the form fields.
    @param resetFields List of fields to reset.
    @param isIncludeList If true "resetFields" specifies the fields to reset. If set to false "resetFields" specifies which fields to exclude.
    }
    procedure ResetForm(AResetFields: TList<String> = nil; AIsIncludeList: Boolean = True);

    { Focuses the first form field control (first in tab order). }
    procedure FocusForm;
  end;

  TgtViewerPDFTextFormField = class (TgtPDFTextFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure GetValueAsString(ACallBack: TgtGetStringCallBackProc);
    procedure SetValueAsString(AValue: String);
    procedure SetFocus();
  end;

  TgtViewerPDFRadioButtonFormField = class (TgtPDFRadioButtonFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure SetFocus();
  end;

  TgtViewerPDFRadioGroupFormField = class (TgtPDFRadioGroupFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure GetValueAsString(ACallBack: TgtGetStringCallBackProc);
    procedure SetValueAsString(AValue: String);
    procedure SetFocus();
    procedure GetSelectedRadioButtonIndex(ACallBack: TgtGetIntegerCallBackProc);
    procedure SetSelectedRadioButtonIndex(AIndex: Integer);
  end;

  TgtViewerPDFComboBoxFormField = class (TgtPDFComboBoxFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure GetValueAsString(ACallBack: TgtGetStringCallBackProc);
    procedure SetValueAsString(AValue: String);
    procedure SetFocus();
    procedure GetSelectedItemIndices(ACallBack: TgtGetIntegerArrayCallBackProc);
    procedure SetSelectedItemIndices(AIndices: TList<Integer>);
  end;

  TgtViewerPDFListBoxFormField = class (TgtPDFListBoxFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure GetValueAsString(ACallBack: TgtGetStringCallBackProc);
    procedure SetValueAsString(AValue: String);
    procedure SetFocus();
    procedure GetSelectedItemIndices(ACallBack: TgtGetIntegerArrayCallBackProc);
    procedure SetSelectedItemIndices(AIndices: TList<Integer>);
  end;

  TgtViewerPDFCheckBoxFormField = class (TgtPDFCheckBoxFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure GetValueAsString(ACallBack: TgtGetStringCallBackProc);
    procedure SetValueAsString(AValue: String);
    procedure SetFocus();
    procedure GetChecked(ACallBack: TgtGetBooleanCallBackProc);
    procedure SetChecked(AChecked: Boolean);
  end;

  TgtViewerPDFSimplePushButtonFormField = class (TgtPDFSimplePushButtonFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure SetFocus();
  end;

  TgtViewerPDFSubmitPushButtonFormField = class (TgtPDFSubmitPushButtonFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure SetFocus();
  end;

  TgtViewerPDFResetPushButtonFormField = class (TgtPDFResetPushButtonFormField)
  private
    FViewerControl: TgtUniStarDocsViewer;

  public
    property ViewerControl: TgtUniStarDocsViewer write FViewerControl;
    procedure SetFocus();
  end;

  TgtViewerPDFFormFields = class
  private
    FTextFormFields: TArray<TgtViewerPDFTextFormField>;
		FRadioGroupFormFields: TArray<TgtViewerPDFRadioGroupFormField>;
		FComboBoxFormFields: TArray<TgtViewerPDFComboBoxFormField>;
		FListBoxFormFields: TArray<TgtViewerPDFListBoxFormField>;
		FCheckBoxFormFields: TArray<TgtViewerPDFCheckBoxFormField>;
		FSubmitPushButtonFormFields: TArray<TgtViewerPDFSubmitPushButtonFormField>;
		FResetPushButtonFormFields: TArray<TgtViewerPDFResetPushButtonFormField>;
		FSimplePushButtonFormFields: TArray<TgtViewerPDFSimplePushButtonFormField>;

  public
    property TextFormFields: TArray<TgtViewerPDFTextFormField> read FTextFormFields;
		property RadioGroupFormFields: TArray<TgtViewerPDFRadioGroupFormField> read FRadioGroupFormFields;
		property ComboBoxFormFields: TArray<TgtViewerPDFComboBoxFormField> read FComboBoxFormFields;
		property ListBoxFormFields: TArray<TgtViewerPDFListBoxFormField> read FListBoxFormFields;
		property CheckBoxFormFields: TArray<TgtViewerPDFCheckBoxFormField> read FCheckBoxFormFields;
		property SubmitPushButtonFormFields: TArray<TgtViewerPDFSubmitPushButtonFormField> read FSubmitPushButtonFormFields;
		property ResetPushButtonFormFields: TArray<TgtViewerPDFResetPushButtonFormField> read FResetPushButtonFormFields;
		property SimplePushButtonFormFields: TArray<TgtViewerPDFSimplePushButtonFormField> read FSimplePushButtonFormFields;

    function GetAllAsList(AViewerControl: TgtUniStarDocsViewer): TList<TgtPDFFormField>;
  end;

implementation

constructor TgtUniStarDocsViewer.Create(AOwner: TComponent);
begin
  inherited;
  FForms := TgtUniStarDocsViewerFormsAPI.Create(Self);
end;

procedure TgtUniStarDocsViewer.ConfigJSClasses(ALoading: Boolean);
begin
  JSObjects.DefaultJSClassName := 'Ext.gnostice.stardocsviewer';
end;

procedure TgtUniStarDocsViewer.Load(AFileNameWithPath: string; APassword: string = '');
var
  DocObject: TgtDocObject;
  ViewResponse: TgtCreateViewResponse;
begin
  FStarDocsSDK.Auth.LoginApp;
  DocObject := nil;
  ViewResponse := nil;
  try
    DocObject := FStarDocsSDK.Storage.Upload(AFileNameWithPath, APassword);
    ViewResponse := FStarDocsSDK.Viewer.CreateView(DocObject, APassword);
    Self.URL := ViewResponse.Url;
  finally
    if Assigned(ViewResponse) then
      ViewResponse.Free;
    if Assigned(DocObject) then
      DocObject.Free;
  end;
end;

procedure TgtUniStarDocsViewer.Load(AStream: TStream; AFileName: string; APassword: string = '');
var
  DocObject: TgtDocObject;
  ViewResponse: TgtCreateViewResponse;
begin
  FStarDocsSDK.Auth.LoginApp;
  DocObject := nil;
  ViewResponse := nil;
  try
    DocObject := FStarDocsSDK.Storage.Upload(AStream, AFileName, APassword);
    ViewResponse := FStarDocsSDK.Viewer.CreateView(DocObject, APassword);
    Self.URL := ViewResponse.Url;
  finally
    if Assigned(ViewResponse) then
      ViewResponse.Free;
    if Assigned(DocObject) then
      DocObject.Free;
  end;
end;

procedure TgtUniStarDocsViewer.H_OnFormLoaded(AThis: TJSObject; EventName: string; AParams: TUniStrings);
begin
  if Assigned(FOnFormLoaded) then
    FOnFormLoaded(Self);
end;

procedure TgtUniStarDocsViewer.H_OnFormModified(AThis: TJSObject; EventName: string; AParams: TUniStrings);
begin
  if Assigned(FOnFormModified) then
    FOnFormModified(Self);
end;

procedure TgtUniStarDocsViewer.H_BeforeFormSubmit(AThis: TJSObject; EventName: string; AParams: TUniStrings);
var
  FormDataJsonStr: string;
  FormData: TgtPDFFormData;
begin
  if Assigned(FBeforeFormSubmit) then
  begin
    // Convert stringified JSON form field object to Delphi type
    FormDataJsonStr := AParams['FormData'].AsString;
    FormData := TJson.JsonToObject<TgtPDFFormData>(FormDataJsonStr);
    FBeforeFormSubmit(Self, FormData);
  end;
end;

procedure TgtUniStarDocsViewer.H_AfterFormSubmit(AThis: TJSObject; EventName: string; AParams: TUniStrings);
begin
  if Assigned(FAfterFormSubmit) then
    FAfterFormSubmit(Self);
end;

procedure TgtUniStarDocsViewer.H_OnFormFieldEvent(AThis: TJSObject; EventName: string; AParams: TUniStrings);
var
  FormFieldJsonStr: string;
  FormField: TgtPDFFormField;
begin
  // Convert stringified JSON form field object to Delphi type
  FormFieldJsonStr := AParams['FormField'].AsString;
  FormField := ConvertJsonToFormField(FormFieldJsonStr);
  // Dispatch based on event name
  if (EventName = 'onFieldClicked') And Assigned(FOnFormFieldClicked) then
    FOnFormFieldClicked(Self, FormField);
  if (EventName = 'onFieldFocusIn') And Assigned(FOnFormFieldEnter) then
    FOnFormFieldEnter(Self, FormField);
  if (EventName = 'onFieldFocusOut') And Assigned(FOnFormFieldExit) then
    FOnFormFieldExit(Self, FormField);
  if (EventName = 'onFieldChanged') And Assigned(FOnFormFieldChanged) then
    FOnFormFieldChanged(Self, FormField);
end;

procedure TgtUniStarDocsViewer.SetOnFormLoaded(AValue: TNotifyEvent);
begin
  FOnFormLoaded := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormLoaded) then
      JSAddEvent('onFormLoaded', [], H_OnFormLoaded)
    else
      JSRemoveEvent('onFormLoaded', H_OnFormLoaded);
  end;
end;

procedure TgtUniStarDocsViewer.SetOnFormModified(AValue: TNotifyEvent);
begin
  FOnFormModified := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormModified) then
      JSAddEvent('onFormModified', [], H_OnFormModified)
    else
      JSRemoveEvent('onFormModified', H_OnFormModified);
  end;
end;

procedure TgtUniStarDocsViewer.SetBeforeFormSubmit(AValue: TgtBeforeFormSubmitEvent);
begin
  FBeforeFormSubmit := AValue;
  if WebMode then
  begin
    if Assigned(FBeforeFormSubmit) then
      JSAddEvent('onBeforeFormSubmit', ['FormData', '%1'], H_BeforeFormSubmit)
    else
      JSRemoveEvent('onBeforeFormSubmit', H_BeforeFormSubmit);
  end;
end;

procedure TgtUniStarDocsViewer.SetAfterFormSubmit(AValue: TNotifyEvent);
begin
  FAfterFormSubmit := AValue;
  if WebMode then
  begin
    if Assigned(FAfterFormSubmit) then
      JSAddEvent('onAfterFormSubmit', [], H_AfterFormSubmit)
    else
      JSRemoveEvent('onAfterFormSubmit', H_AfterFormSubmit);
  end;
end;

procedure TgtUniStarDocsViewer.SetOnFormFieldClicked(AValue: TgtFieldEvent);
begin
  FOnFormFieldClicked := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormFieldClicked) then
      JSAddEvent('onFieldClicked', ['FormField', '%1'], H_OnFormFieldEvent)
    else
      JSRemoveEvent('onFieldClicked', H_OnFormFieldEvent);
  end;
end;

procedure TgtUniStarDocsViewer.SetOnFormFieldEnter(AValue: TgtFieldEvent);
begin
  FOnFormFieldEnter := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormFieldEnter) then
      JSAddEvent('onFieldFocusIn', ['FormField', '%1'], H_OnFormFieldEvent)
    else
      JSRemoveEvent('onFieldFocusIn', H_OnFormFieldEvent);
  end;
end;

procedure TgtUniStarDocsViewer.SetOnFormFieldExit(AValue: TgtFieldEvent);
begin
  FOnFormFieldExit := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormFieldExit) then
      JSAddEvent('onFieldFocusOut', ['FormField', '%1'], H_OnFormFieldEvent)
    else
      JSRemoveEvent('onFieldFocusOut', H_OnFormFieldEvent);
  end;
end;

procedure TgtUniStarDocsViewer.SetOnFormFieldChanged(AValue: TgtFieldEvent);
begin
  FOnFormFieldChanged := AValue;
  if WebMode then
  begin
    if Assigned(FOnFormFieldChanged) then
      JSAddEvent('onFieldChanged', ['FormField', '%1'], H_OnFormFieldEvent)
    else
      JSRemoveEvent('onFieldChanged', H_OnFormFieldEvent);
  end;
end;

procedure TgtUniStarDocsViewer.I_Forms_GetAllFormFields(ACallBack: TgtGetAllFormFieldsCallBackProc);
begin
  FForms_GetAllFormFields_CallBack := ACallBack;
  JSCall('getAllFormFields');
end;

procedure TgtUniStarDocsViewer.I_Forms_GetFormField(AFormFieldName: String; ACallBack: TgtGetFormFieldCallBackProc);
begin
  FForms_GetFormField_CallBack := ACallBack;
  JSCall('getFormField', [AFormFieldName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_SubmitForm(ASubmitUrl: String; ASubmitMethod: TgtFormSubmitMethod = fsmPOST; AIncludeNoValueFields: Boolean = False;
    ASubmitFields: TList<String> = nil; AIsIncludeList: Boolean = True; ACallBackSuccess: TgtSubmitFormSuccessCallBackProc = nil; ACallBackFailure: TgtSubmitFormFailureCallBackProc = nil);
var
  SubmitMethodStr: String;
  SubmitFieldsCSV: String;
begin
  SubmitMethodStr := 'POST';
  if ASubmitMethod = fsmGET then
    SubmitMethodStr := 'GET';

  SubmitFieldsCSV := '';
  if Assigned(ASubmitFields) then
    SubmitFieldsCSV := JoinStrArray(ASubmitFields, ',');

  FForms_SubmitFormSuccess_CallBack := ACallBackSuccess;
  FForms_SubmitFormFailure_CallBack := ACallBackFailure;

  JSCall('submitForm', [ASubmitUrl, SubmitMethodStr, AIncludeNoValueFields, SubmitFieldsCSV, AIsIncludeList]);
end;

procedure TgtUniStarDocsViewer.I_Forms_ResetForm(AResetFields: TList<String> = nil; AIsIncludeList: Boolean = True);
var
   ResetFieldsCSV: String;
begin
  // Convert TList to CSV list
  ResetFieldsCSV := JoinStrArray(AResetFields, ',');
  JSCall('resetForm', [ResetFieldsCSV, AIsIncludeList]);
end;

procedure TgtUniStarDocsViewer.I_Forms_FocusForm;
begin
  JSCall('focusForm');
end;

procedure TgtUniStarDocsViewer.I_Forms_SetFocus(AFullyQualifiedName: String);
begin
  JSCall('setFocus', [AFullyQualifiedName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_SetValueAsString(AFullyQualifiedName: String; AValue: String);
begin
  JSCall('setValueAsString', [AFullyQualifiedName, AValue]);
end;

procedure TgtUniStarDocsViewer.I_Forms_GetValueAsString(AFullyQualifiedName: String; ACallBack: TgtGetStringCallBackProc);
begin
  FForms_GetValueAsString_CallBack := ACallBack;
  JSCall('getValueAsString', [AFullyQualifiedName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_GetSelectedRadioButtonIndex(AFullyQualifiedName: String; ACallBack: TgtGetIntegerCallBackProc);
begin
  FForms_GetRadioButtonSelectedIndex_CallBack := ACallBack;
  JSCall('getSelectedRadioButtonIndex', [AFullyQualifiedName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_SetSelectedRadioButtonIndex(AFullyQualifiedName: String; AIndex: Integer);
begin
  JSCall('setSelectedRadioButtonIndex', [AFullyQualifiedName, IntToStr(AIndex)]);
end;

procedure TgtUniStarDocsViewer.I_Forms_GetSelectedItemIndices(AFullyQualifiedName: String; ACallBack: TgtGetIntegerArrayCallBackProc);
begin
  FForms_ComboListBoxSelectedItemIndices_CallBack := ACallBack;
  JSCall('getSelectedItemIndices', [AFullyQualifiedName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_SetSelectedItemIndices(AFullyQualifiedName: String; AIndices: TList<Integer>);
var
  IndicesCSV: String;
begin
  // Convert array to csv
  IndicesCSV := JoinIntArray(AIndices, ',');
  JSCall('setSelectedItemIndices', [AFullyQualifiedName, IndicesCSV]);
end;

procedure TgtUniStarDocsViewer.I_Forms_GetChecked(AFullyQualifiedName: String; ACallBack: TgtGetBooleanCallBackProc);
begin
  FForms_GetCheckBoxChecked_CallBack := ACallBack;
  JSCall('getChecked', [AFullyQualifiedName]);
end;

procedure TgtUniStarDocsViewer.I_Forms_SetChecked(AFullyQualifiedName: String; AChecked: Boolean);
begin
  JSCall('setChecked', [AFullyQualifiedName, AChecked]);
end;

procedure TgtUniStarDocsViewer.DOHandleEvent(AEventName: string; AParams: TUniStrings);
var
  Param0: String;
  Param1: String;
  ParamInt: Integer;
  ParamIntList: TList<Integer>;
  ParamBoolean: Boolean;
  FormFields: TList<TgtPDFFormField>;
  FormField: TgtPDFFormField;
  ViewerFormFields: TgtViewerPDFFormFields;
begin
  inherited;
  if AEventName = 'getAllFormFields' then
  begin
    // Convert stringified JSON form field object to Delphi type
    Param0 := AParams['gnParam0'].AsString;
    if Param0 <> 'null' then
    begin
      ViewerFormFields := TJson.JsonToObject<TgtViewerPDFFormFields>(Param0);
      FormFields := ViewerFormFields.GetAllAsList(Self);
    end
    Else
      FormFields := TList<TgtPDFFormField>.Create();
    FForms_GetAllFormFields_CallBack(FormFields);
  end
  else if AEventName = 'getFormField' then
  begin
    // Convert stringified JSON form field object to Delphi type
    Param0 := AParams['gnParam0'].AsString;
    FormField := nil;
    if Param0 <> 'null' then
    begin
      FormField := ConvertJSONToFormField(Param0);
    end;
    FForms_GetFormField_CallBack(FormField);
  end
  else if (AEventName = 'submitFormSuccess') and Assigned(FForms_SubmitFormSuccess_CallBack) then
  begin
    FForms_SubmitFormSuccess_CallBack();
  end
  else if (AEventName = 'submitFormFailure') and Assigned(FForms_SubmitFormFailure_CallBack) then
  begin
    Param0 := AParams['gnParam0'].AsString;
    Param1 := AParams['gnParam1'].AsString;
    FForms_SubmitFormFailure_CallBack(Param0, Param1);
  end
  else if (AEventName = 'getValueAsString') and Assigned(FForms_GetValueAsString_CallBack) then
  begin
    Param0 := AParams['gnParam0'].AsString;
    FForms_GetValueAsString_CallBack(Param0);
  end
  else if (AEventName = 'getRadioButtonSelectedIndex') and Assigned(FForms_GetRadioButtonSelectedIndex_CallBack) then
  begin
    ParamInt := AParams['gnParam0'].AsInteger;
    FForms_GetRadioButtonSelectedIndex_CallBack(ParamInt);
  end
  else if (AEventName = 'getComboListBoxSelectedItemIndices') and Assigned(FForms_ComboListBoxSelectedItemIndices_CallBack) then
  begin
    Param0 := AParams['gnParam0'].AsString;
    // Split CSV to array of integers
    ParamIntList := SplitToInt(Param0, ',');
    FForms_ComboListBoxSelectedItemIndices_CallBack(ParamIntList);
  end
  else if (AEventName = 'getCheckBoxChecked') and Assigned(FForms_GetCheckBoxChecked_CallBack) then
  begin
    ParamBoolean := AParams['gnParam0'].AsBoolean;
    FForms_GetCheckBoxChecked_CallBack(ParamBoolean);
  end
end;

function TgtUniStarDocsViewer.ConvertJsonToFormField(JsonStr: String): TgtPDFFormField;
var
  FieldType: String;
begin
  // First get the base type
  Result := TJson.JsonToObject<TgtPDFFormField>(JsonStr);
  // Convert to specific type
  FieldType := Result.FieldType;
  If FieldType = 'text' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFTextFormField>(JsonStr);
    (Result as TgtViewerPDFTextFormField).ViewerControl := Self;
  end
  Else if FieldType = 'radioGroup' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFRadioGroupFormField>(JsonStr);
    (Result as TgtViewerPDFRadioGroupFormField).ViewerControl := Self;
  end
  Else if FieldType = 'comboBox' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFComboBoxFormField>(JsonStr);
    (Result as TgtViewerPDFComboBoxFormField).ViewerControl := Self;
  end
  Else if FieldType = 'listBox' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFListBoxFormField>(JsonStr);
    (Result as TgtViewerPDFListBoxFormField).ViewerControl := Self;
  end
  Else if FieldType = 'checkBox' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFCheckBoxFormField>(JsonStr);
    (Result as TgtViewerPDFCheckBoxFormField).ViewerControl := Self;
  end
  Else if FieldType = 'submitPushButton' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFSubmitPushButtonFormField>(JsonStr);
    (Result as TgtViewerPDFSubmitPushButtonFormField).ViewerControl := Self;
  end
  Else if FieldType = 'resetPushButton' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFResetPushButtonFormField>(JsonStr);
    (Result as TgtViewerPDFResetPushButtonFormField).ViewerControl := Self;
  end
  Else if FieldType = 'simplePushButton' then
  begin
    Result := TJson.JsonToObject<TgtViewerPDFSimplePushButtonFormField>(JsonStr);
    (Result as TgtViewerPDFSimplePushButtonFormField).ViewerControl := Self;
  end;
end;

function TgtUniStarDocsViewer.JoinStrArray(AItems: TList<String>; ASeparator: String): String;
var
  Index: Integer;
begin
  Result := '';
  if Assigned(AItems) then
  begin
    for Index := 1 to AItems.Count do
      if Index < AItems.Count then
        Result := Result + AItems[Index] + ASeparator
      Else
        Result := Result + AItems[Index];
  end;
end;

function TgtUniStarDocsViewer.SplitToInt(AStr: String; ASeparator: String): TList<Integer>;
var
  StrArray: TArray<String>;
  Index: Integer;
begin
  Result := TList<Integer>.Create;
  try
    StrArray := AStr.Split([ASeparator]);
    for Index := 0 to Length(StrArray) - 1 do
      Result.Add(StrToInt(StrArray[Index]));
  except
    Result.Free;
  end;
end;

function TgtUniStarDocsViewer.JoinIntArray(AItems: TList<Integer>; ASeparator: String): String;
var
  Index: Integer;
begin
  Result := '';
  if Assigned(AItems) then
  begin
    for Index := 1 to AItems.Count do
      if Index < AItems.Count then
        Result := Result + IntToStr(AItems[Index]) + ASeparator
      Else
        Result := Result + IntToStr(AItems[Index]);
  end;
end;

{ TgtUniStarDocsViewerFormsAPI }
constructor TgtUniStarDocsViewerFormsAPI.Create(AViewerControl: TgtUniStarDocsViewer);
begin
  FViewerControl := AViewerControl;
end;

procedure TgtUniStarDocsViewerFormsAPI.GetAllFormFields(ACallBack: TgtGetAllFormFieldsCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetAllFormFields(ACallBack);
end;

procedure TgtUniStarDocsViewerFormsAPI.GetFormField(AFormFieldName: String; ACallBack: TgtGetFormFieldCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetFormField(AFormFieldName, ACallBack);
end;

procedure TgtUniStarDocsViewerFormsAPI.SubmitForm(ASubmitUrl: String; ASubmitMethod: TgtFormSubmitMethod = fsmPOST; AIncludeNoValueFields: Boolean = False;
        ASubmitFields: TList<String> = nil; AIsIncludeList: Boolean = True; ACallBackSuccess: TgtSubmitFormSuccessCallBackProc = nil; ACallBackFailure: TgtSubmitFormFailureCallBackProc = nil);
begin
  FViewerControl.I_Forms_SubmitForm(ASubmitUrl, ASubmitMethod, AIncludeNoValueFields,
        ASubmitFields, AIsIncludeList, ACallBackSuccess, ACallBackFailure);
end;

procedure TgtUniStarDocsViewerFormsAPI.ResetForm(AResetFields: TList<String> = nil; AIsIncludeList: Boolean = True);
begin
  FViewerControl.I_Forms_ResetForm(AResetFields, AIsIncludeList);
end;

procedure TgtUniStarDocsViewerFormsAPI.FocusForm;
begin
  FViewerControl.I_Forms_FocusForm;
end;

{ TgtViewerPDFTextFormField }

procedure TgtViewerPDFTextFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

procedure TgtViewerPDFTextFormField.GetValueAsString(ACallBack: TgtGetStringCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetValueAsString(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFTextFormField.SetValueAsString(AValue: String);
begin
  FViewerControl.I_Forms_SetValueAsString(FullyQualifiedName, AValue);
end;

{ TgtViewerPDFRadioButtonFormField }

procedure TgtViewerPDFRadioButtonFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

{ TgtViewerPDFRadioGroupFormField }

procedure TgtViewerPDFRadioGroupFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

procedure TgtViewerPDFRadioGroupFormField.GetValueAsString(ACallBack: TgtGetStringCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetValueAsString(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFRadioGroupFormField.SetValueAsString(AValue: String);
begin
  FViewerControl.I_Forms_SetValueAsString(FullyQualifiedName, AValue);
end;

procedure TgtViewerPDFRadioGroupFormField.GetSelectedRadioButtonIndex(ACallBack: TgtGetIntegerCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetSelectedRadioButtonIndex(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFRadioGroupFormField.SetSelectedRadioButtonIndex(AIndex: Integer);
begin
  FViewerControl.I_Forms_SetSelectedRadioButtonIndex(FullyQualifiedName, AIndex);
end;

procedure TgtViewerPDFComboBoxFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

procedure TgtViewerPDFComboBoxFormField.GetValueAsString(ACallBack: TgtGetStringCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetValueAsString(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFComboBoxFormField.SetValueAsString(AValue: String);
begin
  FViewerControl.I_Forms_SetValueAsString(FullyQualifiedName, AValue);
end;

procedure TgtViewerPDFComboBoxFormField.GetSelectedItemIndices(ACallBack: TgtGetIntegerArrayCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetSelectedItemIndices(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFComboBoxFormField.SetSelectedItemIndices(AIndices: TList<Integer>);
begin
  FViewerControl.I_Forms_SetSelectedItemIndices(FullyQualifiedName, AIndices);
end;

procedure TgtViewerPDFListBoxFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

procedure TgtViewerPDFListBoxFormField.GetValueAsString(ACallBack: TgtGetStringCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetValueAsString(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFListBoxFormField.SetValueAsString(AValue: String);
begin
  FViewerControl.I_Forms_SetValueAsString(FullyQualifiedName, AValue);
end;

procedure TgtViewerPDFListBoxFormField.GetSelectedItemIndices(ACallBack: TgtGetIntegerArrayCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetSelectedItemIndices(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFListBoxFormField.SetSelectedItemIndices(AIndices: TList<Integer>);
begin
  FViewerControl.I_Forms_SetSelectedItemIndices(FullyQualifiedName, AIndices);
end;

procedure TgtViewerPDFCheckBoxFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

procedure TgtViewerPDFCheckBoxFormField.GetValueAsString(ACallBack: TgtGetStringCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetValueAsString(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFCheckBoxFormField.SetValueAsString(AValue: String);
begin
  FViewerControl.I_Forms_SetValueAsString(FullyQualifiedName, AValue);
end;

procedure TgtViewerPDFCheckBoxFormField.GetChecked(ACallBack: TgtGetBooleanCallBackProc);
begin
  if Assigned(ACallBack) then
    FViewerControl.I_Forms_GetChecked(FullyQualifiedName, ACallBack);
end;

procedure TgtViewerPDFCheckBoxFormField.SetChecked(AChecked: Boolean);
begin
  FViewerControl.I_Forms_SetChecked(FullyQualifiedName, AChecked);
end;

{ TgtViewerPDFPushButtonFormField }
procedure TgtViewerPDFSimplePushButtonFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

{ TgtViewerPDFSubmitPushButtonFormField }
procedure TgtViewerPDFSubmitPushButtonFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

{ TgtViewerPDFResetPushButtonFormField }
procedure TgtViewerPDFResetPushButtonFormField.SetFocus();
begin
  FViewerControl.I_Forms_SetFocus(FullyQualifiedName);
end;

{ TgtViewerPDFFormFields }
function TgtViewerPDFFormFields.GetAllAsList(AViewerControl: TgtUniStarDocsViewer): TList<TgtPDFFormField>;
var
  Index: Integer;
begin
  Result := TList<TgtPDFFormField>.Create();
  for Index := 0 to (Length(TextFormFields) - 1) do
  begin
    TextFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(TextFormFields[Index]);
  end;
  for Index := 0 to (Length(RadioGroupFormFields) - 1) do
  begin
    RadioGroupFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(RadioGroupFormFields[Index]);
  end;
  for Index := 0 to (Length(ComboBoxFormFields) - 1) do
  begin
    ComboBoxFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(ComboBoxFormFields[Index]);
  end;
  for Index := 0 to (Length(ListBoxFormFields) - 1) do
  begin
    ListBoxFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(ListBoxFormFields[Index]);
  end;
  for Index := 0 to (Length(CheckBoxFormFields) - 1) do
  begin
    CheckBoxFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(CheckBoxFormFields[Index]);
  end;
  for Index := 0 to (Length(SubmitPushButtonFormFields) - 1) do
  begin
    SubmitPushButtonFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(SubmitPushButtonFormFields[Index]);
  end;
  for Index := 0 to (Length(ResetPushButtonFormFields) - 1) do
  begin
    ResetPushButtonFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(ResetPushButtonFormFields[Index]);
  end;
  for Index := 0 to (Length(SimplePushButtonFormFields) - 1) do
  begin
    SimplePushButtonFormFields[Index].ViewerControl := AViewerControl;
    Result.Add(SimplePushButtonFormFields[Index]);
  end;
end;

initialization
  UniAddJSLibrary('gnostice-ext-js/stardocs-viewer-interface.js', False, [upoFolderJS, upoPlatformDesktop]);
  UniAddJSLibrary('gnostice-ext-js/ext.gnostice.stardocsviewer.js', False, [upoFolderJS, upoPlatformDesktop]);
end.
