{ ------------------------------------------------------------
SimpleRestRequest - A Simple, fluent interfaced REST client for
Delphi XE and up.

https://github.com/jamiei/SimpleRestClient

Licensed under the BSD-3 Open source license.
--------------------------------------------------------------}

{
Copyright (c) 2015, Jamie Ingilby All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer. Redistributions in binary form
must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution. Neither the name of SimpleRestClient for Delphi nor the names
of its contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}



///	<summary>
///	  Contains TRestRequest - The simple, fluent wrapper around Indy's TIdHttp
///	  to make writing RESTful clients easy.
///	</summary>
unit RestRequest;

interface

uses
  SysUtils,
  Classes,

  IdHttp,
  IdAuthentication,
  IdMultipartFormData,
  IdURI,
  IdSSL,
  IdSSLOpenSSL;


  type
    TBeforeRequest = procedure(Sender: TObject; var AHttpClient: TIdHttp) of Object;

    THttpResponse = record

      ///	<summary>
      ///	  The Http Response code returned
      ///	</summary>
      ResponseCode: integer;

      ///	<summary>
      ///	  The Body returned?or an exception message.
      ///	</summary>
      ResponseStr: string;

      ///	<summary>
      ///	  The content type of the response
      ///	</summary>
      ResponseContentType: string;

      LocationHeader: string;
    end;

    TStreamList = record
      FileField: string;
      FileName: string;
      FileStream: TStream;
    end;

    TRestRequest = class(TObject)
      private
        FBasicAuth: boolean;
        FBearerAuth: boolean;
        FScheme: string;
        FDomain: string;
        FPaths: TStringList;
        FUrlParams: TStringList;
        FBodyParams: TStringList;
        FFileParams: TStringList;
        FFileParamsStream: TArray<TStreamList>;
        FHeaders: TStringList;
        FUsername: string;
        FPassword: string;
        FBearerToken: string;
        FResponse: THttpResponse;
        FAccept: string;
        FContentType: string;
        FReadTimeout: Integer;
        FBeforeRequest: TBeforeRequest;
        FSslHandler: TIdSSLIOHandlerSocketOpenSSL;
        FAlwaysMultipartFormData: boolean;

        procedure doBeforeRequest(AHttpInst: TIdHttp);
        function getHttpClientInstance: TIdHttp;
        function getURLAsStr: string;
//        function urlEncode(str: string): string;
        function doPost(aParams: TIdMultiPartFormDataStream): THttpResponse; overload;
        function doPost(aParams: TStringStream): THttpResponse; overload;
        function multipartRequired: boolean;
        function createMultiPartFormDataStreamFromStringList: TIdMultiPartFormDataStream;
        function createStringStreamFromStringList(aBody: string): TStringStream;
        procedure httpAuthorisation(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
        function createSSLHandlerIfRequired(scheme: string; var httpClient: TIdHttp): boolean;

        function getAccept: string;
        procedure setAccept(const Value: string);
        function getContentType: string;
        procedure setContentType(const Value: string);
        function GetBeforeRequest: TBeforeRequest;
        procedure SetBeforeRequest(const Value: TBeforeRequest);
      public
        constructor Create(aIsSSL: boolean); overload;
        constructor Create; overload;
        destructor Destroy; override;
        function Scheme(aScheme: string): TRestRequest;
        function Domain(aDomain: string): TRestRequest;
        function Path(aPath: string): TRestRequest;
        function UrlParam(aKey: string; aValue: string): TRestRequest;
        function BodyParam(aKey: string; aValue: string): TRestRequest;
        function FileParam(aKey: string; aValue: string): TRestRequest; overload;
        function FileParam(aFieldName, aFileName: string; aStream: TStream): TRestRequest; overload;
        function WithReadTimeout(timeout: Integer): TRestRequest;
        function WithHeader(aName: string; aValue: string): TRestRequest;
        function WithCredentials(username, password: string): TRestRequest;
        function WithBearerToken(bearerToken: string): TRestRequest;

        property Response: THttpResponse read FResponse;
        property FullUrl: string read getURLAsStr;
        property Accept: string read getAccept write setAccept;
        property ContentType: string read getContentType write setContentType;
        property BeforeRequest: TBeforeRequest read GetBeforeRequest write SetBeforeRequest;
        property AlwaysMultipartFormData: boolean read FAlwaysMultipartFormData
          write FAlwaysMultipartFormData;

        function Get: THttpResponse;
        // Use for file downloads
        function GetToStream(AOutStream: TStream): THttpResponse;
        function Put(aBody: string): THttpResponse;
        function Post(aBody: string): THttpResponse;
        function Delete: THttpResponse;
        function Options: THttpResponse;
    end;

implementation

{ TRestRequest }

constructor TRestRequest.Create(aIsSSL: boolean);
const
  DEFAULT_ACCEPT = 'application/json, application/xml';
  DEFAULT_SCHEME = 'http';
begin
  inherited Create;
  Self.FPaths := TStringList.Create;
  Self.FUrlParams := TStringList.Create;
  Self.FBodyParams := TStringList.Create;
  Self.FFileParams := TStringList.Create;
  Self.FHeaders := TStringList.Create;
  Self.FAccept := DEFAULT_ACCEPT;
  Self.FScheme := DEFAULT_SCHEME;
  Self.FContentType := 'application/json';
  Self.FBasicAuth := false;
  Self.FBearerAuth := false;
  Self.FReadTimeout := 30000;  // ms default
end;

constructor TRestRequest.Create;
begin
  Create(false);
end;

function TRestRequest.createMultiPartFormDataStreamFromStringList: TIdMultiPartFormDataStream;
var
  i: integer;
  key, value: string;
begin
  Result := TIdMultiPartFormDataStream.Create;
  (*for i := 0 to strings.Count - 1 do
  begin
    key := strings.Names[i];
    value := strings.ValueFromIndex[i];
    Result.AddFormField(key, value);
  end;*)
  for i := 0 to FFileParams.Count - 1 do
  begin
    key := FFileParams.Names[i];
    value := FFileParams.ValueFromIndex[i];
    Result.AddFile(key, value);
  end;

  for i := 0 to Length(FFileParamsStream) - 1 do
  begin
    Result.AddFormField(FFileParamsStream[i].FileField, '', '', FFileParamsStream[i].FileStream, FFileParamsStream[i].FileName);
  end;

  // Add other form data
  for i := 0 to FBodyParams.Count - 1 do
  begin
    key := FBodyParams.Names[i];
    value := FBodyParams.ValueFromIndex[i];
    Result.AddFormField(key, value);
  end;
end;

function TRestRequest.createSSLHandlerIfRequired(scheme: string; var httpClient: TIdHttp): boolean;
begin
  if scheme = 'https' then Self.FSslHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  if Assigned(Self.FSslHandler) then httpClient.IOHandler := FSslHandler;
  Result := True
end;

function TRestRequest.createStringStreamFromStringList(
  aBody: string): TStringStream;
//var
//  i: Integer;
//  key, value: string;
//  strParam: string;
begin
  Result := TStringStream.Create('');
  Result.WriteString(aBody);
  (*for i := 0 to strings.Count - 1 do
  begin
    key := strings.Names[i];
    value := strings.ValueFromIndex[i];
    strParam := urlEncode(key) + '=' + urlEncode(value);
    if not (i = strings.Count - 1) then strParam := strParam + '&';
    Result.WriteString(strParam);
  end;*)
end;

function TRestRequest.Delete: THttpResponse;
var
  httpClient: TIdHttp;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      httpClient.Delete(getURLAsStr);
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := httpClient.ResponseText;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

destructor TRestRequest.Destroy;
begin
  Self.FPaths.Free;
  Self.FUrlParams.Free;
  Self.FBodyParams.Free;
  Self.FFileParams.Free;
  Self.FHeaders.Free;
  if Assigned(FSslHandler) then FSslHandler.Free;

  inherited;
end;

procedure TRestRequest.doBeforeRequest(AHttpInst: TIdHttp);
begin
  if Assigned(FBeforeRequest) then FBeforeRequest(Self, AHttpInst);
end;

function TRestRequest.Domain(aDomain: string): TRestRequest;
begin
  Self.FDomain := Trim(aDomain);
  if FDomain.LowerCase(FDomain).StartsWith('https') then
  begin
    Self.Scheme('https');
  end;
  Result := Self;
end;

function TRestRequest.doPost(
  aParams: TIdMultiPartFormDataStream): THttpResponse;
var
  httpClient: TIdHttp;
  respStr: string;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      httpClient.Request.ContentType := 'multipart/form-data';
      respStr := httpClient.Post(getURLAsStr, aParams);
      Result.LocationHeader := httpClient.Response.Location;
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := respStr;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

function TRestRequest.doPost(aParams: TStringStream): THttpResponse;
var
  httpClient: TIdHttp;
  respStr: string;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      //httpClient.Request.ContentType := 'application/json';
      httpClient.Request.ContentType := Self.FContentType;
      respStr := httpClient.Post(getURLAsStr, aParams);
      Result.LocationHeader := httpClient.Response.Location;
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := respStr;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

function TRestRequest.FileParam(aKey, aValue: string): TRestRequest;
begin
  Self.FFileParams.Add(aKey + '=' + aValue);
  Result := Self;
end;

function TRestRequest.FileParam(aFieldName, aFileName: string; aStream: TStream): TRestRequest;
var
  numItems: Integer;
begin
  numItems := Length(FFileParamsStream);
  SetLength(FFileParamsStream, numItems + 1);
  FFileParamsStream[numItems].FileField := aFieldName;
  FFileParamsStream[numItems].FileName := aFileName;
  FFileParamsStream[numItems].FileStream := aStream;
  Result := Self;
end;

function TRestRequest.Get: THttpResponse;
var
  httpClient: TIdHttp;
  respStr: string;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      respStr := httpClient.Get(getURLAsStr);
      Result.LocationHeader := httpClient.Response.Location;
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := respStr;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

function TRestRequest.GetToStream(AOutStream: TStream): THttpResponse;
var
  httpClient: TIdHttp;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      httpClient.Get(getURLAsStr, AOutStream);
      Result.LocationHeader := httpClient.Response.Location;
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := '';
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := E.ErrorCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

function TRestRequest.getAccept: string;
begin
  Result := FAccept;
end;

function TRestRequest.GetBeforeRequest: TBeforeRequest;
begin
  Result := FBeforeRequest;
end;

function TRestRequest.getContentType: string;
begin
  Result := FContentType;
end;

function TRestRequest.getHttpClientInstance: TIdHttp;
begin
  Result := TIdHttp.Create(nil);
  Result.ConnectTimeout := 30000;
  Result.ReadTimeout := Self.FReadTimeout;
  Result.OnAuthorization := httpAuthorisation;
  Result.MaxAuthRetries := 0;
  Result.HTTPOptions := [hoInProcessAuth];
  // Create an SSL Handler if we need to.
  createSSLHandlerIfRequired(Self.FScheme, Result);
  doBeforeRequest(Result);
  Result.Request.CustomHeaders.Clear;
  Result.Request.CustomHeaders.AddStrings(Self.FHeaders);
  Result.Request.BasicAuthentication := Self.FBasicAuth;
  if Self.FBasicAuth then
  begin
    Result.Request.Username := Self.FUsername;
    Result.Request.Password := Self.FPassword;
  end
  else if Self.FBearerAuth then
  begin
    Result.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + Self.FBearerToken;
  end;
  Result.Request.Accept := FAccept;
  Result.Request.ContentType := FContentType;
end;

function TRestRequest.getURLAsStr: string;
var
  aFullPath: string;
  aFullParams: string;
  i: integer;
begin
  for i := 0 to Self.FPaths.Count - 1 do
  begin
    aFullPath := aFullPath + '/' + Self.FPaths.Strings[i];
  end;
  if Self.FUrlParams.Count > 0 then
  begin
    aFullParams := '?';
    for i := 0 to Self.FUrlParams.Count - 1 do
    begin
      if i > 0 then aFullParams := aFullParams + '&';
      aFullParams := aFullParams + Self.FUrlParams.Names[i] + '=' + Self.FUrlParams.ValueFromIndex[i];
    end;
  end;
  Result := {FScheme + '://' +} FDomain + aFullPath + aFullParams;
end;

procedure TRestRequest.httpAuthorisation(Sender: TObject;
  Authentication: TIdAuthentication; var Handled: Boolean);
begin
  Authentication.Username := Self.FUsername;
  Authentication.Password := Self.FPassword;
  Handled := true;
end;

function TRestRequest.multipartRequired: boolean;
begin
  Result := false;
  if (FFileParams.Count > 0) or (Length(FFileParamsStream) > 0)
    or (FAlwaysMultipartFormData) then
  begin
    Result := true;
  end;
end;

function TRestRequest.Options: THttpResponse;
var
  httpClient: TIdHttp;
  {respStr: string;}
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      httpClient.Options(Self.FullUrl);
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := httpClient.ResponseText;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

function TRestRequest.UrlParam(aKey, aValue: string): TRestRequest;
begin
  Self.FUrlParams.Add(aKey + '=' + aValue);
  Result := Self;
end;

function TRestRequest.BodyParam(aKey: string; aValue: string): TRestRequest;
begin
  Self.FBodyParams.Add(aKey + '=' + aValue);
  Result := Self;
end;

function TRestRequest.Path(aPath: string): TRestRequest;
begin
  Self.FPaths.Append(aPath);
  Result := Self;
end;

function TRestRequest.Post(aBody: string): THttpResponse;
var
  aParamStream: TStringStream;
  aParamMulti: TIdMultiPartFormDataStream;
begin
  if not Self.multipartRequired then
  begin
    aParamStream := Self.createStringStreamFromStringList(aBody);
    try
      Result := doPost(aParamStream);
    finally
      aParamStream.Free;
    end;
  end
  else
  begin
    aParamMulti := Self.createMultiPartFormDataStreamFromStringList;
    try
      Result := doPost(aParamMulti);
    finally
      aParamMulti.Free;
    end;
  end;
end;

function TRestRequest.Put(aBody: string): THttpResponse;
var
  httpClient: TIdHttp;
  params: TStringStream;
  respStr: string;
begin
  httpClient := getHttpClientInstance;
  try
    httpClient.OnAuthorization := Self.httpAuthorisation;
    try
      params := createStringStreamFromStringList(aBody);
      //httpClient.Request.ContentType := 'application/json';
      httpClient.Request.ContentType := Self.FContentType;
      try
        respStr := httpClient.Put(getURLAsStr, params);
      finally
        params.Free;
      end;
      Result.LocationHeader := httpClient.Response.Location;
      Result.ResponseCode := httpClient.ResponseCode;
      Result.ResponseStr := respStr;
      Result.ResponseContentType := httpClient.Response.ContentType;
    except
      on E: EIdHTTPProtocolException do
      begin
        Result.ResponseCode := httpClient.ResponseCode;
        Result.ResponseStr := E.ErrorMessage;//httpClient.ResponseText;
        Result.ResponseContentType := httpClient.Response.ContentType;
      end;
    end;
  finally
    httpClient.Free;
  end;
end;

procedure TRestRequest.setAccept(const Value: string);
begin
  FAccept := value;
end;

procedure TRestRequest.SetBeforeRequest(const Value: TBeforeRequest);
begin
  FBeforeRequest := value;
end;

procedure TRestRequest.setContentType(const Value: string);
begin
  FContentType := Value;
end;

function TRestRequest.Scheme(aScheme: string): TRestRequest;
begin
  Self.FScheme := Trim(aScheme);
  Result := Self;
end;
  {
function TRestRequest.urlEncode(str: string): string;
var
  i: Integer;
const
  UnsafeChars = ['*', '#', '%', '<', '>', ' ','[',']'];  {do not localize
begin
  Result := '';    {Do not Localize
  for i := 1 to Length(str) do
  begin
    if (str[i] in UnsafeChars) or (not (ord(str[i])in [33..128])) then
    begin {do not localize
      Result := Result + '%' + IntToHex(Ord(str[i]), 2);  {do not localize
    end
    else
    begin
      Result := Result + str[i];
    end;
  end;
end;
  }
function TRestRequest.WithReadTimeout(timeout: Integer): TRestRequest;
begin
  Self.FReadTimeout := timeout;
  Result := Self;
end;

function TRestRequest.WithCredentials(username, password: string): TRestRequest;
begin
  Self.FBasicAuth := true;
  Self.FUsername := username;
  Self.FPassword := password;
  Result := Self;
end;

function TRestRequest.WithBearerToken(bearerToken: string): TRestRequest;
begin
  Self.FBearerAuth := true;
  Self.FBearerToken := bearerToken;
  Result := Self;
end;

function TRestRequest.WithHeader(aName, aValue: string): TRestRequest;
begin
  Self.FHeaders.Add(aName + ':' + aValue);
  Result := Self;
end;

end.