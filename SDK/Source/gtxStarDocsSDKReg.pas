{
 Gnostice StarDocs v2
 Copyright © Gnostice Information Technologies Private Limited, Bangalore, India
 http://www.gnostice.com
}

unit gtxStarDocsSDKReg;

interface

uses
	System.Classes, gtxStarDocsSDK;

procedure Register;

implementation
{$R gtxStarDocsSDKReg.dcr}

procedure Register;
begin
  RegisterComponents('Gnostice StarDocs', [TgtStarDocsSDK]);

end;

end.
