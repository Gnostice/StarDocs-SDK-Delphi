{
 Gnostice StarDocs v2
 Copyright © 2002-2017 Gnostice Information Technologies Private Limited, Bangalore, India
 http://www.gnostice.com
}

unit gtxStarDocsSDK_reg;

interface

uses
	System.Classes, gtxStarDocsSDK;

procedure Register;

implementation
{$R gtxStarDocsSDK_reg.dcr}
procedure Register;
begin
  RegisterComponents('Gnostice StarDocs', [TgtStarDocsSDK]);

end;

end.
