unit gtxUniStarDocsViewer_reg;

interface

uses
  System.Classes, gtxUniStarDocsViewer;

procedure Register;

implementation
{$R gtxUniStarDocsViewer_reg.dcr}

procedure Register;
begin
  RegisterComponents('Gnostice StarDocs', [TgtUniStarDocsViewer]);
end;

end.
