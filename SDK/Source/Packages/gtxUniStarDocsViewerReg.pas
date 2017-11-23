unit gtxUniStarDocsViewerReg;

interface

uses
  System.Classes, gtxUniStarDocsViewer;

procedure Register;

implementation
{$R gtxUniStarDocsViewerReg.dcr}

procedure Register;
begin
  RegisterComponents('Gnostice StarDocs', [TgtUniStarDocsViewer]);
end;

end.
