StarDocs SDK for Delphi - Installation Instructions
----------------------------------------------------
1. Close Delphi/RAD Studion IDE.
2. Add "<StarDocs Root>\SDK\Source\Packages\bin" to the system path environment variable.
3. Open project "<StarDocs Root>\SDK\Source\Packages\StarDocsSDK.groupproj". 
4. Right-click "StarDocsSDK" in the Project Manager and choose "Build All".
5. Right-click "gtStarDocsSDK_design250.bpl" in the Project Manager and choose "Install". This will install the TgtStarDocsSDK component.
6. Add "<StarDocs Root>\SDK\Source" folder to library path of your IDE (Optional).

After the installation, a new "StarDocs SDK" tab will be available on the component palette of the IDE. This tab will contain StarDocsSDK component of StarDocs Delphi. 

If you are using UniGUI then please follow the instructions below to install the UniGUI component:
1. Open project "<StarDocs Root>\SDK\Source\Packages\UniStarDocsViewer.groupproj". 
2. Right-click "UniStarDocsViewer" in the Project Manager and choose "Build All".
3. Right-click "gtUniStarDocsViewer_design250.bpl" in the Project Manager and choose "Install". This will install the TgtUniStarDocsViewer component.
4. Copy the "gnostice-ext-js" folder from "<StarDocs Root>\SDK\Source" to "<UniGUI Installation Folder>\Framework\uniGUI\ext-xxx". The default installation folder for UniGUI is "C:\Program Files (x86)\FMSoft".

Note: If <StarDocs Root> needs administrator permission to write then start the IDE in Administrator mode.

StarDocs SDK for Delphi - Uninstallation Instructions
------------------------------------------------------
1. Start the IDE.
2. Select Components | Install Packages option from the main menu. This will display the "Project Options for ...." dialog box. 
3. Select all "Gnostice ..." components in the "Design packages" list of the dialog box. 
4. Click on the "Remove" button. This will remove the StarDocs SDK tab from the component palette. 
5. Close the dialog box. 
6. Close the IDE. 
