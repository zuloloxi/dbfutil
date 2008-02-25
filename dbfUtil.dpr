program dbfUtil;

uses
  FastMM4 ,
  
  Forms,
  dbfViewMemo in 'dbfViewMemo.pas' {MemoDisplayForm},
  dbfAboutBox in 'dbfAboutBox.pas' {frmAboutBox},
  dbfBackLinkDisplay in 'dbfBackLinkDisplay.pas' {frmBackLink},
  dbfBadFieldView in 'dbfBadFieldView.pas' {fmBadFieldView},
  dbfBMPMemoDisplay in 'dbfBMPMemoDisplay.pas' {fmBMPMemoDisplay},
  dbfCommon in 'dbfCommon.pas',
  dbfConstant in 'dbfConstant.pas',
  dbfConvert in 'dbfConvert.pas' {ConvertFileForm},
  dbfConvertFixDBV in 'dbfConvertFixDBV.pas' {frmConvertFixDBV},
  dbfCreate in 'dbfCreate.pas',
  dbfCreateFile in 'dbfCreateFile.pas' {fmCreateFile},
  dbfDataGrid in 'dbfDataGrid.pas' {frmDataGrid},
  dbfExport in 'dbfExport.pas' {frmExport},
  dbfFieldEdit in 'dbfFieldEdit.pas' {fmFieldEdit},
  dbfFieldHexView in 'dbfFieldHexView.pas' {fmFieldHexView},
  dbfFieldView in 'dbfFieldView.pas' {fmViewField},
  dbfFixedMemoDisplay in 'dbfFixedMemoDisplay.pas' {frmFixedMemo},
  dbfFixedMemoHexDisplay in 'dbfFixedMemoHexDisplay.pas' {frmFixedMemoHex},
  dbfFixFields in 'dbfFixFields.pas' {fmFixFields},
  dbfHeaderEdit in 'dbfHeaderEdit.pas' {frmHeaderEdit},
  dbfMain in 'dbfMain.pas' {frmMainDBF},
  dbfMemoFileDisplay in 'dbfMemoFileDisplay.pas',
  dbfMsgs in 'dbfMsgs.pas',
  dbfOptions in 'dbfOptions.pas' {fmOptions},
  dbfRecordDisplay in 'dbfRecordDisplay.pas' {frmRecordDisplay},
  dbfRepairHeader in 'dbfRepairHeader.pas' {frmRepairHeader},
  dbfRepairRecords in 'dbfRepairRecords.pas' {frmRepairRecords},
  dbfRestructure in 'dbfRestructure.pas' {frmRestructure},
  dbfRestructureDBF in 'dbfRestructureDBF.pas' {frmRestructureDBF},
  dbfScanFields in 'dbfScanFields.pas' {frmScanFields},
  dbfScanFieldsMaxMin in 'dbfScanFieldsMaxMin.pas',
  dbfScanInfoDBV in 'dbfScanInfoDBV.pas' {frmScanInfoDBV},
  dbfStatistics in 'dbfStatistics.pas' {fmStatistics},
  dbfStructure in 'dbfStructure.pas',
  dbfStructurePrint in 'dbfStructurePrint.pas' {frmStrucPrint},
  dbfValidateFile in 'dbfValidateFile.pas' {frmValidateFile},
  dbfResources in 'dbfResources.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainDBF, frmMainDBF);
  Application.Run;
end.
