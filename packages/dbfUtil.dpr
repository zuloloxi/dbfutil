program dbfUtil;

uses
  FastMM4,
  Forms,
  dbfViewMemo in '..\source\dbfViewMemo.pas' {MemoDisplayForm},
  dbfAboutBox in '..\source\dbfAboutBox.pas' {frmAboutBox},
  dbfBackLinkDisplay in '..\source\dbfBackLinkDisplay.pas' {frmBackLink},
  dbfBadFieldView in '..\source\dbfBadFieldView.pas' {fmBadFieldView},
  dbfBMPMemoDisplay in '..\source\dbfBMPMemoDisplay.pas' {fmBMPMemoDisplay},
  dbfCommon in '..\source\dbfCommon.pas',
  dbfConstant in '..\source\dbfConstant.pas',
  dbfConvert in '..\source\dbfConvert.pas' {ConvertFileForm},
  dbfConvertFixDBV in '..\source\dbfConvertFixDBV.pas' {frmConvertFixDBV},
  dbfCreate in '..\source\dbfCreate.pas',
  dbfCreateFile in '..\source\dbfCreateFile.pas' {fmCreateFile},
  dbfDataGrid in '..\source\dbfDataGrid.pas' {frmDataGrid},
  dbfExport in '..\source\dbfExport.pas' {frmExport},
  dbfFieldEdit in '..\source\dbfFieldEdit.pas' {fmFieldEdit},
  dbfFieldHexView in '..\source\dbfFieldHexView.pas' {fmFieldHexView},
  dbfFieldView in '..\source\dbfFieldView.pas' {fmViewField},
  dbfFixedMemoDisplay in '..\source\dbfFixedMemoDisplay.pas' {frmFixedMemo},
  dbfFixedMemoHexDisplay in '..\source\dbfFixedMemoHexDisplay.pas' {frmFixedMemoHex},
  dbfFixFields in '..\source\dbfFixFields.pas' {fmFixFields},
  dbfHeaderEdit in '..\source\dbfHeaderEdit.pas' {frmHeaderEdit},
  dbfMain in '..\source\dbfMain.pas' {frmMainDBF},
  dbfMemoFileDisplay in '..\source\dbfMemoFileDisplay.pas',
  dbfMsgs in '..\source\dbfMsgs.pas',
  dbfOptions in '..\source\dbfOptions.pas' {fmOptions},
  dbfRecordDisplay in '..\source\dbfRecordDisplay.pas' {frmRecordDisplay},
  dbfRepairHeader in '..\source\dbfRepairHeader.pas' {frmRepairHeader},
  dbfRepairRecords in '..\source\dbfRepairRecords.pas' {frmRepairRecords},
  dbfRestructure in '..\source\dbfRestructure.pas' {frmRestructure},
  dbfRestructureDBF in '..\source\dbfRestructureDBF.pas' {frmRestructureDBF},
  dbfScanFields in '..\source\dbfScanFields.pas' {frmScanFields},
  dbfScanFieldsMaxMin in '..\source\dbfScanFieldsMaxMin.pas',
  dbfScanInfoDBV in '..\source\dbfScanInfoDBV.pas' {frmScanInfoDBV},
  dbfStatistics in '..\source\dbfStatistics.pas' {fmStatistics},
  dbfStructure in '..\source\dbfStructure.pas',
  dbfStructurePrint in '..\source\dbfStructurePrint.pas' {frmStrucPrint},
  dbfValidateFile in '..\source\dbfValidateFile.pas' {frmValidateFile},
  dbfResources in '..\source\dbfResources.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainDBF, frmMainDBF);
  Application.Run;
end.
