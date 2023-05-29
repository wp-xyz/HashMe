unit hmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnCalculate: TButton;
    btnClose: TButton;
    btnCompareMD5: TButton;
    btnCompareSHA1: TButton;
    btnCompareSHA256: TButton;
    btnCopyAll: TButton;
    edMD5: TEdit;
    edFileName: TFileNameEdit;
    edSHA1: TEdit;
    edSHA256: TEdit;
    ImageMD5: TImage;
    ImageList: TImageList;
    ImageSHA1: TImage;
    ImageSHA256: TImage;
    lblFileName: TLabel;
    cbMD5: TCheckBox;
    cbSHA1: TCheckBox;
    cbSHA256: TCheckBox;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCompareMD5Click(Sender: TObject);
    procedure btnCompareSHA1Click(Sender: TObject);
    procedure btnCompareSHA256Click(Sender: TObject);
    procedure btnCopyAllClick(Sender: TObject);
    procedure edFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FStream: TStream;
    procedure Calculate(const AFileName: String);
    procedure CompareHash(HashName: String; Value: String; Image: TImage);
    procedure ReadIni;
    procedure WriteIni;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles, ClipBrd,
  MD5, SHA1, fpsha256;

const
  BLOCK_SIZE = 1024*1024;

procedure TMainForm.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnCalculate.Enabled := edFileName.Text <> '';
  btnCompareMD5.Enabled := cbMD5.Checked and (edMD5.Text <> '');
  btnCompareSHA1.Enabled := cbSHA1.Checked and (edSHA1.Text <> '');
  btnCompareSHA256.Enabled := cbSHA256.Checked and (edSHA256.Text <> '');
  btnCopyAll.Enabled := btnCompareMD5.Enabled or btnCompareSHA1.Enabled or btnCompareSHA256.Enabled;
end;

procedure TMainForm.btnCalculateClick(Sender: TObject);
begin
  Calculate(edFileName.FileName);
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnCompareMD5Click(Sender: TObject);
begin
  CompareHash('MD5', edMD5.Text, ImageMD5);
end;

procedure TMainForm.btnCompareSHA1Click(Sender: TObject);
begin
  CompareHash('SHA-1', edSHA1.Text, ImageSHA1);
end;

procedure TMainForm.btnCompareSHA256Click(Sender: TObject);
begin
  CompareHash('SHA-256', edSHA256.Text, ImageSHA256);
end;

procedure TMainForm.btnCopyAllClick(Sender: TObject);
var
  s: String;
begin
  s := '';
  if cbMD5.Checked then
    s := s + LineEnding + 'MD5: ' + edMD5.Text;
  if cbSHA1.Checked then
    s := s + LineEnding + 'SHA1: ' + edSHA1.Text;
  if cbSHA256.Checked then
    s := s + LineEnding + 'SHA256: ' + edSHA256.Text;
  if s = '' then
    exit;

  Clipboard.AsText := 'File: ' + ExtractFileName(edFileName.FileName) + s;
end;

procedure TMainForm.Calculate(const AFileName: String);
var
  buffer: array of Byte = nil;
  md5: TMDContext;
  md5Digest: TMDDigest;
  sha1: TSHA1Context;
  sha1Digest: TSHA1Digest;
  sha256: TSHA256;
  sha256Result: String;
  n: Integer;
begin
  edMD5.Text := '';
  edSHA1.Text := '';
  edSHA256.Text := '';

  ImageMD5.ImageIndex := -1;
  ImageSHA1.ImageIndex := -1;
  ImageSHA256.ImageIndex := -1;

  if not FileExists(AFileName) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOK], 0);
    exit;
  end;

  Screen.BeginWaitCursor;
  try
    FStream.Free;
    FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    FStream.Position := 0;

    if cbMD5.Checked then
      MDInit(md5, MD_VERSION_5);
    if cbSHA1.Checked then
      SHA1Init(sha1);
    if cbSHA256.Checked then
      sha256.Init;

    SetLength(buffer, BLOCK_SIZE);
    while FStream.Position < FStream.Size do
    begin
      n := FStream.Read(buffer[0], BLOCK_SIZE);
      if cbMD5.Checked then
        MDUpdate(md5, buffer[0], n);
      if cbSHA1.Checked then
        SHA1Update(sha1, buffer[0], n);
      if cbSHA256.Checked then
        sha256.Update(@buffer[0], n);
    end;
    SetLength(buffer, 0);


    if cbMD5.Checked then
    begin
      MDFinal(md5, md5Digest);
      edMD5.Text := MDPrint(md5Digest);
    end;
    if cbSHA1.Checked then
    begin
      SHA1Final(sha1, sha1Digest);
      edSHA1.Text := SHA1Print(sha1Digest);
    end;
    if cbSHA256.Checked then
    begin
      sha256.Final;
      sha256.OutputHexa(sha256Result);
      edSHA256.Text := sha256Result;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.CompareHash(HashName: String; Value: String; Image: TImage);
var
  s: String;
begin
  s := '';
  if InputQuery('Compare ' + HashName, 'Expected ' + HashName, s) then
  begin
    if Uppercase(Trim(s)) = Uppercase(Value) then
    begin
      Image.ImageIndex := 0;
      ShowMessage('Correct');
    end else
    begin
      Image.ImageIndex := 1;
      ShowMessage('Different');
    end;
  end;
end;

procedure TMainForm.edFileNameAcceptFileName(Sender: TObject;
  var Value: String);
begin
  Calculate(Value);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Constraints.MinHeight := btnClose.Top + btnClose.Height + btnClose.BorderSpacing.Around;
  Constraints.MaxHeight := Constraints.MinHeight;
  ClientHeight := 0;  // enforce constraints;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cInputQueryEditSizePercents := 0;
  cInputQueryEditSizePixels := 320;
  ReadIni;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  w: Integer;
  dir: String;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    w := ini.ReadInteger('MainForm', 'Width', 0);
    if w <> 0 then
      ClientWidth := w;

    cbMD5.Checked := ini.ReadBool('Settings', 'GetMD5', cbMD5.Checked);
    cbSHA1.Checked := ini.ReadBool('Settings', 'GetSHA1', cbSHA1.Checked);
    cbSHA256.Checked := ini.ReadBool('Settings', 'GetSHA256', cbSHA256.Checked);

    dir := ini.ReadString('Settings', 'InitialDir', '');
    if dir <> '' then
      edFileName.InitialDir := dir;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    ini.WriteInteger('MainForm', 'Width', ClientWidth);

    ini.WriteBool('Settings', 'GetMD5', cbMD5.Checked);
    ini.WriteBool('Settings', 'GetSHA1', cbSHA1.Checked);
    ini.WriteBool('Settings', 'GetSHA256', cbSHA256.Checked);

    if edFileName.FileName <> '' then
      ini.WriteString('Settings', 'InitialDir', ExtractFilePath(edFileName.FileName));
  finally
    ini.Free;
  end;
end;

end.

