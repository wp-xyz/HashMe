unit hmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls, Buttons;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnCalculate: TBitBtn;
    btnClose: TBitBtn;
    btnCompareMD5: TButton;
    btnCompareCRC32: TButton;
    btnCompareMD2: TButton;
    btnCompareMD4: TButton;
    btnCompareSHA1: TButton;
    btnCompareSHA256: TButton;
    btnCompareSHA384: TButton;
    btnCompareSHA512: TButton;
    btnCopyAll: TBitBtn;
    cbCRC32: TCheckBox;
    cbMD2: TCheckBox;
    cbMD4: TCheckBox;
    cbSHA384: TCheckBox;
    cbSHA512: TCheckBox;
    edMD5: TEdit;
    edFileName: TFileNameEdit;
    edCRC32: TEdit;
    edMD2: TEdit;
    edMD4: TEdit;
    edSHA1: TEdit;
    edSHA256: TEdit;
    edSHA384: TEdit;
    edSHA512: TEdit;
    ImageMD5: TImage;
    ImageList: TImageList;
    ImageCRC32: TImage;
    ImageMD2: TImage;
    ImageMD4: TImage;
    ImageSHA1: TImage;
    ImageSHA256: TImage;
    ImageSHA384: TImage;
    ImageSHA512: TImage;
    lblFileName: TLabel;
    cbMD5: TCheckBox;
    cbSHA1: TCheckBox;
    cbSHA256: TCheckBox;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCompareCRC32Click(Sender: TObject);
    procedure btnCompareMD2Click(Sender: TObject);
    procedure btnCompareMD4Click(Sender: TObject);
    procedure btnCompareMD5Click(Sender: TObject);
    procedure btnCompareSHA1Click(Sender: TObject);
    procedure btnCompareSHA256Click(Sender: TObject);
    procedure btnCompareSHA384Click(Sender: TObject);
    procedure btnCompareSHA512Click(Sender: TObject);
    procedure btnCopyAllClick(Sender: TObject);
    procedure edFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FStream: TStream;
    FActivated: Boolean;
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
  CRC, MD5, SHA1, fpsha256, fpsha512;

const
  BLOCK_SIZE = 1024*1024;

procedure TMainForm.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnCalculate.Enabled := edFileName.Text <> '';
  btnCompareCRC32.Enabled := cbCRC32.Checked and (edCRC32.Text <> '');
  btnCompareMD2.Enabled := cbMD2.Checked and (edMD2.Text <> '');
  btnCompareMD4.Enabled := cbMD4.Checked and (edMD4.Text <> '');
  btnCompareMD5.Enabled := cbMD5.Checked and (edMD5.Text <> '');
  btnCompareSHA1.Enabled := cbSHA1.Checked and (edSHA1.Text <> '');
  btnCompareSHA256.Enabled := cbSHA256.Checked and (edSHA256.Text <> '');
  btnCompareSHA384.Enabled := cbSHA256.Checked and (edSHA384.Text <> '');
  btnCompareSHA512.Enabled := cbSHA256.Checked and (edSHA512.Text <> '');
  btnCopyAll.Enabled := btnCompareCRC32.Enabled or
    btnCompareMD2.Enabled or btnCompareMD4.Enabled or btnCompareMD5.Enabled or
    btnCompareSHA1.Enabled or btnCompareSHA256.Enabled or
    btnCompareSHA384.Enabled or btnCompareSHA512.Enabled;
end;

procedure TMainForm.btnCalculateClick(Sender: TObject);
begin
  Calculate(edFileName.FileName);
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnCompareCRC32Click(Sender: TObject);
begin
  CompareHash('CRC32', edCRC32.Text, ImageCRC32);
end;

procedure TMainForm.btnCompareMD2Click(Sender: TObject);
begin
  CompareHash('MD2', edMD2.Text, ImageMD2);
end;

procedure TMainForm.btnCompareMD4Click(Sender: TObject);
begin
  CompareHash('MD4', edMD4.Text, ImageMD4);
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

procedure TMainForm.btnCompareSHA384Click(Sender: TObject);
begin
  CompareHash('SHA-384', edSHA384.Text, ImageSHA384);
end;

procedure TMainForm.btnCompareSHA512Click(Sender: TObject);
begin
  CompareHash('SHA-512', edSHA512.Text, ImageSHA512);
end;

procedure TMainForm.btnCopyAllClick(Sender: TObject);
var
  s: String;
begin
  s := '';
  if cbCRC32.Checked then
    s := s + LineEnding + 'CRC32: ' + edCRC32.Text;
  if cbMD2.Checked then
    s := s + LineEnding + 'MD2: ' + edMD2.Text;
  if cbMD4.Checked then
    s := s + LineEnding + 'MD4: ' + edMD4.Text;
  if cbMD5.Checked then
    s := s + LineEnding + 'MD5: ' + edMD5.Text;
  if cbSHA1.Checked then
    s := s + LineEnding + 'SHA1: ' + edSHA1.Text;
  if cbSHA256.Checked then
    s := s + LineEnding + 'SHA256: ' + edSHA256.Text;
  if cbSHA384.Checked then
    s := s + LineEnding + 'SHA384: ' + edSHA384.Text;
  if cbSHA512.Checked then
    s := s + LineEnding + 'SHA512: ' + edSHA512.Text;
  if s = '' then
    exit;

  Clipboard.AsText := 'File: ' + ExtractFileName(edFileName.FileName) + s;
end;

procedure TMainForm.Calculate(const AFileName: String);
var
  buffer: array of Byte = nil;
  crc32Value: Cardinal;
  crc32Result: String;
  md2: TMDContext;
  md2Digest: TMDDigest;
  md4: TMDContext;
  md4Digest: TMDDigest;
  md5: TMDContext;
  md5Digest: TMDDigest;
  sha1: TSHA1Context;
  sha1Digest: TSHA1Digest;
  sha256: TSHA256;
  sha256Result: String;
  sha384: TSHA384;
  sha384Result: String;
  sha512: TSHA512;
  sha512Result: String;
  n: Integer;
begin
  edCRC32.Text := '';
  edMD2.Text := '';
  edMD4.Text := '';
  edMD5.Text := '';
  edSHA1.Text := '';
  edSHA256.Text := '';
  edSHA384.Text := '';
  edSHA512.Text := '';

  ImageCRC32.ImageIndex := -1;
  ImageMD2.ImageIndex := -1;
  ImageMD4.ImageIndex := -1;
  ImageMD5.ImageIndex := -1;
  ImageSHA1.ImageIndex := -1;
  ImageSHA256.ImageIndex := -1;
  ImageSHA384.ImageIndex := -1;
  ImageSHA512.ImageIndex := -1;

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

    if cbMD2.Checked then
      MDInit(md2, MD_VERSION_2);
    if cbMD4.Checked then
      MDInit(md4, MD_VERSION_4);
    if cbMD5.Checked then
      MDInit(md5, MD_VERSION_5);
    if cbSHA1.Checked then
      SHA1Init(sha1);
    if cbSHA256.Checked then
      sha256.Init;
    if cbSHA384.Checked then
      sha384.Init;
    if cbSHA512.Checked then
      sha512.Init;

    SetLength(buffer, BLOCK_SIZE);
    while FStream.Position < FStream.Size do
    begin
      n := FStream.Read(buffer[0], BLOCK_SIZE);
      if cbCRC32.Checked then
        crc32Value := CRC32(0, @buffer[0], n);
      if cbMD2.Checked then
        MDUpdate(md2, buffer[0], n);
      if cbMD4.Checked then
        MDUpdate(md4, buffer[0], n);
      if cbMD5.Checked then
        MDUpdate(md5, buffer[0], n);
      if cbSHA1.Checked then
        SHA1Update(sha1, buffer[0], n);
      if cbSHA256.Checked then
        sha256.Update(@buffer[0], n);
      if cbSHA384.Checked then
        sha384.Update(@buffer[0], n);
      if cbSHA512.Checked then
        sha512.Update(@buffer[0], n);
    end;
    SetLength(buffer, 0);

    if cbCRC32.Checked then
      edCRC32.Text := Format('%.8x', [crc32Value]);
    if cbMD2.Checked then
    begin
      MDFinal(md2, md2Digest);
      edMD2.Text := MDPrint(md2Digest);
    end;
    if cbMD4.Checked then
    begin
      MDFinal(md4, md4Digest);
      edMD4.Text := MDPrint(md4Digest);
    end;
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
    if cbSHA384.Checked then
    begin
      sha384.Final;
      sha384.OutputHexa(sha384Result);
      edSHA384.Text := sha384Result;
    end;
    if cbSHA512.Checked then
    begin
      sha512.Final;
      sha512.OutputHexa(sha512Result);
      edSHA512.Text := sha512Result;
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
  if not FActivated then
  begin
    ReadIni;
    btnClose.Constraints.MinWidth := btnCompareMD5.Width;
    Constraints.MinHeight := btnClose.Top + btnClose.Height + btnClose.BorderSpacing.Bottom;
    Constraints.MaxHeight := Constraints.MinHeight;
    ClientHeight := 0;  // enforce constraints;
  end;
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
  cInputQueryEditSizePixels := 400;
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
    begin
      if w > Screen.Width then w := Screen.Width;
      ClientWidth := w;
    end;

    cbCRC32.Checked := ini.ReadBool('Settings', 'GetCRC32', cbCRC32.Checked);
    cbMD2.Checked := ini.ReadBool('Settings', 'GetMD2', cbMD2.Checked);
    cbMD4.Checked := ini.ReadBool('Settings', 'GetMD4', cbMD4.Checked);
    cbMD5.Checked := ini.ReadBool('Settings', 'GetMD5', cbMD5.Checked);
    cbSHA1.Checked := ini.ReadBool('Settings', 'GetSHA1', cbSHA1.Checked);
    cbSHA256.Checked := ini.ReadBool('Settings', 'GetSHA256', cbSHA256.Checked);
    cbSHA384.Checked := ini.ReadBool('Settings', 'GetSHA384', cbSHA384.Checked);
    cbSHA512.Checked := ini.ReadBool('Settings', 'GetSHA512', cbSHA512.Checked);

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

    ini.WriteBool('Settings', 'GetCRC32', cbCRC32.Checked);
    ini.WriteBool('Settings', 'GetMD2', cbMD2.Checked);
    ini.WriteBool('Settings', 'GetMD4', cbMD4.Checked);
    ini.WriteBool('Settings', 'GetMD5', cbMD5.Checked);
    ini.WriteBool('Settings', 'GetSHA1', cbSHA1.Checked);
    ini.WriteBool('Settings', 'GetSHA256', cbSHA256.Checked);
    ini.WriteBool('Settings', 'GetSHA384', cbSHA384.Checked);
    ini.WriteBool('Settings', 'GetSHA512', cbSHA512.Checked);

    if edFileName.FileName <> '' then
      ini.WriteString('Settings', 'InitialDir', ExtractFilePath(edFileName.FileName));
  finally
    ini.Free;
  end;
end;

end.

