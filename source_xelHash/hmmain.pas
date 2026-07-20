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
    btnCompareCRC64: TButton;
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
    cbCRC64: TCheckBox;
    cbMD2: TCheckBox;
    cbMD4: TCheckBox;
    cbSHA384: TCheckBox;
    cbSHA512: TCheckBox;
    edCRC64: TEdit;
    edMD5: TEdit;
    edFileName: TFileNameEdit;
    edCRC32: TEdit;
    edMD2: TEdit;
    edMD4: TEdit;
    edSHA1: TEdit;
    edSHA256: TEdit;
    edSHA384: TEdit;
    edSHA512: TEdit;
    ImageCRC64: TImage;
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
    procedure ApplicationProperties1Idle(Sender: TObject; var {%H-}Done: Boolean);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCompareCRC32Click(Sender: TObject);
    procedure btnCompareCRC64Click(Sender: TObject);
//    procedure btnCompareCRC128Click(Sender: TObject);
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
    procedure CalcHash(AStream: TStream; AMethod: String; AEdit: TEdit);
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
  Hasher;

procedure TMainForm.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnCalculate.Enabled := edFileName.Text <> '';
  btnCompareCRC32.Enabled := cbCRC32.Checked and (edCRC32.Text <> '');
  btnCompareCRC64.Enabled := cbCRC64.Checked and (edCRC64.Text <> '');
//  btnCompareCRC128.Enabled := cbCRC128.Checked and (edCRC128.Text <> '');
  btnCompareMD2.Enabled := cbMD2.Checked and (edMD2.Text <> '');
  btnCompareMD4.Enabled := cbMD4.Checked and (edMD4.Text <> '');
  btnCompareMD5.Enabled := cbMD5.Checked and (edMD5.Text <> '');
  btnCompareSHA1.Enabled := cbSHA1.Checked and (edSHA1.Text <> '');
  btnCompareSHA256.Enabled := cbSHA256.Checked and (edSHA256.Text <> '');
  btnCompareSHA384.Enabled := cbSHA256.Checked and (edSHA384.Text <> '');
  btnCompareSHA512.Enabled := cbSHA256.Checked and (edSHA512.Text <> '');
  btnCopyAll.Enabled :=
    btnCompareCRC32.Enabled or btnCompareCRC64.Enabled or //btnCompareCRC128.Enabled or
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

procedure TMainForm.btnCompareCRC64Click(Sender: TObject);
begin
  CompareHash('CRC64', edCRC64.Text, ImageCRC64);
end;

{
procedure TMainForm.btnCompareCRC128Click(Sender: TObject);
begin
  CompareHash('CRC128', edCRC128.Text, ImageCRC128);
end;
}

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
  if cbCRC64.Checked then
    s := s + LineEnding + 'CRC64: ' + edCRC64.Text;
  {
  if cbCRC128.Checked then
    s := s + LineEnding + 'CRC128: ' + edCRC128.Text;
  }
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

procedure TMainForm.CalcHash(AStream: TStream; AMethod: String; AEdit: TEdit);
var
  hash: THasher;
begin
  hash := THasher.Create(AMethod);
  try
    if hash.SelfCheck then
    begin
      AStream.Position := 0;
      hash.Update(AStream);
      AEdit.Text := Lowercase(hash.Final);
    end else
      AEdit.Text := 'Self-check failed.';
  finally
    hash.Free;
  end;
end;

procedure TMainForm.Calculate(const AFileName: String);
var
  stream: TStream;
begin
  edCRC32.Text := '';
  edCRC64.Text := '';
  //edCRC128.Text := '';
  edMD2.Text := '';
  edMD4.Text := '';
  edMD5.Text := '';
  edSHA1.Text := '';
  edSHA256.Text := '';
  edSHA384.Text := '';
  edSHA512.Text := '';

  ImageCRC32.ImageIndex := -1;
  ImageCRC64.ImageIndex := -1;
  //ImageCRC128.ImageIndex := -1;
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
  stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if cbCRC32.Checked then
      CalcHash(stream, 'CRC-32 ISO-HDLC', edCRC32);

    if cbCRC64.Checked then
      CalcHash(stream, 'CRC-64', edCRC64);

//    if cbCRC128.Checked then
//      CalcHash(stream, 'CRC-128', edCRC128);      // not available

    if cbMD2.Checked then
      CalcHash(stream, 'MD-2', edMD2);

    if cbMD4.Checked then
      CalcHash(stream, 'MD-4', edMD4);

    if cbMD5.Checked then
      CalcHash(stream, 'MD-5', edMD5);

    if cbSHA1.Checked then
      CalcHash(stream, 'SHA-1', edSHA1);

    if cbSHA256.Checked then
      CalcHash(stream, 'SHA-256', edSHA256);

    if cbSHA384.Checked then
      CalcHash(stream, 'SHA-384', edSHA384);

    if cbSHA512.Checked then
      CalcHash(stream, 'SHA-512', edSHA512);

  finally
    stream.Free;
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
    cbCRC64.Checked := ini.ReadBool('Settings', 'GetCRC64', cbCRC64.Checked);
//    cbCRC128.Checked := ini.ReadBool('Settings', 'GetCRC128', cbCRC128.Checked);
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
    ini.WriteBool('Settings', 'GetCRC64', cbCRC64.Checked);
//    ini.WriteBool('Settings', 'GetCRC128', cbCRC128.Checked);
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

