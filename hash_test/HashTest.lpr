program HashTest;
uses
  SysUtils, crc, sha1, fpsha256, fpsha512;

const
  TestStr: String = '123456789';  // This is the test string of XelHash
  TestStr2: String = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';  // used by DCPcrypt

procedure Test_CRC32;
const
  //https://www.sunshine2k.de/coding/javascript/crc/crc_js.html
  SollCRC32 = 'CBF43926';   // CRC parametrization predefined "CRC32"
var
  hash: LongWord;
begin
  hash := crc32(0, nil, 0);
  hash := crc32(hash, @TestStr[1], Length(TestStr));
  WriteLn;
  WriteLn('CRC-32 (', TestStr, ')');
  WriteLn('  calculated: ', Lowercase(Format('%.8x', [hash])));
  WriteLn('  expected:   ', Lowercase(SollCRC32));
  if Lowercase(Format('%.8x', [hash])) = Lowercase(SollCRC32) then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_CRC64;
const
  //https://www.sunshine2k.de/coding/javascript/crc/crc_js.html
  SollCRC64 = '6C40DF5F0B497347';  // CRC parametrization predefined "CRC64_ECAMA_182"
var
  hash: QWord;
begin
  hash := crc64(0, nil, 0);
  hash := crc64(hash, @TestStr[1], Length(TestStr));
  WriteLn;
  WriteLn('CRC-64 (', TestStr, ')');
  WriteLn('  calculated: ', Lowercase(Format('%.16x', [hash])));
  WriteLn('  expected:   ', Lowercase(SollCRC64));
  if Lowercase(Format('%.16x', [hash])) = Lowercase(SollCRC64) then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA1;
const
  //https://www.sha-generator.de/
  SollSHA1 = 'f7c3bc1d808e04732adf679965ccc34ca7ae3441';
  // XelHash: F7C3BC1D808E04732ADF679965CCC34CA7AE3441';
var
  sha1Digest: TSHA1Digest;
  sha1Value: String;
begin
  sha1Digest := sha1string(TestStr);
  sha1Value := sha1Print(sha1Digest);
  WriteLn;
  WriteLn('SHA-1 (', TestStr, ')');
  WriteLn('  calculated: ', sha1Value);
  WriteLn('  expected:   ', SollSHA1);
  if sha1Value = SollSHA1 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA256;
const
  //https://www.sha-generator.de/
  SollSHA256 = '15e2b0d3c33891ebb0f1ef609ec419420c20e320ce94c65fbc8c3312448eb225';
  // XelHash:   15E2B0D3C33891EBB0F1EF609EC419420C20E320CE94C65FBC8C3312448EB225
var
  sha256: TSHA256;
  sha256Value: String;
begin
  sha256.Init;
  sha256.Update(@TestStr[1], Length(TestStr));
  sha256.Final;
  sha256.OutputHexa(sha256Value);

  WriteLn;
  WriteLn('SHA-256 (', TestStr, ')');
  WriteLn('  calculated: ', lowercase(sha256Value));
  WriteLn('  expected:   ', SollSHA256);
  if lowercase(sha256Value) = SollSHA256 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA256_long;
const
  //https://emn178.github.io/online-tools/sha256.html/
  SollSHA256 = '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1';
var
  sha256: TSHA256;
  sha256Value: String;
begin
  sha256.Init;
  sha256.Update(@TestStr2[1], Length(TestStr2));
  sha256.Final;
  sha256.OutputHexa(sha256Value);

  WriteLn;
  WriteLn('SHA-256 (', TestStr2, ')');
  WriteLn('  calculated: ', lowercase(sha256Value));
  WriteLn('  expected:   ', SollSHA256);
  if lowercase(sha256Value) = SollSHA256 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA384;
const
  //https://www.sha-generator.de/
  SollSHA384 = 'eb455d56d2c1a69de64e832011f3393d45f3fa31d6842f21af92d2fe469c499da5e3179847334a18479c8d1dedea1be3';
  // XelHash:   EB455D56D2C1A69DE64E832011F3393D45F3FA31D6842F21AF92D2FE469C499DA5E3179847334A18479C8D1DEDEA1BE3
var
  sha384: TSHA384;
  sha384Value: String;
begin
  sha384.Init;
  sha384.Update(@TestStr[1], Length(TestStr));
  sha384.Final;
  sha384.OutputHexa(sha384Value);

  WriteLn;
  WriteLn('SHA-384 (', TestStr, ')');
  WriteLn('  calculated: ', lowercase(sha384Value));
  WriteLn('  expected:   ', SollSHA384);
  if lowercase(sha384Value) = SollSHA384 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA512;
const
  //https://www.sha-generator.de/
  SollSHA512 = 'd9e6762dd1c8eaf6d61b3c6192fc408d4d6d5f1176d0c29169bc24e71c3f274ad27fcd5811b313d681f7e55ec02d73d499c95455b6b5bb503acf574fba8ffe85';
  // XelHash:  'D9E6762DD1C8EAF6D61B3C6192FC408D4D6D5F1176D0C29169BC24E71C3F274AD27FCD5811B313D681F7E55EC02D73D499C95455B6B5BB503ACF574FBA8FFE85'
var
  sha512: TSHA512;
  sha512Value: String;
begin
  sha512.Init;
  sha512.Update(@TestStr[1], Length(TestStr));
  sha512.Final;
  sha512.OutputHexa(sha512Value);

  WriteLn;
  WriteLn('SHA-512 (', TestStr, ')');
  WriteLn('  calculated: ', lowercase(sha512Value));
  WriteLn('  expected:   ', SollSHA512);
  if lowercase(sha512Value) = SollSHA512 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

procedure Test_SHA512_long;
const
  // https://emn178.github.io/online-tools/sha512.html
  SollSHA512 = '204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445';
var
  sha512: TSHA512;
  sha512Value: String;
begin
  sha512.Init;
  sha512.Update(@TestStr2[1], Length(TestStr2));
  sha512.Final;
  sha512.OutputHexa(sha512Value);

  WriteLn;
  WriteLn('SHA-512 (', TestStr2, ')');
  WriteLn('  calculated: ', lowercase(sha512Value));
  WriteLn('  expected:   ', SollSHA512);
  if lowercase(sha512Value) = SollSHA512 then
    WriteLn('  --> ok')
  else
    WriteLn('  --> ERROR');
end;

begin
  WriteLn('Test string: ', TestStr);

  Test_CRC32;
  Test_CRC64;
  Test_SHA1;
  Test_SHA256;
  Test_SHA384;
  Test_SHA512;
  WriteLn('--------------------------------------------');
  Test_SHA256_long;
  Test_SHA512_long;


  ReadLn;

end.

