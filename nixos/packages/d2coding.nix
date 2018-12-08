{ stdenv, fetchzip, ... }:

let
  version = "1.3.2";

in fetchzip rec {
  name = "d2coding-font-${version}";
  url = "https://github.com/naver/d2codingfont/releases/download/VER1.3.2/D2Coding-Ver1.3.2-20180524.zip";
  sha256 = "1812r82530wzfki7k9cm35fy6k2lvis7j6w0w8svc784949m1wwj";

  postFetch = ''
    mkdir -p $out/share/fonts/truetype
    unzip -j $downloadedFile D2CodingAll/\*.ttc -d $out/share/fonts/truetype/
  '';
}
