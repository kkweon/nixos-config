self: super:
{
  neovim = super.neovim.override {
    extraPythonPackages = pkgs: with pkgs; [ websocket_client sexpdata ];
    extraPython3Packages = pkgs: with pkgs; [ websocket_client sexpdata ];
    configure = {
      customRC = builtins.readFile(~/.vim/vimrc);
    };
  };

  sh2md = self.haskellPackages.callPackage ~/github/sh2md/default.nix { };

  myPackages = super.buildEnv {
    name = "my-packages";
    paths = [
      # libs
      self.zlib.dev
      self.curl.dev
      self.openssl.dev

      self.xclip
      self.xsel
      self.fd
      self.ripgrep
      self.stow
      self.tree
      self.neovim
      self.gitAndTools.hub
      self.gnupg
			self.unzip
      self.anki
      self.calibre
      self.gimp
      self.protobuf

      # messenger
      self.slack
      self.discord

      self.firefox
      self.heroku
      self.travis

      # screencapture
      self.maim

      # Python
      (self.python.withPackages
        (ps: with ps;
         [
          setuptools
         ])
      )
      (self.python3.withPackages
        (ps: with ps;
        [
          websocket_client
          sexpdata
          black
          pytest
          pylint
          yapf
          tox
          pip
          setuptools
          ipython
          hypothesis
          protobuf
          pynvim
          tkinter
        ])
      )
      self.httpie

      # JS
      self.nodejs-10_x
      self.yarn

      # Exercism
      self.exercism

      # rust
      self.rustup

      # haskell
      self.sh2md
      self.stack
      self.haskellPackages.hlint
      self.haskellPackages.hindent
      self.haskellPackages.stylish-haskell
      self.haskellPackages.hoogle

      # xmonad
      self.dzen2
      self.haskellPackages.xmonad
      self.haskellPackages.xmonad-extras
      self.haskellPackages.xmonad-contrib
      self.haskellPackages.xmobar

      # Java
      self.jdk
      # Scala
      self.sbt
      self.scalafmt

      # Go
      self.go
      self.gotools

      # C/C++
      self.gnumake
      self.gcc
      self.cmake
      self.clang-tools
    ];
  };
}
