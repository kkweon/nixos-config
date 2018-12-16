self: super:
{
  neovim = super.neovim.override {
    extraPythonPackages = pkgs: with pkgs; [ websocket_client sexpdata neovim ];
    extraPython3Packages = pkgs: with pkgs; [ websocket_client sexpdata neovim ];
    configure = {
      customRC = builtins.readFile(~/.vim/vimrc);
    };
  };

  sh2md = self.haskellPackages.callPackage ~/github/sh2md/default.nix { };

  myPackages = super.buildEnv {
    name = "my-packages";
    paths = [
      self.xclip
      self.fd
      self.ripgrep
      self.stow
      self.tree
      self.neovim
      self.gitAndTools.hub
      self.gnupg
      self.dzen2
      self.slack
			self.unzip

      self.firefox

      # Python
      self.python
      (self.python3.withPackages
        (ps: with ps;
        [
          websocket_client
          sexpdata
          neovim
          black
          pytest
          pylint
          yapf
          tox
          pip
          setuptools
        ])
      )
      self.httpie

      # JS
      self.nodejs
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
      self.haskellPackages.xmonad
      self.haskellPackages.xmonad-extras
      self.haskellPackages.xmonad-contrib
      self.haskellPackages.xmobar

      # Java
      self.jdk
      # Scala
      self.sbt

      # Go
      self.go

      # C/C++
      self.gnumake
      self.gcc
      self.cmake
    ];
  };
}
