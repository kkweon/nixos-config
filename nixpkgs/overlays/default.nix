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
      self.google-chrome
      self.tree
      self.neovim
      self.gitAndTools.hub
      self.gnupg
      self.dzen2
      self.slack

      # Python
      self.python
      (self.python3.withPackages (ps: with ps; [ websocket_client sexpdata neovim ]))

      # JS
      self.nodejs
      self.yarn

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
