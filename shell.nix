with import ./default.nix {};
let
  refreshScript = pkgs.writeShellScriptBin "ref"
    ''
    hpack .
    cabal new-build
    '';
  ghcidScript = pkgs.writeShellScriptBin "dev"
    ''
    hpack .
    cabal new-build
    ghcid --command 'cabal new-repl exe:build-site' --allow-eval --warnings -o ghcid.txt
    '';
  runScript = pkgs.writeShellScriptBin "run" "cabal run exe:build-site";
in hsPkgs.shellFor {
    packages = myHsPkgs: [
      myHsPkgs.jonathanlorimerdev
    ];
    # withHoogle = true;
    buildInputs = with pkgs; [
      cabal-install # cabal, haskell build tool
      cabal2nix # Utility to download Haskell packages into Nix format
      haskell-language-server # language server
      hsPkgs.ghcid # haskell repl with hot reloading
      hsPkgs.hpack # generate cabal file from package.yaml

      # Demo dependencies
      nodejs
      nodePackages.serve

      # Scripts
      refreshScript
      ghcidScript
      runScript
    ];
}

