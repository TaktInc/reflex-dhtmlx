(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    dhtmlx = ../reflex-dhtmlx;
  };

  shells = {
    ghc = ["dhtmlx"];
    ghcjs = ["dhtmlx"];
  };
})
