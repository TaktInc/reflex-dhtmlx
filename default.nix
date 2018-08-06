(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    reflex-dhtmlx = ./reflex-dhtmlx;
    reflex-dhtmlx-example = ./reflex-dhtmlx-example;
  };

  shells = {
    ghc = ["reflex-dhtmlx" "reflex-dhtmlx-example"];
    ghcjs = ["reflex-dhtmlx" "reflex-dhtmlx-example"];
  };
})
