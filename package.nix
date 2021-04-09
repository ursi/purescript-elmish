{ ps-pkgs, ps-pkgs-ns }:
  with ps-pkgs;
  { repo = "https://github.com/ursi/purescript-elmish.git";
    rev = "";
    dependencies =
      let inherit (ps-pkgs-ns) ursi; in
      [ console
        foreign-object
        heterogeneous
        js-timers
        parallel
        task
        ursi.debug
        ursi.html
        ursi.prelude
        web-dom
        web-events
        web-html
      ];
  }