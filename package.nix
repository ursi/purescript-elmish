{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  { dependencies =
      let inherit (ps-pkgs-ns) ursi; in
      [ console
        foreign-object
        heterogeneous
        js-timers
        ordered-collections
        parallel
        task
        ursi.debug
        ursi.html
        ursi.murmur3
        ursi.prelude
        ursi.producer
        ursi.refeq
      ];
  }
