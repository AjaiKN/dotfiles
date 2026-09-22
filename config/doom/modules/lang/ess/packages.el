;;; lang/ess/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! ess :pin "254d3297836f2a2a509fe78fc393ba9037d73614")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! ess-view-data :pin "9d12c80097b532a5af4561a8c65f1862cb414c7a")
(package! essgd :pin "d9a3729ebaeeeec78984f00508cf2785bc7e8978")
(package! polymode :pin "8cb72fa5dcc0d98746c680043dc121edc7621e3a")
(package! poly-R :pin "fee0b6e99943fa49ca5ba8ae1a97cbed5ed51946")

(when (modulep! +stan)
  (package! stan-mode :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")
  (package! eldoc-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")
  (when (modulep! :completion company)
    (package! company-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")))
