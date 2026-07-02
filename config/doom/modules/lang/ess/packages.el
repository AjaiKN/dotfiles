;;; lang/ess/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! ess :pin "da7d7dc1d2cf95760f56cb1763eb543c4dadaa0c")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! ess-view-data :pin "7dcbd23d4cef2030753d16e1ca1811d3466484e7")
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
