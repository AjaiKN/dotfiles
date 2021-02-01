;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "56f355acbd835a5a0d65c495e6a80dc01a36d556")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! ess-view-data :pin "dd6a85935bbee0f497d0e8731abdfa07150600b7")
(package! essgd :pin "d9a3729ebaeeeec78984f00508cf2785bc7e8978")
(package! polymode :pin "74ba75d4bcfbea959ccc9080a95ab9ef759849f2")
(package! poly-R :pin "8024e852cfca642dea2045a41b2033baa2f1f9a5")

(when (modulep! +stan)
  (package! stan-mode :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (package! eldoc-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (when (modulep! :completion company)
    (package! company-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")))
