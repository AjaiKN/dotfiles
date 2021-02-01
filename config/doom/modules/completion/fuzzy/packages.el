;; -*- no-byte-compile: t; -*-
;;; completion/fuzzy/packages.el

(package! fussy                   :pin "c870a581d21019f43865242c1f6c681940f6a19c" :disable nil)
(when (or (modulep! +all) (modulep! +flx-rs))
  (package! flx-rs                  :pin "8598c106de5ae34a3542ebcb421bf3a9fbae5e25" :disable nil
    :recipe (:host github :repo "jcs-elpa/flx-rs" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fzf-native))
  (package! fzf-native              :pin "170f57fde8fd84ca7c2df37481c6fc10d4eecadf" :disable nil
    :recipe (:host github :repo "dangduc/fzf-native" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fuz-bin))
  (package! fuz-bin                 :pin "8caedeb1c5f20831757a5882277f930592dd23b5" :disable nil
    :recipe (:host github :repo "jcs-elpa/fuz-bin" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +liquidmetal))
  (package! liquidmetal             :pin "0d771c45ef032781308243f2ffca0abad1a173fa" :disable nil))
(when (or (modulep! +all) (modulep! +sublime-fuzzy))
  (package! sublime-fuzzy           :pin "4f666bd5053b33a82a60a63919554d474ad453b1" :disable nil
    :recipe (:host github :repo "jcs-elpa/sublime-fuzzy" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +hotfuzz))
  (package! hotfuzz                 :pin "48fcdae4b6aef1c9a92e600449e7a1e057b745d7" :disable nil))
