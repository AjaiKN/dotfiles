;; -*- no-byte-compile: t; -*-
;;; completion/fuzzy/packages.el

(package! fussy                   :pin "163ded34be3e9230702201d0abe1e7b85e815c2d" :disable nil)
(when (or (modulep! +all) (modulep! +flx-rs))
  (package! flx-rs                  :pin "1ba793fa044f3f5699780d8f0a1e395ca0648e5b" :disable nil
    :recipe (:host github :repo "jcs-elpa/flx-rs" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fzf-native))
  (package! fzf-native              :pin "b159c581699e0661da015a845760578598880307" :disable nil
    :recipe (:host github :repo "dangduc/fzf-native" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fuz-bin))
  (package! fuz-bin                 :pin "4f924f69cb43e7889242448276c68c0f7d82154b" :disable nil
    :recipe (:host github :repo "jcs-elpa/fuz-bin" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +liquidmetal))
  (package! liquidmetal             :pin "6c1a039996bedee0aefd1f8d096129f29c7a7993" :disable nil))
(when (or (modulep! +all) (modulep! +sublime-fuzzy))
  (package! sublime-fuzzy           :pin "445e8c349d15860aaa6d74ce98b2a9a14608d17e" :disable nil
    :recipe (:host github :repo "jcs-elpa/sublime-fuzzy" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +hotfuzz))
  (package! hotfuzz                 :pin "ff72f544e03dd2afb358f28014b15529104c1d89" :disable nil))
