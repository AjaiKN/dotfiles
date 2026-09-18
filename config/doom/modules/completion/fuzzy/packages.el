;;; completion/fuzzy/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! fussy                   :pin "4e2a5e70c80da35d2ac7ddc0e3146bead827d564" :disable nil)
(when (or (modulep! +all) (modulep! +flx-rs))
  (package! flx-rs                  :pin "313bb342cec3b93b64dd00077d678e897a5990f4" :disable nil
    :recipe (:host github :repo "jcs-elpa/flx-rs" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fzf-native))
  (package! fzf-native              :pin "4b9236e8cd1e9f9f3aaf5f2ebf83f1fc5995d38d" :disable nil
    :recipe (:host github :repo "dangduc/fzf-native" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +fuz-bin))
  (package! fuz-bin                 :pin "4f924f69cb43e7889242448276c68c0f7d82154b" :disable nil
    :recipe (:host github :repo "jcs-elpa/fuz-bin" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +liquidmetal))
  (package! liquidmetal             :pin "0d77273c627ccb1c93834f072aae2b0bf6eff052" :disable nil))
(when (or (modulep! +all) (modulep! +sublime-fuzzy))
  (package! sublime-fuzzy           :pin "445e8c349d15860aaa6d74ce98b2a9a14608d17e" :disable nil
    :recipe (:host github :repo "jcs-elpa/sublime-fuzzy" :files (:defaults "bin"))))
(when (or (modulep! +all) (modulep! +hotfuzz))
  (package! hotfuzz                 :pin "ff72f544e03dd2afb358f28014b15529104c1d89" :disable nil))
