;;; app/omni/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! consult-gh              :pin "c8707e86d36ea4ec38e177ca8fa0313a5374a0a9" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-gh"))
(package! consult-omni            :pin "3a126ee54479755408faed10da945dbc2366303b" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! browser-hist            :pin "aab0a364077bfbf5559085086545d30bbaf7ac5e" :disable nil                                      :recipe (:host github :repo "agzam/browser-hist.el"))
