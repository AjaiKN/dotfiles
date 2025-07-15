;;; config/akn-bindings/init.el -*- lexical-binding: t; -*-

(when (modulep! +leader-keys)
  (if (modulep! :editor evil)
      (setq! doom-leader-key "SPC"
             doom-leader-alt-key "M-SPC"
             doom-localleader-key "C-c"
             doom-localleader-alt-key "C-c")
    (setq! doom-leader-alt-key "M-SPC" ;"C-."
           doom-localleader-alt-key "C-c"
           cursor-type 'bar))

  ;; also see https://github.com/TheJJ/conffiles/blob/master/.config/doom/keys.el
  (setq doom-leader-alt-key-states '(normal visual insert emacs motion operator replace global)))
