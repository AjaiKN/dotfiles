;;; config/akn-bindings/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload 'akn/evil-almost-end-of-line "modules/config/akn-bindings/autoload/evil" nil nil)
(evil-define-motion akn/evil-almost-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type exclusive
  (move-end-of-line nil)
  (evil-backward-char count))
