;;; emacs/vlf/config.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'akn-doom-use-package)
  (require 'doom-keybinds)
  (require 'doom-lib)
  (require 'akn))

;; Largely taken from:
;; https://tecosaur.github.io/emacs-config/config.html#very-large-files

(defvar vlf-application 'ask)

;; When VLF advises `abort-if-file-too-large' to add an option for opening with
;; VLF, it removes the option to open the file literally. Let's remove the
;; advice and instead override `files--ask-user-about-large-file'. This
;; sacrifices the (setq vlf-application 'always) option (which I can’t imagine
;; wanting anyway).
(after! vlf-setup
  (advice-remove #'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  ;; https://git.savannah.gnu.org/cgit/emacs/elpa.git/commit/?h=externals/vlf&id=6192573ee088079bf1f81abc2bf2a370a5a92397
  (advice-remove #'abort-if-file-too-large #'vlf--if-file-too-large))
(advice-add #'files--ask-user-about-large-file :override #'+vlf--files--ask-user-about-large-file-a)

;; TODO: PR, bug in VLF (see `+vlf--scroll-up-a')
;; When scrolling down (or, to use emacs's backwards terminology, "scrolling up"),
;; VLF doesn't check if we're on the last chunk, so it keeps reloading the same
;; chunk and taking us to the top of the bottom chunk.
(after! vlf
  (advice-remove #'scroll-up #'ad-Advice-scroll-up)
  ;; https://git.savannah.gnu.org/cgit/emacs/elpa.git/commit/?h=externals/vlf&id=6192573ee088079bf1f81abc2bf2a370a5a92397
  (advice-remove #'scroll-up #'vlf--scroll-up)
  (advice-add 'scroll-up :around #'+vlf--scroll-up-a))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf
  :commands vlf vlf-mode)

(after! vlf
  (map! :map vlf-prefix-map
        [remap evil-scroll-down]      (lambda (count)
                                        (interactive (list (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
                                        (setq count (evil--get-scroll-count count))
                                        (scroll-up count))
        [remap evil-scroll-up]        (lambda (count)
                                        (interactive (list (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
                                        (setq count (evil--get-scroll-count count))
                                        (scroll-down count))
        [remap evil-scroll-page-down] #'scroll-up-command
        [remap evil-scroll-page-up]   #'scroll-down-command
        [remap beginning-of-buffer]   #'+vlf/beginning-of-file
        [remap end-of-buffer]         #'+vlf/end-of-file)

  (add-hook 'vlf-mode-hook #'+vlf/enable-follow)
  (add-hook 'vlf-mode-hook #'akn/local-smooth-scroll-disabled-mode)

  (advice-add #'vlf-recenter :before-while #'+vlf--recenter-a)
  (advice-add #'evil-goto-line :around #'+vlf--evil-goto-line-a)

  ;; As you go from one chunk fetched by VLF to the next, the displayed line
  ;; number of the first line in each chunk is unchanged. I think it’s
  ;; reasonable to hope for an overall line number, and by tracking chunk’s
  ;; cumulative line numbers we can implement this behaviour fairly easily.
  (add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

  (setq! vlf-batch-size-remote (* 128 1024)))

(setq-hook! 'vlf-mode-hook
  ;; The other thing that doesn’t work too well with VLF is searching with
  ;; anything other than M-x occur. This is because trying to go to the next
  ;; match at the end of a chunk usually wraps the point to the beginning of
  ;; the chunk, instead of moving to the next chunk.
  isearch-wrap-function #'+vlf-isearch-wrap
  ;; Unfortunately, since evil-search doesn’t have an analogue to
  ;; isearch-wrap-function, we can’t easily add support to it.
  evil-search-module 'isearch
  ;; Most often, large buffers aren't things I actually want to edit.
  buffer-read-only t)

;; TODO: remove this once Doom bumps evil-collection
;; (since my PR https://github.com/emacs-evil/evil-collection/pull/843 was accepted)
(define-advice evil-collection-vlf-setup (:override () +vlf-use-prefix-map-not-mode-map)
  "Set up `evil' bindings for `vlf'."
  (evil-set-initial-state 'vlf-mode 'normal)

  (evil-collection-define-key 'normal 'vlf-prefix-map
    "gj" 'vlf-next-batch
    "gk" 'vlf-prev-batch
    (kbd "C-j") 'vlf-next-batch
    (kbd "C-k") 'vlf-prev-batch
    "]]" 'vlf-next-batch
    "[[" 'vlf-prev-batch

    "+" 'vlf-change-batch-size
    "-" 'evil-collection-vlf-decrease-batch-size
    "=" 'vlf-next-batch-from-point

    ;; refresh
    "gr" 'vlf-revert

    "s" 'vlf-re-search-forward
    "S" 'vlf-re-search-backward

    "gg" 'vlf-beginning-of-file
    "G" 'vlf-end-of-file
    "J" 'vlf-jump-to-chunk
    "E" 'vlf-ediff-buffers

    "g%" 'vlf-query-replace
    "go" 'vlf-occur
    "L" 'vlf-goto-line
    "F" 'vlf-toggle-follow))

(add-hook 'vlf-occur-mode-hook #'doom-mark-buffer-as-real-h)
