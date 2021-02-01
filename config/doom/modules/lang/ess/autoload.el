;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/open-julia-repl (&optional arg)
  "Open an ESS Julia REPL"
  (interactive "P")
  (run-ess-julia arg)
  (current-buffer))

;;;###autoload
(defun +ess/open-r-repl ()
  "Open an ESS R REPL"
  (interactive)
  (call-interactively #'ess-switch-to-inferior-or-script-buffer)
  (current-buffer))

;;;###autoload
(defun +ess/insert-pipe ()
  (interactive)
  (insert " |> "))

;;;###autoload
(defun +ess/setup-windows ()
  (interactive)
  (when (derived-mode-p 'inferior-ess-mode-map)
    (call-interactively #'ess-switch-to-inferior-or-script-buffer))
  (maximize-window)
  (call-interactively #'ess-switch-to-inferior-or-script-buffer)
  (call-interactively #'+ess/essgd-r-plot-open))

;;;###autoload
(defun +ess-eval (beg end)
  (ess-eval-region beg end nil))

;;;###autoload
(defun +ess/r-plot-open ()
  (interactive)
  (save-selected-window
    (unless (derived-mode-p 'inferior-ess-r-mode)
      (dolist (w (doom-visible-windows))
        (when (with-selected-window w (derived-mode-p 'inferior-ess-mode))
          (select-window w))))
    (let ((proc (ess-get-process)))
      ;; "if(!(\"httpgd\" %in% rownames(installed.packages()))) install.packages(\"httpgd\")"
      (ess-send-string proc "httpgd::hgd()\n")
      (ess-wait-for-process proc)
      (when (derived-mode-p 'inferior-ess-mode) (goto-char (point-max)))
      (let ((url (with-current-buffer (ess-command "httpgd::hgd_url()\n" ;command
                                                   nil  ;out-buffer
                                                   nil  ;_sleep
                                                   nil  ;no-prompt-check
                                                   nil    ;wait
                                                   proc ;proc
                                                   nil  ;force-redisplay
                                                   0.5) ;timeout
                   (goto-char (point-min))
                   (search-forward-regexp (rx "[1] \"" (group (* any)) "\""))
                   (match-string-no-properties 1)))
            (switch-to-buffer-obey-display-actions t)
            (display-buffer-overriding-action (cons (list #'display-buffer-reuse-mode-window
                                                          #'display-buffer-below-selected)
                                                    '((window-height . 25)
                                                      (mode . (xwidget-webkit-mode essgd-mode))))))
        (xwidget-webkit-browse-url url)
        (tab-line-mode -1)
        (hide-mode-line-mode)
        (set-window-dedicated-p (selected-window) 'plot)))))

;;;###autoload
(defun +ess/essgd-r-plot-open ()
  (interactive)
  (save-selected-window
    (unless (derived-mode-p 'inferior-ess-r-mode)
      (dolist (w (doom-visible-windows))
        (when (with-selected-window w (derived-mode-p 'inferior-ess-mode))
          (select-window w))))
    (let ((switch-to-buffer-obey-display-actions t)
          (display-buffer-overriding-action (cons (list #'display-buffer-reuse-mode-window
                                                        #'display-buffer-below-selected)
                                                  '((window-height . 25)
                                                    (mode . (xwidget-webkit-mode essgd-mode))))))
      (essgd-start))))
