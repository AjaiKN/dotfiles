;;; tools/stgit/autoload.el -*- lexical-binding: t; -*-
(require 'transient)

(defun +stgit--at-patch-p ()
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node)))
    (cl-case (stgit-patch->status patch)
      (work      nil)
      (index     nil)
      (committed nil)
      (t         t))))

;;;###autoload (autoload '+stgit/transient "tools/stgit/autoload/transient" "Transient for `stgit-mode'." t)
(transient-define-prefix +stgit/transient ()
  "Transient for `stgit-mode'."
  [""
   ["Basic"
    ("?" "help" stgit-help)
    ("q" "quit buffer" stgit-quit)]
   ["Reload/repair"
    ("g" "reload buffer" stgit-reload)
    ("C-u g" "repair metadata" stgit-repair)]
   ["Undo"
    ("C-_" "undo" stgit-undo)
    ("C-c C-_" "redo" stgit-redo)]
   ["Shell"
    ("s" "git status" stgit-git-status :if (lambda () (fboundp 'git-status)))
    ("!" "stgit command" stgit-execute)]]
  ["Movement"
   ["Patches"
    ("p" "previous" stgit-previous-patch)
    ("n" "next" stgit-next-patch)]
   ["Patch groups"
    ("M-{" "previous" stgit-previous-patch-group)
    ("M-}" "next" stgit-next-patch-group)]
   ["Marking patches"
    :inapt-if-not +stgit--at-patch-p
    ("SPC" "mark & move down" stgit-mark-down)]
   ["Unmarking patches"
    :inapt-if-not +stgit--at-patch-p
    ("DEL" "unmark & move up" stgit-unmark-up)
    ("u" "unmark & move down" stgit-unmark-down)]]
  ["Patches"
   ["Basic"
    ("RET" "show/hide changed files" stgit-select)
    ("r" "refresh with current changes" stgit-refresh)
    ("=" "show patch log and diff" stgit-diff)
    ("+" "expand" stgit-expand)
    ("-" "collapse" stgit-collapse)]
   ["New"
    ("c" "create" stgit-new-and-refresh)
    ("N" "new empty" stgit-new)
    ("C-o" "new empty before point" stgit-new-here)]
   ["Edit"
    ("C-c C-r" "rename" stgit-rename :inapt-if-not stgit-patch-name-at-point)
    ("e" "edit description" stgit-edit :inapt-if-not stgit-patch-name-at-point)
    ("D" "delete" stgit-delete :inapt-if-not stgit-patches-marked-or-at-point)]]
  [["Index / work tree"
    ("U" "revert all changes" stgit-revert)
    ("i" "toggle all staged" stgit-toggle-index)]
   ["Push/pop"
    (">" "push" stgit-push-next)
    ("<" "pop" stgit-pop-next)
    ("P" "push/pop marked" stgit-push-or-pop)
    ("G" "make current by push/pop" stgit-goto)]
   ["Manipulate"
    :inapt-if (lambda () (< (length stgit-marked-patches) 1))
    ("S" "squash" stgit-squash :inapt-if (lambda () (< (length stgit-marked-patches) 2)))
    ("M" "move marked to point" stgit-move-patches)]
   ["Commit"
    ("C-c C-c" "commit" stgit-commit)
    ("C-c C-u" "uncommit" stgit-uncommit)]]
  [["Files"
    :inapt-if-not stgit-patched-file-at-point
    ("RET" "Open here" stgit-select)
    ("o" "Open other window" stgit-find-file-other-window)
    ("=" "Diff" stgit-diff)
    ("i" "toggle index" stgit-toggle-index)
    ("U" "revert changes" stgit-revert)]
   ["Toggle showing"
    ("t n" "patch names" stgit-toggle-patch-names)
    ("t t" "index and work tree" stgit-toggle-worktree)
    ("t u" "unknown files" stgit-toggle-unknown)
    ("t i" "ignored files" stgit-toggle-ignored)
    ("t h" "recent commits" stgit-toggle-committed)
    ("t s" "subversion information" stgit-toggle-svn)]
   ["Show diff"
    ("=" "of patch or file" stgit-diff)
    ("d r" "for range of patches" stgit-diff-range :inapt-if-not)
    ("d b" "against merge base" stgit-diff-base)
    ("d o" "against our branch" stgit-diff-ours)
    ("d t" "against their branch" stgit-diff-theirs)]
   ["Merge conflicts"
    ("d m" "smerge-ediff" stgit-find-file-merge :inapt-if-not stgit-patched-file-at-point)
    ("R" "Mark resolved" stgit-resolve-file
     :inapt-if-not (lambda ()
                     (let* ((patched-file (stgit-patched-file-at-point))
                            (patch        (stgit-patch-at-point))
                            (patch-name   (and patch (stgit-patch->name patch)))
                            (status       (and patched-file (stgit-file->status patched-file))))
                       (and (memq patch-name '(:work :index))
                            (eq status 'unmerged)))))]
   ["Branch"
    ("B" "Switch/create" stgit-branch)
    ("C-c C-b" "Rebase" stgit-rebase)]])
