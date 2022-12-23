;;; qk-git.el -*- lexical-binding: t; -*-

;; Provides a convenient way of simultaneous browsing through the differences between
;; a pair (or a triple) of files or buffers. Nevertheless, it needs to be configured
;; to remove some of the defaults that are horrible. We will remove the frame that
;; ediff creates, opting for using a window with options if needed.
(use-package ediff
  :commands ediff
  :init
  (setq 
   ediff-diff-options "-w"
   ediff-split-window-function (if (> (frame-width) 150)
                                   'split-window-horizontally
                                 'split-window-vertically))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; `smerge-mode' is a minor mode included in Emacs that provides merging functionality.
;; There has been defined multiple funcions to navigate and act upon changes in files.
;; You are able to use ediff-like functionality to move around and make the changes
;; that you need. The following configuration provides the automatic activation.
(use-package smerge-mode
  :hook (find-file . modi-enable-smerge-maybe)
  :config
  (defun modi-enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1)))))

(after! smerge-mode
  (minor-mode-definer
    :keymaps 'smerge-mode
    "u" 'smerge-keep-upper
    "l" 'smerge-keep-lower
    "n" 'smerge-next
    "p" 'smerge-prev
    "a" 'smerge-keep-all))

(elpaca-use-package magit
  :init 
  (setq
   git-commit-summary-max-length 50
   magit-diff-hide-trailing-cr-characters t)
  :general
  (minor-mode-definer
    :keymaps 'git-commit-mode
    "f" 'with-editor-finish
    "c" 'with-editor-cancel)
  (+general-global-magit
    "s" 'magit-status
    "f" 'magit-find-file
    "l" 'magit-log-buffer-file
    "d" 'magit-diff-buffer-file)
  :config
  (add-hook! 'git-commit-mode-hook (set-fill-column 72))
  (add-hook! 'magit-status-mode (display-line-numbers-mode -1)))

(elpaca-use-package magit-delta
  :hook (magit-mode . magit-delta-mode)
  :init
  (setq magit-delta-default-dark-theme "gruvbox-dark"))

(provide 'qk-git)
;; qk-git.el ends here.
