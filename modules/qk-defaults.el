;;; qk-defaults.el -*- lexical-binding: t; -*-

;; Holds all the configuration that makes emacs better working with what's already
;; in the box, but probably could be thought over. On the other hand, add defaults
;; that seem interesting for the modern user.

(elpaca-use-package no-littering
  :init
  (setq
   backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/")))
   auto-save-list-file-prefix (no-littering-expand-var-file-name "auto-saves/sessions/")
   auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-saves/") t))
   url-cookie-file (no-littering-expand-var-file-name "cookies/cookies")
   eww-bookmarks-directory (concat no-littering-var-directory "eww-bookmarks/")))

(use-package emacs
  :init
  (setq
   auto-revert-interval 1
   revert-without-query '(".*")
   mac-right-option-modifier 'none
   ad-redefinition-action 'accept
   warning-suppress-log-types '((comp))
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   inhibit-startup-message t
   scroll-preserve-screen-position 'always
   scroll-margin 2
   display-line-numbers-type t
   display-line-numbers-width 3
   next-line-add-newlines t
   visible-bell nil
   ring-bell-function 'ignore
   enable-recursive-minibuffers t
   minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
   tab-always-indent 'complete
   x-select-enable-clipboard t
   x-select-enable-primary t)
  (setq-default
   indent-tabs-mode qk-indent-tabs-mode
   tab-width qk-tab-width
   fill-column qk-fill-column)

  (fset 'yes-or-no-p 'y-or-n-p)
  (pixel-scroll-precision-mode)
  (savehist-mode)
  (recentf-mode)
  (add-function :after after-focus-change-function #'(lambda () (save-some-buffers t)))
  (add-hook! 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Use UTF-8 everywhere.
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  :custom-face (bookmark-face ((t nil))))

(defun visiting-buffer-rename (file newname &optional _ok-if-already-exists)
  "Rename buffer visiting FILE to NEWNAME.
Intended as :after advice for `rename-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (set-visited-file-name newname nil t)
        (when (derived-mode-p 'emacs-lisp-mode)
          (save-excursion
            (let* ((base (file-name-nondirectory file))
                   (sans (file-name-sans-extension base))
                   (newbase (file-name-nondirectory newname))
                   (newsans (file-name-sans-extension newbase)))
              (goto-char (point-min))
              (while (search-forward-regexp (format "^;;; %s" base) nil t)
                (replace-match (concat ";;; " newbase)))
              (goto-char (point-max))
              (when
                  (search-backward-regexp (format "^(provide '%s)" sans) nil t)
                (replace-match (format "(provide '%s)" newsans))))))))))

(defun visiting-buffer-kill (file &optional _trash)
  "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (kill-buffer buffer))))

(advice-add 'rename-file :after 'visiting-buffer-rename)
(advice-add 'delete-file :after 'visiting-buffer-kill)

(use-package dired
  :init 
  (setq
   dired-listing-switches qk-dired-listing-switches
   dired-use-ls-dired nil))

(elpaca-use-package dired-subtree
  :after dired
  :init (setq dired-subtree-use-backgrounds nil)
  :general 
  (:keymaps
   '(dired-mode-map)
   "<tab>"  'dired-subtree-toggle
   "<C-tab>" 'dired-subtree-cycle
   "<backtab>" 'dired-subtree-remove))

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode olivetti-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
       Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)

(use-package shr
  :init
  (setq
   shr-use-colors nil
   shr-use-fonts nil            
   shr-max-image-proportion qk-shr-max-image-proportion
   shr-image-animate nil
   shr-width nil              
   shr-discard-aria-hidden t
   shr-cookie-policy nil))

;; “EWW”, the Emacs Web Browser, is a web browser for GNU Emacs. It can load, parse,
;; and display various web pages using “shr.el”. However a GNU Emacs with ‘libxml2’ support is required.
(use-package eww
  :init
  (setq
   eww-restore-desktop t
   eww-desktop-remove-duplicates t
   eww-header-line-format nil
   eww-download-directory qk-eww-downloads-directory
   eww-suggest-uris '(eww-links-at-point thing-at-point-url-at-point)
   eww-history-limit qk-eww-history-limit
   eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)"
   eww-browse-url-new-window-is-tab nil
   eww-form-checkbox-selected-symbol "[X]"
   eww-form-checkbox-symbol "[ ]"
   eww-retrieve-command nil))


(provide 'qk-defaults)
;; qk-defaults.el ends here.
