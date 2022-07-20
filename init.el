;;; init.el -*- lexical-binding: t; -*-

;; Reduce the checks for modifications on packages from straight, which
;; slows the configuration extensively. There are multiple options, but
;; I find `check-on-save' the most interesting, considering there are no
;; external dependencies, nor additional CPU or memory impact.
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(add-to-list 'straight-recipes-gnu-elpa-ignored-packages 'seq)

;; Always defer use-package packages. This means that if I really need
;; a package, I will go to my config and edit the use-package recipe
;;to lazy load it. This reduces my startup time significantly.
(setq use-package-always-defer t)
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; Add modules directory.
(add-load-path! "modules" "modules/personal")

;; Core libs.
(require 'core)
(require 'core-lib)

;; Modules
(require 'qk-general)
(require 'qk-evil)
(require 'qk-theme)
(require 'qk-defaults)
(require 'qk-help)
(require 'qk-window)
(require 'qk-completion)
(require 'qk-consult)
(require 'qk-embark)
(require 'qk-ui)
(require 'qk-org-roam)
;; (require 'qk-denote)
(require 'qk-org)
(require 'qk-org-agenda)
(require 'qk-git)
(require 'qk-project)
(require 'qk-lang)
(require 'qk-mail)
(require 'qk-lsp)
(require 'qk-amz)
(require 'qk-vterm)
(require 'qk-extra)

(setq custom-file "~/.emacs.d/var/custom.el")
(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)
(put 'narrow-to-region 'disabled nil)
