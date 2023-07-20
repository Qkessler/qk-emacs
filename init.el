;;; init.el -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Always defer use-package packages. This means that if I really need
;; a package, I will go to my config and edit the use-package recipe
;;to lazy load it. This reduces my startup time significantly.
(setq use-package-always-defer t)
(require 'use-package)

(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca general (require 'general))
(elpaca-wait)

(add-to-list 'load-path (concat user-emacs-directory "modules"))
;; Add modules directory.
(add-to-list 'load-path (concat user-emacs-directory "modules/personal"))

;; Core libs.
(require 'core)
(require 'core-lib)

;; Modules
(require 'qk-constants)
(require 'qk-general)
(require 'qk-evil)
(require 'qk-theme)
(require 'qk-defaults)
(require 'qk-dired)
(require 'qk-help)
(require 'qk-window)
(require 'qk-completion)
(require 'qk-consult)
(require 'qk-embark)
(require 'qk-ui)
(require 'qk-org)
(require 'qk-org-agenda)
(require 'qk-denote)
(require 'qk-git)
(require 'qk-project)
(require 'qk-tab-bar)
(require 'qk-lang)
(require 'qk-mail)
(require 'qk-eglot)
;; (require 'qk-vterm)
(require 'qk-tramp)
(require 'qk-extra)
(require 'qk-amz)

(setq custom-file "~/.emacs.d/var/custom.el")
(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
