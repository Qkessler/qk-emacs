;;; init.el -*- lexical-binding: t; -*-

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (if-let (((pop-to-buffer buffer '((display-buffer-reuse-window
                                         display-buffer-same-window))))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Always defer use-package packages. This means that if I really need
;; a package, I will go to my config and edit the use-package recipe
;;to lazy load it. This reduces my startup time significantly.
(setq use-package-always-defer t)
(require 'use-package)

;; Add modules directory.
(add-to-list 'load-path (concat user-emacs-directory "modules"))
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
(require 'qk-extra)
(require 'qk-amz)

(setq custom-file "~/.emacs.d/var/custom.el")
(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
