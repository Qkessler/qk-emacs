;;; qk-eglot.el -*- lexical-binding: t; -*-

;; Emacs Polyglot: an Emacs LSP client that stays out of your way: By design,
;; Eglot doesn't depend on anything but Emacs. But there are ELPA dependencies
;; to newer versions of so-called "core packages" developed in the Emacs mainline.

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)
(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :straight t
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-auto-display-help-buffer nil
        eglot-stay-out-of '(flymake))
  :config
  (require 'flycheck-eglot)

  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
              "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
              :around #'eglot--managed-mode
              (letf! (defun eglot-shutdown (server)
                       (if (or (null +lsp-defer-shutdown)
                               (eq +lsp-defer-shutdown 0))
                           (prog1 (funcall eglot-shutdown server)
                             (+lsp-optimization-mode -1))
                         (run-at-time
                          (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                          nil (lambda (server)
                                (unless (eglot--managed-buffers server)
                                  (prog1 (funcall eglot-shutdown server)
                                    (+lsp-optimization-mode -1))))
                          server)))
                     (funcall fn server))))

(provide 'qk-eglot)
;;; qk-eglot.el ends here.

