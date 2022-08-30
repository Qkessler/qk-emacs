;;; qk-lsp.el -*- lexical-binding: t; -*-

(defvar qk-manual-lsp nil)
(defun qk-toggle-manual-lsp ()
  "Toggle automatic initialization of the language server"
  (interactive)
  (setq qk-manual-lsp (not qk-manual-lsp)))

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
      (setq +lsp--default-read-process-output-max read-process-output-max
            +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024)
                    gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package flycheck
  :straight t
  :hook (lsp-mode . flycheck-mode)
  :general
  (minor-mode-definer
    :keymaps 'flycheck-mode
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error))

(use-package lsp-mode
  :straight t
  :commands lsp lsp-deferred qk-lsp-deferred
  :hook
  ((cc-mode
    c++-mode
    c-mode
    objc-mode
    swift-mode
    java-mode
    LaTeX-mode
    python-mode
    js-mode
    json-mode
    rustic-mode
    kotlin-mode
    scala-mode
    ) . qk-lsp-deferred)
  :general
  (minor-mode-definer
    :keymaps 'lsp-mode
    "a" 'lsp-execute-code-action
    "r" 'lsp-rename
    "R" 'lsp-find-references
    "f" 'lsp-format-buffer)
  (general-nmap
    :major-modes '(lsp-mode)
    "gi" 'lsp-goto-implementation)
  :init
  (setq 
   lsp-keep-workspace-alive nil
   lsp-idle-delay 0.5
   lsp-enable-file-watchers nil
   lsp-completion-show-detail nil
   lsp-completion-provider :none
   lsp-enable-snippet nil
   lsp-enable-folding nil
   lsp-enable-text-document-color nil
   lsp-enable-on-type-formatting nil
   lsp-signature-auto-activate nil
   lsp-ui-sideline-show-code-actions nil
   lsp-enable-links nil
   lsp-lens-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-headerline-breadcrumb-enable nil
   lsp-diagnostics-modeline-scope :file)
  :config
  (defun qk-lsp-deferred ()
    "lsp-deferred hook with preview filter."
    (unless qk-manual-lsp
      (lsp-deferred)))

  (add-hook! 'lsp-mode-hook
    (defun +lsp-display-guessed-project-root-h ()
      "Log what LSP things is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root."))))
    #'+lsp-optimization-mode)

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (prog1 (funcall fn restart)
          (+lsp-optimization-mode -1))
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall fn))
                       (+lsp-optimization-mode -1))))
             lsp--cur-workspace)))))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background "#191a1b" :inherit nil))))
  :init
  (setq
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-max-height 35)
  :general
  (minor-mode-definer
    :keymaps 'lsp-mode
    "h" 'lsp-ui-doc-show))

(use-package lsp-java
  :straight t
  :init
  (setq
   lsp-java-completion-max-results 20
   lsp-java-project-resource-filters
   ["node_modules" ".metadata" "archetype-resources" "META-INF/maven" "runtime" "env" ".bemol"]
   lsp-java-format-settings-url
   "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
   lsp-java-import-exclusions
   ["**/node_modules/**" "**/.metadata/**" "**/build" "**/META-INF/maven/**"]
   lsp-java-vmargs (list
                    "-noverify"
                    "--illegal-access=warn"
                    "-Xmx8G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication"
                    (concat "-javaagent:" "/Users/enrikes/.lombok/lombok.jar"))))

(after! lsp-mode
  (pushnew!
   lsp-file-watch-ignored-directories
   "[/\\\\]build$" "[/\\\\]eclipse-bin$" "[/\\\\]env$" "[/\\\\]node_modules$" "[/\\\\]\\.bemol$"))

(use-package python
  :init
  (setq
   lsp-pylsp-jedi-completion-fuzzy t
   lsp-pylsp-plugins-jedi-use-pyenv-environment t
   lsp-pylsp-plugins-black-enabled t
   lsp-pylsp-plugins-rope-completion-enabled t
   lsp-pylsp-plugins-pylint-enabled t))

(use-package dap-mode
  :straight t
  :init
  (setq dap-default-terminal-kind "integrated"))

(use-package lsp-rust
  :init 
  (setq
   lsp-rust-analyzer-cargo-watch-command "clippy"
   lsp-rust-analyzer-server-display-inlay-hints t
   lsp-rust-analyzer-proc-macro-enable t)
  :config
  (after! dap-mode
    (require 'dap-cpptools)
    (dap-register-debug-template
     "Rust::CppTools Run Configuration"
     (list :type "cppdbg"
           :request "launch"
           :name "Rust::Run"
           :MIMode "gdb"
           :miDebuggerPath "rust-gdb"
           :environment []
           :program "${workspaceFolder}/target/debug/amzn-rncc"
           :cwd "${workspaceFolder}"
           :console "external"
           :dap-compilation "brazil-build"
           :dap-compilation-dir "${workspaceFolder}"))))

(use-package dap-cpptools
  :straight dap-mode
  :config )
;; Swift and Objective-C lsp with sourcekit. It's the same lsp that Xcode uses.
(after! lsp-mode
  (defvar lsp-sourcekit-executable "sourcekit-lsp"
    "Binary for the sourcekit language server.")
  (defvar lsp-sourcekit-extra-args nil
    "Shell arguments to be passed to the `lsp-sourcekit-executable'.")

  (if (executable-find "xcrun")
      (setq lsp-sourcekit-executable
            (string-trim
             (shell-command-to-string "xcrun --find sourcekit-lsp"))))

  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (concat
                           lsp-sourcekit-executable
                           lsp-sourcekit-extra-args))
    :major-modes '(swift-mode objc-mode)
    :server-id 'sourcekit-ls)))

;; Metals language server for Scala development. 
(use-package lsp-metals
  :straight t)

(provide 'qk-lsp)
;; qk-lsp.el ends here.
