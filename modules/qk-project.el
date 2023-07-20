;;; qk-project.el -*- lexical-binding: t; -*-

(after! project
  (+general-global-project
    "!" '(project-shell-command :which-key "shell-command")
    "&" '(project-async-shell-command :which-key "async shell-command")
    "D" 'project-dired
    "k" 'project-kill-buffers)
  (global-definer
    "e" 'project-dired))

;; Harpoon offers quick bookmarks separated by project and branch. You can quick
;; navigate between your working files and forget about that files opened that
;; you will not use anymore.  Harpoon persists between emacs sessions.
(use-package harpoon
  :elpaca (harpoon :host github :repo "otavioschwanck/harpoon.el")
  :hook (harpoon-mode . auto-revert-mode)
  :init
  (defun qk-tab-bar-get-current-tab-name ())
  (setq
   harpoon-cache-file (concat no-littering-var-directory "harpoon/")
   harpoon-without-project-function 'qk-tab-bar-get-current-tab-name)
  :general
  (+general-global-help
    "c" 'harpoon-clear
    "t" 'harpoon-toggle-file
    "a" 'harpoon-add-file)
  (global-definer
    "'" 'harpoon-go-to-1
    "," 'harpoon-go-to-2
    "." 'harpoon-go-to-3)
  :config
  (defun qk-tab-bar-get-current-tab-name ()
    (alist-get 'name (tab-bar--current-tab)))
  (defun harpoon-project-root-function () nil))

;; Affe is another package from the great =minad=, which keeps coming out with these
;; amazing integrations to the emacs default functionality, improving the performance
;; out of the park. In this case, he is trying to come up with a replacement to the
;; commonly known fzf.el and fuzzy-find.el modes, which emulate fuzzy find matching
;; running fzf in an emacs terminal process.
(use-package affe :elpaca t
  :general
  (+general-global-window
    "f" 'qk-affe-workspace-find)
  :init
  (setq
   affe-find-command
   (concat qk-rg-command " --color=never --files")
   affe-grep-command
   (concat qk-rg-command " --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ ."))
  :config
  (after! orderless
    (defun affe-orderless-regexp-compiler (input _type _ignorecase)
      (setq input (orderless-pattern-compiler input))
      (cons input (lambda (str) (orderless--highlight input str))))
    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

  (consult-customize affe-grep :preview-key (list :debounce 0.5 'any)))

(use-package detached
  :elpaca t
  :commands qk-run-dyncomp
  :hook (doom-first-input . detached-init)
  :init
  (setq detached-show-output-on-attach t
        detached-terminal-data-command system-type)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :general
  (global-definer
    "C" 'detached-compile)
  (major-mode-definer
    :keymaps '(prog-mode-map conf-mode-map)
    :major-modes '(prog-mode conf-mode)
    "c" (cmd! (qk-run-dyncomp "run"))
    "t" (cmd! (qk-run-dyncomp "test")))
  (+general-global-project
    "c" (cmd! (qk-run-dyncomp "run" t))
    "t" (cmd! (qk-run-dyncomp "test" t)))
  (:keymaps
   '(compilation-mode-map compilation-minor-mode-map)
   "C-j" nil
   "C-k" nil)
  :config
  (defun qk-run-dyncomp (&optional command should-run-in-project-root)
    "Run `dyncomp' CLI with `COMMAND' passed as argument.
If `COMMAND' is not provided, the user will be prompted to enter a command.
The `dyncomp' command and its argument will be passed to `compile' to be run.
The `command' argument is added to the `compile-history' list.
The current directory is set to the root directory of the current project before running `dyncomp'."
    (interactive)
    (let* ((directory (if should-run-in-project-root
                          (expand-file-name (project-root (project-current)))
                        default-directory))
           (argument-command (or command (consult--read compile-history)))
           (dyncomp-command (concat "dyncomp " argument-command)))
      (with-current-directory! directory
        (detached-compile dyncomp-command)
        (add-to-list 'compile-history argument-command)))))

(after! consult
  (defun qk-consult-compile (&optional command)
    "Run compile commands with consult history completion."
    (interactive)
    (compile (or command (consult--read compile-history))))

  (major-mode-definer
    :keymaps '(prog-mode-map)
    :major-modes '(prog-mode)
    "C" 'qk-consult-compile))

(provide 'qk-project)
;; qk-project.el ends here.
