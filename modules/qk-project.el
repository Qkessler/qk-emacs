;;; qk-project.el -*- lexical-binding: t; -*-

(use-package project
  :general
  (+general-global-project
    "!" '(project-shell-command :which-key "shell-command")
    "&" '(project-async-shell-command :which-key "async shell-command")
    "c" 'project-compile
    "D" 'project-dired
    "k" 'project-kill-buffers))

;; Harpoon offers quick bookmarks separated by project and branch. You can quick
;; navigate between your working files and forget about that files opened that
;; you will not use anymore.  Harpoon persists between emacs sessions.
(use-package harpoon
  :straight (harpoon
             :type git
             :host github
             :repo "otavioschwanck/harpoon.el"
             :branch "master")
  :hook (harpoon-mode . auto-revert-mode)
  :init
  (setq harpoon-cache-file (concat no-littering-var-directory "harpoon/"))
  :general
  (+general-global-jump
    "c" 'harpoon-clear
    "t" 'harpoon-toggle-file
    "a" 'harpoon-add-file)
  (global-definer
    "'" 'harpoon-go-to-1
    "," 'harpoon-go-to-2
    "." 'harpoon-go-to-3))

;; Affe is another package from the great =minad=, which keeps coming out with these
;; amazing integrations to the emacs default functionality, improving the performance
;; out of the park. In this case, he is trying to come up with a replacement to the
;; commonly known fzf.el and fuzzy-find.el modes, which emulate fuzzy find matching
;; running fzf in an emacs terminal process.
(use-package affe
  :straight t
  :general
  (+general-global-search
    "f" 'affe-find)
  (+general-global-window
    "f" 'qk-affe-workspace-find)
  :init
  (setq
   affe-find-command
   (concat qk-rg-command " -null --color=never --files")
   affe-grep-command
   (concat qk-rg-command " --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ ."))
  (after! orderless
    (defun affe-orderless-regexp-compiler (input _type _ignorecase)
      (setq input (orderless-pattern-compiler input))
      (cons input (lambda (str) (orderless--highlight input str))))
    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))
  :config
  (consult-customize affe-grep :preview-key (list :debounce 0.5 'any))
  (after! amz-workspace
    (defun qk-affe-workspace-find ()
      "Find files in the amz workspace if the default directory is
         a package. If it isn't call the default `affe-find'."
      (interactive)
      (if amz-package-mode
          (affe-find (amz-workspace-workspace-root))
        (affe-find)))))

(use-package detached
  :straight t
  :init
  (setq detached-show-output-on-attach t
        detached-terminal-data-command system-type)
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session)))

(defun qk-run-dyncomp (&optional command)
  "Run `dyncomp' CLI with `COMMAND' passed as argument.
If `COMMAND' is not provided, the user will be prompted to enter a command.
The `dyncomp' command and its argument will be passed to `compile' to be run.
The `command' argument is added to the `compile-history' list.
The current directory is set to the root directory of the current project before running `dyncomp'."
  (interactive)
  (with-current-directory! (expand-file-name (project-root (project-current)))
    (let* ((argument-command (or command (consult--read compile-history)))
           (dyncomp-command (concat "dyncomp " argument-command)))
      (compile dyncomp-command)
      (add-to-list 'compile-history argument-command))))

(use-package consult
  :general
  (major-mode-definer
    :keymaps '(prog-mode-map)
    :major-modes '(prog-mode)
    "r" 'qk-dyncomp-run 
    "t" 'qk-dyncomp-test
    "C" 'qk-consult-compile)
  :config
  (defun qk-consult-compile (&optional command)
    "Run compile commands with consult history completion."
    (interactive)
    (compile (or command (consult--read compile-history))))

  (defun qk-dyncomp-run ()
    "Run `dyncomp run' using the `qk-run-dyncomp' function."
    (interactive)
    (qk-run-dyncomp "run"))

  (defun qk-dyncomp-test ()
    "Run `dyncomp run' using the `qk-run-dyncomp' function."
    (interactive)
    (qk-run-dyncomp "test")))

(provide 'qk-project)
;; qk-project.el ends here.
