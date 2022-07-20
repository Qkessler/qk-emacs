;;;  qk-general.el -*- lexical-binding: t; -*-
;;; Define spacemacs bindings using the leader SPC key and general
;;; for common Emacs keybidings, to avoid Rsi.

(use-package general
  :straight t
  :demand t
  :config

  ;; Create SPC leader key, to be used in the macro.
  (general-create-definer global-definer
    :keymaps 'override
    :states  '(emacs normal hybrid motion visual operator)
    :prefix  "SPC"
    :non-normal-prefix "C-SPC")

  ;; Add a minor-mode-definer, for each of the modes.
  (general-create-definer major-mode-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix "SPC m"
    "" '(:ignore t :which-key
                 (lambda (arg)
                   `(,(cadr (split-string (car arg) " ")) .
                     ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  ;; Add an additional minor-mode-definer, for each of the modes.
  ;; It is key to remember that in this case, the :keymaps option refers to the minor-mode,
  ;; not the keymap.
  (general-create-definer minor-mode-definer
    :keymaps 'override
    :definer 'minor-mode
    :states '(emacs normal hybrid motion visual operator)
    :prefix "SPC m")

  ;; Configure general to define functions for each of the evil modes.
  (general-evil-setup)

  ;; Emulate different default Emacs keys, as I'm transitioning into evil.
  (global-definer
    ""     nil
    "c"   (general-simulate-key "C-c")
    "SPC" 'execute-extended-command
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x"))

  ;; Macro to define all key-pockets. It adapts to the name passed, and defines additonal macros to be
  ;; used to define keybindings. See `general-global-buffer' below.
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
      Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  ;; Buffer commands.
  (+general-global-menu! "buffer" "b"
    "d"  'kill-current-buffer
    "b" 'switch-to-buffer
    "o" '(switch-to-buffer-other-window :which-key "other-window")
    "r"  'rename-buffer
    "R"  'revert-buffer)

  ;; Evaluation commands.
  (+general-global-menu! "eval" "e"
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-expression
    "s" 'eval-last-sexp)

  ;; Window commands.
  (+general-global-menu! "window" "w"
    "=" 'balance-windows
    "v" 'split-window-vertically
    "s" 'split-window-horizontally
    "m" 'delete-other-windows
    "d" 'delete-window)

  ;; Project commands, using the built-in `project'.
  (+general-global-menu! "project" "p")

  ;; File commands.
  (+general-global-menu! "file" "f"
    "D" 'save-buffers-kill-terminal
    "d" 'dired
    "E" 'sudo-edit
    "f" 'find-file
    "j" 'dired-jump
    "s" 'save-buffer
    "o" 'find-file-other-window
    "S" 'save-some-buffers)

  (+general-global-menu! "insert" "i"
    "e" (lambda () (interactive)(qk-real-insert ?€)))

  ;; Search commands, currently using `consult' and `affe'.
  (+general-global-menu! "search" "s")
  (+general-global-menu! "org" "o")
  (+general-global-menu! "toggle" "t")
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "applications" "a")
  (+general-global-menu! "magit" "g")
  (+general-global-menu! "jump" "j")
  (+general-global-menu! "zoom" "z"))

(provide 'qk-general)
;;; qk-general.el ends here.
