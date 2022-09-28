;;; qk-evil.el  -*- lexical-binding: t; -*-

(use-package evil
  :straight t
  :demand t
  :init 
  (setq
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll t
   evil-want-C-i-jump nil
   evil-respect-visual-line-mode t
   evil-undo-system 'undo-redo
   evil-search-module 'evil-search
   evil-want-Y-yank-to-eol t)
  :general
  (general-mmap
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "gD" 'xref-find-definitions-other-window)
  (general-nmap
    [escape] 'keyboard-quit
    "K" nil)
  (general-vmap
    [escape] 'keyboard-quit
    "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (:keymaps
   '(minibuffer-local-map
     minibuffer-local-ns-map
     minibuffer-local-completion-map
     minibuffer-local-must-match-map
     minibuffer-local-isearch-map)
   [escape] 'minibuffer-keyboard-quit)
  :config
  ;; stop copying each visual state move to the clipboard:
  ;; https://github.com/emacs-evil/evil/issues/336
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-mode t))

(use-package evil-collection
  :straight t
  :demand t
  :init
  (setq
   evil-collection-outline-bind-tab-p nil
   evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(fset 'evil-redirect-digit-argument 'ignore)

(use-package evil-commentary
  :straight t
  :defer 5
  :config (evil-commentary-mode))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :init (setq evil-org-special-o/O '(table-row item))
  :config
  (evil-org-set-key-theme '(textobjects insert additional shift)))

(use-package org-agenda
  :general
  (:keymaps '(org-agenda-mode-map)
   "TAB" nil
   "<tab>" nil)
  (general-mmap
    :keymaps '(org-agenda-mode-map)
    "s" 'org-agenda-schedule
    "r" 'org-agenda-refile
    "[[" 'org-agenda-earlier
    "]]" 'org-agenda-later
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "gr" 'org-agenda-redo
    "." 'org-agenda-goto-today
    "RET" 'org-agenda-goto
    "TAB" 'org-agenda-show
    "t" 'org-agenda-todo
    "C" 'org-agenda-capture))

(use-package dired
  :general
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory))

(use-package evil-matchit
  :straight t
  :defer 3
  :config
  (global-evil-matchit-mode t))

(use-package avy
  :straight t
  :general
  (general-mmap
    "gl" 'avy-goto-line))

(use-package evil-surround
  :straight t
  :defer 3
  :config (global-evil-surround-mode t))

(use-package evil-numbers
  :straight t
  :general
  (general-nmap
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt))

(provide 'qk-evil)
;;; qk-evil.el ends here.
