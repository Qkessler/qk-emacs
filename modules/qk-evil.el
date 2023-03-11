;;; qk-evil.el  -*- lexical-binding: t; -*-

(use-package evil
  :elpaca t
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
    [escape] 'qk-nohl-and-quit 
    "K" nil)
  (general-vmap
    [escape] 'qk-nohl-and-quit
    "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (general-mmap
    "C-u" 'qk-center-scroll-half-page-up
    "C-d" 'qk-center-scroll-half-page-down)
  (:keymaps
   '(minibuffer-local-map
     minibuffer-local-ns-map
     minibuffer-local-completion-map
     minibuffer-local-must-match-map
     minibuffer-local-isearch-map)
   [escape] 'minibuffer-keyboard-quit)
  :config
  (defun qk-nohl-and-quit ()
    "Run nohighlight on escape, in normal mode."
    (interactive)
    (evil-ex-nohighlight)
    (keyboard-quit))

  (defun qk-center-scroll-half-page-down ()
    "Center window after scrolling half page down."
    (interactive)
    (evil-scroll-down nil)
    (evil-scroll-line-to-center nil))

  (defun qk-center-scroll-half-page-up ()
    "Center window after scrolling half page up."
    (interactive)
    (evil-scroll-up nil)
    (evil-scroll-line-to-center nil))

  ;; Stop copying each visual state move to the clipboard.
  (advice-add #'evil-visual-update-x-selection :override #'ignore)
  (fset 'evil-redirect-digit-argument 'ignore)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-mode t))

(use-package evil-collection
  :elpaca t
  :commands evil-collection-init
  :init
  (setq
   evil-collection-outline-bind-tab-p nil
   evil-collection-setup-minibuffer t))

(defvar +evil-collection-disabled-list
  '(anaconda-mode buff-menu calc comint company custom eldoc elisp-mode
                  ert free-keys helm help indent image kotlin-mode outline replace
                  shortdoc simple slime lispy)
  "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

(defvar evil-collection-mode-list
  `(
    2048-game ag alchemist anaconda-mode apropos arc-mode atomic-chrome
    auto-package-update beginend bluetooth bm bookmark
    (buff-menu "buff-menu") calc calendar cider cmake-mode comint
    company compile consult corfu (custom cus-edit) cus-theme daemons
    dashboard deadgrep debbugs debug devdocs dictionary diff-hl
    diff-mode dired dired-sidebar disk-usage doc-view docker ebib ebuku
    edbi edebug ediff eglot explain-pause-mode elfeed eldoc elisp-mode
    elisp-refs elisp-slime-nav embark emms epa ert eshell eval-sexp-fu
    evil-mc eww ,@(if (> emacs-major-version 28) '(emoji)) fanyi finder
    flycheck flymake forge free-keys geiser ggtags git-timemachine gnus
    go-mode grep guix hackernews helm help helpful hg-histedit hungry-delete
    ibuffer image image-dired image+ imenu imenu-list
    (indent "indent") indium info ivy js2-mode leetcode lispy log-edit
    log-view lsp-ui-imenu lua-mode kotlin-mode macrostep man
    (magit magit-repos magit-submodule) magit-section magit-todos
    markdown-mode monky mpc mu4e mu4e-conversation neotree newsticker
    notmuch nov omnisharp org org-present org-roam osx-dictionary outline
    p4 (package-menu package) pass (pdf pdf-tools) popup proced prodigy
    profiler python quickrun racer racket-describe realgud reftex replace
    restclient rg ripgrep rjsx-mode robe rtags ruby-mode scheme scroll-lock
    selectrum sh-script ,@(if (> emacs-major-version 27) '(shortdoc))
    simple simple-mpc slime sly snake so-long speedbar tablist tar-mode
    telega (term term ansi-term multi-term) tetris thread tide timer-list
    transmission trashed tuareg typescript-mode vc-annotate vc-dir vc-git
    vdiff vertico view vlf vterm vundo w3m wdired wgrep which-key woman
    xref xwidget yaml-mode youtube-dl zmusic (ztree ztree-diff)))

(defun +evil-collection-init (module &optional disabled-list)
  "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
  (unless (memq (or (car-safe module) module) disabled-list)
    (doom-log "editor:evil: loading evil-collection-%s %s"
              (or (car-safe module) module)
              (if doom-init-time "" "(too early!)"))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))

(after! evil-collection
  (mapc #'+evil-collection-init '(comint custom)))

(after! evil
  (add-transient-hook! 'help-mode
    (+evil-collection-init 'help))
  (add-transient-hook! 'Buffer-menu-mode
    (+evil-collection-init '(buff-menu "buff-menu")))
  (add-transient-hook! 'calc-mode
    (+evil-collection-init 'calc))
  (add-transient-hook! 'image-mode
    (+evil-collection-init 'image))
  (add-transient-hook! 'emacs-lisp-mode
    (+evil-collection-init 'elisp-mode))
  (add-transient-hook! 'occur-mode
    (+evil-collection-init 'replace))
  (add-transient-hook! 'indent-rigidly
    (+evil-collection-init '(indent "indent")))
  (add-transient-hook! 'minibuffer-setup-hook
    (when evil-collection-setup-minibuffer
      (+evil-collection-init 'minibuffer)
      (evil-collection-minibuffer-insert)))
  (add-transient-hook! 'process-menu-mode
    (+evil-collection-init '(process-menu simple)))
  (add-transient-hook! 'shortdoc-mode
    (+evil-collection-init 'shortdoc))
  (add-transient-hook! 'tabulated-list-mode
    (+evil-collection-init 'tabulated-list))
  (add-transient-hook! 'tab-bar-mode
    (+evil-collection-init 'tab-bar))

  (dolist (mode evil-collection-mode-list)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (+evil-collection-init mode +evil-collection-disabled-list)))))

(use-package evil-commentary
  :elpaca t
  :hook (doom-first-input . evil-commentary-mode))

(use-package evil-org
  :elpaca t
  :hook (org-mode . evil-org-mode)
  :init (setq evil-org-special-o/O '(table-row item))
  :config
  (evil-org-set-key-theme '(textobjects insert additional shift)))

(after! (evil elpaca)
  (general-nmap
    :keymaps 'elpaca-ui-mode-map
    "T" 'elpaca-ui-search-tried
    "U" 'elpaca-ui-unmark
    "b" 'elpaca-ui-browse-package
    "d" 'elpaca-ui-mark-delete
    "g" 'revert-buffer
    "h" 'describe-mode
    "i" 'elpaca-ui-mark-install
    "I" 'elpaca-ui-search-installed
    "l" 'elpaca-log
    "m" 'elpaca-manager
    "n" 'nil
    "p" 'nil
    "q" 'quit-window
    "r" 'elpaca-ui-mark-rebuild
    "s" 'elpaca-ui-search
    "t" 'elpaca-status
    "u" 'elpaca-ui-mark-update
    "v" 'elpaca-visit
    "x" 'elpaca-ui-execute-marks))

(after! org-agenda
  (general-def
    :keymaps 'org-agenda-mode-map
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

(after! dired
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory))

(use-package evil-matchit
  :elpaca t
  :hook (doom-first-input . global-evil-matchit-mode))

(use-package avy
  :elpaca t
  :general
  (global-definer
    "k" 'avy-goto-line-above
    "j" 'avy-goto-line-below))

(use-package evil-surround
  :elpaca t
  :hook (doom-first-input . global-evil-surround-mode))

(use-package evil-numbers
  :elpaca t
  :general
  (general-nmap
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt))

(provide 'qk-evil)
;;; qk-evil.el ends here.
