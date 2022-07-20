;;; qk-window.el -*- lexical-binding: t; -*-

;; The Perspective package provides multiple named workspaces (or "perspectives") in Emacs,
;; similar to multiple desktops in window managers like Awesome and XMonad, and Spaces on
;; the Mac. Each perspective has its own buffer list and its own window layout,
;; along with some other isolated niceties, like the xref ring.
(use-package perspective
  :straight t
  :defer 1
  :general
  (+general-global-project
    "s" 'persp-switch
    "l" 'persp-switch-last)
  :custom
  (persp-sort 'created)
  (persp-state-default-file (concat persp-save-dir "persp-save-state.el"))
  (persp-initial-frame-name "base")
  (persp-suppress-no-prefix-key-warning t)
  :config (persp-mode))

;; Olivetti is a mode for better writting, putting the buffer in a similar way as Quip
;; does. We need to make sure that some of the modes that we enable for other modes,
;; i.e. Display number lines mode, needs to be disabled when olivetti-mode is turned on.
(use-package olivetti
  :straight t
  :general
  (+general-global-toggle
    "o" 'qk-toggle-olivetti-mode)
  :config
  (defun qk-toggle-olivetti-mode ()
    "Toggle olivetti-mode applying different checks and functions to
  remove all the display numbers, and put it back after toggle."
    (interactive)
    (if olivetti-mode
        (progn
          (display-line-numbers-mode t)
          (olivetti-mode -1))
      (progn
        (display-line-numbers-mode -1)
        (olivetti-mode)))))

;; Popup windows are naturally annoying, disturbing the writing/programming flow
;; with every iteration or message. popper.el puts an end to the madness by managing
;; the buffers defined by the `popper-reference-buffers' variable inside its own groups.
;; You can toggle and cycle through them, to avoid losing your place and your window configuration.
(use-package popper
  :straight t
  :general
  (+general-global-toggle
    "p" 'popper-toggle-latest
    "c" 'popper-cycle
    "T" 'popper-toggle-type)
  :init
  (setq
   popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     ("\\*Asing-native-compile-log\\*" . hide)
     "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
     "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
     "^\\*term.*\\*$"   term-mode   ;term as a popup
     "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
     "\\*pytest\\*.*"
     help-mode
     helpful-mode
     "^.*\\/harpoon\\/.*\\#.*$" harpoon-mode))
  (popper-mode t))

;; Switch between windows using the keys in the home-row.
(use-package ace-window
  :straight t
  :general
  (+general-global-window
    "o" 'ace-window)
  :init
  (setq
   aw-keys '(?h ?j ?k ?l ?a ?o ?e ?i ?u)
   aw-background nil))

;; Winner Mode is a global minor mode that allows you to “undo” and “redo” changes
;; in Window configuration. It is included in GNU Emacs, and documented as winner-mode.
(use-package winner
  :defer 3
  :general
  (+general-global-window
    "u" 'winner-undo
    "r" 'winner-redo)
  :config (winner-mode))

;; Bufler is a command that lists the buffers per project, which is useful when some buffers
;; that might have expensive hooks running (i.e. lsp) are active in the background, and you
;; want to have a fresh go at Emacs.
(use-package bufler
  :straight t
  :general
  (+general-global-buffer
    "l" 'bufler-list)
  (general-nmap
    :keymaps '(bufler-list-mode-map)
    "x" 'bufler-list-buffer-kill
    "s" 'bufler-list-buffer-save
    "RET" 'bufler-list-buffer-switch
    "gr" 'bufler-list))

(provide 'qk-window)
;; qk-window.el ends here.
