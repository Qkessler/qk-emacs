;;; qk-window.el -*- lexical-binding: t; -*-

;; Olivetti is a mode for better writting, putting the buffer in a similar way as Quip
;; does. We need to make sure that some of the modes that we enable for other modes,
;; i.e. Display number lines mode, needs to be disabled when olivetti-mode is turned on.
(elpaca-use-package olivetti
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
(elpaca-use-package popper
 :hook (doom-first-input . popper-mode)
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
     "^.*\\/harpoon\\/.*\\#.*$" harpoon-mode)))

;; Switch between windows using the keys in the home-row.
(elpaca-use-package ace-window
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
  :hook (doom-first-buffer . winner-mode))

(after! (winner general)
  (+general-global-window
    "u" 'winner-undo
    "r" 'winner-redo))

(after! general
  (+general-global-buffer
    "l" 'ibuffer))

(provide 'qk-window)
;; qk-window.el ends here.
