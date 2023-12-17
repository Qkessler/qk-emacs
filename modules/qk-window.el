;;; qk-window.el -*- lexical-binding: t; -*-

;; Olivetti is a mode for better writting, putting the buffer in a similar way as Quip
;; does. We need to make sure that some of the modes that we enable for other modes,
;; i.e. Display number lines mode, needs to be disabled when olivetti-mode is turned on.
(use-package olivetti :elpaca t
  :general
  (+general-global-toggle
    "o" 'qk-toggle-olivetti-mode)
  :config
  (defun qk-toggle-olivetti-mode ()
    "Toggle olivetti-mode applying different checks and functions to
  remove all the display numbers, and put it back after toggle."
    (interactive)
    (let* ((is-active olivetti-mode)
           (state (if is-active -1 t)))
      (olivetti-mode state))))

;; Popup windows are naturally annoying, disturbing the writing/programming flow
;; with every iteration or message. popper.el puts an end to the madness by managing
;; the buffers defined by the `popper-reference-buffers' variable inside its own groups.
;; You can toggle and cycle through them, to avoid losing your place and your window configuration.
(use-package popper :elpaca t
  :hook (doom-first-input . popper-mode)
  :general
  (+general-global-toggle
    "p" 'popper-toggle-latest
    "c" 'popper-cycle
    "T" 'popper-toggle-type)
  :init (setq popper-reference-buffers qk-popper-reference-buffers))

;; Switch between windows using the keys in the home-row.
(use-package ace-window :elpaca t
  :general
  (+general-global-window
    "o" 'ace-window
    "v" 'qk-split-window-vertically
    "s" 'qk-split-window-horizontally)
  :init
  (setq
   aw-keys qk-aw-keys
   aw-background nil)
  :config
  (defun qk-split-window-vertically ()
    "Split the window vertically and focus the newly opened window."
    (interactive)
    (let ((window (split-window-vertically)))
      (aw-switch-to-window window)))

  (defun qk-split-window-horizontally ()
    "Split the window horizontally and focus the newly opened window."
    (interactive)
    (let ((window (split-window-horizontally)))
      (aw-switch-to-window window))))

;; Winner Mode is a global minor mode that allows you to “undo” and “redo” changes
;; in Window configuration. It is included in GNU Emacs, and documented as winner-mode.
(use-package winner
  :hook (doom-first-buffer . winner-mode))

(after! (winner general)
  (+general-global-window
    "u" 'winner-undo
    "r" 'winner-redo))

(after! general
  (general-define-key
   :keymaps 'override
   :states '(emacs motion insert)
   "C-l" 'windmove-right
   "C-h" 'windmove-left
   "C-j" 'windmove-down)
  (general-define-key
   :keymaps 'override
   :states '(motion insert)
   "C-k" 'windmove-up))

(use-package bufler
  :elpaca t
  :general
  (+general-global-buffer
    "l" 'bufler)
  (general-nmap
    :keymaps 'bufler-list-mode-map
    "q" 'quit-window))

(provide 'qk-window)
;; qk-window.el ends here.
