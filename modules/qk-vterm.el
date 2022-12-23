;;; qk-vterm.el -*- lexical-binding: t; -*-

(elpaca-use-package vterm
  :init
  (setq
   vterm-max-scrollback 10000
   vterm-timer-delay nil)
  :general
  (major-mode-definer
    :keymaps '(vterm-mode-map)
    :major-modes '(vterm-mode)
    "c" 'vterm-copy-mode
    "C-c" 'vterm-send-C-c)
  :config
  (add-hook! 'vterm-mode-hook
    (display-fill-column-indicator-mode -1)
    (setq-local evil-move-cursor-back nil))

  (define-key global-map (kbd "C-c") nil)
  (pushnew! popper-reference-buffers '("^\\*vterm.*\\*$" vterm-mode)))

(elpaca-use-package vterm-toggle
  :hook (vterm-toggle-show . evil-insert-state)
  :init
  (setq
   vterm-toggle-reset-window-configration-after-exit t
   vterm-toggle-hide-method 'delete-window)
  :general
  (+general-global-applications
    "t" 'vterm-toggle-cd))

(provide 'qk-vterm)
;; qk-vterm.el ends here.
