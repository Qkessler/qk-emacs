;;; qk-vterm.el -*- lexical-binding: t; -*-

(elpaca-use-package vterm
  :init
  (setq
   vterm-max-scrollback qk-vterm-max-scrollback
   vterm-timer-delay qk-vterm-timer-delay)
  :general
  (major-mode-definer
    :keymaps '(vterm-mode-map)
    :major-modes '(vterm-mode)
    "c" 'vterm-copy-mode
    "C-c" 'vterm-send-C-c)
  (+general-global-applications
    "t" 'qk-vterm-toggle-on-tab)
  :config
  (add-hook! 'vterm-mode-hook
    (display-fill-column-indicator-mode -1)
    (setq-local evil-move-cursor-back nil)
    (evil-insert-state))

  (defun qk-vterm-toggle-on-tab ()
    "Vterm toggle on the tab listed on `qk-vterm-tab'."
    (interactive)
    (unless (string= (alist-get 'name (tab-bar--current-tab)) qk-vterm-tab)
      (tab-bar-switch-to-tab qk-vterm-tab))
    (if (get-buffer vterm-buffer-name)
        (switch-to-buffer vterm-buffer-name)
      (vterm)))

  (defun qk-return-to-non-vterm-tab ()
    "Return to the previous non-vterm tab. Useful for using with Keyboard maestro."
    (interactive)
    (when (string= (alist-get 'name (tab-bar--current-tab)) qk-vterm-tab)
      (qk-tab-bar-switch-to-last-tab)))

  (define-key global-map (kbd "C-c") nil))

(provide 'qk-vterm)
;; qk-vterm.el ends here.
