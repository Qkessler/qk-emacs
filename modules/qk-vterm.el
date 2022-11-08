;;; qk-vterm.el -*- lexical-binding: t; -*-

(use-package vterm
  :straight `(:pre-build
              ((shell-command "rm -fr build && mkdir build && cd $_ && cmake .. && make")))
  :general
  (major-mode-definer
    :keymaps '(vterm-mode-map)
    :major-modes '(vterm-mode)
    "c" 'vterm-copy-mode
    "C-c" 'vterm-send-C-c)
  :init
  (setq
   vterm-max-scrollback 10000
   vterm-timer-delay nil)
  :config
  (add-hook! 'vterm-mode-hook
    (display-fill-column-indicator-mode -1)
    (setq-local evil-move-cursor-back nil))

  (define-key global-map (kbd "C-c") nil)
  (pushnew! popper-reference-buffers '("^\\*vterm.*\\*$" vterm-mode)))

(use-package vterm-toggle
  :straight t
  :hook (vterm-toggle-show . evil-insert-state)
  :general
  (+general-global-applications
    "t" 'vterm-toggle-cd)
  :init
  (setq
   vterm-toggle-reset-window-configration-after-exit t
   vterm-toggle-hide-method 'delete-window))

(provide 'qk-vterm)
;; qk-vterm.el ends here.
