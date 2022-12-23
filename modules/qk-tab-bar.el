;; qk-tab-bar.el -*- lexical-binding: t; -*-

(use-package tab-bar
  :hook (doom-first-buffer . tab-bar-mode)
  :init (setq tab-bar-show nil))

(after! (tab-bar general)
  (defvar qk-tab-bar-initial-tab-name "base")
  (defvar qk-tab-bar--last-tab qk-tab-bar-initial-tab-name)
  (defun qk-tab-bar-switch-to-last-tab ()
    "Switch to the previously accessed tab. If there isn't a previous tab, do nothing."
    (interactive)
    (when qk-tab-bar--last-tab
      (tab-bar-switch-to-tab qk-tab-bar--last-tab)))

  (defun qk-tab-bar--set-last-tab (&rest _)
    "Set the current tab as last tab."
    (let* ((current-tab (tab-bar--current-tab))
           (tab-name (alist-get 'name current-tab)))
      (setq qk-tab-bar--last-tab tab-name)))

  (advice-add 'tab-bar-switch-to-tab :before #'qk-tab-bar--set-last-tab)

  (+general-global-project
    "s" 'tab-bar-switch-to-tab
    "l" 'qk-tab-bar-switch-to-last-tab))

(provide 'qk-tab-bar)
;; qk-tab-bar.el ends here.
