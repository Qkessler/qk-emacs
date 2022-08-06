;; qk-tab-bar.el -*- lexical-binding: t; -*-

(defvar qk-tab-bar-initial-tab-name "base")
(use-package tab-bar
  :init
  (setq
   tab-bar-show nil
   harpoon-without-project-function 'qk-tab-bar-get-current-tab-name
   )
  (tab-bar-mode)
  :general
  (+general-global-project
    "s" 'tab-bar-switch-to-tab
    "l" 'qk-tab-bar-switch-to-last-tab)
  :config
  (add-hook! server-after-make-frame (tab-rename qk-tab-bar-initial-tab-name))
  (defun qk-tab-bar-get-tab-name (tab)
    "Return the `name' property of TAB."
    (alist-get 'name tab))

  (defun qk-tab-bar-get-current-tab-name ()
    "Return the `name' for the current tab."
    (qk-tab-bar-get-tab-name (tab-bar--current-tab)))

  (defun qk-tab-bar-switch-to-last-tab ()
    "Switch to the previously accessed tab. If there isn't a previous tab, do nothing."
    (interactive)
    (when qk-tab-bar--last-tab
      (tab-bar-switch-to-tab qk-tab-bar--last-tab)))

  (defvar qk-tab-bar--last-tab qk-tab-bar-initial-tab-name)
  (defun qk-tab-bar--set-last-tab (&rest _)
    "Set the current tab as last tab."
    (setq qk-tab-bar--last-tab (qk-tab-bar-get-current-tab-name)))

  (advice-add 'tab-bar-switch-to-tab :before #'qk-tab-bar--set-last-tab))

(provide 'qk-tab-bar)
