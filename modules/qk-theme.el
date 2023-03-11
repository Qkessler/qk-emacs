;;; qk-theme.el  -*- lexical-binding: t; -*-

;; I add the theme list here to be able to add to it when a theme is tangled,
;; considering that I will most likely want to toggle two themes, the ones
;; that are tangled: dark and light.
(setq qk-themes-list nil)

;; The list will be (light dark), choose the index
;; according to the theme that you want loaded.
(setq qk-themes-index 1)

(defun qk-cycle-theme ()
  "Change the theme to the next index in the `qk-themes-list'. I would normally use this for switching from light to dark modes."
  (interactive)
  (setq qk-themes-index (% (1+ qk-themes-index) (length qk-themes-list)))
  (qk-load-indexed-theme))

(defun qk-load-theme (theme)
  "Load theme without slip-through, disable and enable
  the theme completely. I believe this should be modified
  in the next versions of Emacs; keeping this for now." 
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

(defun qk-load-indexed-theme ()
  "Load the active indexed theme in the `qk-themes-list'"
  (qk-load-theme (nth qk-themes-index qk-themes-list)))

(use-package gruvbox-theme
  :elpaca t
  :init
  (add-to-list 'qk-themes-list 'gruvbox-dark-medium)
  (add-to-list 'qk-themes-list 'modus-operandi)

  (qk-load-indexed-theme))

;; Emacs does not have an =after-load-theme-hook=, which a I find key for adding
;; or changing some of the faces dinamically. Not everything is lost, we still
;; have the =advice= sintax. We can advise the change-theme function to define a hook.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice consult-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(defadvice qk-load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(provide 'qk-theme)
;; qk-theme.el ends here.
