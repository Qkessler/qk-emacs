;;; qk-ui.el -*- lexical-binding: t; -*-

;; In order to be able to differentiate the parenthesis in all programming
;; modes, rainbow-delimiters considers different faces from your current theme
;; and adds the same face to the matching parens.
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-base-error-face
   ((t (:foreground "#fc0303" :inherit nil))))
  (rainbow-delimiters-mismatched-face
   ((t (:foreground "#fc0303" :inherit nil))))
  (rainbow-delimiters-unmatched-face
   ((t (:foreground "#fc0303" :inherit nil)))))

(use-package all-the-icons
  :straight t
  :commands all-the-icons-install-fonts)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :init
  (defface egoge-display-time
    '((((type x w32 mac))
       (:inherit highlight))
      (((type tty))
       (:foreground "blue")))
    "Face used to display the time in the mode line.")
  (setq
   doom-modeline-mu4e t
   doom-modeline-project-detection 'project
   doom-modeline-modal-icon nil
   doom-modeline-window-width-limit fill-column
   doom-modeline-buffer-file-name-style 'truncate-with-project
   display-time-string-forms
   '((propertize (concat " " 24-hours ":" minutes " ")
                 'face 'egoge-display-time)))
  :config
  (display-time-mode)
  (display-time-update))

(provide 'qk-ui)
;; qk-ui.el ends here.
