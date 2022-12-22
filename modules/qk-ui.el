;;; qk-ui.el -*- lexical-binding: t; -*-

;; In order to be able to differentiate the parenthesis in all programming
;; modes, rainbow-delimiters considers different faces from your current theme
;; and adds the same face to the matching parens.
(elpaca-use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(elpaca-use-package all-the-icons
  :commands all-the-icons-install-fonts)

(elpaca-use-package doom-modeline
  :hook (elpaca-after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-mu4e t
   doom-modeline-project-detection 'project
   doom-modeline-modal-icon nil
   doom-modeline-window-width-limit fill-column
   doom-modeline-buffer-file-name-style 'truncate-with-project
   display-time-string-forms '((concat " " 24-hours ":" minutes " ")))
  (display-time-mode)
  (display-time-update))


(provide 'qk-ui)
;; qk-ui.el ends here.
