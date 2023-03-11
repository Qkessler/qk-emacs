;;; qk-consult.el -*- lexical-binding: t; -*-

;; Consult provides various practical commands based on the Emacs completion function
;; completing-read, which allows to quickly select an item from a list of candidates
;; with completion. Consult offers in particular an advanced buffer switching command
;; consult-buffer to switch between buffers and recently opened files. 
(use-package consult :elpaca t
  :commands
  consult--read consult-xref consult-register-format consult-register-window
  :general
  (global-definer
    "s" 'consult-line)
  (+general-global-buffer
    "b" 'consult-buffer
    "o" '(consult-buffer-other-window :which-key "other-window"))
  (+general-global-project
    "g" 'consult-ripgrep)
  (+general-global-org
    "h" 'consult-org-heading)
  (+general-global-file
    "r" 'consult-recent-file)
  (+general-global-help
    "g" `(,(cmd! (consult-ripgrep ".")) :which-key "Grep here")
    "f" `(,(cmd! (consult-find ".")) :which-key "Find here"))
  :init
  (setq
   consult-narrow-key qk-consult-narrow-key
   consult-preview-key qk-consult-preview-key
   consult-ripgrep-args (concat qk-rg-command
                                " --null"
                                " --hidden"
                                " --glob \"!**/.git/**\""
                                " --line-buffered"
                                " --color=never"
                                " --max-columns=1000"
                                " --path-separator /"
                                " --smart-case"
                                " --no-heading"
                                " --line-number .")
   consult-find-args (concat qk-fd-command " --color=never" " -H" " .")
   register-preview-delay 0)
  :config
  (consult-customize :preview-key (kbd "C-.")
                     consult-bookmark
                     consult-recent-file
                     consult-xref
                     consult-theme))

(after! (register consult)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

(after! (xref consult)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Consult-dir allows you to easily insert directory paths into the minibuffer
;; prompt in Emacs.  When using the minibuffer, you can switch - with completion
;; and filtering provided by your completion setup - to any directory you’ve
;; visited recently, or to a project or bookmarked directory. The minibuffer
;; prompt will be replaced with the directory you choose.
(use-package consult-dir :elpaca t
  :general
  (+general-global-project
    "d" 'consult-dir)
  (general-imap
    "C-d" nil)
  (:keymaps
   '(vertico-map)
   "C-d" 'consult-dir))

;; Personal package, using the same approach as consult-projectile by OlMon!
;; Creates a three source view (project files, projec buffers and known projects)
;; for the built-in package project.el.
(elpaca (consult-project-extra
         :host github
         :repo "Qkessler/consult-project-extra")
  :general
  (+general-global-project
    "f" 'consult-project-extra-find
    "o" '(consult-project-extra-find-other-window :which-key "find-other-window")))

(provide 'qk-consult)
;; qk-consult.el ends here.
