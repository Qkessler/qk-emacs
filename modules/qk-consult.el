;;; qk-consult.el -*- lexical-binding: t; -*-

;; Consult provides various practical commands based on the Emacs completion function
;; completing-read, which allows to quickly select an item from a list of candidates
;; with completion. Consult offers in particular an advanced buffer switching command
;; consult-buffer to switch between buffers and recently opened files. 
(use-package consult
  :straight t
  :commands consult--read
  :general
  (+general-global-file
    "r" 'consult-recent-file)
  (general-nmap
    :prefix "SPC"
    "s" 'consult-line)
  (+general-global-buffer
    "b" 'consult-buffer
    "o" '(consult-buffer-other-window :which-key "other-window"))
  (+general-global-project
    "g" 'consult-ripgrep)
  (+general-global-window
    "g" 'qk-amz-workspace-grep)
  (+general-global-org
    "h" 'consult-org-heading)
  :init
  (setq consult-narrow-key "<"
        consult-preview-key (list :debounce 0.5 'any)
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
        register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize :preview-key (kbd "C-.")
                     consult-bookmark
                     consult-recent-file
                     consult-xref
                     consult-theme)
  (after! amz-workspace
    (defun qk-amz-workspace-grep ()
      "Find lines in the amz workspace if the default directory is
         a package. If it isn't call the default `affe-grep'."
      (interactive)
      (if amz-package-mode
          (consult-ripgrep (amz-workspace-workspace-root))
        (consult-ripgrep)))))

;; Consult-dir allows you to easily insert directory paths into the minibuffer
;; prompt in Emacs.  When using the minibuffer, you can switch - with completion
;; and filtering provided by your completion setup - to any directory you’ve
;; visited recently, or to a project or bookmarked directory. The minibuffer
;; prompt will be replaced with the directory you choose.
(use-package consult-dir
  :straight t
  :general
  (+general-global-project
    "d" 'consult-dir)
  (general-imap
    "C-d" nil)
  (:keymaps
   '(vertico-map)
   "C-d" 'consult-dir))

;; The consult integration of the better flycheck.
(use-package consult-flycheck
  :straight t
  :general
  (minor-mode-definer
    :keymaps 'lsp-mode
    "e" 'consult-flycheck))

;; Personal package, using the same approach as consult-projectile by OlMon!
;; Creates a three source view (project files, projec buffers and known projects)
;; for the built-in package project.el.
(use-package consult-project-extra
  :straight (consult-project-extra
             :type git
             :host github
             :repo "Qkessler/consult-project-extra")
  :general
  (+general-global-project
    "f" 'consult-project-extra-find
    "o" '(consult-project-extra-find-other-window :which-key "find-other-window")))

(provide 'qk-consult)
;; qk-consult.el ends here.
