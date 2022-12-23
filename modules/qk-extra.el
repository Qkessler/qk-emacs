;;; qk-extra.el -*- lexical-binding: t; -*-

(elpaca-use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (add-hook! 'pdf-view-mode-hook
    (display-line-numbers-mode -1)
    (auto-revert-mode)))

(after! pdf-tools
  (elpaca org-pdftools (org-pdftools-setup-link)))

;; Instead of using the default authinfo password manager, which
;; defaults to the .authinfo.gpg file, configure Emacs to use
;; https://www.passwordstore.org/.

;; (elpaca-use-package auth-source-pass
;;   :init (auth-source-pass-enable))

;; The previous snippet configures Emacs to be able to access
;; the gpg files when a password is required. There is also a
;; pass major mode to insert and copy passwords from Emacs.
(elpaca-use-package pass
  :init (setq pass-username-field "login")
  :general
  (+general-global-applications
    "p" 'pass))

;; Emacs minor mode for making Anki cards with Org
(elpaca-use-package anki-editor
  :init
  (setq
   anki-editor-create-decks t
   anki-editor-org-tags-as-anki-tags t)
  :general
  (major-mode-definer
    :major-modes '(org-mode)
    :keymaps '(org-mode-map)
    "p" 'qk-push-anki-notes)
  :config
  (defun qk-push-anki-notes ()
    "Push anki notes to the server under heading."
    (interactive)
    (anki-editor-push-notes '(4))))

(after! org-capture
  (add-to-list 'org-capture-templates
               `("a" "Anki basic" entry (file org-default-notes-file)
                 ,(concat
                   "* %<%H:%M>   %^g\n:PROPERTIES:\n"
                   ":ANKI_NOTE_TYPE: Basic\n"
                   ":ANKI_DECK: %?\n"
                   ":VISIBILITY: folded\n"
                   ":END:\n"
                   "** Front\n\n"
                   "** Back\n\n"))))

;; Thanks to the amazing Ledger command line tool, which is an double-entry
;; accounting system that allows for fast queries and reports we are able to
;; connect our emacs --and ledger files-- to the amazing ledger-mode.
(elpaca-use-package ledger-mode
  :mode "\\.ledger\\'"
  :init
  (setq
   ledger-clear-whole-transactions t
   ledger-reports qk-ledger-reports)
  :general
  (major-mode-definer
    :major-modes '(ledger-mode)
    :keymaps '(ledger-mode-map)
    "y" 'ledger-copy-transaction-at-point
    "r" 'ledger-report)
  (major-mode-definer
    :major-modes '(ledger-report-mode)
    :keymaps '(ledger-report-mode-map)
    "e" 'ledger-report-edit-report
    "r" 'ledger-report)
  :custom-face
  (ledger-occur-xact-face ((t (:background "#222324" :inherit nil)))))

(elpaca-use-package (chatgpt
                     :host github
                     :repo "joshcho/ChatGPT.el"
                     :files ("dist" "*.el"))
  :init
  (setq chatgpt-repo-path qk-chatgpt-repo-path)
  :general
  (major-mode-definer
    :major-modes '(prog-mode text-mode org-mode)
    "q" 'chatgpt-query))

(provide 'qk-extra)
;; qk-extra.el ends here. 
