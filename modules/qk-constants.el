;;;  qk-constants.el -*- lexical-binding: t; -*-

(defconst )

(defvar qk-notes-directory (expand-file-name "~/Documents/slipbox/"))
(defvar qk-eww-download-directory (expand-file-name "~/Downloads/eww-downloads"))
(defvar qk-manual-lsp nil)
(defvar qk-ledger-reports
  '(("bal" "%(binary) -f %(ledger-file) bal --real")
    ("reg" "%(binary) -f %(ledger-file) reg")
    ("reg this month" "%(binary) -f %(ledger-file) reg -p \"this month\"")
    ("reg last month" "%(binary) -f %(ledger-file) reg -p \"last month\"")
    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
    ("account" "%(binary) -f %(ledger-file) reg %(account)")
    ("expenses this month" "%(binary) -p \"this month\" -f %(ledger-file) bal Expenses and not \
  \\(Expenses:Rebalancing or Expenses:Refundable or Expenses:Lent money\\)")
    ("expenses last month" "%(binary) -p \"last month\" -f %(ledger-file) bal Expenses and not \
  \\(Expenses:Rebalancing or Expenses:Refundable or Expenses:Lent money\\)")
    ("budget" "%(binary) -f %(ledger-file) -E bal ^Budget and not Budget:Checking")
    ("income last month" "%(binary) -f %(ledger-file) -p \"last month\" bal ^Income")))
(defvar qk-mu4e-load-path "/usr/local/Cellar/mu/1.6.10/share/emacs/site-lisp/mu/mu4e")
(defvar qk-mu4e-get-mail-command "mbsync amazon")
(defvar qk-mu4e-compose-signature "Quique Kessler Martínez\n")
(defvar qk-icalendar-org-capture-file (concat qk-notes-directory "/pages/meetings.org"))

(provide 'qk-constants)
;; qk-constants.el ends here.
