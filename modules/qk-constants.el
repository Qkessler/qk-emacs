;;; qk-constants.el -*- lexical-binding: t; -*-

(defvar qk-indent-tabs-mode nil
  "Default: indent with spaces instead of tabs. See `indents-tabs-mode'.")
(defvar qk-tab-width 4
  "Default: indent with 4 columns. See `tab-width'.")
(defvar qk-fill-column 120
  "Default: add 80 characters as fill-column. See `fill-column'.")
(defvar qk-line-numbers-type 'visual
  "Default: Relative line numbers, for ergonomic line jumps. See `display-line-numbers-type'.")
(defvar qk-dired-listing-switches "-aBhl"
  "Default: show hidden files with full information in human readable format.
See `dired-listing-switches'.")
(defvar qk-dired-all-the-icons t
  "Default: show icons on dired buffers.")
(defvar qk-dired-all-the-icons-show-colors t
  "Default: show colors on the icons on dired buffers. Only works if `qk-dired-all-the-icons' is `t'.")
(defvar qk-shr-max-image-proportion 0.6
  "Default: scale images to 0.6 when decoding. See `shr-max-image-proportion'.")
(defvar qk-eww-downloads-directory (expand-file-name "/tmp/eww-downloads")
  "Default: use /tmp/eww-downloads as downloads dir. See `eww-download-directory'.")
(defvar qk-eww-history-limit 150 
  "Default: only keep 150 searches as history. See `eww-history-limit'.")
(defvar qk-popper-reference-buffers
  '("\\*Messages\\*"
    "\\*Warnings\\*"
    "Output\\*$"
    "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
    "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
    "^\\*term.*\\*$"   term-mode   ;term as a popup
    "\\*pytest\\*.*"
    help-mode helpful-mode
    "^.*\\/harpoon\\/.*\\#.*$" harpoon-mode
    ("\\*Asing-native-compile-log\\*" . hide))
  "Default: all buffers that would make sense with the loaded packages.
See `popper-reference-buffers'")
(defvar qk-aw-keys '(?h ?j ?k ?l ?a ?o ?e ?i ?u)
  "Default: Dvorak's home-row keys for `ace-window' jumping. See `aw-keys'.")
(defvar qk-corfu-auto t
  "Default: `corfu' completion automatically pops-up on `qk-corfu-auto-prefix' characters.
See `corfu-auto'.")
(defvar qk-corfu-auto-prefix 1
  "Default: number of characters on which to pop-up `corfu' auto completion. Only valid
 if `qk-corfu-auto' is `t'. See `corfu-auto-prefix'.")
(defvar qk-corfu-auto-delay 0.25
  "Default: delay to pop-up `corfu' completion. Only valid if `qk-corfu-auto' is `t'.
See `corfu-auto-delay'.")
(defvar qk-corfu-popupinfo-delay 1
  "Default: delay to show help on `corfu' completion candidates. See `corfu-popupinfo-delay'.")
(defvar qk-corfu-in-minibuffer t
  "Default: `corfu' pop-up also happens on the minibuffer.")
(defvar qk-cape-dabbrev-min-length 3
  "Default: number of characters before starting cape dabbrev, for in-buffer keyword
completion. See `cape-dabbrev-min-length'.")
(defvar qk-rg-command "rga"
  "Default: use `rga' CLI to search between lines. See `consult-ripgrep-args'.")
(defvar qk-fd-command "fd"
  "Default: use `fd' command for file lookups. See `affe-find-command'.")
(defvar qk-consult-narrow-key "<"
  "Default: use this key to narrow candidates when running `consult' commands.
See `consult-narrow-key'.")
(defvar qk-consult-preview-key (list :debounce 0.5 'any)
  "Default: automatic preview on 0.5 seconds for most commands.
See `consult-preview-key'")
(defvar qk-doom-modeline-buffer-file-name-style 'truncate-with-project
  "Default: show <PROJECT_NAME>/<LETTER>/<LETTER>/<FILE_NAME> on modeline.
See `doom-modeline-buffer-file-name-style' for options.")
(defvar qk-modeline-display-time t
  "Default: display updating time on modeline. See `display-time-update'.")
(defvar qk-notes-directory (expand-file-name "~/slipbox-sync/slipbox/")
  "Default: my slipbox's path.")
(defvar qk-org-archive-location (concat qk-notes-directory "../archive/%s_archive::")
  "Default: archive name concatenated to archive. See `org-archive-location'.")
(defvar qk-org-agenda-deadline-warning-days 3
  "Default: number of days upon which to warn the user of a deadline.
See `org-agenda-deadline-warning-days'")
(defvar qk-org-agenda-skip-scheduled-if-deadline-is-shown t
  "Default: skip scheduled date if deadline is present.
See `org-agenda-skip-scheduled-if-deadline-is-shown'")
(defvar qk-denote-directory (concat qk-notes-directory "pages/")
  "Default: pages directory inside our `qk-notes-directory'.
See `denote-directory'.")
(defvar qk-notes-dailies-directory (expand-file-name (concat qk-denote-directory "../dailies/"))
  "Default: directory where to store dailies, which contain DONE items when moving headers from
TODO to DONE.")
(defvar qk-org-default-notes-file (concat qk-denote-directory "20230109T114537--refile__learning_tasks.org")
  "Default: `qk-notes-directory' 's refile.org. See `org-default-notes-file'.")
(defvar qk-org-meetings-file (concat qk-denote-directory "20230112T171448--meetings__project_learning.org")
  "Default: my personal meetings file, you can change it to the file that denote generates for you.")
(defvar qk-denote-prompts '(title keywords)
  "Default: only prompt for the title and keywords on file creation.
See `denote-prompts'.")
(defvar qk-git-commit-summary-max-length 50
  "Default: number of characters that the commit message should have.
See `git-commit-summary-max-length'.")
(defvar qk-git-commit-fill-column 72
  "Default: set fill column to this number if on `git-commit-mode'.")
(defvar qk-tab-bar-show nil
  "Default: hide tab-bar, though use it in the shadows. See `tab-bar-show'.")
(defvar qk-python-shell-interpreter "python3"
  "Default: command to use when running python. See `python-shell-interpreter'.")
(defvar qk-markdown-command "pandoc -t html5"
  "Default: command to use when compiling markdown to html.
See `markdown-command'.")
(defvar qk-mu4e-load-path "/opt/homebrew/Cellar/mu/1.8.10/share/emacs/site-lisp/mu/mu4e/"
  "Default: mu4e load path. See `mu4e-load-path'.")
(defvar qk-mu4e-maildir "~/.Mail"
  "Default: mail directory that mu4e will use to pull emails from.
See `mu4e-maildir'.")
(defvar qk-mu4e-attachment-dir "/tmp"
  "Default: temp directory for attaching and saving attachments. See `mu4e-attachment-dir'.")
(defvar qk-mu4e-get-mail-command "mbsync amazon"
  "Default: the mbsync command that you want to run on pull.
See `mu4e-get-mail-command'.")
(defvar qk-mu4e-compose-signature "Enrique Kessler Martínez\n"
  "Default: default signature to use when composing. See `mu4e-compose-signature'.")
(defvar qk-mu4e-update-interval (* 5 60)
  "Default: interval on which update mail. See `mu4e-update-interval'.")

(defvar qk-notificator-team-mu-query "to:kindle-notifications-dev@amazon.com")
(defvar qk-manager-mu-query "(from:josli@amazon.com OR from:josli@amazon.es)")
(defvar qk-to-me-mu-query "(to:enrikes@amazon.com OR to:enrikes@amazon.es)")
(defvar qk-unread-mu-query " AND g:unread AND NOT g:trashed")
(defvar qk-mu4e-maildir-shortcuts
  '((:maildir "/amazon/CRs" :key ?c)
    (:maildir "/amazon/Issues" :key ?i)
    (:maildir "/amazon/Pipelines" :key ?P)
    (:maildir "/Gmail/Work/Inbox" :key ?p)
    (:maildir "/amazon/Quip" :key ?q)
    (:maildir "/amazon/Asana" :key ?a))
  "Default: shorcuts for different maildirs. See `mu4e-maildir-shortcuts'.")
(defvar qk-mu4e-bookmarks
  `((:name "All Unread" :query "g:unread" :key ?u)
    (:name "Today's messages" :query "d:today..now" :key ?t)
    (:name "Direct to Me" :query ,(concat qk-to-me-mu-query qk-unread-mu-query) :key ?m)
    (:name "josli@" :query ,(concat qk-manager-mu-query qk-unread-mu-query) :key ?j)
    (:name "CRs" :query ,(concat "maildir:/amazon/CRs" qk-unread-mu-query) :key ?c)
    (:name "kindle-notifications-dev"
           :query ,(concat qk-notificator-team-mu-query qk-unread-mu-query)
           :key ?d)

    (:name "qkessler" :query ,(concat "maildir:/Gmail/Personal/Inbox" qk-unread-mu-query) :key ?q)
    (:name "enrique.kesslerm" :query ,(concat "maildir:/Gmail/Work/Inbox" qk-unread-mu-query) :key ?e))
  "Default: my own bookmarks for consuming email. See `mu4e-bookmarks'.")
(defvar qk-mu4e-contexts
  (list
   (make-mu4e-context
    :name "Amazon"
    :match-func
    (lambda (msg)
      (when msg
        (string-prefix-p "/amazon" (mu4e-message-field msg :maildir))))
    :vars '((user-mail-address . "enrikes@amazon.com")
            (user-full-name    . "Quique Kessler Martínez")
            (mu4e-drafts-folder  . "/amazon/Drafts")
            (mu4e-sent-folder  . "/amazon/Sent Items")
            (mu4e-refile-folder  . "/amazon/Archive")
            (mu4e-trash-folder  . "/amazon/Deleted Items")
            (smtpmail-smtp-user . "enrikes")
            (smtpmail-default-smtp-server . "exch-eu.amazon.com")
            (smtpmail-smtp-server . "exch-eu.amazon.com")
            (smtpmail-smtp-service . 1587)))
   (make-mu4e-context
    :name "Personal"
    :match-func
    (lambda (msg)
      (when msg
        (string-prefix-p "/Gmail/Personal" (mu4e-message-field msg :maildir))))
    :vars '((user-mail-address . "qkessler@gmail.com")
            (user-full-name    . "Enrique Kessler Martínez")
            (mu4e-drafts-folder  . "/Gmail/Personal/[Gmail]/Drafts")
            (mu4e-sent-folder  . "/Gmail/Personal/[Gmail]/Sent Mail")
            (mu4e-refile-folder  . "/Gmail/Personal/[Gmail]/All Mail")
            (mu4e-trash-folder  . "/Gmail/Personal/[Gmail]/Trash")
            (smtpmail-smtp-user . "qkessler@gmail.com")
            (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-service . 587)))
   (make-mu4e-context
    :name "Work"
    :match-func
    (lambda (msg)
      (when msg
        (string-prefix-p "/Gmail/Work" (mu4e-message-field msg :maildir))))
    :vars '((user-mail-address . "enrique.kesslerm@gmail.com")
            (user-full-name    . "Enrique Kessler Martínez")
            (mu4e-drafts-folder  . "/Gmail/Work/[Gmail]/Drafts")
            (mu4e-sent-folder  . "/Gmail/Work/[Gmail]/Sent Mail")
            (mu4e-refile-folder  . "/Gmail/Work/[Gmail]/All Mail")
            (mu4e-trash-folder  . "/Gmail/Work/[Gmail]/Trash")
            (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-user . "enrique.kesslerm@gmail.com")
            (smtpmail-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-service . 587))))
  "Default: my contexts when working with email. See `mu4e-contexts'.")
(defvar qk-sendmail-program "/opt/homebrew/bin/msmtp"
  "Default: use `msmtp' for queuing and sending email. See `sendmail-program'.")
(defvar qk-mu4e-alert-interesting-mail-query
  (concat "((to:enrikes@amazon.com OR to:enrikes@amazon.es) " qk-unread-mu-query ")"
          " OR "
          "((from:josli@amazon.com OR from:josli@amazon.es) " qk-unread-mu-query ")")
  "Default: mu search of email that could be interesting, that we want to count
for modeline highlighting. See `mu4e-alert-interesting-mail-query'.")
(defvar qk-vterm-max-scrollback 10000
  "Default: number of lines to have as scrollback. See `vterm-max-scrollback'.")
(defvar qk-vterm-timer-delay nil
  "Default: tweak this to your liking, to have vterm snappy but not too
 resource intensive. See `vterm-timer-delay'.")
(defvar qk-vterm-tab "vterm"
  "Default: vterm name for the tab to run vterm on.")
(defvar qk-ledger-reports
  '(("bal" "%(binary) -f %(ledger-file) bal --real")
    ("reg" "%(binary) -f %(ledger-file) reg")
    ("reg this month" "%(binary) -f %(ledger-file) reg -p \"this month\"")
    ("reg last month" "%(binary) -f %(ledger-file) reg -p \"last month\"")
    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
    ("account" "%(binary) -f %(ledger-file) reg %(account)")
    ("account this month" "%(binary) -f %(ledger-file) -p \"this month\" reg %(account)")
    ("account last month" "%(binary) -f %(ledger-file) -p \"last month\" reg %(account)")
    ("expenses this month" "%(binary) -p \"this month\" -f %(ledger-file) bal Expenses and not \
  \\(Expenses:Rebalancing or Expenses:Refundable or Expenses:Lent money\\)")
    ("expenses last month" "%(binary) -p \"last month\" -f %(ledger-file) bal Expenses and not \
  \\(Expenses:Rebalancing or Expenses:Refundable or Expenses:Lent money\\)")
    ("budget" "%(binary) -f %(ledger-file) -E bal ^Budget and not Budget:Checking")
    ("income last month" "%(binary) -f %(ledger-file) -p \"last month\" bal ^Income"))
  "Default: reports available when running `ledger-report'.")
(defvar qk-chatgpt-repo-path "~/.emacs.d/elpaca/repos/ChatGPT/"
  "Default: repo using the `elpaca' package manager. See `chatgpt-repo-path'.")

(provide 'qk-constants)
;; qk-constants.el ends here.
