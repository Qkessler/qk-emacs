;;; qk-mail.el --- Mail configuration using mu4e -*- lexical-binding: t; -*-

;; Adding mu4e configuration that was configured with mbsync.
;; We have installed it with the package manager, in order to make sure
;; that the mu4e version is in sync with the mu binary from my distro.
(add-to-list 'load-path "/usr/local/Cellar/mu/1.6.10/share/emacs/site-lisp/mu/mu4e")

(use-package mu4e
  :defer 2
  :general
  (major-mode-definer
    :major-modes '(mu4e-compose-mode)
    :keymaps '(mu4e-compose-mode-map) 
    "f" 'message-send-and-exit
    "c" 'message-dont-send
    "a" 'mail-add-attachment)
  (:keymaps
   '(mu4e-main-mode-map)
   "q" nil)
  (general-nmap
    :keymaps '(mu4e-main-mode-map)
    "q" 'quit-window)
  (+general-global-applications
    "m" 'mu4e)
  (general-vmap
    :keymaps '(mu4e-headers-mode-map)
    "!" 'mu4e-headers-mark-for-read)
  :hook (mu4e-compose-mode . flyspell-mode)
  :init
  (setq
   mu4e-maildir "~/.Mail"
   mu4e-attachment-dir "~/Downloads"
   mu4e-get-mail-command "mbsync amazon"
   mu4e-change-filenames-when-moving t
   mu4e-headers-show-threads nil
   mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
   mu4e-hide-index-messages t
   mu4e-compose-signature "Enrique Kessler Martínez\n"
   mu4e-update-interval (* 5 60)
   mu4e-compose-signature-auto-include t
   mu4e-confirm-quit nil
   mu4e-sent-messages-behavior 'sent
   mu4e-headers-auto-update t
   message-kill-buffer-on-exit t
   mu4e-headers-skip-duplicates t
   mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:from . 22)
     (:subject))
   mu4e-view-show-addresses t
   mu4e-display-update-status-in-modeline t
   mu4e-view-show-images nil
   mu4e-context-policy 'pick-first
   mu4e-compose-format-flowed t)
  :config
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (defvar org-agenda-archives-mode nil)
  (after! org-agenda
    (use-package mu4e-icalendar
      :demand t)
    (mu4e-icalendar-setup)
    (setq gnus-icalendar-org-capture-file (concat qk-notes-directory "/pages/meetings.org"))
    (setq gnus-icalendar-org-capture-headline '("Meetings"))
    (gnus-icalendar-org-setup))

  (defun qk-restart-mu ()
    "Run the restart-mu command to unblock mu4e's indexing and querying."
    (interactive)
    (shell-command-to-string "restart-mu"))
  (general-nmap
    :keymaps '(mu4e-main-mode-map)
    "q" 'quit-window))

;;; Maildir and Bookmark shortcuts

;; Here is the most important part of the config, as it defines the workflow that
;; I follow when I review the emails. Let's make a great note out of this, so when
;; I visit it again in the future I remember the flow that I have at each stage of
;; the config.

;; The maildir review is not that interesting, but I believe the bookmarks to be
;; the workflow zen mode. An equivalent in the corp world would be outlook rules.
;; In this case, the syntax is the same that you find for the `mu' find tool. Please,
;; refer to the mu-find man page for details on the bookmark queries.

;; Finally, I don't believe having a lot of folders in outlook is the way to go. The
;; supperior workflow would be to have a folder with all the info, turn off the notifications
;; and come back to emacs and setup the queries that would organize the email in a
;; review order that would let me classify and create actionable tasks for the mail that
;; I receive, folling the GTD workflow.

(defvar qk-notificator-team-mu-query "to:kindle-notifications-dev@amazon.com")
(defvar qk-manager-mu-query "(from:josli@amazon.com OR from:josli@amazon.es)")
(defvar qk-to-me-mu-query "(to:enrikes@amazon.com OR to:enrikes@amazon.es)")
(defvar qk-unread-mu-query " AND g:unread AND NOT g:trashed")

(use-package mu4e
  :init 
  (setq
   mu4e-maildir-shortcuts
   '( (:maildir "/amazon/CRs" :key ?c)
      (:maildir "/amazon/Issues" :key ?i)
      (:maildir "/amazon/Pipelines" :key ?P)
      (:maildir "/Gmail/Work/Inbox" :key ?p)
      (:maildir "/amazon/Quip" :key ?q)
      (:maildir "/amazon/Asana" :key ?a)
      )

   mu4e-bookmarks
   `(
     (:name "All Unread" :query "g:unread" :key ?u)
     (:name "Today's messages" :query "d:today..now" :key ?t)
     (:name "Direct to Me" :query ,(concat qk-to-me-mu-query qk-unread-mu-query) :key ?m)
     (:name "josli@" :query ,(concat qk-manager-mu-query qk-unread-mu-query) :key ?j)
     (:name "CRs" :query ,(concat "maildir:/amazon/CRs" qk-unread-mu-query) :key ?c)
     (:name "kindle-notifications-dev"
            :query ,(concat qk-notificator-team-mu-query qk-unread-mu-query)
            :key ?d)

     (:name "qkessler" :query ,(concat "maildir:/Gmail/Personal/Inbox" qk-unread-mu-query) :key ?q)
     (:name "enrique.kesslerm" :query ,(concat "maildir:/Gmail/Work/Inbox" qk-unread-mu-query) :key ?e))))

;;; Mail contexts

;; The contexts define a series of variables that are used building the templates
;; for the emails that I send. Once you are composing a message, you are asked for
;; the context you wish to use, and in every other situation the context is selected
;; for you.

(use-package mu4e-context
  :after mu4e
  :custom
  (mu4e-contexts
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
             (smtpmail-smtp-service . 587))))))

;;; Sending mail: the message package

;; For sending email I'm toying with async support for sending mail: that is
;; I would prefer if the send mail program wouldn't stop for the time it takes
;; to wait for the default mail sender.

;; The only benefit that I see in using msmtp (yes, we're using msmtp) is that
;; in case that the internet connection is not great, it queues the messages and
;; sends them all in batches when ready.

(use-package message
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (compose-mail-user-agent-warnings nil)
  (starttls-use-gnutls t)
  (message-mail-user-agent nil)    ; default is `gnus'
  (message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-wide-reply-confirm-recipients t)
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (message-default-charset 'utf-8)
  :config (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)))

;; If you don't want to use msmtp anymore, remove this config.

(use-package message
  :custom
  (sendmail-program "/usr/local/bin/msmtp")
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'message-send-mail-with-sendmail))

;;; Alerts for incoming mail: mu4e-alert

;; The alert package gives me the alert when in any other buffer and not looking
;; email. I believe this is a great way to surface any really important email,
;; but avoid needing to look at email all the time.

;; The variable to change is the `mu4e-alert-interesting-mail-query' variable,
;; just add a new line with "OR" and finally the line with the query you are
;; interested in. 

;; In my case, I highlight any mail that is sent to my email directly, any
;; email from my boss and any mail that requires inmediate attention or mail
;; that I'm interested in. For the latter, I still need to define a clear set
;; of rules for reducing the amount of unneded email that I mark as read everyday. 

(use-package mu4e-alert
  :straight t
  :defer 5
  :hook (after-init . mu4e-alert-enable-mode-line-display)
  :custom
  (mu4e-alert-interesting-mail-query
   (concat "((to:enrikes@amazon.com OR to:enrikes@amazon.es) AND g:unread AND NOT g:trashed)"
           " OR "
           "(g:unread AND g:trashed AND (from:josli@amazon.com OR from:josli@amazon.es))"
           ;; " OR "
           ;; "g:uNread maildir:/UMU/Inbox to:enrique.kesslerm@um.es"
           )))

(provide 'qk-mail)
;;; qk-mail.el ends here.
