;;; qk-mail.el --- Mail configuration using mu4e -*- lexical-binding: t; -*-

;; Adding mu4e configuration that was configured with mbsync.
;; We have installed it with the package manager, in order to make sure
;; that the mu4e version is in sync with the mu binary from my distro.
(add-to-list 'load-path qk-mu4e-load-path)
(use-package mu4e
  :hook (mu4e-compose-mode . flyspell-mode)
  :commands mu4e mu4e-headers-search
  :init
  (setq
   mu4e-maildir qk-mu4e-maildir
   mu4e-attachment-dir qk-mu4e-attachment-dir
   mu4e-get-mail-command qk-mu4e-get-mail-command
   mu4e-change-filenames-when-moving t
   mu4e-headers-show-threads nil
   mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
   mu4e-hide-index-messages t
   mu4e-compose-signature qk-mu4e-compose-signature
   mu4e-update-interval qk-mu4e-update-interval
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
   mu4e-view-show-images t
   mu4e-context-policy 'pick-first
   mu4e-compose-format-flowed t
   )
  :config
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (defvar org-agenda-archives-mode nil)
  (after! org-agenda
    (use-package mu4e-icalendar
      :demand t)
    (mu4e-icalendar-setup)
    (setq gnus-icalendar-org-capture-file (concat qk-notes-directory "meetings.org"))
    (setq gnus-icalendar-org-capture-headline '("Meetings"))
    (gnus-icalendar-org-setup)))

(after! general
  (+general-global-applications
    "m" 'mu4e
    "s" 'mu4e-headers-search))

(after! mu4e
  (add-hook! mu4e-main-mode
    (general-nmap
      :keymaps 'mu4e-main-mode-map
      "q" 'quit-window))
  (major-mode-definer
    :major-modes '(mu4e-compose-mode)
    :keymaps '(mu4e-compose-mode-map) 
    "f" 'message-send-and-exit
    "c" 'message-dont-send
    "a" 'mail-add-attachment)
  (general-nmap
    :keymaps '(mu4e-view-mode-map)
    "F" 'mu4e-compose-forward)
  (general-vmap
    :keymaps '(mu4e-headers-mode-map)
    "!" 'mu4e-headers-mark-for-read))

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

(use-package mu4e
  :init 
  (setq
   mu4e-maildir-shortcuts qk-mu4e-maildir-shortcuts
   mu4e-bookmarks qk-mu4e-bookmarks))

;;; Mail contexts

;; The contexts define a series of variables that are used building the templates
;; for the emails that I send. Once you are composing a message, you are asked for
;; the context you wish to use, and in every other situation the context is selected
;; for you.

(use-package mu4e-context
  :after mu4e
  :config (setq mu4e-contexts qk-mu4e-contexts))

;;; Sending mail: the message package

;; For sending email I'm toying with async support for sending mail: that is
;; I would prefer if the send mail program wouldn't stop for the time it takes
;; to wait for the default mail sender.

;; The only benefit that I see in using msmtp (yes, we're using msmtp) is that
;; in case that the internet connection is not great, it queues the messages and
;; sends them all in batches when ready.

(use-package message
  :init
  (setq
   compose-mail-user-agent-warnings nil
   starttls-use-gnutls t
   message-mail-user-agent nil    ; default is `gnus'
   message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n"
   message-citation-line-function 'message-insert-formatted-citation-line
   message-wide-reply-confirm-recipients t
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   message-default-charset 'utf-8)
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)))

;; If you don't want to use msmtp anymore, remove this config.

(use-package message
  :init
  (setq
   sendmail-program qk-sendmail-program
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'message-send-mail-with-sendmail))

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

(elpaca-use-package mu4e-alert
  :hook (mu4e-main-mode . mu4e-alert-enable-mode-line-display)
  :init (setq mu4e-alert-interesting-mail-query qk-mu4e-alert-interesting-mail-query))

;;; Org-mime

;; Send messages in org-mode and html format.

(elpaca-use-package org-mime
  :hook (message-send . org-mime-htmlize)
  :init (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

(provide 'qk-mail)
;;; qk-mail.el ends here.
