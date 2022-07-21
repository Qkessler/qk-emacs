;;; qk-org-agenda.el -*- lexical-binding: t; -*-

(use-package org-agenda
  :init
  (setq
   org-agenda-prefix-format "  %?-12t% s"
   org-agenda-archives-mode nil
   org-agenda-skip-comment-trees nil
   org-agenda-skip-function nil
   org-agenda-hide-tags-regexp ".*"
   org-roam-v2-ack t
   org-deadline-warning-days 3
   org-agenda-dim-blocked-tasks nil
   org-agenda-inhibit-startup t
   org-agenda-use-tag-inheritance nil
   org-agenda-ignore-drawer-properties '(effort appt category)
   org-agenda-block-separator ?—
   org-agenda-custom-commands
   '(("d" "Agenda"
      ((agenda ""
               ((org-agenda-overriding-header "Today's Schedule:")
                (org-agenda-span 'day)
                (org-agenda-ndays 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+0d")
                (org-agenda-skip-function
                 '(cond ((equal (file-name-nondirectory (buffer-file-name)) "refile.org")
                         (outline-next-heading)
                         (1- (point)))
                        (t (org-agenda-skip-entry-if 'todo 'done))))
                (org-agenda-todo-ignore-deadlines nil)))
       (todo "PROJECT"
             ((org-agenda-overriding-header "Project list:")
              (org-tags-match-list-sublevels nil)))
       (tags "REFILING"
             ((org-agenda-overriding-header "Tasks to Refile:")
              (org-tags-match-list-sublevels nil)))
       (todo "TODO"
             ((org-agenda-overriding-header "Unscheduled Tasks:")
              (org-tags-match-list-sublevels nil)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
       (todo "WAITING|SOMEDAY"
             ((org-agenda-overriding-header "Waiting/Someday Tasks:")
              (org-tags-match-list-sublevels nil)))
       (todo "NOTE"
             ((org-agenda-overriding-header "Notes:")
              (org-tags-match-list-sublevels nil)))
       (agenda ""
               ((org-agenda-overriding-header "Upcoming:")
                (org-agenda-span 7)
                (org-agenda-start-day "+1d")
                (org-agenda-start-on-weekday nil)
                (org-agenda-skip-function
                 '(cond
                   ((equal (file-name-nondirectory (buffer-file-name)) "refile.org")
                    (outline-next-heading) (1- (point)))
                   (t (org-agenda-skip-entry-if 'todo 'done))))
                (org-agenda-todo-ignore-deadlines nil))))))
   org-agenda-skip-scheduled-if-deadline-is-shown t)
  :general
  (+general-global-org
    "a" '(qk-silently-open-agenda :which-key "project agenda")
    "t" '(qk-silently-open-todo-agenda :which-key "day/week agenda")
    "c" 'org-capture
    "s" 'org-save-all-org-buffers)
  :config
  (defun qk-silently-open-agenda ()
    "Using the `with-silent-modifications' macro, open the agenda on the 'd' view
which is the one that contains all the projects I follow."
    (interactive)
    (with-silent-modifications (org-agenda nil "d")))

  (defun qk-silently-open-todo-agenda ()
    "Using the `with-silent-modifications' macro, open the agenda on the 'a' view
which is the one that contains the todos for the day/week."
    (interactive)
    (with-silent-modifications (org-agenda nil "a"))))

(provide 'qk-org-agenda)
;; qk-org-agenda.el ends here.
