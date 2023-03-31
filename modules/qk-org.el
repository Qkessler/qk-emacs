;;; qk-org.el -*- lexical-binding: t; -*-

(use-package org
  :hook
  (org-clock-in-hook . org-save-all-org-buffers)
  (org-clock-out-hook . org-save-all-org-buffers)
  (org-mode . visual-line-mode)
  (org-mode . bg-org-fill-paragraph-config)
  (org-after-refile-insert . qk-org-clean-tags)
  (org-cycle-tab-first . 
                       (lambda ()
                         (when (org-in-src-block-p t)
                           (let* ((elt (org-element-at-point))
                                  (lang (intern (org-element-property :language elt)))
                                  (langs org-babel-load-languages))
                             (unless (alist-get lang langs)
                               (indent-to 4))))))
  :init
  (setq
   org-return-follows-link t
   org-default-notes-file qk-org-default-notes-file
   org-archive-location qk-org-archive-location
   org-src-fontify-natively t
   org-columns-default-format "%50ITEM(Task) %10Effort{:} %10CLOCKSUM"
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-agenda-restore-windows-after-quit t
   org-clock-persistence-insinuate t
   org-clock-persist t
   org-clock-in-resume t
   org-hide-leading-stars t
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-image-actual-width nil
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-enforce-todo-dependencies t
   org-confirm-babel-evaluate nil
   org-log-repeat nil
   org-modules '()
   org-refile-targets (quote ((org-agenda-files :todo . "PROJECT")))
   org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DONE(d)")
           (sequence "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)")
           (sequence "WAITING(w)" "|")
           (sequence "REVIEW (r)" "|")
           (sequence "|" "CANCELLED(c)")
           (sequence "|" "OPTIONAL(o)")
           (sequence "SOMEDAY(s)" "|" "CANCELLED(c)")
           (sequence "MEETING(m)" "|" "DONE(d)")
           (sequence "NOTE(n)" "|" "DONE(d)")))
   org-todo-keyword-faces
   '(("PROJECT" . (:foreground "#a87600" :weight bold))
     ("OPTIONAL" . (:foreground "#08a838" :weight bold))
     ("WAITING" . (:foreground "#fe2f92" :weight bold))
     ("CANCELLED" . (:foreground "#999999" :weight bold))
     ("SOMEDAY" . (:foreground "#ab82ff" :weight bold))
     ("MEETING" . (:foreground "#81A1C1" :weight bold))
     ("NOTE" . (:foreground "#fcba03" :weight bold))))
  (pushnew! warning-suppress-types '(org-element-cache))
  :config
  (defun ar-org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (read-string
                       "title: "
                       (with-current-buffer
                           (url-retrieve-synchronously clipboard-url)
                         (dom-text (car
                                    (dom-by-tag (libxml-parse-html-region
                                                 (point-min)
                                                 (point-max))
                                                'title))))))))
            (t (call-interactively 'org-insert-link)))))

  (defun bg-org-fill-paragraph-with-link-nobreak-p ()
    "Do not allow `fill-paragraph' to break inside the middle of Org mode links."
    (and (assq :link (org-context)) t))

  (defun bg-org-fill-paragraph-config ()
    "Configure `fill-paragraph' for Org mode."
    ;; Append a function to fill-nobreak-predicate similarly to how org-mode does
    ;; inside `org-setup-filling':
    (when (boundp 'fill-nobreak-predicate)
      (setq-local
       fill-nobreak-predicate
       (org-uniquify
        (append fill-nobreak-predicate
                '(bg-org-fill-paragraph-with-link-nobreak-p))))))

  (let* ((headline           `(:inherit default :weight bold)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ))))
     `(org-level-7 ((t (,@headline ))))
     `(org-level-6 ((t (,@headline ))))
     `(org-level-5 ((t (,@headline ))))
     `(org-level-4 ((t (,@headline  :height 1.1))))
     `(org-level-3 ((t (,@headline  :height 1.15))))
     `(org-level-2 ((t (,@headline  :height 1.25))))
     `(org-level-1 ((t (,@headline  :height 1.5))))
     `(org-document-title ((t (,@headline  :height 1.75 :underline nil))))))

  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (defun qk-org-remove-inherited-tag-strings ()
    "Removes inherited tags from the headline-at-point's tag string.
Note this does not change the inherited tags for a headline, just the tag string."
    (org-set-tags (seq-remove
                   (lambda (tag) (get-text-property 0 'inherited tag))
                   (org-get-tags))))

  (defun qk-org-remove-refiling-tag ()
    "Remove the REFILING tag once the item has been refiled."
    (org-toggle-tag "REFILING" 'off))

  (defun qk-org-clean-tags ()
    "Visit last refiled headline and remove inherited tags from tag string."
    (save-window-excursion
      (org-refile-goto-last-stored)
      (qk-org-remove-inherited-tag-strings)
      (qk-org-remove-refiling-tag))))

(use-package org-capture
  :hook
  (org-capture-after-finalize . org-save-all-org-buffers)
  (org-capture-mode . (lambda ()
                        (let* ((headline `(:inherit default :weight bold)))
                          (face-remap-add-relative
                           'org-level-1 '(,@headline)))))
  :init
  (setq
   org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           (done ("WAITING") ("CANCELLED"))
           ("DONE" ("WAITING") ("CANCELLED"))
           ("TODO" ("WAITING") ("CANCELLED"))))
   org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Refile items")
      "* TODO %? :REFILING:\n%a\n")
     ("m" "Meeting/Interruption" entry (file+headline org-default-notes-file "Refile items")
      "* MEETING %? :REFILING:MEETING:\n")
     ("i" "Idea" entry (file+headline org-default-notes-file "Refile items")
      "* %? :REFILING:IDEA:\n")
     ("s" "Someday" entry (file+headline org-default-notes-file "Refile items")
      "* SOMEDAY %? :REFILING:SOMEDAY:\n")
     ("p" "Project creation: @work or @home")
     ("pw" "@work Project entry" entry (file+headline org-default-notes-file "Refile items")
      "* PROJECT %? :@work:REFILING:PROJECT:\n")
     ("ph" "@home Project entry" entry (file+headline org-default-notes-file "Refile items")
      "* PROJECT %? :@home:REFILING:PROJECT:\n")
     ("b" "Book" entry (file org-book-list-file)
      "* %^{TITLE}\n:PROPERTIES:\n:ADDED: %<[%Y-%02m-%02d]>\n:END:%^{AUTHOR}p\n%^{URL}p\n"))))

(after! org
  (defun org-md-example-block (example-block _contents info)
    "Transcode EXAMPLE-BLOCK element into Markdown format.
  CONTENTS is nil.  INFO is a plist used as a communication
  channel."
    (concat "```\n"
            (org-remove-indentation
             (org-export-format-code-default example-block info))
            "```"))
  (add-to-list 'org-export-backends 'md)

  (defun qk-org-capture-here ()
    "Org-capture in the current buffer, passing the 0 prefix
to the org-capture function."
    (interactive)
    (setq current-prefix-arg 0) ; C-0
    (call-interactively 'org-capture))

  (defun qk-schedule-for-now ()
    "Add an org timestamp for the current date time."
    (interactive)
    (org-schedule nil (format-time-string "%F %R")))

  (major-mode-definer
    :major-modes '(org-mode)
    :keymaps '(org-mode-map)
    "h" 'qk-org-capture-here)
  (minor-mode-definer
    :keymaps 'org-capture-mode
    "f" 'org-capture-finalize
    "c" '(org-capture-kill :which-key "org-capture cancel")
    "r" 'org-capture-refile
    "p" 'org-priority
    "n" 'qk-schedule-for-now)
  (major-mode-definer
    :major-modes '(org-mode)
    :keymaps '(org-mode-map)
    "c" '(org-edit-special :which-key "open source block")
    "d" 'org-deadline
    "e" 'org-export-dispatch
    "l" 'ar-org-insert-link-dwim
    "s" 'org-schedule
    "t" 'org-todo
    "r" 'org-refile
    "f" 'org-fold-hide-sublevels
    "n" 'outline-next-heading
    "p" 'outline-previous-heading)
  (general-nmap
    :keymaps '(org-mode-map)
    "gx" 'org-return)
  (minor-mode-definer
    :keymaps 'org-src-mode
    "f" 'org-edit-src-exit
    "c" 'org-edit-src-abort)
  (+general-global-org
    "l" 'org-store-link
    "n" 'qk-schedule-for-now)
  (+general-global-insert
    "d" (cmd! (forward-char) (insert (format-time-string "%F")))))

(use-package org-appear :elpaca t
  :hook (org-mode . org-appear-mode)
  :init
  (setq
   org-appear-trigger 'always
   org-appear-autolinks t))

(provide 'qk-org)
;; qk-org.el ends here.
