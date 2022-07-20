;;; qk-org-roam.el -*- lexical-binding: t; -*-

;; I use the Zettelkasten (slip-box) method for taking and recalling notes and
;; information.
(use-package org-roam
  :straight t
  :commands org-roam-db-query
  :init
  (setq
   org-roam-directory "~/Documents/slipbox/"
   qk-notes-directory org-roam-directory
   org-roam-v2-ack t
   org-roam-node-display-template "${title}                                          ${tags}"
   org-roam-capture-templates `(("d" "default" plain "%?" :if-new
                                 (file+head
                                  "pages/${slug}.org"
                                  ,(concat
                                    ":PROPERTIES:\n:ID: "
                                    "%(org-id-uuid)\n:END:\n"
                                    "#+title: ${title}\n"
                                    "#+filetags: \n\n"))
                                 :unnarrowed t)))
  (defvar org-book-list-file (concat org-roam-directory "/pages/book_list.org"))
  :general
  (+general-global-notes
    "f" 'org-roam-node-find
    "i" 'org-roam-node-insert
    "I" 'org-roam-node-insert-immediate
    "t" 'org-roam-buffer-toggle
    "p" 'qk-org-roam-find-project)
  :config
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (defun qk-org-roam-find-project ()
    "Create a project note with a PROJECT header, for the NabyiNotificator team."
    (interactive)
    (org-roam-node-find nil nil
                        (lambda (node)
                          (member "project" (org-roam-node-tags node)))
                        nil
                        :templates `(("d" "default" plain "%?" :if-new
                                      (file+head
                                       "pages/nabyinotificator_${slug}.org"
                                       ,(concat
                                         ":PROPERTIES:\n"
                                         ":ID: %(org-id-uuid)\n"
                                         ":END:\n"
                                         "#+title: NabyiNotificator - ${title}\n"
                                         "#+filetags: \n\n"
                                         "* PROJECT ${title} :@work:PROJECT:\n"))
                                      :unnarrowed t))))

  (org-roam-db-autosync-enable))

(after! org
  ;; The V2 version of org-roam adds properties to the files, which are then
  ;; used to query and index the files. Using this org-mode native sintax
  ;; (instead of buffer properties), a higher performance and consistency is
  ;; achieved.
  (defun sc-org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun sc-org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun sc-org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (sc-org-show-properties)
      (sc-org-hide-properties)))

  (add-hook 'org-roam-find-file-hook #'sc-org-hide-properties))

;; The big problem this is solving is that org-agenda takes ages to load if the node
;; count goes up. For that, we use clever techniques and advices to check on save and
;; on load for a file, allowing to use per-note TODOs and fast query for the org-agenda buffer.
;; This has been taken out of https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(after! org-agenda
  (use-package vulpea
    :straight t
    :commands vulpea-buffer-tags-get vulpea-buffer-tags-add)

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))
          (unless (eq original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
  (add-hook! (find-file before-save) 'vulpea-project-update-tag))

(provide 'qk-org-roam)
;; qk-org-roam.el ends here.
