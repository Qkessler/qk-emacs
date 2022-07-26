;;; qk-denote.el -*- lexical-binding: t; -*-

(defvar qk-local-repo (expand-file-name "~/.emacs.d/local/"))
(pushnew! load-path qk-local-repo)
(use-package denote
  :straight `(denote :local-repo ,(concat qk-local-repo "denote"))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :init
  (setq
   denote-directory (expand-file-name "~/Documents/slipbox/pages/")
   qk-notes-directory denote-directory
   org-default-notes-file (concat qk-notes-directory "refile.org")
   denote-known-keywords '()
   denote-prompts '(title keywords)
   denote-allow-multi-word-keywords t
   denote-date-format nil
   denote-link-fontify-backlinks t
   denote-dired-rename-expert nil)
  :general
  (+general-global-notes
    "f" 'qk-denote-find-notes
    "i" 'denote-link
    "d" 'qk-denote-find-dailies)
  (+general-global-org
    "a" 'qk-denote-open-agenda)
  :config
  (defun qk-denote-find-dailies ()
    "Find daily notes in the current `qk-notes-dailies-directory'."
    (interactive)
    (with-current-directory! qk-notes-dailies-directory (call-interactively 'find-file))
    (cd qk-notes-dailies-directory))

  (defun qk-denote-find-notes ()
    "Find notes in the current `denote-directory'."
    (interactive)
    (with-current-directory! denote-directory (call-interactively 'find-file))
    (cd denote-directory))

  (defun qk-denote--get-projects-process-events-for-agenda (process event)
    "Process the events for the rg program getting the `project' tagged files,
 for building the agenda."
    (cond ((string= event "finished\n")
           (qk-denote--get-projects-set-agenda)
           (after! qk-org-agenda
             (call-interactively 'qk-silently-open-agenda)))
          ((string= event "exited abnormally with code 1\n")
           (message "qk-denote: rg didn't find any files."))
          ((string= event "exited abnormally with code 2\n")
           (message
            (format "qk-denote: error. Check the %s"
                    qk-denote-get-projects-buffer)))))

  (defun qk-denote-open-agenda ()
    "Silently open agenda after"
    (interactive)
    (qk-denote--get-projects #'qk-denote--get-projects-process-events-for-agenda)))

(use-package denote-org-capture
  :init
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  :general
  (+general-global-notes
    "c" 'qk-denote-org-capture)
  :config
  (defvar qk-denote-capture-template
    '(("n" "New note (with denote.el)" plain
        (file denote-last-path)
        #'denote-org-capture
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t)))

  ;; I wonder whether this affects performance. This :config block is only run
  ;; when the `qk-denote-org-capture' is computed, and that's kind of my intention.
  (require 'org-capture)

  (defun qk-denote-org-capture ()
    "Org-capture with only the denote template."
    (interactive)
    (let ((org-capture-templates qk-denote-capture-template))
      (org-capture nil "n"))))

(defvar qk-denote-get-projects-name "qk-denote-get-projects")
(defvar qk-denote-get-projects-buffer "*qk-denote-get-projects*")
(defvar qk-denote-get-projects-pattern "\\+filetags: .*project")
(defvar qk-denote-get-projects--lock t)
(defun qk-denote--get-projects (process-function &rest _)
  "Run `rg' process to get the projects that have the file tag."
  (when qk-denote-get-projects--lock
    (setq qk-denote-get-projects--lock nil)
    (set-process-sentinel
     (start-process
      qk-denote-get-projects-name
      qk-denote-get-projects-buffer
      qk-rg-command "-l" qk-denote-get-projects-pattern denote-directory)
     process-function)))

(defun qk-denote--get-projects-process-events (process event)
  "Process the events for the rg program getting the `project' tagged files."
  (cond ((string= event "finished\n") (qk-denote--get-projects-set-agenda))
        ((string= event "exited abnormally with code 1\n")
         (message "qk-denote: rg didn't find any files."))
        ((string= event "exited abnormally with code 2\n")
         (message
          (format "qk-denote: error. Check the %s"
                  qk-denote-get-projects-buffer)))))

(defun qk-denote--get-projects-cleanup ()
  "Cleanup the buffer that was created for the async process."
  (kill-buffer qk-denote-get-projects-buffer)
  (setq qk-denote-get-projects--lock t))

(defun qk-denote--get-projects-set-agenda ()
  "Return the parsed project tagged files list.
Consumes the buffer and takes the \n splitted paths to make the list. "
  (let ((project-list
         (with-current-buffer qk-denote-get-projects-buffer
           (s-lines (buffer-string)))))
    (qk-denote--get-projects-cleanup)
    (setq org-agenda-files project-list)))

(defvar qk-notes-dailies-directory (expand-file-name (concat qk-notes-directory "../dailies/")))
(defun qk-denote--dailies-project-p ()
  "Return `t' if the current heading has the PROJECT tag."
  (let* ((element (org-element-at-point))
         (tags (org-element-property :tags element)))
    (member "PROJECT" tags)))

(defun qk-denote--dailies-archive ()
  "If moved to DONE state, move to the daily note for the day."
  (when (and (string= org-state "DONE") (not (qk-denote--dailies-project-p)))
    (let ((org-archive-location
           (concat qk-notes-dailies-directory (format-time-string "%F") "::")))
      (when (org-get-repeat)
        (setq current-prefix-arg '(4))
        (org-clone-subtree-with-time-shift 1 nil))
      (org-archive-subtree))))
(add-hook! org-after-todo-state-change 'qk-denote--dailies-archive)

(defvar qk-denote--rename-file-refile (concat qk-notes-directory "refile.org"))
(defvar qk-denote--rename-file-block-list `(,qk-denote--rename-file-refile))
(defun qk-denote--rename-file-on-tags-change ()
  "If the filetags property on the file changes, rename the current
file following `denote''s title best practices, to contain the new filetags."
  (when (and (not (bound-and-true-p org-capture-mode))
             (not (-contains-p qk-denote--rename-file-block-list (buffer-file-name)))
             (s-contains-p denote-directory (file-name-directory (buffer-file-name))))
    (let* ((file-name (file-name-nondirectory buffer-file-name))
           (directory (file-name-directory buffer-file-name))
           (_ (string-match "\\(.*__\\)\\(.*\\)\\(\\..*\\)" file-name))
           (name-without-tags (match-string 1 file-name))
           (name-tags (match-string 2 file-name))
           (extension (match-string 3 file-name))
           (current-tags (string-join (vulpea-buffer-tags-get) "_"))
           (new-name (concat name-without-tags current-tags extension)))
      (unless (string= name-tags current-tags)
        (rename-file (buffer-file-name) new-name t)
        (set-visited-file-name new-name t t)))))
(add-hook! before-save 'qk-denote--rename-file-on-tags-change)

(defvar qk-denote--migrate-blocklisted `(,(concat denote-directory ".DS_Store")))
(defun qk-denote--migrate ()
  "Visit and open all files in a directory."
  (cl-loop for file in (directory-files denote-directory) collect
           (unless (or
                    (member (concat denote-directory file) qk-denote--migrate-blocklisted)
                    (file-directory-p file))
             (find-file (concat denote-directory file)))))

(after! org-agenda
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

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory qk-notes-directory))
          (file-name-directory buffer-file-name))))
  (add-hook! (find-file before-save) 'vulpea-project-update-tag))

(after! org-refile
  (defun org-refile-get-targets (&optional default-buffer)
    "Produce a table with refile targets."
    (let ((case-fold-search nil)
	      ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
	      (entries (or org-refile-targets '((nil . (:level . 1)))))
	      targets tgs files desc descre)
      (message "Getting targets...")
      (with-current-buffer (or default-buffer (current-buffer))
        (dolist (entry entries)
	      (setq files (car entry) desc (cdr entry))
	      (cond
	       ((null files) (setq files (list (current-buffer))))
	       ((eq files 'org-agenda-files)
	        (setq files org-agenda-files))
	       ((and (symbolp files) (fboundp files))
	        (setq files (funcall files)))
	       ((and (symbolp files) (boundp files))
	        (setq files (symbol-value files))))
	      (when (stringp files) (setq files (list files)))
	      (cond
	       ((eq (car desc) :tag)
	        (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
	       ((eq (car desc) :todo)
	        (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
	       ((eq (car desc) :regexp)
	        (setq descre (cdr desc)))
	       ((eq (car desc) :level)
	        (setq descre (concat "^\\*\\{" (number-to-string
					                        (if org-odd-levels-only
					                            (1- (* 2 (cdr desc)))
					                          (cdr desc)))
			                     "\\}[ \t]")))
	       ((eq (car desc) :maxlevel)
	        (setq descre (concat "^\\*\\{1," (number-to-string
					                          (if org-odd-levels-only
						                          (1- (* 2 (cdr desc)))
					                            (cdr desc)))
			                     "\\}[ \t]")))
	       (t (error "Bad refiling target description %s" desc)))
	      (dolist (f files)
	        (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
	          (or
	           (setq tgs (org-refile-cache-get (buffer-file-name) descre))
	           (progn
	             (when (bufferp f)
		           (setq f (buffer-file-name (buffer-base-buffer f))))
	             (setq f (and f (expand-file-name f)))
	             (when (eq org-refile-use-outline-path 'file)
		           (push (list (and f (file-name-nondirectory f)) f nil nil) tgs))
	             (when (eq org-refile-use-outline-path 'buffer-name)
		           (push (list (buffer-name (buffer-base-buffer)) f nil nil) tgs))
	             (when (eq org-refile-use-outline-path 'full-file-path)
		           (push (list (and (buffer-file-name (buffer-base-buffer))
                                    (file-truename (buffer-file-name (buffer-base-buffer))))
                               f nil nil) tgs))
	             (org-with-wide-buffer
		          (goto-char (point-min))
		          (setq org-outline-path-cache nil)
		          (while (re-search-forward descre nil t)
		            (beginning-of-line)
		            (let ((case-fold-search nil))
		              (looking-at org-complex-heading-regexp))
		            (let ((begin (point))
			              (heading (match-string-no-properties 4)))
		              (unless (or (and
				                   org-refile-target-verify-function
				                   (not
				                    (funcall org-refile-target-verify-function)))
				                  (not heading))
		                (let ((re (format org-complex-heading-regexp-format
					                      (regexp-quote heading)))
			                  (target
			                   (if (not org-refile-use-outline-path) heading
			                     (mapconcat
				                  #'identity
				                  (append
				                   (pcase org-refile-use-outline-path
				                     (`file (list
                                             (and (buffer-file-name (buffer-base-buffer))
                                                  (file-name-nondirectory
                                                   (buffer-file-name (buffer-base-buffer))))))
				                     (`full-file-path
				                      (list (buffer-file-name
					                         (buffer-base-buffer))))
				                     (`buffer-name
				                      (list (buffer-name
					                         (buffer-base-buffer))))
				                     (_ nil))
				                   (mapcar (lambda (s) (replace-regexp-in-string
						                                "/" "\\/" s nil t))
					                       (org-get-outline-path t t)))
				                  "/"))))
			              (push (list target f re (org-refile-marker (point)))
			                    tgs)))
		              (when (= (point) begin)
		                ;; Verification function has not moved point.
		                (end-of-line)))))))
	          (when org-refile-use-cache
	            (org-refile-cache-put tgs (buffer-file-name) descre))
	          (setq targets (append tgs targets))))))
      (message "Getting targets...done")
      (delete-dups (nreverse targets)))))

(provide 'qk-denote)
;; qk-denote.el ends here.
