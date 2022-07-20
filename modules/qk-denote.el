;;; qk-denote.el -*- lexical-binding: t; -*-

(defvar qk-local-repo (expand-file-name "~/.emacs.d/local/"))
(pushnew! load-path qk-local-repo)
(use-package denote
  :straight `(denote :local-repo ,(concat qk-local-repo "denote"))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :init
  (setq
   denote-directory (expand-file-name "~/Documents/testnotes/")
   qk-notes-directory denote-directory
   denote-known-keywords '()
   denote-prompts '(title)
   denote-allow-multi-word-keywords t
   denote-date-format nil
   denote-link-fontify-backlinks t
   denote-dired-rename-expert nil)
  :general
  (+general-global-notes
    "f" 'qk-denote-find-notes
    "i" 'denote-link
    "r" 'denote-dired-rename-file-and-rewrite-front-matter)
  :config
  (defun qk-denote-find-notes ()
    "Use `affe-find' for searching the `denote-directory'."
    (interactive)
    (affe-find denote-directory)))

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

(defconst qk-denote-get-projects-name "qk-denote-get-projects")
(defconst qk-denote-get-projects-buffer "*qk-denote-get-projects*")
(defconst qk-denote-get-projects-pattern "\\+filetags: .*project")
(setq org-roam-directory (expand-file-name org-roam-directory))

(defun qk-denote--get-projects (&rest _)
  "Run `rg' process to get the projects that have the file tag."
  (set-process-sentinel
   (start-process
    qk-denote-get-projects-name
    qk-denote-get-projects-buffer
    qk-rg-command "-l" qk-denote-get-projects-pattern org-roam-directory)
   #'qk-denote--get-projects-process-events))

(defun qk-denote--get-projects-process-events (process event)
  "Process the events for the rg program getting the `project' tagged files."
  (cond ((string= event "finished\n") (qk-denote--get-projects-rg))
        ((string= event "exited abnormally with code 1\n")
         (message "qk-denote: rg didn't find any files."))
        ((string= event "exited abnormally with code 2\n") (message "error"))))

(defun qk-denote--get-projects-cleanup ()
  "Cleanup the buffer that was created for the async process."
  (kill-buffer qk-denote-get-projects-buffer))

(defun qk-denote--get-projects-rg ()
  "Return the parsed project tagged files list.
Consumes the buffer and takes the \n splitted paths to make the list. "
  (let ((project-list
         (with-current-buffer qk-denote-get-projects-buffer
           (s-lines (buffer-string)))))
    (progn
      (qk-denote--get-projects-cleanup)
      (setq org-agenda-files project-list))))

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

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory qk-notes-directory))
          (file-name-directory buffer-file-name))))
  (add-hook! org-agenda-finalize 'qk-denote--get-projects)
  (add-hook! (find-file before-save) 'vulpea-project-update-tag))

(provide 'qk-denote)
;; qk-denote.el ends here.
