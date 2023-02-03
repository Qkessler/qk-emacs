;;; qk-dired.el -*- lexical-binding: t; -*-

;; Commentary
;;
;;; The idea of this module is to have dired also work like a treemacs alternative,
;;; to still have the project exploration without all the bells and whistles,
;;; that I don't need.
;;;
;;; Requirements:
;;; - I want to be able to peak to files when I have them open.
;;; - I want to have icons on the files.
;;; - I want to be able to see more info about the files on a keypress.
;;; - I want to toggle the directories with <tab>.
;;; - I want to have a keybinding to open the project's dired tree dired buffer.

(use-package dired
  :init 
  (setq
   dired-listing-switches qk-dired-listing-switches
   dired-use-ls-dired nil
   dired-kill-when-opening-new-dired-buffer t)
  :hook (dired-mode . dired-hide-details-mode))

(after! general
  (defun qk-dired-preview-other-window ()
    "Preview the file at point on the other-window, but keep the cursor at the current dired window."
    (interactive)
    (let ((previous-window (frame-selected-window)))
      (dired-find-file-other-window)
      (select-window previous-window))))

(after! dired
  (add-hook! dired-mode (general-nmap
                          :keymaps '(dired-mode-map)
                          "o" 'qk-dired-preview-other-window)))

(elpaca-use-package dired-subtree
  :after dired
  :init (setq dired-subtree-use-backgrounds nil)
  :general 
  (:keymaps '(dired-mode-map)
   "<tab>"  'dired-subtree-toggle
   "<C-tab>" 'dired-subtree-cycle
   "<backtab>" 'dired-subtree-remove))

(elpaca-use-package all-the-icons-dired
  :if qk-dired-all-the-icons
  :init (setq all-the-icons-dired-monochrome (not qk-dired-all-the-icons-show-colors))
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'qk-dired)
;; qk-defaults.el ends here.
