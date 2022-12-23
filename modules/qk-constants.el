;;; qk-constants.el -*- lexical-binding: t; -*-

(defvar qk-indent-tabs-mode nil
  "Default: indent with spaces instead of tabs. See `indents-tabs-mode'.")
(defvar qk-tab-width 4
  "Default: indent with 4 columns. See `tab-width'.")
(defvar qk-fill-column 80
  "Default: add 80 characters as fill-column. See `fill-column'.")
(defvar qk-dired-listing-switches "-aBhl"
  "Default: show hidden files with full information in human readable format.
See `dired-listing-switches'.")
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
    "\\*Async Shell Command\\*"
    "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
    "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
    "^\\*term.*\\*$"   term-mode   ;term as a popup
    "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
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
(defvar qk-notes-directory (expand-file-name "~/Documents/slipbox/")
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
(defvar qk-org-default-notes-file (concat qk-denote-directory "refile.org")
  "Default: `qk-notes-directory' 's refile.org. See `org-default-notes-file'.")
(defvar qk-denote-prompts '(title keywords)
  "Default: only prompt for the title and keywords on file creation.
See `denote-prompts'.")
(defvar qk-git-commit-summary-max-length 50
  "Default: number of characters that the commit message should have.
See `git-commit-summary-max-length'.")
(defvar qk-git-commit-fill-column 72
  "Default: set fill column to this number if on `git-commit-mode'.")

(provide 'qk-constants)
;; qk-constants.el ends here.
