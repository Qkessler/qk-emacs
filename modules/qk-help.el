;;; qk-help.el -*- lexical-binding: t; -*-

(elpaca-use-package transient
  :general
  (:keymaps
   '(transient-base-map)
   "<escape>" 'transient-quit-one))

(elpaca-use-package helpful
  :general
  (general-nmap
    :keymaps '(helpful-mode-map)
    "gx" 'push-button)
  (general-nmap
    "K" 'helpful-at-point)
  (+general-global-help
    "b" 'describe-bindings
    "d" '(:ignore t)
    "dc" 'helpful-command
    "df" 'helpful-function
    "dv" 'helpful-variable
    "dm" 'helpful-macro
    "dk" 'helpful-key
    "e" 'info-emacs-manual
    "i" 'info
    "m" 'describe-mode)
  :config
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines)))))

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings).
(elpaca-use-package which-key
 :hook (doom-first-input . (lambda ()
                             (which-key-setup-minibuffer)
                             (which-key-mode))))

(provide 'qk-help)
;; qk-help.el ends here.
