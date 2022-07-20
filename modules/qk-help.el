;;; qk-help.el -*- lexical-binding: t; -*-

(use-package transient
  :straight t
  :commands transient-define-prefix
  :general
  (:keymaps
   '(transient-base-map)
   "<escape>" 'transient-quit-one))

(use-package helpful
  :straight t
  :general
  (global-definer
    "h" 'qk/help-transient)
  (general-nmap
    :keymaps '(helpful-mode-map)
    "gx" 'push-button)
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
      (s-trim (s-join "\n" relevant-lines))))

  (defun qk-helpful-at-point-dwim ()
    "Show help for symbol at point. If `lsp-ui-doc-mode' is t, show the doc from the lsp."
    (interactive)
    (if (bound-and-true-p lsp-ui-doc-mode)
        (after! lsp-ui
          (lsp-ui-doc-show))
      (helpful-at-point)))

  (transient-define-prefix qk/help-transient ()
    "help commands that i use. a subset of c-h with others thrown in."
    ["help commands"
     ["mode & bindings"
      ("m" "mode" describe-mode)
      ("b" "bindings" describe-bindings)
      ]
     ["describe"
      ("d c" "command" helpful-command)
      ("d f" "function" helpful-function)
      ("d v" "variable" helpful-variable)
      ("d m" "macro" helpful-macro)
      ("d k" "key" helpful-key)
      ]
     ]
    [
     ["dwim"
      ("." "at point   " qk-helpful-at-point-dwim)
      ]
     ["info manuals"
      ("i" "info" info)
      ("4" "other window " info-other-window)
      ("e" "emacs" info-emacs-manual)]]))

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings).
(use-package which-key
  :straight t
  :defer 1
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(provide 'qk-help)
;; qk-help.el ends here.
