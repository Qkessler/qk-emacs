;;; qk-lang.el -*- lexical-binding: t; -*-

;; Display line width indicator.
(add-hook! prog-mode 'display-fill-column-indicator-mode)

;; Invoke bashrc / zshrc loading when using the compile function.
(defadvice compile (around use-zshrc activate)
  "Load .zshrc in any calls to bash / zsh (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

(defadvice recompile (around use-zshrc activate)
  "Load .zshrc in any calls to bash / zsh (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

;; Tree-sitter is a parser generator tool and an incremental parsing library.
;; It can build a concrete syntax tree for a source file and efficiently update the
;; syntax tree as the source file is edited. It could be the next generation of sintax
;; parsers, as it has been really accepted by the community and the Github's Atom
;; team has been working on implementing a ton of languages.
(unless (treesit-available-p)
  (elpaca-use-package tree-sitter
    :hook
    (doom-first-buffer . global-tree-sitter-mode)
    (tree-sitter-after-on . tree-sitter-hl-mode))

  (elpaca-use-package tree-sitter-langs
    :after tree-sitter
    :config
    (defun suppress-messages (old-fun &rest args)
      (cl-flet ((silence (&rest args1) (ignore)))
        (advice-add 'message :around #'silence)
        (unwind-protect
            (apply old-fun args)
          (advice-remove 'message #'silence))))
    (advice-add 'tree-sitter-langs-install-grammars :around #'suppress-messages)))

(use-package compile
  :hook (compilation-filter . colorize-compilation-buffer)
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       (save-excursion 
         (goto-char compilation-filter-start) 
         (line-beginning-position))
       (point)))))

(after! general
  (major-mode-definer
    :keymaps '(prog-mode-map)
    :major-modes '(prog-mode)
    "C" 'qk-consult-compile))

(setq c-basic-offset qk-tab-width)
(if (treesit-available-p)
    (use-package c++-ts-mode :mode "\\.cpp\\'")
  (use-package c++-mode :mode ("\\.cpp\\'" "\\.c\\'")))

(setq
 python-shell-interpreter qk-python-shell-interpreter
 compilation-ask-about-save nil
 python-indent-guess-indent-offset-verbose nil
 compilation-scroll-output t)

(when (treesit-available-p)
  (use-package python-ts-mode :mode "\\.python\\'"))

(after! python
  (defun mk-compile-python-buffer ()
    "Use compile to run python programs."
    (interactive)
    (compile (concat python-shell-interpreter " " (buffer-name))))

  (major-mode-definer
    :keymaps '(python-mode-map)
    :major-modes '(python-mode)
    "c" 'mk-compile-python-buffer))

;; This is a simple global minor mode which will replicate the changes done by
;; virtualenv activation inside Emacs. The main entry points are
;; pyvenv-activate, which queries the user for a virtual environment directory
;; to activate, and pyvenv-workon.
(elpaca-use-package pyvenv
  :hook (python-mode . rh-pyvenv-autoload)
  :config
  (elpaca f)
  (defun rh-pyvenv-autoload ()
    (interactive)
    "Auto activate .env directory if it exists"
    (f-traverse-upwards
     (lambda (path)
       (let ((venv-path (f-expand ".env" path)))
         (when (f-exists? venv-path)
           (pyvenv-activate venv-path)))))))

;; Colors need to be highlighted in other buffers too, i.e. elisp-mode. For that
;; I intend to use rainbow-mode, which may be enabled with M-x `rainbow-mode'.
(elpaca-use-package rainbow-mode
  :commands rainbow-mode)

(use-package mhtml-mode
  :mode ("\\.html\\'" "\\.hbs\\'"))

(if (not (treesit-available-p))
    (use-package js-mode
      :mode ("\\.js\\'" "\\.tsx\\'" "\\.ts\\'"))
  (progn
    (use-package js-ts-mode :mode "\\.js\\'")
    (use-package typescript-ts-mode :mode "\\.ts\\'"
      :init (setq typescript-ts-mode-indent-offset qk-tab-width))
    (use-package tsx-ts-mode :mode "\\.tsx\\'")))

(if (not (treesit-available-p))
    (elpaca-use-package json-mode
      :mode "\\.json\\'")
  (use-package json-ts-mode :mode "\\.json\\'"
    :init (setq json-ts-mode-indent-offset qk-tab-width)))

(if (not (treesit-available-p))
    (elpaca-use-package yaml-mode
      :mode ("\\.yml\\'" "\\.yaml\\'"))
  (use-package yaml-ts-mode
    :mode ("\\.yaml\\'" "\\.yml\\'")))

(when (treesit-available-p)
  (use-package java-ts-mode :mode "\\.java\\'"))

(when (treesit-available-p)
  (use-package bash-ts-mode :mode ("\\.bash\\'" "\\.sh\\'")))

(if (treesit-available-p)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  ;; Configure some emacs packages to reach the IDE level experience
  ;; when Rust programming. Testing =rustic=, which is a newer fork of
  ;; rust-mode. I won't be using the hydras the support, but having
  ;; some cargo commands implemented is nice. The lsp configuration is
  ;; also seamless, pretty good package.
  (elpaca-use-package rustic
    :commands rustic-cargo-add rustic-cargo-run rustic-cargo-test rustic-cargo-build
    :init
    (setq
     rustic-format-on-save nil
     rustic-lsp-client nil)))

;; Emacs, as always has its own integration of the key functions. I just use the
;; `cheat-sh-search', which is safe to say to be great.
(elpaca-use-package cheat-sh
  :general
  (major-mode-definer
    :keymaps '(prog-mode-map)
    :major-modes '(prog-mode)
    "s" 'cheat-sh-search-topic))

;; Using the hl-todo package, we are able to highlight keywords
;; related to the working environment, like: TODO, FIXME and some
;; more.
(elpaca-use-package hl-todo
  :hook ((prog-mode gfm-mode org-mode) . hl-todo-mode)
  :config 
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   `(("TODO"       (or org-todo error) bold)
     ("FIXME"      error bold)
     ("HACK"       font-lock-constant-face bold)
     ("REVIEW"     font-lock-keyword-face bold)
     ("NOTE"       success bold)
     ("DONE"       success bold)
     ("DEPRECATED" font-lock-doc-face bold))))

;; Markdown configuration, which I use specially often when editing README files
;; on Github. The are some interesting options like the change of the markdown-command
;; to pandoc which is way better at compiling html5. 
(elpaca-use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command qk-markdown-command)
  :general
  (major-mode-definer
    :keymaps '(markdown-mode-map)
    :major-modes '(gfm-mode markdown-mode)
    "c" 'markdown-insert-code))

;; Make sure .h files default to Objective-C mode, since I don't really code
;; in C. There isn't a problem anyway to do this. If worried about LSP not working,
;; we default to clangd or ccls if sourcekit-lsp is not found, and therefore, it
;; shouldn't have a problem with C programming.
(use-package objc-mode
  :mode "\\.h\\'")

;; Kotlin still doesn't have full support inside emacs. To add the kotlin major
;; mode, use the kotlin-mode package.
(if (treesit-available-p)
    (elpaca-use-package
        (kotlin-ts-mode :host gitlab :repo "bricka/emacs-kotlin-ts-mode")
      :mode "\\.kt\\'")
  (elpaca kotlin-mode))

(elpaca swift-mode)
(elpaca scala-mode)

(if (not (treesit-available-p))
    (elpaca go-mode)
  (pushnew! auto-mode-alist
            '("\\.go\\'" . go-ts-mode)
            '("go.mod" . go-mod-ts-mode)))

(if (not (treesit-available-p))
    (elpaca-use-package apheleia
      :hook
      ((rustic-mode go-mode) . apheleia-mode))
  (elpaca-use-package apheleia
    :hook ((rust-ts-mode go-ts-mode) . apheleia-mode)
    :config
    (pushnew! apheleia-mode-alist
              '(go-ts-mode . gofmt)
              '(rust-ts-mode . rustfmt))))

(provide 'qk-lang)
;; qk-lang.el ends here.
