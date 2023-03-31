;;; qk-completion.el -*- lexical-binding: t; -*-

;; Vertico provides a minimalistic vertical completion UI, which is based on the default
;; completion system. By reusing the default system, Vertico achieves full compatibility
;; with built-in Emacs commands and completion tables. Vertico is pretty bare-bone and
;; comes with only a minimal set of commands.
(use-package vertico :elpaca t
  :hook (doom-first-buffer . vertico-mode)
  :init (setq vertico-cycle t))

;; Orderless is one of the same emacs packages that works modularly, using the basic emacs
;; API. This package provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of the components
;; in any order. Each component can match in any one of several ways: literally, as a regexp,
;; as an initialism, in the flex style, or as multiple word prefixes. By default, regexp
;; and literal matches are enabled.
(use-package orderless :elpaca t
  :init
  (setq
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides nil))

;; Marginalia are marks or annotations placed at the margin of the page of a book
;; or in this case helpful colorful annotations placed at the margin of the minibuffer
;; for your completion candidates. Marginalia can only add annotations to be displayed
;; with the completion candidates. It cannot modify the appearance of the candidates themselves,
;; which are shown as supplied by the original commands.
(use-package marginalia :elpaca t
  :hook (doom-first-buffer . marginalia-mode))

;; (use-package corfu
;;   :elpaca (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
;;   :hook
;;   (doom-first-input . global-corfu-mode)
;;   :general
;;   (:keymaps
;;    '(corfu-map)
;;    "C-n" 'corfu-next
;;    "C-p" 'corfu-previous)
;;   (general-imap
;;     "C-n" nil
;;     "C-p" nil)
;;   :init
;;   (setq
;;    corfu-cycle t
;;    corfu-quit-no-match t
;;    corfu-quit-at-boundary t
;;    corfu-auto qk-corfu-auto
;;    corfu-auto-delay qk-corfu-auto-delay
;;    corfu-auto-prefix qk-corfu-auto-prefix)
;;   (setq corfu-popupinfo-delay qk-corfu-popupinfo-delay)
;;   :config
;;   (use-package corfu-popupinfo
;;     :commands corfu-popupinfo-toggle
;;     :hook (corfu-mode . corfu-popupinfo-mode)
;;     :general (:keymaps 'corfu-map "C-h" 'corfu-popupinfo-toggle)
;;     :custom-face (corfu-popupinfo ((t (:inherit corfu-default)))))

;;   (setq kind-icon-default-face 'corfu-default)
;;   (when qk-corfu-in-minibuffer
;;     (defun corfu-enable-in-minibuffer ()
;;       "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;       (when (where-is-internal #'completion-at-point
;;                                (list (current-local-map)))
;;         (corfu-mode t)))
;;     (add-hook! minibuffer-setup 'corfu-enable-in-minibuffer)))

;; (use-package kind-icon :elpaca t
;;   :commands kind-icon-margin-formatter)

;; ;; Cape provides a bunch of Completion At Point Extensions which can be used in
;; ;; combination with my Corfu completion UI or the default completion UI. The completion
;; ;; backends used by completion-at-point are so called completion-at-point-functions (Capfs).
;; (use-package cape :elpaca t
;;   :hook
;;   ((text-mode prog-mode) . qk-update-completion-functions)
;;   (lsp-completion-mode . qk-update-lsp-completion-functions)
;;   :init
;;   (setq cape-dabbrev-min-length qk-cape-dabbrev-min-length)
;;   :config
;;   (defun qk-update-completion-functions ()
;;     "Add the file and dabbrev backends to `completion-at-point-functions'"
;;     (dolist (backend '(cape-file cape-dabbrev))
;;       (add-to-list 'completion-at-point-functions backend t)))
;;   (defun qk-update-lsp-completion-functions ()
;;     "Bust the capf for `lsp-completion-mode'. To be used in a hook."
;;     (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))))

;; Use tempel instead of using yasnippet. It uses the local templates file, which I have
;; added to the `user-emacs-directory'. It's much more lightweight than yasnippet, and
(use-package tempel :elpaca t
;; we can always add more snippets if we need to.
  :hook
  (prog-mode . tempel-setup-capf)
  (prog-mode . tempel-abbrev-mode)
  (text-mode . tempel-setup-capf)
  :general
  (:keymaps
   '(tempel-map)
   "C-n" 'tempel-next
   "C-p" 'tempel-previous)
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))

(provide 'qk-completion)
;; qk-completion.el ends here.
