;;; qk-lsp-bridge.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/source-repos/lsp-bridge"))

(elpaca yasnippet)
(elpaca posframe)

(add-hook! elpaca-after-init
  (require 'lsp-bridge)
  (global-lsp-bridge-mode))

(provide 'qk-lsp-bridge)
;; qk-lsp-bridge.el ends here.
