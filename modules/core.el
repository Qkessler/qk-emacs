;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5
;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set initial major mode to fundamental-mode, to avoid possible eager loading on text-mode.
(setq initial-major-mode 'fundamental-mode)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Garbage collection strategy used by doom-emacs. Enforce a sneaky Garbage Collection
;; strategy to minimize GC interference with user activity. During normal use a high GC
;; threshold is set. When idling GC is triggered and a low threshold is set.
;; A more detailed explanation can be found at http://akrl.sdf.org/
(use-package gcmh
  :elpaca t
  :demand t
  :init
  (setq
   gcmh-idle-delay 'auto  ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024))
  :config 
  (gcmh-mode 1))

;; Emacs daemon doesn't seem to look for environment variables in the usual places
;; like .profile and such. Installing the package exec-path-from-shell, we make sure
;; that those important config files are loaded.
(use-package exec-path-from-shell
  :elpaca t
  :defer 3
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config 
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

;; Benchmark Emacs Startup time without ever leaving your Emacs.
(use-package esup
  :elpaca t
  :commands esup
  :init (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(provide 'core)
;;; core.el ends here.
