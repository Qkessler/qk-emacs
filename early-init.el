;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Even though straight.el already byte-compiles all installed packages, setting it to `t' doesn't hurt.
(setq comp-deferred-compilation t)

;; Remove all package.el loading at startup time, we are using straight.el and use-package.
;; Also avoid package.el adding `custom-set-variables' to init.el.
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(font . "Source Code Pro for Powerline-16") default-frame-alist)
(push '(cursor . "#ebdbb2") default-frame-alist)
(push '(undecorated . t) default-frame-alist)

;; Faster to disable these here (before they've been initialized)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)
(defun qk/disable-scroll-bars (frame)
          "Remove all possible scroll-bars, both vertical and horizontal."
          (modify-frame-parameters frame
                                   '((vertical-scroll-bars . nil)
                                     (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'qk/disable-scroll-bars)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Always follow vc symlinks, I use stow for managing my dotfiles
(setq vc-follow-symlinks t)

;; Set `load-prefer-newer' to non-nil value, to avoid emacs using older byte-compiled files. Using newer files, we force emacs to "byte-compile" the files that it is trying to use.
(setq load-prefer-newer noninteractive)
