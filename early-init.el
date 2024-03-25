;;; early-init.el --- Emacs 27+ pre-initialisation config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:


(load-theme 'modus-operandi :no-confirm nil)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      )

(tool-bar-mode -1)
(scroll-bar-mode -1)
(size-indication-mode t)

;;; hide all fringe for all frames.
;; (fringe-mode 0)

;; resize frames pixelwise, no rounding occurs to the frame's current value of 'frame-char-height'
;; and 'frame-char-width'.
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq package-enable-at-startup nil)
(setq byte-compile-warnings '(not cl-functions docstrings))
;; (setq inhibit-compacting-font-caches t)

;; (chllib-rust-module-load "chllib.so")
;; (setq-default warning-minimum-level :error)
;; (setq-default warning-minimum-log-level :error)
;; (setq-default warning-suppress-types :warning)
;; (setq-default warning-suppress-log-types :warning)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
