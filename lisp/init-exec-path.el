;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

;; disable warning message when PATH was set in .bashrc or .zshrc.
(setq-default exec-path-from-shell-warn-duration-millis 10000)
(setq exec-path-from-shell-check-startup-files nil)
;; (setq exec-path-from-shell-debug t)


(after-load 'exec-path-from-shell (nconc exec-path-from-shell-variables '("SHELL" "GOPATH" "LANG" "LC_CTYPE")))

(when (or (memq window-system '(ns x))
          (unless (memq system-type '(pc w32))
            (daemonp)))
  ;; (setq exec-path-from-shell-arguments '("-l" "-i"))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
