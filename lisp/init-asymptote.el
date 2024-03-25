;;; package --- init-asymptote  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; Time-stamp: <2023-07-13 11:07:11 charles>
;;; Commentary:
;;; Code:

;; activate `asy-mode' only if installed

(when-let* ((texmf-dist (shell-command-as-string "kpsewhich -var-value TEXMFDIST"))
            (asymptote-dir (expand-file-name "asymptote" texmf-dist)))
  (add-to-list 'load-path asymptote-dir)
  (use-package asy-mode
    :init
    (ensure-lib-from-url 'two-mode-mode "https://www.welton.it/freesoftware/files/two-mode-mode.el")
    :mode "\\.asy\\'"
    :interpreter "asy"))

;; (autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
;; (autoload 'lasy-mode "asy-mode.el" "hybird Asymptote/Latex major mode." t)
;; (autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
;; (add-to-list 'auto-mode-alist '("\\.asy\\'" . asy-mode))

(provide 'init-asymptote)
;;; init-asymptote.el ends here
;; Local Variables:
;; coding: utf-8
;; End:
