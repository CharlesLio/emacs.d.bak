;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package diredfl
  :ensure t
  :defer t
  :hook dired-mode)

(use-package dired-single
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook dired-mode
  :diminish all-the-icons-dired-mode)

(eval-when-compile (require 'ls-lisp))

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
;; (if-let ((gls (executable-find "gls")))
;;     (setq insert-directory-program gls))

;; Hook up dired-x global bindings without loading it up-front
;; (define-key ctl-x-map "\C-j" 'dired-jump)
;; (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)
(define-key ctl-x-map (kbd "C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)

(with-eval-after-load 'dired
  (if ls-lisp-use-insert-directory-program
      (setq insert-directory-program (executable-find "gls")
            dired-use-ls-dired t
            dired-listing-switches "-alh --group-directories-first --time-style=long-iso")
    (setq ls-lisp-format-time-list '("%Y-%m-%dT%H:%M:%S" "%Y-%m-%dT%H:%M:%S")
          ls-lisp-use-localized-time-format t))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

;; get dired+ from github
;; git clone https://github.com/emacsmirror/dired-plus.git to `site-lisp' directory
;; change `cl' pacakge to `cl-lib', `case' to `cl-case' manually.
;; (require 'dired+)


(provide 'init-dired)
;;; init-dired.el ends here
