;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser
(eval-when-compile (require 'cl-lib)) ;; for `cl-nsubstitute'

(use-package dash-at-point
  :ensure t
  :if (eq system-type 'darwin)
  :ensure-system-package
  ("/Applications/Dash.app" . "brew cask install dash")
  :config
  (add-to-list 'dash-at-point-mode-alist '(julia-mode . "julia"))
  (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
  (add-to-list 'dash-at-point-mode-alist '(elisp-mode . "elisp"))
  (cl-nsubstitute '(c-mode . "c,glibc,glib,manpages")
                  '(c-mode . "c,glib,gl2,gl3,gl4,manpages")
                  dash-at-point-mode-alist :test #'(lambda (a b) (equal (car a) (car b))))
  :bind
  (("C-c D" . dash-at-point)))

(provide 'init-dash)
;;; init-dash.el ends here
