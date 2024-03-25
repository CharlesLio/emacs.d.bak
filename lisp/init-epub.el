;;; init-epub.el --- Insert description here -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nov
  :ensure t
  :demand t
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename))
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Rubik" :height 1.0))
  (add-hook 'nov-mode-hook #'my-nov-font-setup))

(provide 'init-epub)
;;; init-epub.el ends here
