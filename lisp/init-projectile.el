;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer-projectile :ensure t)

(use-package projectile
  :ensure t
  :demand t
  :custom
  (projectile-mode-line-prefix " Proj")
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(provide 'init-projectile)
;;; init-projectile.el ends here
