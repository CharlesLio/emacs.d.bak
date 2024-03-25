;;; init-yaml.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.erb\\'")
  :hook (yaml-hook . goto-address-prog-mode))

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(provide 'init-yaml)
;;; init-yaml.el ends here
