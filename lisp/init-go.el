;;; init-go.el --- Support for the Google Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'go-mode)
(maybe-require-package 'lsp-mode)

(add-hook 'go-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Format buffer before saving."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'init-go)
;;; init-go.el ends here
