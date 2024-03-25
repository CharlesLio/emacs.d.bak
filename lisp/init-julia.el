;;; init-julia.el --- Julia mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package julia-mode :ensure t)

(use-package eglot-jl
  :ensure t
  :defer t
  :hook (julia-mode . eglot-ensure)
  :config
  (setq eglot-connect-timeout 1000)
  (eglot-jl-init))

;; (add-hook 'julia-mode-hook #'julia-formatter-mode)
;; (add-hook 'julia-mode-hook #'julia-formatter--ensure-server)
;; (add-hook 'julia-mode-hook #'lsp-mode)
;; ;; (setq-default lsp-julia-default-environment
;;               (shell-command-to-string "julia -e 'import Pkg; print(Pkg.envdir() * \"/\" * Base.VERSION_STRING[1:3])'"))
;; (setq-default lsp-julia-default-environment
;;               (shell-command-to-string
;;                "julia -e 'import Pkg; print(\"$(Pkg.envdir())/$(VERSION.major).$(VERSION.minor)\")'"))
;; (add-hook 'julia-mode-hook #'lsp)

;; (defun julia-formatter-format-buffer ()
;;   (julia-formatter-format-region (point-min) (point-max)))

;; (add-hook 'julia-mode-hook (lambda () (add-hook 'before-save-hook #'julia-formatter-format-buffer nil t)))

(provide 'init-julia)
;;; init-julia.el ends here
