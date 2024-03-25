;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil))

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(use-package pyvenv
  :ensure t)
(use-package pyvenv-auto
  :demand t
  :after (pyvenv)
  :hook ((python-mode . pyvenv-auto-run)))

;; (setq python-shell-interpreter "python3")
(if-let ((python3-path (shell-command-as-string "pyenv which python3")))
    (setq python-shell-interpreter python3-path
          python-interpreter "python3"
          ))

(require-package 'pip-requirements)

;; (when (maybe-require-package 'flycheck-posframe)
;;   (with-eval-after-load 'flycheck
;;     ;; (require 'flycheck-posframe)
;;     (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)))

;; (when (maybe-require-package 'flycheck-pycheckers)
;;   (setq flycheck-pycheckers-checkers '(flake8)
;;         flycheck-pycheckers-max-line-length 120
;;         )
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

;; (when (maybe-require-package 'anaconda-mode)
;;   (after-load 'python
;;     ;; Anaconda doesn't work on remote servers without some work, so
;;     ;; by default we enable it only when working locally.
;;     (add-hook 'python-mode-hook
;;               (lambda () (unless (file-remote-p default-directory)
;;                       (anaconda-mode 1))))
;;     (setq-default python-indent-offset 4)
;;     (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))
;;   (after-load 'anaconda-mode
;;     (define-key anaconda-mode-map (kbd "M-?") nil))
;;   (when (maybe-require-package 'company-anaconda)
;;     (after-load 'company
;;       (after-load 'python
;;         (add-to-list 'company-backends 'company-anaconda))))
;;   )

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

;; (when (maybe-require-package 'reformatter)
;;   (reformatter-define python-format :program (shell-command-as-string "pyenv which yapf")
;;     :args '("-p"))
;;   (add-hook 'python-mode-hook #'(lambda () (python-format-on-save-mode t))))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t
        lsp-auto-guess-root t
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-enable-on-type-formatting t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-signature-doc-lines 10)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode)

(provide 'init-python)
;;; init-python.el ends here
