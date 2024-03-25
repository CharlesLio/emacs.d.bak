;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  ;; (require-package 'flycheck-pkg-config)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; use `whence -m` to pattern match GCC with version number, like gcc-11, gcc-12 ...
  (if-let ((gcc-path (shell-command-to-string "whence -m \"gcc-1[0-9]\"")))
      (if (length> gcc-path 0)
          (setq flycheck-c/c++-gcc-executable (substring gcc-path 0 -1))))

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(with-eval-after-load 'flycheck
  ;; (add-hook 'c-mode-hook (lambda () (progn
  ;;                                ;; (setq flycheck-clang-include-path (list (expand-file-name "./include")))
  ;;                                (setq flycheck-gcc-language-standard "gnu11")
  ;;                                (setq flycheck-clang-language-standard "gnu11"))))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc emacs-lisp-checkdoc emacs-lisp)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
