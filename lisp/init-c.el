;;; init-c.el --- configuation for c programming -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib)) ;; for `cl-psetq'
(require-package 'clang-capf)

(defun chl-clang-capf-init ()
  "Add `clang-capf' to `completion-at-point-functions'."
  (add-hook 'completion-at-point-functions #'clang-capf nil t))

(add-hook 'c-mode-hook #'chl-clang-capf-init)

(use-package clang-format :ensure t
  :config
  (with-eval-after-load 'cmacexp
    (advice-add 'c-macro-display-buffer :before 'clang-format-buffer)))

(use-package clang-format+
  :ensure t
  :after clang-format
  :hook (c-mode c-mode-common)
  :config
  (cl-psetq clang-format+-always-enable t
            clang-format+-context 'buffer
            clang-format-style "file"
            clang-format-fallback-style "llvm"))

(add-hook 'c-mode-hook
          #'(lambda () (c-set-style "cc-mode")
              (setq-local c-basic-offset 4
                          tab-width 4
                          c-tab-always-indent t)
              (setq-default c-macro-preprocessor "clang -E --std=gnu11 -C -o - -"
                            c-macro-prompt-flag t
                            c-macro-cppflags "-I."
                            c-macro-shrink-window-flag t)
              ;; (c-toggle-auto-hungry-state t)
              (fset 'c-indent-region 'clang-format-region)))

(add-hook 'c-mode-hook #'subword-mode)

(provide 'init-c)
;;; init-c.el ends here
