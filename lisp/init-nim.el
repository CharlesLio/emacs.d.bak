;;; init-nim.el --- Nim Language editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'nim-mode)
(require-package 'flycheck-nimsuggest)
(require 'nim-mode)

(add-hook 'nim-mode-hook 'nimsuggest-mode)


(provide 'init-nim)
;;; init-nim.el ends here
