;;; init-lua.el --- Lua editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :ensure t
  )

(use-package lsp-mode
  :hook lua-mode
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "lua-language-server")
                    :activation-fn (lsp-activate-on "lua")
                    :server-id 'lua-mode)))

(provide 'init-lua)
;;; init-lua.el ends here
