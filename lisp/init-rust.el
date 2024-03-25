;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'rustic)
  (setq rustic-format-process-name "rustfmt"
        rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-all-targets nil
        lsp-rust-analyzer-diagnostics-disabled
        ["unresolved-proc-macro" "unresolved-module" "unresolved-import" "unresolved-extern-crate"]
        ;; lsp-eldoc-hook nil
        lsp-enable-symbol-highlighting nil
        lsp-signature-auto-activate nil
        lsp-rust-analyzer-server-display-inlay-hints t
        rustic-format-trigger 'on-save)
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil
                  buffer-save-without-query t)))
  (add-hook 'rustic-mode-hook (lambda () (prettify-symbols-mode -1)))
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook))

(provide 'init-rust)
;;; init-rust.el ends here
