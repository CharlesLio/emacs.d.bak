;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :ensure t
  :demand t
  :ensure company-c-headers
  :ensure company-posframe
  :bind (("M-C-/" . company-complete)
         :map company-mode-map
         ("M-/" . company-complete)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook (after-init . global-company-mode)
  :config
  (dolist (backend '(company-clang company-eclim company-semantic))
    (delq backend company-backends))
  (setq-default company-dabbrev-other-buffers t
                company-tooltip-align-annotations t)
  (use-package company-c-headers
    :config
    (when *is-a-mac*
      (delete "/usr/include/" company-c-headers-path-system)
      (add-to-list 'company-c-headers-path-system
                   "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/"))
    (add-to-list 'company-backends 'company-c-headers))
  (use-package company-posframe
    :config
    (setq company-tooltip-minimum-width 40)
    (company-posframe-mode t)
    :diminish company-posframe-mode)
  :diminish company-mode)

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
;; (after-load 'company
;;   (after-load 'page-break-lines
;;     (defvar-local sanityinc/page-break-lines-on-p nil)

;;     (defun sanityinc/page-break-lines-disable (&rest ignore)
;;       (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
;;         (page-break-lines-mode -1)))

;;     (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
;;       (when sanityinc/page-break-lines-on-p
;;         (page-break-lines-mode 1)))

;;     (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
;;     (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))

(provide 'init-company)
;;; init-company.el ends here
