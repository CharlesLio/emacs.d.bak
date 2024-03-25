;;; init-editing-utils.el --- day-to-day editing helpers -*- lexical-binding: t -*-
;;; commentary:
;;; code:

(use-package unfill :ensure t)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(use-package list-unicode-display :ensure t)

;; (add-hook 'find-file-hook (lambda () (ruler-mode t)))


;; some basic preferences
(setq-default
 blink-cursor-interval 0.5
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 large-file-warning-threshold (* 1024 1024 100) ;; 100MiB
 )

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)

(delete-selection-mode t)

;; huge files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  (defun ffap-vlf ()
    "find file at point with vlf."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file) (error "file does not exist: %s" file))
      (vlf file))))

;;; a simple visible bell which works in all terminal types
(use-package mode-line-bell
  :ensure t
  :hook (after-init . mode-line-bell-mode))

(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode)
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20))

(with-eval-after-load 'subword
  (diminish 'subword-mode))


(use-package display-line-numbers-mode
  :init
  (setq-default display-line-numbers-width-start t)
  :hook (emacs-lisp-mode python-mode))

;; (when (fboundp 'display-line-numbers-mode)
;;   ;; (setq-default display-line-numbers-grow-only nil)
;;   (setq-default display-line-numbers-width-start t)
;;   (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;   (add-hook 'display-line-numbers-mode-hook
;;             #'(lambda () (set-face-background 'line-number (face-attribute 'default :background))
;;                 (set-face-background 'line-number-current-line (face-attribute 'default :background)))))

(use-package goto-line-preview
  :ensure t
  :bind
  (([remap goto-line] . goto-line-preview)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(use-package symbol-overlay
  :ensure t
  :hook (prog-mode html-mode yaml-mode conf-mode)
  :bind (:map symbol-overlay-mode-map
              ([M-i] . symbol-overlay-put)
              ([M-i] . symbol-overlay-remove-all)
              ([M-n] . symbol-overlay-jump-next)
              ([M-p] . symbol-overlay-jump-prev))
  :diminish symbol-overlay-mode)


;; zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc" "kill up to, but not including argth occurrence of char.")
(keymap-global-set "M-z" 'zap-up-to-char)

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-separator "\f")
  :bind
  (([M-y] . browse-kill-ring)
   :map browse-kill-ring-mode-map
   ([C-g] . browse-kill-ring-quit)
   ([M-n] . browse-kill-ring-forward)
   ([M-p] . browse-kill-ring-previous)))


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

;; Expand region
(use-package expand-region
  :ensure t
  :demand t
  :bind (("C-=" . er/expand-region)))

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Rectangle selections, and overwrite text when the selection is active
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;; Handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(global-set-key (kbd "C-z") 'scroll-up-line)
(global-set-key (kbd "C-M-z") 'scroll-down-line)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

;; (require-package 'multiple-cursors)
;; ;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; ;; From active region to multiple cursors:
;; (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c m c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

;; (defun kill-back-to-indentation ()
;;   "Kill from point back to the first non-whitespace character on the line."
;;   (interactive)
;;   (let ((prev-pos (point)))
;;     (back-to-indentation)
;;     (kill-region (point) prev-pos)))

;; (global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;; Page break lines
(use-package page-break-lines
  :ensure t
  :config
  (setq page-break-lines-max-width 100)
  (dolist (mode '(browse-kill-ring-mode tags-table-mode c-mode sh-mode))
    (add-to-list 'page-break-lines-modes mode))
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  :diminish page-break-lines-mode)


;; Shift lines up and down with M-up and M-down.
;; Duplicate line up and down with M-S-up and M-S-down.
(use-package move-dup
  :ensure t
  :bind (([M-up] . move-dup-move-lines-up)
         ([M-down] . move-dup-move-lines-down)
         ([M-S-up] . move-dup-duplicate-up)
         ([M-S-down] . move-dup-duplicate-down)))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :ensure t
  :config
  (add-hook 'after-init-hook 'whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package highlight-escape-sequences
  :ensure t
  :config
  (add-hook 'after-init-hook 'hes-mode))

(use-package which-key
  :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq-default which-key-idle-delay 1.5)
  :diminish which-key-mode)

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-j" . crux-smart-open-line)
         ;; ("C-x p" . crux-switch-to-previous-buffer)
         ([remap kill-line] . crux-smart-kill-line)
         ("C-M-<backspace>" . crux-kill-line-backwards)
         ("C-x M-k" . crux-kill-other-buffers)
         ("<f2>" . crux-visit-term-buffer)))


;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up

;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)

;; Some local minor modes clash with CUA rectangle selection

(defvar-local sanityinc/suspended-modes-during-cua-rect nil
  "Modes that should be re-activated when cua-rect selection is done.")

(eval-after-load 'cua-rect
  (advice-add 'cua--deactivate-rectangle :after
              (lambda (&rest _)
                (dolist (m sanityinc/suspended-modes-during-cua-rect)
                  (funcall m 1)
                  (setq sanityinc/suspended-modes-during-cua-rect nil)))))

(defun sanityinc/suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (eval-after-load 'cua-rect
    (advice-add 'cua--activate-rectangle :after
                (lambda (&rest _)
                  (when (bound-and-true-p mode-name)
                    (add-to-list 'sanityinc/suspended-modes-during-cua-rect mode-name)
                    (funcall mode-name 0))))))

(sanityinc/suspend-mode-during-cua-rect-selection 'whole-line-or-region-local-mode)

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key [C-o] 'sanityinc/open-line-with-reindent)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
