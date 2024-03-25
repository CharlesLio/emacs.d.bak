;;; init-local.el --- user local configuation -*- lexical-binding: t -*-
;;; package --- User customization  -*- mode: emacs-lisp; -*-
;;; Time-stamp: <2023-08-09 17:28:12 charles>
;;; Commentary:


;;; Code:
(setq-default user-full-name "Charles Liu"
              user-mail-address "liuchenxing@gmail.com")

;; packages
(require-package 'chinese-conv)
(require-package 'buffer-move)
(when *is-a-mac*
  (require-package 'osx-dictionary)
  ;; keybindings for osx-dictionary package, notice that C-s-d will bring up the
  ;; general macOS word searching at point.
  ;; (setq osx-dictionary-use-chinese-text-segmentation t)
  (global-set-key (kbd "C-s-s") 'osx-dictionary-search-pointer)
  (global-set-key (kbd "C-s-i") 'osx-dictionary-search-input))

(use-package smart-cursor-color
  :ensure t
  :demand t
  :init
  (smart-cursor-color-mode)
  :diminish smart-cursor-color-mode)

;; (set-mouse-color (foreground-color-at-point))

;; enable yasnippet mode
(use-package yasnippet
  :ensure t
  :demand t
  :config (yas-reload-all)
  :hook (prog-mode . yas-minor-mode)
  :diminish yas-minor-mode)

;; (require-package '0xc)
;; (require-package 'xcscope)


;;; user defined functions & variables

(defun open-user-init-file (arg)
  "Open the user init file."
  (interactive "P")
  (if arg (find-file user-init-file)
    (find-file (expand-file-name "lisp/init-local.el" user-emacs-directory))))
(global-set-key (kbd "<f12>") 'open-user-init-file)

(require 'thingatpt)
(when *is-a-mac*
  (defun say ()
    "Say the active region or word at current point if there is no selection."
    (interactive)
    (if (use-region-p)
        (setq words-to-say (buffer-substring (mark) (point)))
      (setq words-to-say (word-at-point)))
    (osx-lib-say words-to-say))
  (global-set-key (kbd "H-s") 'say))


(set-default 'line-spacing 1) ;; the default line-spacing value
(defun toggle-line-spacing ()
  "Toggle line spacing between nil/0 to 6 pixels."
  (interactive)
  (defconst line-spacing-max-pixels 4 "The max pixels of line-spacing.")
  (if (not line-spacing) (set-default 'line-spacing 0)) ;; dealwith nil case
  (if (< line-spacing line-spacing-max-pixels)
      (set-default 'line-spacing (+ line-spacing 1))
    (if (eq line-spacing line-spacing-max-pixels)
        (set-default 'line-spacing nil))) ;; loop back to nil
  (message (format "line-spacing: %s" line-spacing))
  (redraw-frame (selected-frame)))

(keymap-global-set "s-<f7>" 'toggle-line-spacing)


(setq read-quoted-char-radix 16)
(defun unicode-insert (character)
  "Read a unicode code point and insert said CHARACTER.
Input uses `read-quoted-char-radix'.
If you want to copy the values from the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (ucs-insert character))


;; simple customization
(setq display-time-format "%F %I:%M%p")
(setq display-time-format "%Y-%m-%d %H:%M")
(setq-default display-time-default-load-average nil
              display-time-use-mail-icon t)

(use-package celestial-mode-line
  :ensure t
  :demand t
  :if (and (fboundp 'macos-location-update) (macos-location-update) (not (eql calendar-longitude 0)))
  :config
  (setq celestial-mode-line-phase-representation-alist '((0 . "🌕") (1 . "🌓") (2 . "🌑") (3 . "🌗")))
  (setq celestial-mode-line-sunrise-sunset-alist '((sunrise . "🌅") (sunset . "🌇")))
  (setq global-mode-string '(" " display-time-string celestial-mode-line-string))
  (celestial-mode-line-start-timer))

(display-time-mode)

;; (setq confirm-kill-emacs 'yes-or-no-p)
;; confirm when exiting emacs in case of accidental key strokes, wait for 10s to prevent or just exit.
(defun timeout-kill-emacs-function ()
  (y-or-n-p-with-timeout "Do you really want to exit? " 10 "y"))
(add-hook 'kill-emacs-query-functions #'timeout-kill-emacs-function 'append)

;; advanced printing facilities
(require 'printing)
(pr-update-menus)

(add-hook 'emacs-startup-hook (lambda () (setq default-directory "~/")))

;; remap ¥ to \ to adapt to japanese keyboard layout
(global-set-key (kbd "¥") (lambda () (interactive) (insert-char #x5c)))
;; rebound `just-one-space' "M-SPC" to "C-\", since "M-SPC" was assigned to Alfred.
;; deprecate bounding of `delete-horizontal-space' "M-\", using `fixup-whitespace'
;; instead.
(global-set-key (kbd "C-\\") 'just-one-space)
(global-set-key (kbd "M-\\") 'fixup-whitespace)


;;; chinese utilities
(require 'ox)

;; chinese-conv opencc backend customization
(setq chinese-conv-opencc-data "/usr/local/share/opencc/")


;;; cmake
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(global-set-key (kbd "s-c") 'cua-copy-region)
(global-set-key (kbd "s-x") 'cua-cut-region)
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-o") 'menu-find-file-existing)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-a") 'mark-whole-buffer)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; (defun delete-frame-unless-lastone ()
;;   "Delete current frame unless it's the last one left, prompt for confirmation."
;;   (interactive)
;;   (defun only-one-frame () (= (length (frame-list)) 1))
;;   (if (or
;;        (not (only-one-frame))
;;        (and (only-one-frame)
;;             (yes-or-no-p "Attempt to delete the sole visible or iconified frame, do you want to proceed? ")))
;;       (progn
;;         (kill-buffer)
;;         (delete-frame (selected-frame) t))))

;; (global-set-key (kbd "s-w") 'delete-frame-unless-lastone)

;; (global-set-key (kbd "C-x 9 l") 'buf-move-left)
;; (global-set-key (kbd "C-x 9 r") 'buf-move-right)

(define-key global-map (kbd "C-c <f9> r") 'remember)
(define-key global-map (kbd "C-c <f9> R") 'remember-region)

;; update time-stamp before file saving.
(add-hook 'before-save-hook 'time-stamp)
;; perform whitespace cleanup before file saving.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; let apropos sort by relevancy
(setq apropos-sort-by-scores t)

;; setup emacs source code directory under macOS.
(when *is-a-mac*
  (let ((emacs-src (expand-file-name "~/Developer/emacs-src")))
    (when (file-accessible-directory-p emacs-src)
      (setq source-directory emacs-src)
      (setq find-function-C-source-directory (expand-file-name "src" emacs-src)))))

;; set buffer to read-only for files opened within `lisp-directory' and `source-directory'.
(defun find-file-read-only-in-source-directory ()
  (if (and buffer-file-truename
           (string-match-p (rx (| (literal lisp-directory) (literal source-directory))) buffer-file-truename))
      (setq-local buffer-read-only t)))

(add-hook 'find-file-hook #'find-file-read-only-in-source-directory)

;; enable abbrev mode
(setq-default abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(if (file-exists-p abbrev-file-name)
    (progn (setq default-abbrev-mode t)
           (abbrev-mode t)))
(setq save-abbrevs t)

(add-hook 'prog-mode-hook #'hl-line-mode)

;; set all local variables to be safe, take the danger.
(setq enable-local-variables :all)
(setq-default fill-column 100)

(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%d %T" nowtime) (format ".%d]" now-ms))))

(defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepend a timestamp formated by.
FORMAT-STRING with ARGS to each message."
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (sh/current-time-microseconds) " ")))))

;; (advice-add 'message :before 'sh/ad-timestamp-message)


;; on macOS Mojave or above, the following error occur by using deprecated/removed API.
;; Failed to initialize color list unarchiver: Error Domain=NSCocoaErrorDomain Code=4864
;; "* -[NSKeyedUnarchiver _initForReadingFromData:error:throwLegacyExceptions:]: non-keyed
;; archive cannot be decoded by NSKeyedUnarchiver" UserInfo={NSDebugDescription=*
;;-[NSKeyedUnarchiver _initForReadingFromData:error:throwLegacyExceptions:]:
;;non-keyed archive cannot be decoded by NSKeyedUnarchiver}
;; delete '~/Library/Colors/Emacs.clr' every time when Emacs quit, it will be recreated
;; everytime Emacs started.
;; https://stackoverflow.com/questions/52521587/emacs-error-failed-to-initialize-color-list-unarchiver-when-i-call-it-in-the-t
;; (when *is-a-mac*
;;   (defun delete-macos-color-file ()
;;     (let ((color-file (expand-file-name "~/Library/Colors/Emacs.clr")))
;;       (if (file-exists-p color-file) (delete-file color-file))))
;;   (add-hook 'kill-emacs-hook 'delete-macos-color-file))

(require-package 'chinese-number)

;; (setq treesit-language-source-alist
;;       '((yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (rust-mode . rust-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  (defun my/treesit-mark-bigger-node ()
    "https://emacs-china.org/t/treesit-expand-region-el/23406"
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))

  (with-eval-after-load 'expand-region (add-to-list 'er/try-expand-list 'my/treesit-mark-bigger-node)))

(use-package command-log-mode
  :ensure t
  :vc (:url "https://github.com/lewang/command-log-mode.git"
            :rev :newest)
  :hook (LaTeX-mode))

(provide 'init-local)
;;; init-local.el ends here
;; Local Variables:
;; coding: utf-8-unix
;; End:
