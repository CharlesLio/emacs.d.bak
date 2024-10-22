;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; String utilities missing from core emacs
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;; Execute shell command COMMAND and return its output as a string without leading or trailing [[:blanks:]].
(defun shell-command-as-string (command)
  "Execute shell command COMMAND and return its output as a string without newline,
return nil If command not found."
  (let ((output (string-trim (shell-command-to-string command))))
    (if (cl-search "command not found" output)
        nil output)))


;; 2023-07-05 begin replaced by function `crux-delete-file-and-buffer' by crux package
;; Delete the current file
;; (defun delete-this-file ()
;;   "Delete the current file, and kill the buffer."
;;   (interactive)
;;   (unless (buffer-file-name)
;;     (error "No file is currently being edited"))
;;   (when (yes-or-no-p (format "Really delete '%s'?"
;;                              (file-name-nondirectory buffer-file-name)))
;;     (delete-file (buffer-file-name))
;;     (kill-this-buffer)))


;; 2023-07-05 begin replaced by function `crux-rename-file-and-buffer' by crux package
;; Rename the current file
;; (defun rename-this-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (unless filename
;;       (error "Buffer '%s' is not visiting a file!" name))
;;     (progn
;;       (when (file-exists-p filename)
;;         (rename-file filename new-name 1))
;;       (set-visited-file-name new-name)
;;       (rename-buffer new-name))))


;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide 'init-utils)
;;; init-utils.el ends here
