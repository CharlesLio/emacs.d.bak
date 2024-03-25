;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-check-signature nil)
(eval-when-compile (require 'cl-lib)) ;; for `cl-loop'
(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;;; Standard package repositories

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                    (not (gnutls-available-p))))
;;       (proto (if no-ssl "http" "https")))
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
;;   Official MELPA Mirror, in case necessary.
;;   (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
;;   (unless no-ssl (setcdr (assoc "gnu" package-archives) "https://elpa.emacs-china.org/gnu/"))
;;  )

;; setup package source provided by USTC, https://mirrors.ustc.edu.cn/elpa/
;; alternative source by Emacs-China, http://1.15.88.122/
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)
;; AOT native compilation
(setq package-native-compile t)
;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.

(defvar my/required-packages nil)

(defun sanityinc/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note it in `my/required-packages'."
  (let ((available (apply oldfun package args)))
    (prog1 available
      (when (and available (boundp 'package-selected-packages))
        (add-to-list 'my/required-packages package)))))

(advice-add 'require-package :around 'sanityinc/note-selected-package)

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
            (lambda () (package--save-selected-packages
                   (seq-uniq (append my/required-packages package-selected-packages))))))

(require-package 'fullframe)
(fullframe list-packages quit-window)

(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

;;; logged package require.
(defmacro maybe-require (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(let ((require-result (require ,feature ,file 'noerror)))
     (with-current-buffer (get-buffer-create "*Startup Log*")
       (let* ((startup-log-format-string-prefix "%-20s--------[")
              (startup-log-format-string-postfix "%s")
              (startup-status (if require-result "LOADED" "FAILED"))
              (startup-status-face `(face (:foreground
                                           ,(if require-result "green" "red")))))
         (insert (format startup-log-format-string-prefix ,feature))
         (let ((start-pos (point)))
           (insert (format startup-log-format-string-postfix startup-status))
           (add-text-properties start-pos (point) startup-status-face)
           (insert "]\n"))))
     require-result))

;;; makes `use-package' ask the Emacs package manager to
;;; install a package if it is not already present on your system.
(require 'use-package)

(defun use-package/note-selected-package (package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note it in `my/required-packages'."
  (when (and (package-installed-p package) (boundp 'package-selected-packages))
    (add-to-list 'my/required-packages package)))

(advice-add 'use-package-ensure-elpa :after 'use-package/note-selected-package)

(use-package system-packages
  :ensure t
  :config
  (setq system-packages-use-sudo nil))

(use-package use-package-ensure-system-package)

(setq use-package-verbose t)
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;;; add quelpa keyword to `use-package'
(use-package quelpa-use-package
  :ensure t
  :config
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package diminish
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom (auto-package-update-hide-results nil)
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 1)
  (auto-package-update-maybe))

(provide 'init-elpa)
;;; init-elpa.el ends here
