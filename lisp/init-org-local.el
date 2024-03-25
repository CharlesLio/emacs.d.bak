;;; Package --- local configuration for org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; additional org keybindings and customization

(maybe-require-package 'org-download)

(require-package 'org-roam)
(require-package 'org-roam-timestamps)
(require-package 'org-roam-ui)
(require-package 'citar-org-roam)
(require-package 'cdlatex)

(require 'org-agenda)
(require 'org-download)

(require-package 'ox-pandoc)
;; customize `org-pandoc-menu-entry' variable to change its default menu entries
;; (setq org-pandoc-emnu-entry )
;; delay loading of `ox-pandoc' when `org-pandoc-menu-entry' is customized
(with-eval-after-load 'ox (require 'ox-pandoc))

(add-hook 'org-mode-hook
          #'(lambda () (define-key org-mode-map (kbd "C-c b") 'org-switchb)))

;; (use-package org-superstar
;;   :ensure t
;;   :hook (org-mode . #'org-superstar-mode))

(setq org-directory (expand-file-name "~/Documents/org"))
;; create the directory if not exist.
(unless (file-directory-p org-directory)
  (make-directory org-directory t))
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-agenda-files (expand-file-name ".agenda_files" org-directory))

;; display inline images when file was open and use ad-hoc customized size for display
(setq org-startup-with-inline-images t
      org-image-actual-width nil)

;; When non-nil, resolve open clocks if the user is idle more than X minutes.
(setq org-clock-idle-time 30)
;; Non-nil means to start clocking from the last clock-out time, if any.
(setq org-clock-continuously t)

;; Non-nil means ‘C-a’ and ‘C-e’ behave specially in headlines and items.
(setq org-special-ctrl-a/e t)

;; Non-nil means ‘C-k’ will behave specially in headlines.
;; When nil, ‘C-k’ will call the default ‘kill-line’ command.
;; When t, the following will happen while the cursor is in the headline:
;; - When the cursor is at the beginning of a headline, kill the entire
;; line and possible the folded subtree below the line.
;; - When in the middle of the headline text, kill the headline up to the tags.
;; - When after the headline text, kill the tags.
(setq org-special-ctrl-k t)
(setq org-ctrl-k-protect-subtree nil)
(setq org-catch-invisible-edits 'show-and-error)
;; indent one start per level under org-indent-mode
(setq org-indent-indentation-per-level 2)
;; save the clock history across emacs sessions

(org-clock-persistence-insinuate)
;; Number of days before expiration during which a deadline becomes active.
(setq org-deadline-warning-days 7)
(setq org-agenda-span 3)
(setq org-agenda-include-diary t)
(setq org-M-RET-may-split-line nil)
;; (setq org-agenda-use-time-grid t)
;; (setq org-agenda-show-current-time-in-grid t)
(setq org-agenda-time-leading-zero t)
(setq org-agenda-timegrid-use-ampm t)

;; automatically change to DONE when all children are done.
(defun org-summary-todo (done not-done)
  "Switch entry to DONE from NOT-DONE when all subentries are DONE, to todo otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defalias 'org-agenda-format-date-aligned 'lawlist-org-agenda-format-date-aligned)

(defun lawlist-org-agenda-format-date-aligned (date)
  "Format a `date' string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (substring (calendar-day-name date) 0 3))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format "%4d-%02d-%02d %s%s" year month day dayname weekstring)))

(when (and *is-a-mac*
           (or (file-directory-p "/Applications/org-clock-statusbar.app")
               (file-directory-p (expand-file-name "~/Applications/org-clock-statusbar.app"))))
  (defun org-clock-in-statusbar ()
    (if (featurep 'ns)
        (ns-do-applescript (format "tell application \"org-clock-statusbar\" to clock in \"%s\""
                                   org-clock-current-task))
      (call-process "/usr/bin/osascript" nil 0 nil
                    (expand-file-name "lisp/org-clock-in.scpt" user-emacs-directory)
                    org-clock-current-task)))
  (defun org-clock-out-statusbar ()
    (if (featurep 'ns)
        (ns-do-applescript "tell application \"org-clock-statusbar\" to clock out")
      (call-process "/usr/bin/osascript" nil 0 nil
                    (expand-file-name "lisp/org-clock-out.scpt" user-emacs-directory))))
  (add-hook 'org-clock-in-hook #'org-clock-in-statusbar)
  (add-hook 'org-clock-out-hook #'org-clock-out-statusbar))


(require-package 'plantuml-mode)
(maybe-require-package 'flycheck-plantuml)
(use-package plantuml-mode
  ;; :defer t
  :init
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
  (setq plantuml-exec-mode "jar")
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))


;;; code for export org file with themes, manual installation
;; (require-package 'htmlize)

;;; install `org-html-themify' to `site-lisp'
;; git clone https://github.com/DogLooksGood/org-html-themify.git
;; (require 'org-html-themify)
;; download `hexrgb.el' to the same directory
;; https://www.emacswiki.org/emacs/download/hexrgb.el

;; NOTE: require proper theme to be installed in `init-themes.el'
;; (setq org-html-themify-themes '((dark . modus-vivendi) (light . leuven)))

;; (add-hook 'org-mode-hook 'org-html-themify-mode)



(defun org-buffer-face-mode ()
  "Set font for `org-mode' buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ysabeau Office"))
  (buffer-face-mode))
(add-hook 'org-mode-hook #'org-buffer-face-mode)
(add-hook 'org-agenda-mode-hook #'org-buffer-face-mode)

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package emacs
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   ;; org-agenda-time-grid
   ;; '((daily today require-timed)
   ;;   (800 1000 1200 1400 1600 1800 2000)
   ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   ;; org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
   ))

;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :weight normal))))
;;  '(org-level-2 ((t (:inherit outline-2 :weight normal))))
;;  '(org-level-3 ((t (:inherit outline-3 :weight normal))))
;;  '(org-level-4 ((t (:inherit outline-4 :weight normal))))
;;  '(org-level-5 ((t (:inherit outline-5 :weight normal))))
;;  '(org-level-6 ((t (:inherit outline-6 :weight normal))))
;;  '(org-level-7 ((t (:inherit outline-7 :weight normal))))
;;  '(org-level-8 ((t (:inherit outline-8 :weight normal))))
;;  ;; '(org-document-title ((t (:family "Ysabeau Infant"))))
;;  ;; '(org-document-info ((t (:family "Ysabeau Infant"))))
;;  ;; '(org-document-info-keyword ((t (:family "Ysabeau Infant"))))
;;  ;; '(org-meta-line ((t (:family "Ysabeau Infant"))))
;;  ;; '(org-drawer ((t (:family "Iosevka Comfy"))))
;;  ;; '(org-date ((t (:family "Iosevka Comfy"))))
;;  ;; '(org-block-begin-line ((t (:family "Iosevka Comfy"))))
;;  ;; '(org-block-end-line ((t (:family "Iosevka Comfy"))))
;;  ;; '(org-block ((t (:family "Iosevka Comfy"))))
;;  )

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(add-hook 'org-mode-hook 'visual-line-mode)


(provide 'init-org-local)
;;; init-org-local.el ends here
