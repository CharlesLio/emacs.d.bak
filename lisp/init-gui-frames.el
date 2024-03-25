;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; (defun hide-minibuffer-fringe (&rest r)
;;   "Hide minibuffer fringe when display fringe on either side."
;;   (when (> (+ (frame-parameter nil 'left-fringe) (frame-parameter nil 'right-fringe)) 0)
;;     (set-window-fringes (minibuffer-window (selected-frame)) 0 0 t t)))

;; (add-hook 'after-make-window-system-frame-hooks 'hide-minibuffer-fringe)
;; (advice-add 'set-fringe-mode :after 'hide-minibuffer-fringe)

(use-package modern-fringes
  :ensure t
  :demand t
  :config
  (let ((inhibit-message t)
        (message-log-max nil))
    (modern-fringes-invert-arrows)
    (modern-fringes-mode t)))

;; show current line indicator in fringe
(use-package fringe-current-line
  :ensure t
  :demand t
  ;; :hook (prog-mode org-agenda-mode)
  )


;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if *is-a-mac*
    (progn
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (unless (display-graphic-p frame)
                    (set-frame-parameter frame 'menu-bar-lines 0)))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))


(defun set-frame-top-left ()
  "Set current frame to top left corner of the display."
  (interactive)
  (set-frame-position nil 0 0))

;; resize the current frame vertically.
(defun current-frame-fill-vertically ()
  "Resize the current frame to fill vertically based on the screen size."
  (interactive)
  (if (display-graphic-p)
      (set-frame-height (selected-frame) (/ (x-display-pixel-height) (frame-char-height)))))
;; resize the current frame horizontally.
(defun current-frame-fill-horizontally ()
  "Resize the current frame to fill horizontally based on the screen size."
  (interactive)
  (if (display-graphic-p)
      (set-frame-width (selected-frame) (/ (x-display-pixel-width) (frame-char-width)))))

(global-set-key [s-C-down] 'current-frame-fill-vertically)
(global-set-key [s-C-right] 'current-frame-fill-horizontally)
;;; --------------------------------------------------------------------------------

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))

(require-package 'disable-mouse)

(dolist (margin '("<left-margin> " "<right-margin> "))
  (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
  (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
  (dolist (multiple '("" "double-" "triple-"))
    (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
    (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))

;;; Stop C-z from minimizing windows under macOS
(defun sanityinc/maybe-suspend-frame ()
  "Supress window frames unless under macOS. on which platform will have no effect at all."
  (interactive)
  (if (and *is-a-mac* (display-graphic-p))
      (message "Use [⌘+m] to `iconify-frame' under macOS.")
    (suspend-frame)))

(keymap-global-set "C-x C-z" 'sanityinc/maybe-suspend-frame)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
