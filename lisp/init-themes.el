;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package all-the-icons
  :quelpa (all-the-icons :fetcher github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :quelpa (all-the-icons-dired :fetcher github :repo "jtbm37/all-the-icons-dired")
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-completion
;;   :ensure t
;;   :after (marginalia all-the-icons)
;;   :if (display-graphic-p)
;;   :init
;;   (all-the-icons-completion-mode)
;;   :hook (marginalia-mode . all-the-icons-completion-mode))

(use-package timu-caribbean-theme
  :ensure t
  :custom
  (timu-caribbean-scale-org-document-title nil)
  (timu-caribbean-scale-org-document-info nil)
  (timu-caribbean-scale-org-level-1 nil)
  (timu-caribbean-scale-org-level-2 nil)
  (timu-caribbean-scale-org-level-3 nil)
  (timu-caribbean-org-intense-colors nil)
  (timu-caribbean-mode-line-border nil))

(use-package modus-themes
  :custom
  (modus-themes-inhibit-reload t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'grey-background)
  (modus-themes-deuteranopia nil)
  (modus-themes-tabs-accented t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-syntax '(faint alt-syntax))
  (modus-themes-paren-match '(bold underline intense))
  (modus-themes-region '(accented bg-only no-extend))
  (modus-themes-common-palette-overrides modus-themes-preset-overrides-warmer)
  :bind ("<f5>" . #'modus-themes-toggle)
  :config
  (setq custom-enabled-themes '(modus-vivendi modus-operandi timu-caribbean))
  (load-theme 'modus-operandi :no-confirm nil))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(timu-caribbean modus-operandi-tinted modus-operandi modus-vivendi))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t nil)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; `load-theme' from `early-init.el', not here
;; (add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light and dark
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-operandi))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-vivendi))
  (reapply-themes))

;; (use-package auto-dark
;;   :ensure t
;;   :if (display-graphic-p)
;;   :commands auto-dark-mode
;;   :init
;;   (add-hook 'after-init-hook #'auto-dark-mode)
;;   :config
;;   (setq auto-dark-dark-theme 'modus-vivendi
;;         auto-dark-light-theme 'modus-operandi
;;         auto-dark-allow-osascript t
;;         auto-dark-detection-method 'applescript)
;;   :diminish auto-dark-mode)

(use-package dimmer
  :ensure t
  :commands dimmer-mode
  :init
  (add-hook 'after-init-hook 'dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)

  ;; TODO: file upstream as a PR
  (advice-add 'frame-set-background-mode :after #'(lambda (&rest args) (dimmer-process-all)))

  ;; Don't dim in terminal windows. Even with 256 colours it can
  ;; lead to poor contrast.  Better would be to vary dimmer-fraction
  ;; according to frame type.
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))

(provide 'init-themes)
;;; init-themes.el ends here
