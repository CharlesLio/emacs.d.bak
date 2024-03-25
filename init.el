;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Time-stamp:
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; (setq debug-on-quit t)
(setq use-short-answers t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin) "If running on Darwin (GNU-Darwin, macOS, ...).")
(defconst *is-a-linux* (eq system-type 'gnu/linux) "If running on GNU/Linux system.")
(defconst *is-a-winnt* (eq system-type 'windows-nt) "If running as native W32 application.")


;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            #'(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; silent or disable the warnings and errors from native compilation of elisp code
(setq native-comp-async-jobs-number (1- (num-processors 'all)))

(setq-default native-comp-async-report-warnings-errors 'silent) ;; nil or 'silent
;; (setq-default native-comp-driver-options (if *is-a-mac* '("-Wl,-w")))
(setq-default comp-libgccjit-reproducer nil)
(add-hook 'kill-emacs-hook 'native-compile-prune-cache)

;; recursively invoke minibuffer
(setq enable-recursive-minibuffers t)

(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;;; Set up correct exec-path etc from $PATH...
;; (require 'init-exec-path)

;;; setup correct env variables like PATH, LANG etc. using dynamic module.
;;; must come after `init-site-lisp'.
(require 'init-env-vars)

;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; user customize fonts
(when (display-graphic-p)
  (require 'init-fonts)
  (set-bilingual-font))

(require 'init-themes)

(require 'init-frame-hooks)
(require 'init-gui-frames)
;; (require 'init-xterm)
(when *is-a-mac*
  (require 'init-macos))

;; Load configs for specific features and modes
(require 'init-dired)
(require 'init-isearch)

(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-grep)
(require 'init-recentf)
(defvar inf-completion-library 'vertico "vertico vs ivy")
(pcase inf-completion-library
  ('vertico
   (require 'init-vertico)
   (require 'init-corfu))
  ('ivy
   (require 'init-smex)
   (require 'init-ivy)
   (require 'init-company)))

(require 'init-hippie-expand)

(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)

;; (require 'init-vc)
;; (require 'init-darcs) -
;; (require 'init-git)
;; (require 'init-github) -

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-org)
(require 'init-org-local)


;; (require 'init-nxml)
(require 'init-html)
;; (require 'init-css)
;; (require 'init-haml)
;; (require 'init-haskell)

(require 'init-http)
(require 'init-python)
;; (require 'init-elm)
;; (require 'init-sql)
(require 'init-rust)
(require 'init-toml)
(require 'init-scheme)
(require 'init-yaml)

(require 'init-c)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)

;; (require 'init-go) -
(require 'init-lua)

(require 'init-ocaml)

(require 'init-julia)

;; (require 'init-nim)
;; (require 'init-twitter)
;; (require 'init-mu)
(require 'init-ledger)
(require 'init-swift)
;; (require 'init-direnv)


;; Extra packages which don't require any configuration

;; (require-package 'sudo-edit)
;; ;; (require-package 'gnuplot)
;; (require-package 'htmlize)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 100)
  (add-hook 'after-init-hook #'(lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))
(global-eldoc-mode -1)

(require 'init-asymptote)

(require 'init-epub)


;; Allow access from emacsclient
(defun server-ensure-start ()
  (require 'server) (unless (server-running-p) (server-start)))
(add-hook 'after-init-hook #'server-ensure-start)


;; Variables configured via the interactive 'customize' interface
(if (file-exists-p custom-file) (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)


;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local)


(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (let ((ligatures-for-current-font
         (pcase (alist-get 'latin latin-cjk-fonts-alist)
           ("JetBrains Mono" '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
                               "***" ";;" ";;;" "!!" "??"  "???" "?:" "?."  "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<"
                               ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#"
                               "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>"
                               "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>"
                               "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>"
                               "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|"
                               "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".."  ".=" ".-" "..<" ".?" "::"
                               ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
           ("Iosevka Comfy" '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-" "=<<"
                              "=<" "=<=" "<==" "<===" "<<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>=" "<->" "<-->"
                              "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__" "<~~" "</" "</>" "/>" "~~>"
                              "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!=" "<:" ":=" "*=" "*+" "<*" "<*>"
                              "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>" "(*" "*)" "/*" "*/" "[|" "|]"
                              "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!--" "<=" ";;" ";;;" ":-" "<!---" "<~"
                              "~>" ":+" "+:" "-:" "<******>"))
           ("Victor Mono" '("</" "</>" "/>" "~-" "-~" "~@" "<~" "<~>" "<~~" "~>" "~~" "~~>" ">=" "<=" "<!--" "##" "###"
                            "####" "|-" "-|" "|->" "<-|" ">-|" "|-<" "|=" "|=>" ">-" "<-" "<--" "-->" "->" "-<" ">->"
                            ">>-" "<<-" "<->" "->>" "-<<" "<-<" "==>" "=>" "=/=" "!==" "!=" "<==" ">>=" "=>>" ">=>"
                            "<=>" "<=<" "=<=" "=>=" "<<=" "=<<" ".-" ".=" "=:=" "=!=" "==" "===" "::" ":=" ":>" ":<"
                            ">:" "<:" "<|" "<|>" "|>" "<>" "<$" "<$>" "$>" "<+" "<+>" "+>" "?=" "/=" "/==" "/\\" "\\/"
                            "__" "&&" "++" "+++" ";;"))
           ("Cascadia Code" '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>" ":::" "::=" "=:=" "===" "==>"
                              "=!=" "=>>" "=<<" "=/=" "!==" "!!."  ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                              "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-" "<<<"
                              "<+>" "</>" "###" "#_(" "..<" "..."  "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~=" "~>"
                              "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>"
                              "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:" "<$"
                              "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##" "#(" "}#" "#?" "#_" "%%" ".="
                              ".-" ".." ".?" "+>" "++" "?:" "%%" "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "/*"
                              "|||" "(*" "*)" "\\\\" "://" "<-----" "<-----|" "|----->" "----->" "<<----" "<<----|"
                              "|---->>" "---->>" "___|___" "#####"))
           (_ nil))))
    (ligature-set-ligatures 'prog-mode ligatures-for-current-font))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'
  (global-ligature-mode t))


(provide 'init)
;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; fill-column: 120
;; End:
