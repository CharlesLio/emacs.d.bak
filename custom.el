(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "mbp2020") tramp-connection-local-darwin-ps-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
                                          (egid . number) (comm . 52) (state . 5) (ppid . number)
                                          (pgrp . number) (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number) (nice . number)
                                          (vsize . number) (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string) (group . string)
                                          (comm . 52) (state . 5) (ppid . number) (pgrp . number)
                                          (ttname . string) (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
                                          (egid . number) (group . string) (comm . 52)
                                          (state . string) (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number) (time . tramp-ps-time)
                                          (pri . number) (nice . number) (vsize . number)
                                          (rss . number) (etime . number) (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(package-selected-packages
   '(add-node-modules-path ag aggressive-indent alert all-the-icons-completion all-the-icons-dired
                           anaconda-mode anzu auto-compile auto-dark auto-package-update avy beacon
                           browse-at-remote browse-kill-ring buffer-move cal-china-x cape cask-mode
                           cdlatex celestial-mode-line chinese-conv chinese-number
                           chinese-word-at-point citar-org-roam clang-capf clang-format
                           clang-format+ command-log-mode consult corfu crontab-mode crux csv-mode
                           dash-at-point diff-hl diminish dimmer dired-single diredfl disable-mouse
                           eglot-jl elisp-format elisp-slime-nav elm-mode elm-test-runner
                           expand-region f flycheck flycheck-color-mode-line flycheck-elm
                           flycheck-ledger flycheck-package flycheck-plantuml flycheck-relint
                           flycheck-yamllint fringe-current-line fullframe geiser geiser-chez
                           geiser-chibi geiser-chicken geiser-gambit geiser-gauche geiser-guile
                           geiser-mit geiser-racket gerser-gambit git-blamed git-commit
                           git-timemachine gnu-elpa-keyring-update goto-line-preview grab-mac-link
                           helpful highlight-escape-sequences highlight-quoted hippie-expand-slime
                           httprepl ibuffer-projectile ibuffer-vc immortal-scratch info-colors
                           ipretty julia-mode ledger-mode ligature list-unicode-display lsp-mode
                           lsp-ui lua-mode macrostep macrostep-geiser magit magit-todos marginalia
                           markdown-mode mmm-mode mode-line-bell modern-fringes move-dup nov
                           orderless org-cliplink org-download org-modern org-pomodoro org-roam
                           org-roam-timestamps org-roam-ui origami osx-dictionary ox-pandoc
                           page-break-lines pip-requirements plantuml-mode projectile pyvenv
                           pyvenv-auto quelpa-use-package rainbow-delimiters rainbow-mode
                           reformatter regex-tool restclient rg rustic seq slime smart-cursor-color
                           sqlformat svg-mode-line-theme swift-mode switch-window symbol-overlay
                           system-packages tagedit textile-mode timu-caribbean-theme toml-mode
                           unfill uptimes vertico vlf wgrep wgrep-ag which-key
                           whitespace-cleanup-mode whole-line-or-region writeroom-mode yaml-mode
                           yasnippet))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
