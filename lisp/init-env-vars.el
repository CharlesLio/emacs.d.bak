;;; init-env-vars.el --- Emacs lisp settings, and common config for other lisps  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; using function imported from dynamic module instead of package `exec-path-from-shell'
;;; try to load module chllib.so, fallback to systempath.so if failed
(if (module-load (site-lisp-dir-for "rs_module/rs_module.so"))
    (if-let* ((vars ["PATH" "CPATH" "GOPATH" "JAVA_HOME" "LEDGER_FILE" "JULIA_PKG_SERVER" "JULIA_DEPOT_PATH"])
              (env-vars-vector (rs-module/env-vars-from-shell vars)))
        (mapc #'(lambda (env-var)
                  (let ((var (car env-var))
                        (value (cdr env-var)))
                    (if (string-empty-p value)
                        (message (format "%s env variable not set..." var))
                      (setenv var value))
                    (when (string-equal var "PATH")
                      (setq-default eshell-path-env value)
                      (setq exec-path (append (parse-colon-path value) (list exec-directory))))))
              env-vars-vector))
  ;; fallback incase rs-module loading fail.
  (if (module-load (site-lisp-dir-for "systempath/systempath.so"))
      (when-let ((path (get-system-path)))
        (setenv "PATH" path)
        (setq-default eshell-path-env path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (user-error "Env variables were not configured")))

(provide 'init-env-vars)
;;; init-env-vars.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
