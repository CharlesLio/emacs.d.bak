;;; package --- init-mail  -*- mode: emacs-lisp; -*-
;;; Time-stamp: <2022-11-23 13:43:20 charles>
;;; Commentary:

;;; Code:
(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)
(setq ps-paper-type 'a4
      ps-line-number t
      ;; ps-font-family 'Anka/Coder
      ps-font-family 'Courier
      ps-header-font-size 15
      ps-print-color-p nil)

(provide 'init-mail)
;;; init-mail.el ends here
;; Local Variables:
;; coding: utf-8-unix
;; End:
