;;; init-common-lisp.el --- Common Lisp support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See http://bc.tech.coop/blog/070927.html
(add-auto-mode 'lisp-mode "\\.cl\\'")
(add-hook 'lisp-mode-hook
          #'(lambda () (unless (featurep 'slime) (require 'slime) (normal-mode))))

(with-eval-after-load 'slime
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl" "--core" "/Users/charles/.slime/sbcl.core-for-slime")
                        :coding-system utf-8-unix))
    (setq inferior-lisp-program "sbcl"))
  (when (executable-find "clisp")
    (add-to-list 'slime-lisp-implementations
                 '(clisp ("clisp" "-I") :coding-system iso-latin-1-unix)))
  (when (executable-find "ccl64")
    (add-to-list 'slime-lisp-implementations
                 '(ccl64 ("ccl64") :coding-system utf-8-unix)))
  (setq slime-default-lisp 'sbcl))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searche lispdoc.com for SYMBOL, which is by default the symbol currently under the curser."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here
