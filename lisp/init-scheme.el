;;; package --- scheme init -*- mode: emacs-lisp; -*-
;;; Time-stamp: <2023-08-10 10:32:52 charles>
;;; Commentary:
;;; Code:
;;; --------------------------------------------------------------------------------
(use-package geiser
  :ensure t
  :custom
  (geiser-active-implementations '(guile chez racket mit chibi gambit)))

(use-package geiser-racket :ensure t)
(use-package geiser-guile :ensure t)
(use-package geiser-mit :ensure t)
(use-package geiser-gauche :ensure t)

(use-package geiser-gambit
  :ensure t
  :custom (geiser-gambit-binary (executable-find "gambit-gsi")))

(use-package geiser-chez
  :ensure t
  :custom (geiser-chez-binary '("chez" "petite")))

(use-package geiser-chibi :ensure t)
(use-package geiser-chicken :ensure t)
(use-package macrostep-geiser :ensure t)

(provide 'init-scheme)

;;; init-scheme.el ends here
;; Local Variables:
;; coding: utf-8-unix
;; End:
