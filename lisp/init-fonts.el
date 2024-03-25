;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)

(when (featurep 'ns)
  (setq ns-use-thin-smoothing t))


;; apply font setup under GUI

(defvar latin-cjk-fonts-alist
  (cond
   ;; Latin and 中日韓/CJK fonts
   ;; (*is-a-mac* '((latin . "Iosevka Comfy") (cjk . "PingFang SC") (height . 150) (cjk-size . 14)))
   ;; (*is-a-mac* '((latin . "Iosevka Comfy") (cjk . "lXGW WenKai Mono") (height . 150) (cjk-size . 14)))
   ;; (*is-a-mac* '((latin . "Victor Mono") (cjk . "Sarasa Mono SC") (height . 140) (weight . semi-bold)))
   (*is-a-mac* '((latin . "Lilex") (cjk . "Sarasa Mono SC") (height . 140) (weight . normal)))
   ;; (*is-a-mac* '((latin . "Anka/Coder Condensed") (cjk . "LXGW WenKai Mono") (height . 150) (cjk-size . 15)))
   ;; (*is-a-mac* '((latin . "Anka/Coder Narrow") (cjk . "PingFang SC") (height . 150) (cjk-size . 14)))
   ;; (*is-a-mac* '((latin . "JetBrains Mono") (cjk . "Noto Sans SC") (height . 130) (weight . normal)))
   ;; (*is-a-mac* '((latin . "IBM Plex Mono") (cjk . "Noto Sans SC") (height . 12) (cjk-size . 13)))
   (*is-a-linux* '((latin . "Ubuntu Mono") (cjk . "WenQuanYi Zen Hei")))
   (*is-a-winnt* '((latin . "Consolas") (cjk . "Microsoft YaHei")))
   (t nil))
  "User defined latin and CJK fonts pair, with optional font properties.")

(defun set-bilingual-font (&optional height)
  "Set both latin and CJK fonts under GUI respectively with font `height', default to 130."
  (interactive)
  (when-let* (;; Latin, Greek, Cyrillic
              (latin-font (alist-get 'latin latin-cjk-fonts-alist))
              (cjk-font (alist-get 'cjk latin-cjk-fonts-alist))
              (latin-font-size (or height (alist-get 'height latin-cjk-fonts-alist 130)))
              ;; CJK
              (cjk-font-size (alist-get 'cjk-size latin-cjk-fonts-alist (/ latin-font-size 10)))
              (latin-font-weight (alist-get 'weight latin-cjk-fonts-alist 'normal))
              (cjk-font-weight (alist-get 'cjk-weight latin-cjk-fonts-alist 'normal))
              (mode-line-font "Iosevka Aile"))
    ;; Latin character font, better than calling `set-frame-font' like
    ;; (set-frame-font (format "%s-%d" (car latin-cjk-fonts) (car font-sizes)) t t nil)
    (set-face-attribute 'default nil :family latin-font :height latin-font-size :weight latin-font-weight)
    (set-face-attribute 'fixed-pitch nil :family latin-font :height latin-font-size :weight latin-font-weight)
    ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
    ;; (set-face-attribute 'mode-line nil :family "Iosevka Aile")

    ;;; Specific faces
    (let ((faces '(mode-line mode-line-active mode-line-inactive mode-line-buffer-id mode-line-emphasis
                             mode-line-highlight variable-pitch)))
      (mapc (lambda (face) (set-face-attribute face nil :font mode-line-font :height 140)) faces))

    ;; emoji
    (set-fontset-font (frame-parameter nil 'font) 'greek (font-spec :family latin-font))
    ;; (set-fontset-font (frame-parameter nil 'font) 'symbol (font-spec :family "Symbola"))
    (set-fontset-font (frame-parameter nil 'font) 'emoji (font-spec :family "Symbola"))

    ;; CJK font中文
    ;; TC: 「憂鬱的臺灣烏龜」 注音符號ㄅㄆㄇㄈ
    ;; SC: 忧郁的台湾乌龟
    ;; JP: 『源氏物語』（げんじものがたり）ひらがな カタカナ
    ;; 魅惑の文字にみせられて
    (dolist (charset '(han cjk-misc kana hangul))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family cjk-font :size cjk-font-size :weight cjk-font-weight)))
    ))

;; (use-package font-utils :ensure t)
;; (use-package ucs-utils :ensure t)
;; (use-package list-utils :ensure t)

;; (use-package unicode-fonts
;;   :ensure t
;;   :demand t
;;   :custom
;;   (unicode-fonts-block-font-mapping '(
;;                                       ("Bopomofo" ("PingFang SC"))
;;                                       ("CJK Compatibility" ("PingFang SC"))
;;                                       ("Emoticons" ("Symbola" "Segoe UI Symbol"))
;;                                       ("Hiragana" ("Noto Sans CJK JP" "Klee" "Klee One"))
;;                                       ("Katakana" ("Noto Sans CJK JP" "Klee" "Klee One"))
;;                                       ("Mathematical Operators" ("Noto Sans Symbols" "Symbola"))
;;                                       ("Greek and Coptic" ("Anka/Coder Condensed" "Iosevka Comfy"))
;;                                       ))
;;   (unicode-fonts-skip-font-groups
;;    '(chinese-simplified chinese-traditional chinese-hanja chinese-kanji decorative low-quality-glyphs))
;;   :config
;;   (unicode-fonts-setup))

(provide 'init-fonts)
;;; init-fonts.el ends here
