;;; init-osx-keys.el --- Configuration specific to macOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; remap special keys on macOS.
(setq
 ns-use-proxy-icon nil ;;; do not display icon in the title bar
 ns-use-native-fullscreen t
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;; Make mouse wheel / trackpad scrolling less jerky
 ns-command-modifier 'super
 ns-option-modifier 'meta
 ns-function-modifier 'hyper)

(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

(when (display-graphic-p)
  (if (featurep 'ns) ;; check if ns-win feature was compiled with Emacs
      (progn
        (keymap-global-set "M-`" 'ns-next-frame)
        (keymap-global-set "s-h" 'ns-do-hide-emacs)
        (keymap-global-set "M-s-h" 'ns-do-hide-others)
        (keymap-global-set "C-s-f" 'toggle-frame-fullscreen)
        (after-load 'nxml-mode (define-key nxml-mode-map (kbd "M-h") nil)))
    ;; if ns-win was unavailable, use Applescript to emulate
    (use-package osx-lib
      :ensure t
      :custom
      (osx-lib-debug-level nil "Debug level for osx-lib")
      :config
      (if osx-lib-debug-level (get-buffer-create "*OsaScript*"))
      (defun mac-do-hide-others ()
        "Hide all applications other than Emacs"
        (interactive)
        (osx-lib-run-osascript (concat "tell application \"System Events\" to "
                                       "set visible of every process whose visible is true "
                                       "and name is not \"Emacs\" and frontmost is false to false")))
      (defun mac-do-hide ()
        "Hide Emacs GUI application"
        (interactive)
        (osx-lib-run-osascript (concat "tell application \"System Events\" to "
                                       "set visible of process \"Emacs\" to false")))
      :bind
      (("s-h" . mac-do-hide)
       ("M-s-h" . mac-do-hide-others))))

  (keymap-global-set "s-m" 'iconify-or-deiconify-frame))


;; make sure to install location utilities
;; `brew install locateme' / "brew install --cask corelocationcli"
(defun get-geo-location-by (location-util)
  "Get current device geo location under macOS using CLI utility."
  (if-let* ((output-string (pcase location-util
                             ('locateme (shell-command-as-string "locateme -f \"{LAT} {LON}\""))
                             ('corelocationcli (shell-command-as-string "corelocationcli"))
                             (_ (user-error "brew install locateme/corelocationcli"))))
            (output (mapcar 'string-to-number (split-string output-string)))
            (latitude (car output))
            (longitude (cdr output)))
      (unless (or (eql latitude 0) (eql longitude 0))
        `((lat . ,(car output)) (lon . ,(cadr output))))))

(defun macos-location-update ()
  "Update `calendar-longitude', `calendar-latitude' and `calendar-location-name' manually"
  (interactive)
  (when-let (geo-location (get-geo-location-by 'corelocationcli))
    (setq calendar-longitude (alist-get 'lon geo-location))
    (setq calendar-latitude (alist-get 'lat geo-location))
    (setq calendar-location-name (format "(%s, %s)" calendar-latitude calendar-longitude))))

(macos-location-update)

(if (file-exists-p "/Applications/Alfred.app")
    (defun alfred-search (b e)
      "Activate Alfred with the selected text."
      (interactive "r")
      (do-applescript
       (format "tell application \"Alfred 4\" to search \"%s\""
               ;; In AppleScript String, " and \ are speical characters
               (mapconcat
                (lambda (char)
                  (pcase char
                    (?\" (string ?\\ ?\"))
                    (?\\ (string ?\\ ?\\))
                    (_   (string char))))
                (buffer-substring b e) "")))))


;;; packages to work with macOS
;; keybindings for osx-dictionary package, notice that C-s-d will bring up the
;; general macOS word searching at point.
(use-package chinese-word-at-point :ensure t)

;;; make sure install 结巴分词 with `shell-command' `python3 -m pip install jieba'
(use-package osx-dictionary
  :ensure t
  :custom
  (osx-dictionary-use-chinese-text-segmentation t "enable Chinese text segmentation")
  :bind
  (("C-s-s" . osx-dictionary-search-pointer)
   ("C-s-i" . osx-dictionary-search-input)))

(provide 'init-macos)
;;; init-osx-keys.el ends here
