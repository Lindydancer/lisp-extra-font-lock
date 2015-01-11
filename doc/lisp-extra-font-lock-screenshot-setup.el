;; lisp-extra-font-lock-screenshot-setup.el --- prepare Emacs for screenshot.

;; Usage:
;;
;;   emacs -q -l lisp-extra-font-lock-screenshot-setup.el
;;
;;   Take screenshot. OS X: Cmd-Shift-4 SPC click on window.

(setq inhibit-startup-screen t)

(blink-cursor-mode -1)

(defvar lisp-extra-font-lock-screenshot-dir
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory))

(load (concat lisp-extra-font-lock-screenshot-dir
              "../lisp-extra-font-lock.el"))
(lisp-extra-font-lock-global-mode 1)
(find-file (concat lisp-extra-font-lock-screenshot-dir "demo.el"))

(set-frame-size (selected-frame) 80 20)

(message "")

;; lisp-extra-font-lock-screenshot-setup.el ends here
