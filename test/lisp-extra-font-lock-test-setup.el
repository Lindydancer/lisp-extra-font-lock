;;; lisp-extra-font-lock-test-setup.el --- Setup and execute all tests.

;;; Commentary:

;; This package sets up a suitable enviroment for testing
;; lisp-extra-font-lock, and executes the tests.
;;
;; Usage:
;;
;;   emacs -q -l lisp-extra-font-lock-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.
;;
;; Note that different Emacs versions highlight things slightly
;; differently. The corresponding .faceup file was generated using
;; Emacs 24.3.

;;; Code:

(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)

(defvar lisp-extra-font-lock-test-setup-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".." "../../faceup"))
  (add-to-list 'load-path (concat lisp-extra-font-lock-test-setup-dir dir)))

;; Emacs 25.1 contains a bug which prevents if from highlighting
;; "lambda" as a keyword, this may cause false errors.
;;
;; See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23465
(when (and (eq emacs-major-version 25)
           (eq emacs-minor-version 1))
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\_<lambda\\_>" (0 font-lock-keyword-face)))))

(require 'lisp-extra-font-lock)
(require 'lisp-extra-font-lock-test-files)

(ert t)

;;; lisp-extra-font-lock-test-setup.el ends here
