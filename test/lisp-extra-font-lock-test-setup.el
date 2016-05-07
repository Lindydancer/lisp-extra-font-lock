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

(require 'lisp-extra-font-lock)
(require 'lisp-extra-font-lock-test-files)

(lisp-extra-font-lock-global-mode 1)

(ert t)

;;; lisp-extra-font-lock-test-setup.el ends here
