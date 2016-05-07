;;; lisp-extra-font-lock-test-files.el --- Test for lisp-extra-font-lock.

;; Copyright (C) 2016 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Regression test of `lisp-extra-font-lock', a package providing
;; additional font-lock rules for lisp languages. This module verifies
;; fontification of a number of files. This is done by keeing a text
;; representation of the fontification using `faceup' markup, in
;; addition to the original files.
;;
;; The actual check is performed using `ert', with font-lock test
;; function provided by `faceup'.

;;; Code:

(require 'faceup)

(defvar lisp-extra-font-lock-test-dir (faceup-this-file-directory))

(defun lisp-extra-font-lock-test-file (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (faceup-test-font-lock-file 'emacs-lisp-mode
                              (concat
                               lisp-extra-font-lock-test-dir
                               file)))
(faceup-defexplainer lisp-extra-font-lock-test-file)


(ert-deftest lisp-extra-font-lock-test-files ()
  (should (lisp-extra-font-lock-test-file "files/demo.el"))
  (should (lisp-extra-font-lock-test-file "files/hash-quote.el")))

(provide 'lisp-extra-font-lock-test-files)

;; lisp-extra-font-lock-test-files.el ends here.
