;;; demo.el --- Demonstration program for `lisp-extra-font-lock'.

;; The package `lisp-extra-font-lock' highlights variables bound by
;; `defun', `lambda', `let', and `dolist'. It also highlight quoted
;; and backquoted expressions, excluding any comma operator
;; expressions.

(defun my-function (alpha beta)
  (let ((gamma (lambda (lst) (+ lst 1)))
        delta)
    (dolist (entry '(one two three))
      (do-something entry))
    (if (eq alpha beta)
        'equal
      `(not-equal ,alpha ,beta))))

;;; demo.el ends here
