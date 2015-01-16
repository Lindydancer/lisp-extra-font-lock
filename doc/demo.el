;;; demo.el --- Demonstration program for `lisp-extra-font-lock'.

;; The package `lisp-extra-font-lock' highlights variables bound by
;; `defun', `lambda', `let', `dolist', etc. It also highlight quoted
;; and backquoted expressions -- excluding any comma operator
;; expressions.

(defun my-function (next)                          ; <- Parameters
  (let ((numbers '(one two three))                 ; <- `let' and quoted expr
        (buffer-read-only t))                      ; <- Special variable
    `(,@numbers and ,next)))                       ; <- Backquote and comma

;; (my-function 'four) => (one two three and four)

;;; demo.el ends here
