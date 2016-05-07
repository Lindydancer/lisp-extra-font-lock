;; A #' should not trigger the normal quoting system.

;; Special case for hash-quoted symbols.
#'symbol

;; Ensure the lambda isn't highlighted as a constant.
(mapcar #'(lambda (x) (+ x 1)) '(1 2 3 4))

;; Embedded hash-quoted lambda.
'(alpha #'(lambda (x)))
