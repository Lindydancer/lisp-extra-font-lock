;; A plain function.
(defun my-function1 (arg)
  (let ((tmp 'value))
    nil))

;; &optional should not be affected, but subsequent arguments should
;; be highlighted.
(defun my-function2 (arg &optional arg2)
  nil)

;; Reserved words should be highlighted.
(defun my-function3 (arg mode-name)
  (let ((major-mode 'what)
        (my-major-mode 'better))
    nil))

;; `dolist'.
(defun my-dlist ()
  (dolist (arg '(1 2 3))
    nil))

;; `loop'.
(defun my-loop (my-list)
  (cl-loop for (key . value) in my-list
           collect key
           collect value))
