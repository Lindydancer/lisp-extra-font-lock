;;; list-extra-font-lock.el --- Highlight bound variables and quoted exprs.

;; Copyright (C) 2014-2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; Created: 2014-11-22
;; Version: 0.0.0
;; URL: https://github.com/Lindydancer/lisp-extra-font-lock

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package highlight bound variables and quoted expressions in
;; lisp code.
;;
;; The following screenshot demonstrates the highlighting effect of
;; this package:
;;
;; ![See doc/demo.png for screenshot](doc/demo.png)

;; What is highlighted:
;;
;; * Parameters in functions and lambdas
;;
;; * Variables bound by `let' and `dolist'
;;
;; * Quoted expressions
;;
;; * Backquoted expressions. However, subexpressions using the "," or
;;   ",@" are not highlighted. Also, the actual backquote and the
;;   comma operators are highlighted as a warning.

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'lisp-extra-font-lock-mode)
;;    (lisp-extra-font-lock-global-mode 1)

;; Customization:
;;
;; You can modify the following lists to add more functions that are
;; recognized:
;;
;; * `lisp-extra-font-lock-let-functions' -- List of function with the
;;   same syntax as `let'.
;;
;; * `lisp-extra-font-lock-defun-functions' -- List of function with
;;   the same syntax as `defun'.
;;
;; * `lisp-extra-font-lock-lambda-functions' -- List of function with
;;   the same syntax as `lambda'.
;;
;; * `lisp-extra-font-lock-dolist-functions' -- List of function with
;;   the same syntax as `dolist'.
;;
;; * `lisp-extra-font-lock-bind-first-functions' -- List of function
;;   that bind their first argument, like `condition-case'.
;;
;; The following faces are used when highlighting. You can either
;; redefine the face (e.g. using a theme), or you can rebind the
;; corresponding variable.
;;
;; * Variables are highlighted using the standard face
;;   `font-lock-variable-name-face'.
;;
;; * Quoted expressions use the face bound to the variable
;;   `lisp-extra-font-lock-quoted-face' (by default
;;   `lisp-extra-font-lock-quoted', which inherits from
;;   `font-lock-constant-face'.)
;;
;; * The backquote and comma operators use the face bound to the
;;   variable `lisp-extra-font-lock-backquote-face' (by default
;;   `lisp-extra-font-lock-backquote', which inherits from
;;   `font-lock-warning-face'.)
;;
;; Example:
;;
;; To set the face used to highlight quoted expressions to a gray
;; color, you can use:
;;
;;     (custom-set-faces
;;       '(lisp-extra-font-lock-quoted ((t :foreground "grey50"))))

;;; Code:

;; ------------------------------
;; Customizable variables
;;


(defgroup lisp-extra-font-lock nil
  "Highlight bound variables and quoted expressions in lisp."
  :group 'faces)


;;;###autoload
(defcustom lisp-extra-font-lock-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'lisp-extra-font-lock)


;; ----------
;; Faces and corresponding variable.
;;

(defface lisp-extra-font-lock-backquote
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight backquotes and the comma operator."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-backquote-face 'lisp-extra-font-lock-backquote
  "The face used to highlight backquotes and the comma operator.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-quoted
  '((t :inherit font-lock-constant-face))
  "The default face used to highlight quoted expressions."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-face 'lisp-extra-font-lock-quoted
  "The face used to highlight quoted expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


;; ----------
;; Function lists
;;

(defcustom lisp-extra-font-lock-let-functions
  '("let"
    "let*"
    "lexical-let"
    "lexical-let*")
  "List of function using same syntax as `let' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-defun-functions
  '("defun"
    "defun*"
    "defmacro"
    "defmacro*")
  "List of function using same syntax as `defun' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-lambda-functions
  '("lambda")
  "List of function using same syntax as `lambda' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-dolist-functions
  '("dolist"
    "cl-dolist")
  "List of function using same syntax as `dolist' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-bind-first-functions
  '("condition-case")
  "List of function that bind their first argument."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


;; ------------------------------
;; The modes
;;

;;;###autoload
(define-minor-mode lisp-extra-font-lock-mode
  "Minor mode that highlights bound variables and quoted expressions in lisp."
  :group 'lisp-extra-font-lock
  (if lisp-extra-font-lock-mode
      (lisp-extra-font-lock-add-keywords)
    (lisp-extra-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode lisp-extra-font-lock-global-mode
  lisp-extra-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p lisp-extra-font-lock-modes)
      (lisp-extra-font-lock-mode 1)))
  :group 'lisp-extra-font-lock)


(defvar lisp-extra-font-lock-keywords
  `(;; Function and lambda parameters
    (,(concat "("
              "\\(?:"
              (regexp-opt lisp-extra-font-lock-defun-functions)
              "[ \t]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              (regexp-opt lisp-extra-font-lock-lambda-functions)
              "\\)"
              "[ \t]+(")
     (lisp-extra-font-lock-match-argument-list
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      nil
      (0 font-lock-variable-name-face nil t)))
    ;; Variables bound by `let'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-let-functions)
              "[ \t]+(")
     (lisp-extra-font-lock-match-let
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 font-lock-variable-name-face)))
    ;; Loop variables.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-dolist-functions)
              "[ \t]+(\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 font-lock-variable-name-face))
    ;; Bind first argument like `condition-case'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-bind-first-functions)
              "[ \t]+\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 (and (not (string= (match-string 1) "nil"))
             font-lock-variable-name-face)))
    (;; Quote and backquote.
     ;;
     ;; Matcher: Set match-data 1 if backquote.
     lisp-extra-font-lock-match-quote-and-backquote
     (1 lisp-extra-font-lock-backquote-face nil t)
     (;; Submatcher, match part of quoted expression or comma.
      lisp-extra-font-lock-match-quoted-content
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (lisp-extra-font-lock-end-position))
      ;; Post-match form
      (goto-char (match-end 0))
      ;; Highlight rules for submatcher.
      (1 lisp-extra-font-lock-quoted-face append)
      (2 lisp-extra-font-lock-backquote-face nil t)))))


(defun lisp-extra-font-lock-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (setq font-lock-multiline t)
  (font-lock-add-keywords
   nil
   lisp-extra-font-lock-keywords
   'append))


(defun lisp-extra-font-lock-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil lisp-extra-font-lock-keywords))


;; ----------------------------------------
;; Matcher functions
;;

(defun lisp-extra-font-lock-end-position ()
  "Suitable end position of expression after point.
If expression is open-ended, the beginning of the next top-level
form is used, or `point-max' if none is found."
  (save-match-data
    (save-excursion
      (or (condition-case nil
              (progn
                (forward-sexp)
                (point))
            (error nil))
          (and (re-search-forward "^(" nil t)
               (match-beginning 0))
          (point-max)))))

(defun lisp-extra-font-lock-match-argument-list (limit)
  (forward-comment (buffer-size))
  (and (< (point) limit)
       (let ((res (looking-at "\\_<\\(?:\\sw\\|\\s_\\)+\\_>")))
         (when res
           (goto-char (match-end 0)))
         res)))


(defun lisp-extra-font-lock-match-let (limit)
  "Match next variable introduced by `let'-like constructs."
  (forward-comment (buffer-size))
  (let ((p (point)))
    (cond ((eq (following-char) ?\( )
           ;; Match "(var initial-valoue)"
           (forward-char)
           (forward-comment (buffer-size))
           (and
            (< (point) limit)
            (let ((res (looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")))
              (when res
                (goto-char p)
                (condition-case nil
                    (forward-sexp)
                  (error (goto-char limit))))
              res)))
          ((looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")
           ;; Match "var"
           (goto-char (match-end 0))
           (<= (point) limit))
          (t
           nil))))


(defun lisp-extra-font-lock-is-in-comment-or-string ()
  "Return non-nil if point is in comment or string.

This assumes that Font Lock is active and has fontified comments
and strings."
  (let ((props (text-properties-at (point)))
        (faces '()))
    (while props
      (let ((pr (pop props))
            (value (pop props)))
        (if (eq pr 'face)
            (setq faces value))))
    (unless (listp faces)
      (setq faces (list faces)))
    (or (memq 'font-lock-comment-face faces)
        (memq 'font-lock-string-face faces)
        (memq 'font-lock-doc-face faces))))


(defun lisp-extra-font-lock-match-quote-and-backquote (limit)
  "Search for quote and backquote in in code.
Set match data 1 if character matched is backquote."
  (let (res)
    (while (progn (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
                  (and res
                       (or
                        (lisp-extra-font-lock-is-in-comment-or-string)
                        ;; Don't match ?' and ?`.
                        (eq (char-before (match-beginning 0)) ??)))))
    res))


(defun lisp-extra-font-lock-match-quoted-content (limit)
  "Match next part of a quoted content.

Match up to next comma operator, or to the end of the quoted expression."
  (and (< (point) limit)
       (let ((p (point))
             res)
         (while (progn
                  (setq res (re-search-forward "\\(,@?\\|[`']\\)" limit t))
                  (and res
                       (or
                        (lisp-extra-font-lock-is-in-comment-or-string)
                        ;; Don't match ?<char>
                        (eq (char-before (match-beginning 0)) ??)))))
         (if res
             ;; Match up to next quoted subpart or comma operator.
             (let ((is-comma (eq (char-after (match-beginning 0)) ?,)))
               (set-match-data (list
                                ;; Match data 0: Full match.
                                p (match-end 0)
                                ;; Match data 1: Part of the quoted expression
                                p
                                (match-beginning 0)
                                ;; Match data 2; Comma operator (if present)
                                (and is-comma (match-beginning 0))
                                (and is-comma (match-end 0))))
               (condition-case nil
                   (forward-sexp)
                 (error (goto-char limit))))
           ;; Match to the end of the quoted expression.
           (set-match-data (list p limit
                                 p limit))
           (goto-char limit))
         t)))


(provide 'lisp-extra-font-lock)

;;; lisp-extra-font-locks.el ends here.
