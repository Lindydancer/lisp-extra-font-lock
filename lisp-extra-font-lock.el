;;; lisp-extra-font-lock.el --- Highlight bound variables and quoted exprs.

;; Copyright (C) 2014-2018 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; Created: 2014-11-22
;; Version: 0.0.6
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

;; This package highlight the location where local variables is
;; created (bound, for example by `let') as well as quoted and
;; backquoted constant expressions.

;; Example:
;;
;; Below, `^' is used indicate highlighted normal variables and
;; constant expressions. `*' is used to show highlighting of special
;; variables (i.e. those defined by `defvar') and of the backquote and
;; comma operators.
;;
;; (defun my-function (next)
;;                     ^^^^             <- Parameters
;;   (let ((numbers '(one two three))
;;          ^^^^^^^  ^^^^^^^^^^^^^^^    <- Var bound by `let' and quoted expr.
;;         (buffer-read-only t))
;;          ****************            <- Special variable (different color)
;;     `(,@numbers and ,next)))
;;     *^**        ^^^ *    ^           <- Backquote and comma
;;
;; Screenshot:
;;
;; ![See doc/demo.png for screenshot](doc/demo.png)

;; What is highlighted:
;;
;; * Parameters in functions and lambdas
;;
;; * Variables bound by specal constructs like `let', `dolist',
;;   `condition-case', and `pcase-let'
;;
;; * Normal variables and variables declared as globals using `defvar'
;;   are highlighted in different colors, as a warning
;;
;; * Quoted expressions
;;
;; * Backquoted expressions. Subexpressions using the "," or ",@" are
;;   not highlighted (as they are evaluted and thus not constant).
;;   Also, the backquote and the comma operators themselves are
;;   highlighted using a bright color as a warning.
;;
;; * Hash-quoted symbols.

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'lisp-extra-font-lock)
;;    (lisp-extra-font-lock-global-mode 1)

;; Customization:
;;
;; You can modify the following lists to add more functions that are
;; recognized:
;;
;; * `lisp-extra-font-lock-let-functions' -- List of function with the
;;   same syntax as `let'
;;
;; * `lisp-extra-font-lock-defun-functions' -- List of function with
;;   the same syntax as `defun'
;;
;; * `lisp-extra-font-lock-lambda-functions' -- List of function with
;;   the same syntax as `lambda'
;;
;; * `lisp-extra-font-lock-dolist-functions' -- List of function with
;;   the same syntax as `dolist'
;;
;; * `lisp-extra-font-lock-bind-first-functions' -- List of function
;;   that bind their first argument, like `condition-case'.
;;
;; * `lisp-extra-font-lock-loop-functions' -- List of functions with
;;   the same syntax as `cl-loop'.
;;
;; The following faces are used when highlighting. You can either
;; redefine the face (e.g. using a theme), or you can rebind the
;; corresponding variable.
;;
;; * Local variables are highlighted using the standard face
;;   `font-lock-variable-name-face'
;;
;; * Special (global) variables that are rebound are highlighted using
;;   the face bound to the variable
;;   `lisp-extra-font-lock-special-variable-name-face' (by default
;;   `lisp-extra-font-lock-special-variable-name', which inherits from
;;   `font-lock-warning-face')
;;
;; * Quoted expressions use the face bound to the variable
;;   `lisp-extra-font-lock-quoted-face' (by default
;;   `lisp-extra-font-lock-quoted', which inherits from
;;   `font-lock-constant-face')
;;
;; * The backquote and comma operators use the face bound to the
;;   variable `lisp-extra-font-lock-backquote-face' (by default
;;   `lisp-extra-font-lock-backquote', which inherits from
;;   `font-lock-warning-face').
;;
;; * Named arguments to `cl-loop' are highlighted using
;;   `font-lock-builtin-face'.
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


(defface lisp-extra-font-lock-quoted-function
  '((t :inherit font-lock-function-name-face))
  "The default face used to highlight #'-quoted function symbols."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-function-face
  'lisp-extra-font-lock-quoted-function
  "The face used to highlight #'-quoted function symbols.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-special-variable-name
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight special variables bound by `let'."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-special-variable-name-face
  'lisp-extra-font-lock-special-variable-name
  "The face used to highlight special variables bound by `let'.

A special variable is a global variable defined by `defvar'. See
`special-variable-p' for details.

To disable this highlighting, set this to nil. To highlight
special variables like plain variables, set this to
`font-lock-variable-name-face'."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


;; ----------
;; Function lists
;;

(defcustom lisp-extra-font-lock-let-functions
  '("let"
    "let*"
    "letf"
    "letf*"
    "lexical-let"
    "lexical-let*"
    "multiple-value-bind"
    "pcase-let"                         ; Highlights entire UPAT:s.
    "pcase-let*"
    "cl-letf"
    "cl-letf*"
    "cl-multiple-value-bind")
  "List of function using same syntax as `let' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-defun-functions
  '("defun"
    "defun*"
    "defmacro"
    "defmacro*"
    "defsubst"
    "cl-defun"
    "cl-defmacro"
    "cl-defsubst")
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
    "dotimes"
    "cl-dolist"
    "cl-dotimes")
  "List of function using same syntax as `dolist' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-bind-first-functions
  '("condition-case")
  "List of function that bind their first argument."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-loop-functions
  '("loop"
    "cl-loop")
  "List of functions using same syntax as `loop' to bind variables.."
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


(defun lisp-extra-font-lock-variable-face-form (name)
  "A form suitable for a font-lock face expression.

NAME is a form that should evalute to the name of the symbol, as a string."
  `(if (ignore-errors (let ((symbol (intern-soft ,name)))
                        (and symbol
                             (special-variable-p symbol))))
       lisp-extra-font-lock-special-variable-name-face
     font-lock-variable-name-face))

(defun lisp-extra-font-lock-keywords ()
  "Font-lock keywords used by `lisp-extra-font-lock'.
The keywords highlight variable bindings and quoted expressions."
  `(;; Function and lambda parameters
    (,(concat "("
              "\\(?:"
              (regexp-opt lisp-extra-font-lock-defun-functions)
              "[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              (regexp-opt lisp-extra-font-lock-lambda-functions)
              "\\)"
              "[ \t\n]+(")
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
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0))
         nil t)))
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
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0)))))
    ;; Variables bound by `cl-dolist' etc.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-dolist-functions)
              "[ \t]+(\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
    ;; Bind first argument like `condition-case'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-bind-first-functions)
              "[ \t]+\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 (and (not (string= (match-string 1) "nil"))
             ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))
    ;; Bind variables and named arguments to `cl-loop'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-loop-functions)
              "\\_>")
     (lisp-extra-font-lock-match-loop-keywords
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        (save-excursion
          (goto-char (match-beginning 0))
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form.
      (goto-char (match-end 0))
      (1 font-lock-builtin-face)
      (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2)) nil t)))
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
      (2 lisp-extra-font-lock-backquote-face nil t)))
    ;; Function read syntax
    ("#'\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
     1 lisp-extra-font-lock-quoted-function-face)))


(defvar lisp-extra-font-lock--installed-keywords nil)

(defun lisp-extra-font-lock-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'lisp-extra-font-lock--installed-keywords)
    (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))
  (let ((keywords (lisp-extra-font-lock-keywords)))
    (set (make-local-variable 'lisp-extra-font-lock--installed-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))


(defun lisp-extra-font-lock-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))


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


(defun lisp-extra-font-lock-is-in-comment-or-string (pos)
  "Return non-nil if POS is in a comment, string, constant, or reader macro.

This assumes that Font Lock is active and has fontified comments
and strings."
  (or (nth 8 (save-excursion
	       (syntax-ppss pos)))   ; In comment or string.
      ;; Plain character constant ?<char>.
      (eq (char-before pos) ??)
      ;; Escaped character constant ?\<char>.
      (and (eq (char-before pos) ?\\)
           (eq (char-before (- pos 1)) ??))
      ;; Reader macro like #'.
      (eq (char-before pos) ?#)))


(defun lisp-extra-font-lock-match-quote-and-backquote (limit)
  "Search for quote and backquote in in code.
Set match data 1 if character matched is backquote."
  (let (res)
    (while
        (progn (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
    res))


(defun lisp-extra-font-lock-match-quoted-content (limit)
  "Match next part of a quoted content.

Match up to next comma operator or quoted subexpression, or to
the end of the quoted expression."
  (and (< (point) limit)
       (let ((p (point))
             res)
         (while
             (progn
               (setq res (re-search-forward "\\(,@?\\|[`']\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
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

(defvar lisp-extra-font-lock-loop-keywords
  '("=" "above" "across" "across-ref" "always" "and" "append" "as"
    "being" "below" "buffer" "buffers" "by"
    "collect" "collecting" "concat" "count"
    "do" "doing" "downfrom" "downto"
    "each" "element" "elements" "else" "end"
    "extent" "extents" "external-symbol" "external-symbols"
    "finally" "frames" "from"
    "hash-key" "hash-keys" "hash-value" "hash-values"
    "if" "in" "in-ref" "initially" "interval" "intervals"
    "key-binding" "key-bindings" "key-code" "key-codes" "key-seq" "key-seqs"
    "maximize" "minimize"
    "named" "nconc" "nconcing" "never"
    "of" "of-ref" "on" "overlay" "overlays"
    "present-symbol" "present-symbols" "property"
    "repeat" "return"
    "screen" "screens" "sum" "symbol" "symbols"
    "the" "then" "thereis" "to"
    "unless" "until" "upfrom" "upto" "using"
    "vconcat"
    "when" "while" "windows")
  "List of `cl-loop' named parameters, excluding variable binding ones.")

(defvar lisp-extra-font-lock-loop-keywords-with-var '("for"
                                                      "index"
                                                      "into"
                                                      "with")
  "List of `cl-loop' named variable binding parameters.")


;; Match named loop keywords, and (optionally) any bound variables.
;;
;; Note, does not support "destructuring", i.e. binding several
;; variables using pattern matching. If this is used, the entire
;; expression is highlighted as a variable.
(defun lisp-extra-font-lock-match-loop-keywords (limit)
  "Match named keyword of `loop' and highlight variable arguments."
  (while
      (progn
        (forward-comment (buffer-size))
        (and (< (point) limit)
             (not (looking-at
                   (concat
                    "\\_<"
                    "\\("
                    (regexp-opt (append
                                 lisp-extra-font-lock-loop-keywords-with-var
                                 lisp-extra-font-lock-loop-keywords))
                    "\\)"
                    "\\_>")))))
    (condition-case nil
        (forward-sexp)
      (error (goto-char limit))))
  (if (not (< (point) limit))
      nil
    (goto-char (match-end 0))
    (when (member (match-string 1) lisp-extra-font-lock-loop-keywords-with-var)
      (forward-comment (buffer-size))
      (let ((var-start (point)))
        (when (condition-case nil
                  (progn
                    (forward-sexp)
                    t)
                (error nil))
          (set-match-data (list
                           (match-beginning 0)
                           (point)
                           (match-beginning 1)
                           (match-end 1)
                           var-start
                           (point))))))
    t))

(provide 'lisp-extra-font-lock)

;;; lisp-extra-font-lock.el ends here.
