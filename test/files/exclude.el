;; Don't highlight quotes in certain contexts.

;; Don't highlight 'no in a comment.

"Don't highlight 'no in a string."

;; Not in a character constant:

(setq exclude-quote (cons ?' 'yes))

;; Not in an escapted character constant:

(setq exclude-quote (cons ?\' 'yes))

;; exclude.el ends here.
