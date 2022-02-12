;;; csharpto-function.el --- C# function text object -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(defun csharpto--get-function-range (&optional include-around unrestricted)
  "Return a range (BEG END) of the nearest function.

This uses a heuristics-based approach with regular expressions to
try to match the boundaries of the nearest function.
Both regular and expression-bodied functions are supported.

If INCLUDE-AROUND is non-nil, include surrounding blank lines
in the range (like `evil-a-paragraph' but for functions).

If UNRESTRICTED is non-nil, return the range of the
nearest scope, not restricted to functions. (Experimental feature)

The function detection relies heavily on blank lines and indentation
to work properly, so with \"badly\" formatted code it won't work
out of the box.

It should work in most cases given:
  - Functions are separated by [at least one] blank lines;
  - There's no blank lines within a function signature,
    nor anywhere inside an expression-bodied function;
  - There's no fields/properties between functions;
  - There's no weird indentation and comments."
  (rx-let-eval '((line-comment (seq ?/ ?/ (0+ nonl)))
                 (block-comment (seq ?/ ?* (*? anything) ?* ?/))
                 (comment (or line-comment block-comment))
                 (blank-or-comment (seq (0+ space) (opt comment)))

                 (build-scope-header (first-line-pattern
                                      open-scope-pattern
                                      indentation)
                  (seq first-line-pattern
                       ;; optional extra header-lines
                       (minimal-match
                        (repeat 0 10
                                ?\n indentation (0+ space) (0+ nonl)))
                       ;; optional lf before { or =>
                       (opt ?\n indentation (0+ space))
                       open-scope-pattern))

                 (function-signature (indentation open-delimiter-group)
                  (build-scope-header
                   ;first-line-pattern
                   (seq
                    ;; Type or access modifier followed by a space
                    (seq alpha
                         (opt (0+ (not (any ?\n ?=)))
                              (not (any ?\n ?\) ?= ?: ?, space)))
                         space)
                    ;; Anything with at least one "(" on 1st line
                    (seq (1+ (not (any ?\n ?=))) ?\( (0+ nonl)))

                   ;open-scope-pattern
                   (or (seq (group-n open-delimiter-group "{")
                            blank-or-comment eol)
                       (group-n open-delimiter-group "=>"))

                   indentation))

                 (any-bracketed-scope-signature (indentation open-delimiter-group)
                  (build-scope-header
                   ;first-line-pattern
                   ;; A letter + anything, not ending with ";"
                   (seq alpha (0+ nonl) (not (any ?\n ?\;)))

                   ;open-scope-pattern
                   (seq (group-n open-delimiter-group "{")
                        blank-or-comment eol)

                   indentation)))
    (let* ((preceding-blank-lines-group 1)
           (header-group 2)
           (comments-group 3)
           (indentation-group 4)
           (attributes-group 5)
           (header-required-group 6)
           (open-delimiter-group 7)
           (header-regexp
            (rx-to-string
             `(seq
               ;; As emacs' backward search stops at first match, it'll never match
               ;; optional patterns in the beginning, even if they are present.
               ;; We have to guess and [non-optionally] match something before
               ;; the header and its preceding blank lines, so everything below
               ;; is included in the match
               (seq (or
                     ;; Anything
                     (seq (not (any space ?\n)) (0+ space) ?\n
                          ;; followed by at least one blank line
                          (group-n ,preceding-blank-lines-group
                                   (1+ (seq (0+ space) ?\n))))
                     ;; Or the beginning of the buffer
                     (seq buffer-start (0+ space) (opt ?\n)
                          ;; followed by optional blank lines
                          (group-n ,preceding-blank-lines-group
                                   (0+ (seq (0+ space) ?\n))))
                     ;; Or a line
                     (seq bol (0+ space)
                          ;; not starting an attribute/comment
                          (not (any space ?\n ?\[ ?\] ?/ ?*))
                          ;; and not ending an attribute/comment
                          (opt (0+ nonl) (not (any ?\n ?\[ ?\] ?/))) (0+ space) ?\n
                          ;; followed by optional blank lines
                          (group-n ,preceding-blank-lines-group
                                   (0+ (seq (0+ space) ?\n))))))
               (group-n ,header-group
                        (group-n ,indentation-group
                                 bol (0+ space))
                        (group-n ,comments-group
                                 (0+ (seq (or ?/ ?*) (0+ nonl) ?\n
                                          (backref ,indentation-group) (0+ space))))
                        (group-n ,attributes-group ;; this also matches any comments in between
                                 (0+ (seq ?\[ (+? anything) ?\] (0+ space)
                                          (opt ?\n (backref ,indentation-group) (0+ space)))))
                        (group-n ,comments-group
                                 (0+ (seq (or ?/ ?*) (0+ nonl) ?\n
                                          (backref ,indentation-group) (0+ space))))
                        (group-n ,header-required-group
                                 (or (function-signature (backref ,indentation-group)
                                                          ,open-delimiter-group)

                                     ,(if unrestricted
                                          `(any-bracketed-scope-signature (backref ,indentation-group)
                                                                          ,open-delimiter-group)
                                        'unmatchable)))))))
           (end-of-scope-group 8)
           (succeeding-blank-lines-group 9)
           (build-end-of-scope-regexp
            (lambda (indentation beg-of-scope-delimiter)
              "Build a regexp matching the end of the function."
              (let ((end-of-scope
                     (if (string= beg-of-scope-delimiter "=>")
                         ";"
                       ;; TODO Accept a semi-colon "};" for generic
                       ;; headers (not functions)
                       `(bol ,indentation "}"))))
                ;; FIXME Find last before indentation shorter than functions'.
                ;;       This is matching any statement
                (rx-to-string
                 `(seq (group-n ,end-of-scope-group
                                ,@end-of-scope blank-or-comment ?\n)
                       (group-n ,succeeding-blank-lines-group
                                (0+ (0+ space) ?\n))))))))
     (catch 'range
       (save-excursion
         (let* ((p (point))
                (prev-fun-match-data
                 (and (re-search-backward header-regexp nil t)
                      (match-data)))
                (next-fun-match-data
                 (and (goto-char
                       (or (and prev-fun-match-data (1- (match-end header-group)))
                           ;; try to go to beginning of empty lines
                           ;; to include all of them in the match
                           (and (re-search-backward (rx bol (0+ space) ?\n) nil t)
                                (re-search-backward (rx (or buffer-start (not space))
                                                        (0+ space)
                                                        ?\n) nil t))
                           (point-min)))
                      (re-search-forward header-regexp nil t)
                      (match-data))))
           (cond
            ((and next-fun-match-data
                  (>= p (match-beginning header-group)))
             (set-match-data next-fun-match-data))
            ((and next-fun-match-data
                  (>= p (match-beginning preceding-blank-lines-group)))
             (if include-around
                 (set-match-data next-fun-match-data)
               (throw 'range `(,(match-beginning preceding-blank-lines-group)
                               ,(match-end       preceding-blank-lines-group)))))
            (prev-fun-match-data
             (set-match-data prev-fun-match-data))
            (t
             (throw 'range '())))

           (when-let* ((preceding-blank-lines-beg (match-beginning preceding-blank-lines-group))
                       (header-line-beg   (match-beginning header-group))
                       (header-text-beg   (match-end indentation-group))

                       ;; This assumes there's no other declarations between functions
                       (header-end (goto-char (match-end header-group)))

                       (end-of-scope-regexp
                        (funcall build-end-of-scope-regexp
                                 (match-string-no-properties indentation-group)
                                 (match-string-no-properties open-delimiter-group)))
                       (function-end (re-search-forward end-of-scope-regexp nil t))

                       (succeeding-blank-lines-beg (match-beginning succeeding-blank-lines-group))
                       (succeeding-blank-lines-end (match-end succeeding-blank-lines-group)))

             (if (>= p succeeding-blank-lines-beg)
                 (throw 'range `(,succeeding-blank-lines-beg
                                 ,succeeding-blank-lines-end))
               (if (>= p header-line-beg)
                   (throw 'range
                          `(,(if (and include-around
                                      ;; no succeeding blank lines
                                      (not (> (length (match-string
                                                       succeeding-blank-lines-group))
                                              0)))
                                 preceding-blank-lines-beg
                               (if include-around
                                   header-line-beg
                                 header-text-beg))
                            ,(if include-around
                                 (match-end 0)
                               (1- (match-end end-of-scope-group)))))
                 (throw 'range
                        `(,preceding-blank-lines-beg
                          ,(match-end end-of-scope-group))))))))))))

(provide 'csharpto-function)

;;; csharpto-function.el ends here
