;;; csharpto-function.el --- C# function text object -*- lexical-binding: t -*-

;;; Code:

(rx-define line-comment (seq ?/ ?/ (0+ nonl)))
(rx-define block-comment (seq ?/ ?* (*? anything) ?* ?/))
(rx-define comment (or line-comment block-comment))
(rx-define blank-or-comment (seq (0+ space) (opt comment)))

(defun csharpto-get-function-range (&optional include-around)
  "Return a line-wise range (BEG END) of function under point.

If INCLUDE-AROUND is non-nil, include surrouding blank lines
following vim-like conventions (return \"a\" function).

This uses a heuristic-based approach to find boudaries
of regular/expression-bodied functions.

It should work in most cases given:
  1. Functions are separated by blank lines;
  2. There's no blank lines within a function signature,
     nor anywhere inside an expression-bodied function;
  3. There's no fields/properties between functions;
  4. There's no weird indentation and comments."
  (let* ((preceding-blank-lines-group 1)
         (header-group 2)
         (comments-group 3)
         (indent-group 4)
         (attributes-group 5)
         (signature-group 6)
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
                      (group-n ,indent-group
                               bol (0+ space))
                      (group-n ,comments-group
                               (0+ (seq (or ?/ ?*) (0+ nonl) ?\n
                                        (backref ,indent-group) (0+ space))))
                      (group-n ,attributes-group ;; this also matches any comments in between
                               (0+ (seq ?\[ (+? anything) ?\] (0+ space)
                                        (opt ?\n (backref ,indent-group) (0+ space)))))
                      (group-n ,comments-group
                               (0+ (seq (or ?/ ?*) (0+ nonl) ?\n
                                        (backref ,indent-group) (0+ space))))
                      (group-n ,signature-group
                               ;; Type or access modifier followed by a space:
                               (seq alpha
                                    (opt (0+ (not (any ?\n ?=)))
                                         (not (any ?\n ?\) ?= ?: ?, space)))
                                    space)
                               ;; Anything with at least one "(" on 1st line
                               ;; (this with above prevents matching lambda
                               ;;  expressions but also classes/namespaces):
                               (seq (1+ (not (any ?\n ?=))) ?\( (0+ nonl))
                               ;; Optional multi-line signature:
                               (minimal-match
                                (repeat 0 10 (seq ?\n
                                                  (backref ,indent-group)
                                                  (0+ space)
                                                  (0+ nonl))))
                               ;; ?\) (0+ space) ;; this would be nice but makes it too slow
                               ;; Optional line feed before scope opening:
                               (opt ?\n
                                    (backref ,indent-group)
                                    (0+ space))
                               (or (seq (group-n ,open-delimiter-group "{")
                                        blank-or-comment eol)
                                   (group-n ,open-delimiter-group "=>")))))))
         (end-of-scope-group 8)
         (succeeding-blank-lines-group 9)
         (build-end-of-scope-regexp
          (lambda (indent-string beg-of-scope-delimiter)
            "Build a regexp matching the end of the function."
            (let ((end-of-scope
                   (if (string= beg-of-scope-delimiter "=>")
                       ";"
                     `(bol ,indent-string "}"))))
              ;; FIXME Find last before indent shorter than functions'.
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
                      (indent-string     (match-string-no-properties indent-group))
                      (open-scope-string (match-string-no-properties open-delimiter-group))

                      ;; This assumes there's no other declarations between functions
                      (_ (goto-char (match-end header-group)))
                      (end-of-scope-regexp (funcall build-end-of-scope-regexp
                                                    indent-string
                                                    open-scope-string))
                      (_ (re-search-forward end-of-scope-regexp nil t))

                      (succeeding-blank-lines-beg (match-beginning succeeding-blank-lines-group))
                      (succeeding-blank-lines-end (match-end succeeding-blank-lines-group)))

            (if (>= p succeeding-blank-lines-beg)
                (throw 'range `(,succeeding-blank-lines-beg
                                ,succeeding-blank-lines-end))
              (if (>= p header-line-beg)
                  (throw 'range
                         `(,(if (and include-around
                                     (not (> (length (match-string
                                                      succeeding-blank-lines-group))
                                             0)))
                                preceding-blank-lines-beg
                              header-line-beg)
                           ,(if include-around
                                (match-end 0)
                              (match-end end-of-scope-group))))
                (throw 'range
                       `(,preceding-blank-lines-beg
                         ,(match-end end-of-scope-group)))))))))))

(provide 'csharpto-function)

;;; csharpto-function.el ends here
