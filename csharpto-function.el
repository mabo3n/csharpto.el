;;; csharpto-function.el --- C# function text object -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(defun csharpto--get-function-range (&optional include-around)
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
  (rx-let-eval '((line-comment (seq ?/ ?/ (0+ nonl)))
                 (block-comment (seq ?/ ?* (*? anything) ?* ?/))
                 (comment (or line-comment block-comment))
                 (blank-or-comment (seq (0+ space) (opt comment)))
                 (opt-extra-header-lines (indentation)
                  (minimal-match
                   (repeat 0 10 ?\n indentation (0+ space) (0+ nonl))))
                 (opt-linefeed-before-open-scope-delimiter (indentation)
                  (opt ?\n indentation (0+ space))))
    (let* ((preceding-blank-lines-group 1)
           (contents-group 2)
           (comments-group 3)
           (indent-group 4)
           (attributes-group 5)
           (header-group 6)
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
               (group-n ,contents-group
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
                        (group-n ,header-group
                                 (or (seq
                                      ;; Type or access modifier followed by a space:
                                      (seq alpha
                                           (opt (0+ (not (any ?\n ?=)))
                                                (not (any ?\n ?\) ?= ?: ?, space)))
                                           space)
                                      ;; Anything with at least one "(" on 1st line
                                      (seq (1+ (not (any ?\n ?=))) ?\( (0+ nonl))
                                      (opt-extra-header-lines (backref ,indent-group))
                                      (opt-linefeed-before-open-scope-delimiter (backref ,indent-group))
                                      (or (seq (group-n ,open-delimiter-group "{")
                                               blank-or-comment eol)
                                          (group-n ,open-delimiter-group "=>")))

                                     (seq
                                      (seq alpha (0+ nonl) (not (any ?\n ?\;)))
                                      (opt-extra-header-lines (backref ,indent-group))
                                      ;; ?\) (0+ space) ;; this would be nice but makes it too slow
                                      (opt-linefeed-before-open-scope-delimiter (backref ,indent-group))
                                      (seq (group-n ,open-delimiter-group "{")
                                           blank-or-comment eol)))

                                 )))))
           (end-of-scope-group 8)
           (succeeding-blank-lines-group 9)
           (build-end-of-scope-regexp
            (lambda (indent-string beg-of-scope-delimiter)
              "Build a regexp matching the end of the function."
              (let ((end-of-scope
                     (if (string= beg-of-scope-delimiter "=>")
                         ";"
                       ;; TODO Accept a semi-colon "};" for generic
                       ;; headers (not functions)
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
                       (or (and prev-fun-match-data (1- (match-end contents-group)))
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
                  (>= p (match-beginning contents-group)))
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
                       (header-line-beg   (match-beginning contents-group))
                       (header-text-beg   (match-end indent-group))

                       ;; This assumes there's no other declarations between functions
                       (header-end (goto-char (match-end contents-group)))

                       (end-of-scope-regexp
                        (funcall build-end-of-scope-regexp
                                 (match-string-no-properties indent-group)
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
