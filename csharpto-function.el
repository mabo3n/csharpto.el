;;; csharpto-function.el --- C# function text object -*- lexical-binding: t -*-

;;; Code:

(defun csharpto-get-function-region
    (&optional include-around)
  "Return (BEG END) of function under point.

If INCLUDE-AROUND is non-nil, include surrouding blank lines
following vim-like conventions (return \"a\" function).

This uses a heuristic based method to find the boudaries
of regular/expression-bodied functions.

It should work in most cases given:
  1. Functions are separated by blank lines;
  2. There's no blank lines inside a function header
     or expression-bodied function;
  3. There's no fields/properties between functions;
  4. There's no weird indentation and comments."
  (let* ((preceding-blank-lines-group 1)
         (header-group 2)
         (indent-group 3)
         (attributes-group 4)
         (signature-group 5)
         (open-delimiter-group 6)
         (header-regexp
          (rx-to-string
           `(seq
             ;; As emacs' backward search stops at first match, it'll never match
             ;; optional patterns in the beginning, even if they are present.
             ;; We have to guess and [non-optionally] match something before
             ;; the header and its preceding blank lines, so everything below
             ;; is included in the match
             (seq (or buffer-start
                      ;; FIXME Don't match attribute + comments e.g.
                      ;; "[Attribute] // Comment not ending with ?\]."
                      ;; Below hangs while matching:
                      ;;
                      ;; (seq bol (0+ space)
                      ;;      (not (any space ?\n ?\[ ?\/))
                      ;;      (0+ nonl) (not (any space ?\n ?\])) eol)
                      ;;
                      ;; Workaround:
                      (not (any space ?\n ?\])))
                  (0+ space) (opt ?\n))
             (group-n ,preceding-blank-lines-group
                      (0+ (seq (0+ space) ?\n)))
             (group-n ,header-group
                      (group-n ,indent-group
                               bol (0+ space))
                      (group-n ,attributes-group
                               (0+ (seq ?\[ (+? anything) ?\] (0+ space)
                                        (opt ?\n (backref ,indent-group) (0+ space)))))
                      (group-n ,signature-group
                               (seq alpha (0+ nonl) (not (any ?\n ?\;)))
                               (repeat 0 10 (seq ?\n
                                                 (backref ,indent-group)
                                                 (0+ space)
                                                 (0+ nonl)))
                               (opt ?\n ;; TODO cant this be simplified? Almost same as above
                                    (backref ,indent-group)
                                    (0+ space))
                               (or (seq (group-n ,open-delimiter-group "{")
                                        (0+ space) eol)
                                   (group-n ,open-delimiter-group "=>")))))))
         (end-of-scope-group 7)
         (succeeding-blank-lines-group 8)
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
                              ,@end-of-scope (0+ space) ?\n)
                     (group-n ,succeeding-blank-lines-group
                              (0+ (0+ space) ?\n))))))))
    (catch 'region
      (save-excursion
        (let* ((p (point))
               (prev-fun-match-data
                (and (re-search-backward header-regexp nil t)
                     (match-data)))
               (next-fun-match-data
                (and (goto-char
                      (or (and (match-end 0) (1- (match-end header-group)))
                          ;; go to beg of empty lines to include
                          ;; all of them in the match
                          (and (re-search-backward (rx bol (0+ space) ?\n) nil t)
                               (re-search-backward (rx (or buffer-start (not space))
                                                       (0+ space)
                                                       ?\n) nil t)
                               (1+ (match-end 0)))
                          (point-at-bol)))
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
              (throw 'region `(,(match-beginning preceding-blank-lines-group)
                               ,(match-end       preceding-blank-lines-group)))))
           (prev-fun-match-data
            (set-match-data prev-fun-match-data))
           (t
            (throw 'region '())))

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
                (throw 'region `(,succeeding-blank-lines-beg
                                 ,succeeding-blank-lines-end))
              (if (>= p header-line-beg)
                  (throw 'region
                         `(,(if (and include-around
                                     (not (> (length (match-string
                                                      succeeding-blank-lines-group))
                                             0)))
                                preceding-blank-lines-beg
                              header-line-beg)
                           ,(if include-around
                                (match-end 0)
                              (match-end end-of-scope-group))))
                (throw 'region
                       `(,preceding-blank-lines-beg
                         ,(match-end end-of-scope-group)))))))))))

(provide 'csharpto-function)

;;; csharpto-function.el ends here
