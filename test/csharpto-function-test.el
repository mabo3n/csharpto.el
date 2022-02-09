;;; csharpto-function-test.el --- Tests for csharpto-function.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'csharpto-function)
(require 'csharp-mode)
(require 'dash)
(require 'subr-x)

(defun csharpto--test-log-message (format-string &rest args)
  "Log FORMAT-STRING with ARGS into the test run buffer."
  (save-excursion
    (let ((test-buffer "*Test run*"))
      (unless (get-buffer test-buffer)
        (set-buffer (get-buffer-create test-buffer))
        (insert "*** Test run ***\n\n")
        (compilation-mode))
      (set-buffer test-buffer)
      (pop-to-buffer test-buffer)
      (widen)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (if args
                    (apply 'format format-string args)
                  format-string))))))

(defmacro csharpto--test-with-replica-buffer (original-buffer &rest body)
  "Execute BODY in a temp buffer with same contents as ORIGINAL-BUFFER."
  (declare (indent 1) (debug t))
  `(let ((buf ,original-buffer))
     (with-temp-buffer
       (insert-buffer-substring buf)
       (progn ,@body))))

(defun csharpto--test-buffer-fancy-substring (ranges &optional context-lines)
  "Return a substring of current buffer with RANGES highlighted.

RANGES has form `((BEG [END] PLIST)...)` where PLIST is a property
list to be applied to the respective range in the substring.
If END is not provided, `(1+ BEG)` is used. IF BEG is nil,
the range is ignored.

The returned substring includes N extra lines before and N extra
after the matched ranges, where N = CONTEXT-LINES. N Defaults to 0."
  (let ((ranges (--filter (numberp (car it)) ranges))
        (context-lines (or context-lines 0)))
    (csharpto--test-with-replica-buffer (current-buffer)
      (dolist (range ranges)
        (let* ((beg   (car range))
               (end   (or (and (consp (cadr range)) (1+ beg))
                          (cadr range)))
               (plist (or (caddr range) (cadr range))))
          (add-text-properties beg end `(font-lock-face ,plist))))
      (let* ((positions   (append (mapcar 'car  ranges)
                                  (mapcar 'cadr ranges)))
             (positions   (-filter #'numberp positions))
             (context-beg (progn
                            (goto-char (apply 'min positions))
                            (beginning-of-line (1+ (- context-lines)))
                            (point)))
             (context-end (progn
                            (goto-char (apply 'max positions))
                            (end-of-line (1+ context-lines))
                            (point))))
        (buffer-substring context-beg context-end)))))

(defun csharpto--test-range-overlap (&rest ranges)
  "Return a range that overlaps all RANGES.

RANGES has form `((BEG END)...)'."
  (-let* (((begs . rest)     (apply '-zip-lists ranges))
          (ends              (car (last rest)))
          (highest-beginning (apply 'max begs))
          (lowest-end        (apply 'min ends)))
    (and (< highest-beginning lowest-end)
         (list highest-beginning lowest-end))))

(defun csharpto--test-log-code-snippet (point range expected-range)
  "Log a properly formatted code-snippet.

Format and forward POINT, RANGE and EXPECTED-RANGE to
function `csharpto--test-buffer-fancy-substring'."
  (let* ((overlap (and range expected-range
                       (csharpto--test-range-overlap range expected-range)))
         (ranges (list `(,@expected-range  (:background "LightGoldenrod3"))
                       `(,@range           (:background "IndianRed2"))
                       `(,@overlap         (:background "SeaGreen2"))
                       `(,point            (:background "gray")))))
    (csharpto--test-log-message
     "...\n%s\n..."
     (csharpto--test-buffer-fancy-substring ranges 1))))

(defun csharpto--test-log-failure (point range expected-range)
  "Log a failure message with details.

Output a code snippet highlighting POINT, RANGE and EXPECTED-RANGE."
  (csharpto--test-log-message
   "FAIL: range mismatch.\n      Expected %s but got %s:\n"
   expected-range
   range)
  (csharpto--test-log-code-snippet
   (point)
   range
   expected-range))

(defvar csharpto--test-sentences-alist
  '((:signature     . ((single-line  . "The function has a single-line signature")
                       (multi-line   . "The function has a multi-line signature")))
    (:scope-type    . ((expression   . "The function is expression-bodied =>")
                       (brackets     . "The function body is defined with brackets { }")))
    (:scope-lf      . "There's a line feed before opening the function scope")
    (:attributes    . ((single-preceding   . "The function has a preceding attribute")
                       (single-inline      . "The function has an inline attribute")
                       (multiple-preceding . "The function has multiple preceding attributes")
                       (multiple-inline    . "The function has multiple inline attributes")))
    (:cursor-line   . ((preceding-blank    . "The cursor lies in blank lines before the function")
                       (comments     . "The cursor lies in comments above the function")
                       (attributes   . "The cursor lies in attribute declarations before the function")
                       (signature    . "The cursor lies in the function signature")
                       (beg-of-scope . "The cursor lies in the beginning of function scope")
                       (body         . "The cursor lies in the function body")
                       (end-of-scope . "The cursor lies in the end of function scope")
                       (succeeding-blank  . "The cursor lies in blank lines succeeding function")
                       (blank             . "The cursor lies in blank lines between functions")))
    (:cursor-column . ((beg-of-line       . "The cursor lies in the beginning of line")
                       (preceding-blank   . "The cursor lies in the indentation")
                       (text              . "The cursor lies in the text")
                       (succeeding-blank  . "The cursor lies in the blank spaces ending the line")
                       (end-of-line       . "The cursor lies in the end of line")))
    (:class-only    . "The file starts with the class definition (no namespace)")
    (:item-before   . ((none       . "There's nothing before the cursor inside the function")
                       (lambda-exp . "There's a lambda expression before the cursor inside the function")))
    (:item-under    . ((none       . "There's nothing under the cursor inside the function")
                       (lambda-exp . "There's a lambda expression under the cursor inside the function")))
    (:item-after    . ((none       . "There's nothing after the cursor inside the function")
                       (lambda-exp . "There's a lambda expression after the cursor inside the function")))
    (:generic-type     . ((none      . "The function does not specifies generic types")
                          (single    . "The function specifies a single generic type")
                          (multiple  . "The function specifies multiple generic types")))
    (:type-constraints . ((none      . "The function has no type constraints")
                          (single    . "The function specifies a constraint for a generic type")
                          (multiple  . "The function specifies multiple constraints for generic types")))
    (:comment-line  . ((before           . "There's a line comment \"//\" before the function")
                       (above-attributes . "There's a line comment \"//\" above the function attribute(s)")
                       (attributes       . "There's a line comment \"//\" within the function attribute(s)")
                       (above            . "There's a line comment \"//\" above the function")
                       (signature        . "There's a line comment \"//\" within the function signature")
                       (beg-of-scope     . "There's a line comment \"//\" in the beginning of scope")
                       (end-of-scope     . "There's a line comment \"//\" in the end of scope")
                       (below            . "There's a line comment \"//\" below the function")))
    (:comment-block . ((before           . "There's a block comment \"/**/\" before the function")
                       (above-attributes . "There's a block comment \"/**/\" above the function attribute(s)")
                       (attributes       . "There's a block comment \"/**/\" within the function attribute(s)")
                       (above            . "There's a block comment \"/**/\" above the function")
                       (signature        . "There's a block comment \"/**/\" within the function signature")
                       (beg-of-scope     . "There's a block comment \"/**/\" in the beginning of scope")
                       (end-of-scope     . "There's a block comment \"/**/\" in the end of scope")
                       (below            . "There's a block comment \"/**/\" below the function"))))
  "Mapping between test prop values and their equivalent sentences.")


(defun csharpto--test-generate-sentences (&rest plist)
  "Return a list of corresponding sentences for each prop in PLIST.

See variable `csharpto--test-sentences-alist'."
  (when-let* ((plist plist)
              (key   (car plist))
              (value (cadr plist))
              (options  (alist-get key csharpto--test-sentences-alist))
              (sentence (or (and (stringp options) value options)
                            (alist-get value options)
                            (format "Property %s has value '%s"
                                    key value))))
    (cons sentence (apply #'csharpto--test-generate-sentences (cddr plist)))))

(defun csharpto--test-generate-scenario-description (&rest clauses)
  "Generate a BDD scenario description given CLAUSES.

CLAUSES is a property list that accepts `:given' `:when' and `:then'
properties, each one being a string or a list of strings."
  (let ((given (plist-get clauses :given))
        (when  (plist-get clauses :when))
        (then  (plist-get clauses :then)))
    (concat (and given (concat "GIVEN " (string-join (flatten-list given) "\n  AND ")))
            (and when (concat "\n WHEN " (string-join (flatten-list when) "\n  AND ")))
            (and then (concat "\n THEN " (string-join (flatten-list then) "\n  AND "))))))

(defun csharpto--test-buffer-setup (fixture search-regexp goto-beg-of-match)
  "Setup current buffer with FIXTURE contents for the test.

FIXTURE is the fixture file path.
Perform a `re-search-forward' with SEARCH-REGEXP and go to
beginning of match if GOTO-BEG-OF-MATCH is non-nil."
  (insert-file-contents fixture)
  (csharp-mode)
  (re-search-forward search-regexp nil t)
  (when goto-beg-of-match
    (goto-char (match-beginning 0))))

(defun csharpto-test-run (&rest properties)
  "Run a test according to PROPERTIES.

PROPERTIES is a property list (PROP1 VALUE1 PROP2 VALUE2 ...)
where the supported properties and their respective value are:

:id     -> A unique identifier to the test

:setup  -> A property list (PROP1 VALUE1 PROP2 VALUE2 ...)
           with parameters to setup the buffer for the test.
           Supported properties and their respective value are:

           :file -> A string representing a (relative) file path
                    to the fixture file to be used in the test.

           :find -> A REGEXP to search for (and move the cursor to)
                    a pattern in the fixture file.

           :goto-beginning-of-match
                 -> Whether the cursor should be moved to the
                    beginning of the text matched by :find REGEXP.

:props  -> A property list (PROP1 VALUE1 PROP2 VALUE2 ...)
           with properties detailing the test scenario.
           For all supported properties see variable
           `csharpto--test-sentences-alist'.

:test   -> A list of EXPECTATIONS to be tested for the scenario.
           EXPECTATIONS has form ((FCALL RANGE [TEXT]) ...) where
           FCALL is a form to be evaluated in the test,
           RANGE is the expected return of evaluating FCALL
           and has form (BEG END), and
           TEXT is a (optional) string detailing the expectation."
  (let ((test-id          (plist-get properties :id))
        (setup-plist      (plist-get properties :setup))
        (assertions-alist (plist-get properties :test))
        (scenario-plist   (plist-get properties :props)))

    (csharpto--test-log-message "\n %1$s Test #%2$s %1$s \n"
                                (make-string 24 ?-)
                                test-id)

    (let ((setup-file   (plist-get setup-plist :file))
          (setup-regexp (plist-get setup-plist :find))
          (setup-regexp-goto-beg
           (plist-get setup-plist :goto-beginning-of-match)))

      (csharpto--test-log-message
       "\nFile: %s\nCursor in the %s of match: \"%s\"\n"
       setup-file
       (if setup-regexp-goto-beg "beginning" "end")
       setup-regexp)

      (csharpto--test-log-message
       "\n%s"
       (csharpto--test-generate-scenario-description
        :given (apply #'csharpto--test-generate-sentences scenario-plist)))

      (cl-loop
       for (fcall expected-range expectation-text) in assertions-alist
       do (with-temp-buffer
            (csharpto--test-buffer-setup setup-file
                                         setup-regexp
                                         setup-regexp-goto-beg)

            (csharpto--test-log-message
             (csharpto--test-generate-scenario-description
              :when (concat "I call '" (symbol-name (car fcall))
                            (and (cdr fcall)
                                 (format " with args '%s" (cdr fcall))))
              :then (or expectation-text
                        (format "'%s should be returned"
                                expected-range))))

            (let* ((returned-range (ignore-errors (eval fcall)))
                   (ranges-match-p (equal returned-range expected-range)))
              (if ranges-match-p
                  (csharpto--test-log-message "\n      PASS")
                (csharpto--test-log-message "\n      ")
                (csharpto--test-log-failure (point) returned-range expected-range))))))))

(csharpto-test-run
 :id 1
 :setup (list :file "./fixtures/Entity.cs"
              :find "public MyEntity(string name)"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (421 581))
        ((csharpto-get-function-range t)    (421 582)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'text))

(csharpto-test-run
 :id 2
 :setup (list :file "./fixtures/Entity.cs"
              :find "^\n\s *int OneLiner()"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (581 582))
         ((csharpto-get-function-range t)   (581 628)))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 3
 :setup (list :file "./fixtures/Entity.cs"
              :find "3 \\+ 4 \\+ 5; $"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (582 628))
         ((csharpto-get-function-range t)   (582 630)))
 :props (list :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'succeeding-blank))

(csharpto-test-run
 :id 4
 :setup (list :file "./fixtures/Entity.cs"
              :find "level = default\n\\s *)"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (630 768))
         ((csharpto-get-function-range t)   (630 769)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'preceding-blank))

(csharpto-test-run
 :id 5
 :setup (list :file "./fixtures/Entity.cs"
              :find "return a \\+ b;"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (769 888))
         ((csharpto-get-function-range t)   (769 891)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 6
 :setup (list :file "./fixtures/Entity.cs"
              :find "\n\n.+IEnumerable"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (888 891))
         ((csharpto-get-function-range t)   (888 1010)))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 7
 :setup (list :file "./fixtures/Entity.cs"
              :find "=> this"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (891 1010))
         ((csharpto-get-function-range t)   (888 1010)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 8
 :setup (list :file "./fixtures/ClassWithSingleFunction.cs"
              :find "^.+SomeFunction"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (128 290))
         ((csharpto-get-function-range t)   (127 290)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 9
 :setup (list :file "./fixtures/Attributes.cs"
              :find "ChangeName() {"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (660 983))
         ((csharpto-get-function-range t)   (659 983)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :attributes    'single-inline
              :cursor-line   'signature
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 10
 :setup (list :file "./fixtures/Attributes.cs"
              :find "^.+\\[Theory\\]"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (177 659))
         ((csharpto-get-function-range t)   (177 660)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'attributes
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 11
 :setup (list :file "./fixtures/Attributes.cs"
              :find "() => new"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (177 659))
         ((csharpto-get-function-range t)   (177 660)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'body
              :cursor-column 'text
              :item-under    'lambda-exp))

(csharpto-test-run
 :id 12
 :setup (list :file "./fixtures/ClassOnlyNoImports.cs"
              :find "Hello"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (035 129))
         ((csharpto-get-function-range t)   (035 129)))
 :props (list :cursor-line   'signature))

(csharpto-test-run
 :id 13
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "MinValue\n"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (282 616))
         ((csharpto-get-function-range t)   (282 617)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :item-before  'lambda-exp))

(csharpto-test-run
 :id 14
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "        \.First"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (617 795))
         ((csharpto-get-function-range t)   (617 796)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'preceding-blank
              :item-before   'lambda-exp))

(csharpto-test-run
 :id 15
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "(owner),"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (1226 1517))
         ((csharpto-get-function-range t)   (1225 1517)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :attributes    'single-preceding
              :cursor-line   'body
              :cursor-column 'end-of-line
              :item-before   'lambda-exp
              :item-after    'lambda-exp))

(csharpto-test-run
 :id 16
 :setup (list :file "./fixtures/Generics.cs"
              :find "=> default"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (63 97))
         ((csharpto-get-function-range t)   (63 98)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'text
              :generic-type  'single))

(csharpto-test-run
 :id 17
 :setup (list :file "./fixtures/Generics.cs"
              :find "T: new"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (98 197))
         ((csharpto-get-function-range t)   (97 197)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'text
              :generic-type  'multiple
              :type-constraint 'single))

(csharpto-test-run
 :id 18
 :setup (list :file "./fixtures/Comments.cs"
              :find "Id = "
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (421 716))
         ((csharpto-get-function-range t)   (421 717)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'text
              :comment-line  'above))

(csharpto-test-run
 :id 19
 :setup (list :file "./fixtures/Comments.cs"
              :find "OneLiner"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (717 805))
         ((csharpto-get-function-range t)   (717 807)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 20
 :setup (list :file "./fixtures/Comments.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (807 1070))
         ((csharpto-get-function-range t)   (807 1071)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'signature))

(csharpto-test-run
 :id 21
 :setup (list :file "./fixtures/Comments.cs"
              :find "name=\"a\""
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1115 1490))
         ((csharpto-get-function-range t)   (1115 1491)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'comments
              :cursor-column 'text
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 22
 :setup (list :file "./fixtures/Comments.cs"
              :find (rx "x++")
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1491 1738))
         ((csharpto-get-function-range t)   (1491 1739)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'text
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto-test-run
 :id 23
 :setup (list :file "./fixtures/Comments.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1739 1925))
         ((csharpto-get-function-range t)   (1738 1925)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :comment-line  'above
              :comment-line  'signature
              :comment-block 'signature
              :comment-line  'beg-of-scope
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 24
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "This is a"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (428 676))
         ((csharpto-get-function-range t)   (428 677)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'comments
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'above-attributes))

(csharpto-test-run
 :id 25
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "//Comment$"
              :goto-beginning-of-match nil)
 :test '(((csharpto-get-function-range nil) (677 743))
         ((csharpto-get-function-range t)   (677 744)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'end-of-line
              :attributes    'single-inline
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 26
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (744 959))
         ((csharpto-get-function-range t)   (744 960)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'signature))

(csharpto-test-run
 :id 27
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "end of SomeMethod"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1004 1232))
         ((csharpto-get-function-range t)   (1004 1233)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'end-of-scope
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'before
              :comment-line  'above-attributes
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 28
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "\n +/\\* Block"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1299 1300))
         ((csharpto-get-function-range t)   (1299 1558)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'preceding-blank
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-block 'before
              :comment-block 'above-attributes
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto-test-run
 :id 29
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1559 1956))
         ((csharpto-get-function-range t)   (1558 1956)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :attributes    'multiple-preceding
              :comment-block 'above-attribute
              :comment-line  'above-attribute
              :comment-block 'above
              :comment-line  'signature
              :comment-block 'signature
              :comment-line  'end-of-scope))

(provide 'csharpto-function-test)

;;; csharpto-function-test.el ends here
