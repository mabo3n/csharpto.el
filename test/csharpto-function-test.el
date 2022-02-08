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
  "Output a properly formatted code-snippet.

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

(defvar csharpto--test-docstrings-alist
  '((:signature        . ((single-line  . "The function has a single-line signature")
                          (multi-line   . "The function has a multi-line signature")))
    (:scope-type       . ((expression   . "The function is expression-bodied =>")
                          (brackets     . "The function body is defined with brackets { }")))
    (:scope-lf         . "There's a line feed before opening the function scope")
    (:attributes       . ((single-preceding     .  "The function has a preceding attribute")
                          (single-inline        . "The function has an inline attribute")
                          (multiple-preceding   . "The function has multiple preceding attributes")
                          (multiple-inline      . "The function has multiple inline attributes")))
    (:cursor-line      . ((preceding-blank      . "The cursor lies in blank lines before the function")
                          (comments     . "The cursor lies in comments above the function")
                          (attributes   . "The cursor lies in attribute declarations before the function")
                          (signature    . "The cursor lies in the function signature")
                          (beg-of-scope . "The cursor lies in the beginning of function scope")
                          (body         . "The cursor lies in the function body")
                          (end-of-scope . "The cursor lies in the end of function scope")
                          (succeeding-blank . "The cursor lies in blank lines succeeding function")
                          (blank            . "The cursor lies in blank lines between functions")))
    (:cursor-column    . ((beg-of-line      . "The cursor lies in the beginning of line")
                          (preceding-blank  . "The cursor lies in the indentation")
                          (text             . "The cursor lies in the text")
                          (succeeding-blank . "The cursor lies in the blank spaces ending the line")
                          (end-of-line      . "The cursor lies in the end of line")))
    (:item-before      . ((none       . "There's nothing before the cursor inside the function")
                          (lambda-exp . "There's a lambda expression before the cursor inside the function")))
    (:item-after       . ((none       . "There's nothing after the cursor inside the function")
                          (lambda-exp . "There's a lambda expression after the cursor inside the function")))
    (:generic-type     . ((none       . "The function does not specifies generic types")
                          (single     . "The function specifies a single generic type")
                          (multiple   . "The function specifies multiple generic types")))
    (:type-constraints . ((none     . "The function has no type constraints")
                          (single   . "The function specifies a constraint for a generic type")
                          (multiple . "The function specifies multiple constraints for generic types")))
    (:comment-line     . ((before           .  "There's a line comment \"//\" before the function")
                          (above-attributes .  "There's a line comment \"//\" above the function attribute(s)")
                          (attributes       .  "There's a line comment \"//\" within the function attribute(s)")
                          (above            .  "There's a line comment \"//\" above the function")
                          (signature        .  "There's a line comment \"//\" within the function signature")
                          (beg-of-scope     .  "There's a line comment \"//\" in the beginning of scope")
                          (end-of-scope     .  "There's a line comment \"//\" in the end of scope")
                          (below            .  "There's a line comment \"//\" below the function")))
    (:comment-block    . ((before           . "There's a block comment \"/*\" before the function")
                          (above-attributes . "There's a block comment \"/*\" above the function attribute(s)")
                          (attributes       . "There's a block comment \"/*\" within the function attribute(s)")
                          (above            . "There's a block comment \"/*\" above the function")
                          (signature        . "There's a block comment \"/*\" within the function signature")
                          (beg-of-scope     . "There's a block comment \"/*\" in the beginning of scope")
                          (end-of-scope     . "There's a block comment \"/*\" in the end of scope")
                          (below            . "There's a block comment \"/*\" below the function"))))
  "Mapping between test prop values and their textual description.")

(defun csharpto--test-generate-sentences (&rest plist)
  "Return a list of corresponding sentences for each prop in PLIST.

See variable `csharpto--test-docstrings-alist'."
  (when-let* ((plist plist)
              (key   (car plist))
              (value (cadr plist))
              (options  (alist-get key csharpto--test-docstrings-alist))
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
            (and then (concat "\n THEN " (string-join (flatten-list then) "\n  AND ")))
            (and (or given when then) "."))))

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

(defun csharpto-test-run (scenario arrange-function fcall expected-range)
  "Run a test according to arguments."
  (csharpto--test-log-message "\n%s\n\n" scenario)
  (with-temp-buffer
    (eval arrange-function)
    (let* ((returned-range (ignore-errors (eval fcall)))
           (ranges-match-p (equal returned-range expected-range)))
      (if ranges-match-p
          (csharpto--test-log-message "Pass\n")
        (csharpto--test-log-message "Fail: range mismatch\n\n")
        (csharpto--test-log-message "%s ⇒ %s\n\n" fcall returned-range)
        (csharpto--test-log-message "Expected: %s\n" expected-range)
        (csharpto--test-log-code-snippet (point) returned-range expected-range))))
  (csharpto--test-log-message "\n%s" (make-string 30 ?-)))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'signature
          :cursor-column 'text)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(421 581)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "public MyEntity(string name)" t)
 '(csharpto-get-function-range nil)
 '(421 581))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      'break
          :cursor-line   'body
          :cursor-column 'preceding-blank)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(421 582)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "  logs = new List<LogEntry>" t)
 '(csharpto-get-function-range t)
 '(421 582))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :cursor-line   'blank
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(581 582)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
 '(csharpto-get-function-range nil)
 '(581 582))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :cursor-line   'blank
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(581 628)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
 '(csharpto-get-function-range t)
 '(581 628))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'succeeding-blank)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(582 628)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
 '(csharpto-get-function-range nil)
 '(582 628))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'succeeding-blank)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(582 630)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
 '(csharpto-get-function-range t)
 '(582 630))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :scope-lf      t
          :cursor-line   'signature
          :cursor-column 'preceding-blank)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(630 768)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
 '(csharpto-get-function-range nil)
 '(630 768))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :scope-lf      t
          :cursor-line   'signature
          :cursor-column 'preceding-blank)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(630 769)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
 '(csharpto-get-function-range t)
 '(630 769))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'body
          :cursor-column 'end-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(769 888)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
 '(csharpto-get-function-range nil)
 '(769 888))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'body
          :cursor-column 'end-of-line)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(769 891)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
 '(csharpto-get-function-range t)
 '(769 891))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :cursor-line   'blank
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(888 1010)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "\n\n\\s +public IEnumerable" t)
 '(csharpto-get-function-range t)
 '(888 1010))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'end-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(891 1010)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "=> this" nil)
 '(csharpto-get-function-range nil)
 '(891 1010))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'end-of-line)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(888 1010)))
 '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "=> this" nil)
 '(csharpto-get-function-range t)
 '(888 1010))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'signature
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(127 290)))
 '(csharpto--test-buffer-setup "./fixtures/ClassWithSingleFunction.cs" "^.+SomeFunction" t)
 '(csharpto-get-function-range t)
 '(127 290))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'signature
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(128 290)))
 '(csharpto--test-buffer-setup "./fixtures/ClassWithSingleFunction.cs" "^.+SomeFunction" t)
 '(csharpto-get-function-range nil)
 '(128 290))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :attributes    'single-inline
          :cursor-line   'signature
          :cursor-column 'end-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(660 983)))
 '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "ChangeName() {" nil)
 '(csharpto-get-function-range nil)
 '(660 983))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :scope-lf      t
          :attributes    'multiple-preceding
          :cursor-line   'attributes
          :cursor-column 'beg-of-line)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(177 659)))
 '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "^.+\\[Theory\\]" t)
 '(csharpto-get-function-range nil)
 '(177 659))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :scope-lf      t
          :attributes    'multiple-preceding
          :cursor-line   'body
          :cursor-column 'text)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(177 660)))
 '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "() => new" nil)
 '(csharpto-get-function-range t)
 '(177 660))

;; Regression
(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given `("File starts with the class definition (no namespace)"
           ,@(csharpto--test-generate-sentences
              :cursor-line   'signature))
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(035 129)))
 '(csharpto--test-buffer-setup "./fixtures/ClassOnlyNoImports.cs" "Hello" t)
 '(csharpto-get-function-range nil)
 '(035 129))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :item-before  'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(282 616)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "MinValue\n" nil)
 '(csharpto-get-function-range nil)
 '(282 616))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :item-before   'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(282 617)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "MinValue\n" nil)
 '(csharpto-get-function-range t)
 '(282 617))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :item-before   'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(282 617)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "MinValue\n" nil)
 '(csharpto-get-function-range t)
 '(282 617))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'preceding-blank
          :item-before   'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(617 795)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "        \.First" t)
 '(csharpto-get-function-range nil)
 '(617 795))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'preceding-blank
          :item-before   'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(617 796)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "        \.First" t)
 '(csharpto-get-function-range t)
 '(617 796))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :attributes    'single-preceding
          :cursor-line   'body
          :cursor-column 'end-of-line
          :item-before   'lambda-exp
          :item-after    'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1226 1517)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "(owner)," nil)
 '(csharpto-get-function-range nil)
 '(1226 1517))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :attributes    'single-preceding
          :cursor-line   'body
          :cursor-column 'end-of-line
          :item-before   'lambda-exp
          :item-after    'lambda-exp)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1225 1517)))
 '(csharpto--test-buffer-setup "./fixtures/BlogRepository.cs" "(owner)," nil)
 '(csharpto-get-function-range t)
 '(1225 1517))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'text
          :generic-type  'single)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(63 98)))
 '(csharpto--test-buffer-setup "./fixtures/Generics.cs" "=> default" nil)
 '(csharpto-get-function-range t)
 '(63 98))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'text
          :generic-type  'single)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(63 97)))
 '(csharpto--test-buffer-setup "./fixtures/Generics.cs" "=> default" nil)
 '(csharpto-get-function-range nil)
 '(63 97))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'signature
          :cursor-column 'text
          :generic-type  'multiple
          :type-constraint 'single)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(98 197)))
 '(csharpto--test-buffer-setup "./fixtures/Generics.cs" "T: new" t)
 '(csharpto-get-function-range nil)
 '(98 197))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'signature
          :cursor-column 'text
          :generic-type  'multiple
          :type-constraint 'single)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(97 197)))
 '(csharpto--test-buffer-setup "./fixtures/Generics.cs" "T: new" t)
 '(csharpto-get-function-range t)
 '(97 197))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'text
          :comment-line  'above)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(426 716)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "Id = " nil)
 '(csharpto-get-function-range nil)
 '(426 716))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'body
          :cursor-column 'text
          :comment-line  'above)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(426 717)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "Id = " nil)
 '(csharpto-get-function-range t)
 '(426 717))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(717 805)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "OneLiner" nil)
 '(csharpto-get-function-range nil)
 '(717 805))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(717 806)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "OneLiner" nil)
 '(csharpto-get-function-range t)
 '(717 806))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :comment-line  'signature)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(807 1070)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "void Log" t)
 '(csharpto-get-function-range nil)
 '(807 1070))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :comment-line  'signature)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(807 1071)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "void Log" t)
 '(csharpto-get-function-range t)
 '(807 1071))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'comments
          :cursor-column 'text
          :comment-line  'above
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1115 1490)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "name=\"a\"" t)
 '(csharpto-get-function-range nil)
 '(1115 1490))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'comments
          :cursor-column 'text
          :comment-line  'above
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1115 1491)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" "name=\"a\"" t)
 '(csharpto-get-function-range nil)
 '(1115 1491))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'body
          :cursor-column 'text
          :comment-block 'above
          :comment-block 'beg-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1491 1738)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" (rx "x++") t)
 '(csharpto-get-function-range nil)
 '(1491 1738))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'body
          :cursor-column 'text
          :comment-block 'above
          :comment-block 'beg-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1491 1739)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" (rx "x++") t)
 '(csharpto-get-function-range t)
 '(1491 1739))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :comment-line  'above
          :comment-line  'signature
          :comment-block 'signature
          :comment-line  'beg-of-scope
          :comment-line  'end-of-scope
          )
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1739 1978)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" ".+ToString" t)
 '(csharpto-get-function-range nil)
 '(1739 1978))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :comment-line  'above
          :comment-line  'signature
          :comment-block 'signature
          :comment-line  'beg-of-scope
          :comment-line  'end-of-scope
          )
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1738 1978)))
 '(csharpto--test-buffer-setup "./fixtures/Comments.cs" ".+ToString" t)
 '(csharpto-get-function-range t)
 '(1738 1978))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'comments
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'above-attributes)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(428 676)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "This is a" t)
 '(csharpto-get-function-range nil)
 '(428 676))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :scope-lf      t
          :cursor-line   'comments
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'above-attributes)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(428 677)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "This is a" t)
 '(csharpto-get-function-range t)
 '(428 677))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'end-of-line
          :attributes    'single-inline
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(677 743)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "//Comment$" nil)
 '(csharpto-get-function-range nil)
 '(677 743))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'end-of-line
          :attributes    'single-inline
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(677 744)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "//Comment$" nil)
 '(csharpto-get-function-range t)
 '(677 744))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'signature)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(744 959)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "void Log" t)
 '(csharpto-get-function-range nil)
 '(744 959))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'expression
          :cursor-line   'signature
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'signature)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(744 960)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "void Log" t)
 '(csharpto-get-function-range t)
 '(744 960))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'end-of-scope
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'before
          :comment-line  'above-attributes
          :comment-line  'above
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1004 1232)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "end of SomeMethod" t)
 '(csharpto-get-function-range nil)
 '(1004 1232))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'brackets
          :cursor-line   'end-of-scope
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-line  'before
          :comment-line  'above-attributes
          :comment-line  'above
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1004 1233)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "end of SomeMethod" t)
 '(csharpto-get-function-range t)
 '(1004 1233))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'preceding-blank
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-block 'before
          :comment-block 'above-attributes
          :comment-block 'above
          :comment-block 'beg-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1299 1300)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "\n +/\\* Block" t)
 '(csharpto-get-function-range nil)
 '(1299 1300))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'multi-line
          :scope-type    'brackets
          :cursor-line   'preceding-blank
          :cursor-column 'text
          :attributes    'multiple-preceding
          :comment-block 'before
          :comment-block 'above-attributes
          :comment-block 'above
          :comment-block 'beg-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1299 1558)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" "\n +/\\* Block" t)
 '(csharpto-get-function-range t)
 '(1299 1558))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :attributes    'multiple-preceding
          :comment-block 'above-attribute
          :comment-line  'above-attribute
          :comment-block 'above
          :comment-line  'signature
          :comment-block 'signature
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range nil))
  :then (format "%s should be returned" '(1559 1956)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" ".+ToString" t)
 '(csharpto-get-function-range nil)
 '(1559 1956))

(csharpto-test-run
 (csharpto--test-generate-scenario-description
  :given (csharpto--test-generate-sentences
          :signature     'single-line
          :scope-type    'expression
          :cursor-line   'body
          :cursor-column 'beg-of-line
          :attributes    'multiple-preceding
          :comment-block 'above-attribute
          :comment-line  'above-attribute
          :comment-block 'above
          :comment-line  'signature
          :comment-block 'signature
          :comment-line  'end-of-scope)
  :when (format "I call %s" '(csharpto-get-function-range t))
  :then (format "%s should be returned" '(1558 1956)))
 '(csharpto--test-buffer-setup "./fixtures/CommentsAndAttributes.cs" ".+ToString" t)
 '(csharpto-get-function-range t)
 '(1558 1956))

(provide 'csharpto-function-test)

;;; csharpto-function-test.el ends here
