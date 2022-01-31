;;; csharpto-function-test.el --- Tests for csharpto-function.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'csharpto-function)

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

(defun csharpto--test-buffer-fancy-substring (regions &optional context-lines)
  "Return a substring of current buffer with REGIONS highlighted.

REGIONS has form `((BEG [END] PLIST)...)` where PLIST is a property
list to be applied to the respective region in the substring.
If END is not provided, `(1+ BEG)` is used. IF BEG is nil,
the region is ignored.

The returned substring includes N extra lines before and N extra
after the matched regions, where N = CONTEXT-LINES. N Defaults to 0."
  (let ((regions (--filter (numberp (car it)) regions))
        (context-lines (or context-lines 0)))
    (csharpto--test-with-replica-buffer (current-buffer)
      (dolist (region regions)
        (let* ((beg   (car region))
               (end   (or (and (consp (cadr region)) (1+ beg))
                          (cadr region)))
               (plist (or (caddr region) (cadr region))))
          (add-text-properties beg end `(font-lock-face ,plist))))
      (let* ((positions   (append (mapcar 'car  regions)
                                  (mapcar 'cadr regions)))
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

(defun csharpto--test-region-overlap (&rest regions)
  "Return a region that overlaps all REGIONS.

REGIONS has form `((BEG END)...)'."
  (-let* (((begs . rest)     (apply '-zip-lists regions))
          (ends              (car (last rest)))
          (highest-beginning (apply 'max begs))
          (lowest-end        (apply 'min ends)))
    (and (< highest-beginning lowest-end)
         (list highest-beginning lowest-end))))

(defun csharpto--test-log-code-snippet (point region expected-region)
  "Output a properly formatted code-snippet.

Format and forward POINT, REGION and EXPECTED-REGION to
function `csharpto--test-buffer-fancy-substring'."
  (let* ((overlap (and region expected-region
                       (csharpto--test-region-overlap region expected-region)))
         (regions (list `(,@expected-region (:background "LightGoldenrod3"))
                        `(,@region          (:background "IndianRed2"))
                        `(,@overlap         (:background "SeaGreen2"))
                        `(,point            (:background "gray")))))
    (csharpto--test-log-message
     "...\n%s\n..."
     (csharpto--test-buffer-fancy-substring regions 1))))

(defvar csharpto--test-docstrings-alist
  '((:signature     . ((single-line  . "The function has a single-line signature")
                       (multi-line   . "The function has a multi-line signature")))
    (:scope-type    . ((expression   . "The function is expression-bodied =>")
                       (brackets     . "The function body is defined with brackets { }")))
    (:scope-lf      . "There's a line feed before opening the function scope")
    (:attributes    . ((single-preceding     .  "The function has a preceding attribute")
                       (single-inline        . "The function has an inline attribute")
                       (multiple-preceding   . "The function has multiple preceding attributes")
                       (multiple-inline      . "The function has multiple inline attributes")))
    (:cursor-line   . ((preceding-blank      . "The cursor lies in blank lines before the function")
                       (attributes   . "The cursor lies in attribute declarations before the function")
                       (signature    . "The cursor lies in the function signature")
                       (beg-of-scope . "The cursor lies in the beginning of function scope")
                       (body         . "The cursor lies in the function body")
                       (end-of-scope . "The cursor lies in the end of function scope")
                       (succeeding-blank . "The cursor lies in blank lines succeeding function")
                       (blank            . "The cursor lies in blank lines between functions")))
    (:cursor-column . ((beg-of-line      . "The cursor lies in the beginning of line")
                       (preceding-blank  . "The cursor lies in the indentation")
                       (text             . "The cursor lies in the text")
                       (succeeding-blank . "The cursor lies in the blank spaces ending the line")
                       (end-of-line      . "The cursor lies in the end of line"))))
  "Mapping between test prop values and their textual description.")

(defun csharpto--test-generate-sentences (&rest plist)
  "Return a list of corresponding sentences for each prop in PLIST.

See variable `csharpto--test-docstrings-alist'."
  (when-let* ((_     plist)
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

(defun csharpto-test-run (scenario arrange-function fcall expected-region)
  "Run a test according to arguments."
  (csharpto--test-log-message "\n%s\n\n" scenario)
  (with-temp-buffer
    (eval arrange-function)
    (let* ((returned-region (ignore-errors (eval fcall)))
           (regions-match-p (equal returned-region expected-region)))
      (if regions-match-p
          (csharpto--test-log-message "Pass\n")
        (csharpto--test-log-message "Fail: region mismatch\n\n")
        (csharpto--test-log-message "%s ⇒ %s\n\n" fcall returned-region)
        (csharpto--test-log-message "Expected: %s\n" expected-region)
        (csharpto--test-log-code-snippet (point) returned-region expected-region))))
  (csharpto--test-log-message "\n%s" (make-string 30 ?-)))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :scope-lf      t
                            :cursor-line   'signature
                            :cursor-column 'text)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(421 581)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "public MyEntity(string name)" t)
          '(csharpto-get-function-region nil)
          '(421 581))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :scope-lf      'break
                            :cursor-line   'body
                            :cursor-column 'preceding-blank)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(421 582)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "  logs = new List<LogEntry>" t)
          '(csharpto-get-function-region t)
          '(421 582))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :cursor-line   'blank
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(581 582)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
          '(csharpto-get-function-region nil)
          '(581 582))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :cursor-line   'blank
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(581 628)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
          '(csharpto-get-function-region t)
          '(581 628))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :scope-type    'expression
                            :cursor-line   'body
                            :cursor-column 'succeeding-blank)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(582 628)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
          '(csharpto-get-function-region nil)
          '(582 628))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :scope-type    'expression
                            :cursor-line   'body
                            :cursor-column 'succeeding-blank)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(582 630)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
          '(csharpto-get-function-region t)
          '(582 630))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'multi-line
                            :scope-type    'expression
                            :scope-lf      t
                            :cursor-line   'signature
                            :cursor-column 'preceding-blank)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(630 768)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
          '(csharpto-get-function-region nil)
          '(630 768))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'multi-line
                            :scope-type    'expression
                            :scope-lf      t
                            :cursor-line   'signature
                            :cursor-column 'preceding-blank)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(630 769)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
          '(csharpto-get-function-region t)
          '(630 769))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :cursor-line   'body
                            :cursor-column 'end-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(769 888)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
          '(csharpto-get-function-region nil)
          '(769 888))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :cursor-line   'body
                            :cursor-column 'end-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(769 891)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
          '(csharpto-get-function-region t)
          '(769 891))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :cursor-line   'blank
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(888 1010)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "\n\n\\s +public IEnumerable" t)
          '(csharpto-get-function-region t)
          '(888 1010))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'expression
                            :cursor-line   'body
                            :cursor-column 'end-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(891 1010)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "=> this" nil)
          '(csharpto-get-function-region nil)
          '(891 1010))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'expression
                            :cursor-line   'body
                            :cursor-column 'end-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(888 1010)))
          '(csharpto--test-buffer-setup "./fixtures/Entity.cs" "=> this" nil)
          '(csharpto-get-function-region t)
          '(888 1010))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :cursor-line   'signature
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(127 290)))
          '(csharpto--test-buffer-setup "./fixtures/ClassWithSingleFunction.cs" "^.+SomeFunction" t)
          '(csharpto-get-function-region t)
          '(127 290))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :cursor-line   'signature
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(128 290)))
          '(csharpto--test-buffer-setup "./fixtures/ClassWithSingleFunction.cs" "^.+SomeFunction" t)
          '(csharpto-get-function-region nil)
          '(128 290))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'single-line
                            :scope-type    'brackets
                            :attributes    'single-inline
                            :cursor-line   'signature
                            :cursor-column 'end-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(660 983)))
          '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "ChangeName() {" nil)
          '(csharpto-get-function-region nil)
          '(660 983))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'multi-line
                            :scope-type    'expression
                            :scope-lf      t
                            :attributes    'multiple-preceding
                            :cursor-line   'attributes
                            :cursor-column 'beg-of-line)
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(177 659)))
          '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "^.+\\[Theory\\]" t)
          '(csharpto-get-function-region nil)
          '(177 659))

(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given (csharpto--test-generate-sentences
                            :signature     'multi-line
                            :scope-type    'expression
                            :scope-lf      t
                            :attributes    'multiple-preceding
                            :cursor-line   'body
                            :cursor-column 'text)
                    :when (format "I call %s" '(csharpto-get-function-region t))
                    :then (format "%s should be returned" '(177 660)))
          '(csharpto--test-buffer-setup "./fixtures/Attributes.cs" "() => new" nil)
          '(csharpto-get-function-region t)
          '(177 660))

;; Regression
(csharpto-test-run (csharpto--test-generate-scenario-description
                    :given `("File starts with the class definition (no namespace)"
                            ,@(csharpto--test-generate-sentences
                               :cursor-line   'signature))
                    :when (format "I call %s" '(csharpto-get-function-region nil))
                    :then (format "%s should be returned" '(035 129)))
                   '(csharpto--test-buffer-setup "./fixtures/ClassOnlyNoImports.cs" "Hello" t)
                   '(csharpto-get-function-region nil)
                   '(035 129))

(provide 'csharpto-function-test)

;;; csharpto-function-test.el ends here
