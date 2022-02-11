;;; csharpto-test.el --- Custom test framework -*- lexical-binding: t -*-

;;; Commentary:

;; Utility functions for writing tests.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'evil)
(require 'csharp-mode)

(defconst csharpto--test-buffer-name "*Test run*"
  "Name of the buffer created to output test results.")

(defvar csharpto--test-sentences-alist
  '((:cursor-line   . ((preceding-blank  . "The cursor lies in blank lines before the text object")
                       (under            . "The cursor lies under the text object")
                       (succeeding-blank . "The cursor lies in blank lines succeeding the text object")
                       (blank            . "The cursor lies in blank lines between text objects")))
    (:cursor-column . ((beg-of-line      . "The cursor lies in the beginning of line")
                       (preceding-blank  . "The cursor lies in the indentation")
                       (text             . "The cursor lies in the text")
                       (succeeding-blank . "The cursor lies in the blank spaces ending the line")
                       (end-of-line      . "The cursor lies in the end of line"))))
  "Mapping between test prop values and their equivalent sentences.

This variable can (and should) be modified by each test file to enrich
the test scenarios and to allow differentiating them.")

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

(defun csharpto--test-reset-buffer ()
  "If existent, kill the test buffer."
  (interactive)
  (ignore-errors (kill-buffer csharpto--test-buffer-name)))

(defun csharpto--test-log-message (format-string &rest args)
  "Log FORMAT-STRING with ARGS into the test run buffer."
  (save-excursion
    (let ((test-buffer csharpto--test-buffer-name))
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
   point
   range
   expected-range))

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

(defun csharpto--test-visualize-range-at-point ()
  "Visualize range of `csharpto-test-run' expectation at point.

Open fixture file and visual select the range of expectation.

This is a facility to debug tests, intended for interactive use."
  (interactive)
  (evil-exit-visual-state)
  (let* ((p (point))
         (range (read
                 (and (re-search-forward (rx eol))
                      (re-search-backward "csharpto--get-function-range")
                      (re-search-forward (rx digit (+ (or digit space))))
                      (goto-char (1- (point)))
                      (thing-at-point 'list t))))
         (fixture-path (save-match-data
                         (and (re-search-backward ":file")
                             (forward-symbol 2)
                             (thing-at-point 'symbol t)))))
    (goto-char (match-beginning 0))
    (evil-visual-char)
    (goto-char (match-end 0))
    (find-file fixture-path)
    (evil-exit-visual-state)
    (goto-char (car range))
    (evil-visual-char)
    (goto-char (1- (cadr range)))))

(provide 'csharpto-test)

;;; csharpto-test.el ends here
