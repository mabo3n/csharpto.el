;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; (require 'mabo3n/csharp-text-objects--function)

(defun test-log-message (format-string &rest args)
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

(defmacro with-replica-buffer (original-buffer &rest body)
  "Execute BODY in a temp buffer with same contents as ORIGINAL-BUFFER."
  (declare (indent 1) (debug t))
  `(let ((buf ,original-buffer))
     (with-temp-buffer
       (insert-buffer-substring buf)
       (progn ,@body))))

(defun buffer-fancy-substring (regions &optional context-lines)
  "Return a substring of current buffer with REGIONS highlighted.

REGIONS has form `((BEG END PLIST)...)` where PLIST is a property
list to be applied to the respective region in the substring.

The returned substring includes N extra lines before and N extra
after the matched regions, where N = CONTEXT-LINES. N Defaults to 0."
  (let ((context-lines (or context-lines 0)))
    (with-replica-buffer (current-buffer)
      (dolist (region regions)
        (let ((beg   (car region))
              (end   (cadr region))
              (plist (caddr region)))
          (add-text-properties beg end `(font-lock-face ,plist))))
      (let* ((positions   (append (mapcar 'car  regions)
                                  (mapcar 'cadr regions)))
             (context-beg (progn
                            (goto-char (apply 'min positions))
                            (beginning-of-line (1+ (- context-lines)))
                            (point)))
             (context-end (progn
                            (goto-char (apply 'max positions))
                            (end-of-line (1+ context-lines))
                            (point))))
        (buffer-substring context-beg context-end)))))

(defun region-overlap (&rest regions)
  "Return a region that overlaps all REGIONS.

REGIONS has form `((BEG END)...)'."
  (-let* (((begs . rest)     (apply '-zip-lists regions))
          (ends              (car (last rest)))
          (highest-beginning (apply 'max begs))
          (lowest-end        (apply 'min ends)))
    (and (< highest-beginning lowest-end)
         (list highest-beginning lowest-end))))

(defun test-log-code-snippet (point region expected-region)
  "Output a properly formatted code-snippet.

Format and forward POINT, REGION and EXPECTED-REGION to
function `buffer-fancy-substring'."
  (let ((overlap (region-overlap region expected-region)))
    (test-log-message
     "...\n%s\n..."
     (buffer-fancy-substring
      (list `(,@expected-region  (:background "LightGoldenrod3"))
            `(,@region           (:background "IndianRed2"))
            `(,@overlap          (:background "SeaGreen2"))
            `(,point ,(1+ point) (:background "gray")))
      1))))

(defvar docstrings-alist
  '((:header        . ((single-line  . "The function has a single-line header")
                       (multi-line   . "The function has a multi-line header")))
    (:scope-type    . ((expression   . "The function is expression-bodied =>")
                       (brackets     . "The function body is defined with brackets { }")))
    (:scope-lf      . "There's a line feed before opening the function scope")
    (:cursor-line   . ((preceding-blank . "The cursor lies in blank lines before the function")
                       (header       . "The cursor lies in the function header")
                       (beg-of-scope . "The cursor lies in the beginning of function scope")
                       (body         . "The cursor lies in the function body")
                       (end-of-scope . "The cursor lies in the end of function scope")
                       (succeeding-blank . "The cursor lies in blank lines succeeding function")
                       (blank        . "The cursor lies in blank lines between functions")))
    (:cursor-column . ((beg-of-line  . "The cursor lies in the beginning of line")
                       (preceding-blank  . "The cursor lies in the indentation")
                       (text         . "The cursor lies in the text")
                       (succeeding-blank . "The cursor lies in the blank spaces ending the line")
                       (end-of-line  . "The cursor lies in the end of line"))))
  "Mapping between test prop values and their textual description.")

(defun generate-sentences (&rest plist)
  "Return a list of corresponding sentences for each prop in PLIST.

See variable `docstrings-alist'."
  (when-let* ((_     plist)
              (key   (car plist))
              (value (cadr plist))
              (options  (alist-get key docstrings-alist))
              (sentence (or (and (stringp options) value options)
                            (alist-get value options)
                            (format "Property %s has value '%s"
                                    key value))))
    (cons sentence (apply #'generate-sentences (cddr plist)))))

(defun generate-scenario-description (&rest clauses)
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

(defun buffer-setup (fixture search-regexp goto-beg-of-match)
  "Setup current buffer with FIXTURE contents for the test.

FIXTURE is the fixture file path.
Perform a `re-search-forward' with SEARCH-REGEXP and go to
beginning of match if GOTO-BEG-OF-MATCH is non-nil."
  (insert-file-contents fixture)
  (csharp-mode)
  (re-search-forward search-regexp nil t)
  (when goto-beg-of-match
    (goto-char (match-beginning 0))))

(defun run-test (scenario arrange-function fcall expected-region)
  "Run a test according to arguments."
  (test-log-message "\n%s\n\n" scenario)
  (with-temp-buffer
    (eval arrange-function)
    (let* ((returned-region (eval fcall))
           (regions-match-p (equal returned-region expected-region)))
      (if regions-match-p
          (test-log-message "Pass\n")
        (test-log-message "Fail: region mismatch\n\n")
        (test-log-message "%s ⇒ %s\n\n" fcall returned-region)
        (test-log-message "Expected: %s\n" expected-region)
        (test-log-code-snippet (point) returned-region expected-region))))
  (test-log-message "\n%s" (make-string 30 ?-)))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'brackets
                                      :scope-lf      t
                                      :cursor-line   'header
                                      :cursor-column 'text)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(421 581)))
          '(buffer-setup "./fixtures/Entity.cs" "public MyEntity(string name)" t)
          '(mabo3n/csharp--get-function-region nil)
          '(421 581))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'brackets
                                      :scope-lf      'break
                                      :cursor-line   'body
                                      :cursor-column 'preceding-blank)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(421 582)))
          '(buffer-setup "./fixtures/Entity.cs" "  logs = new List<LogEntry>" t)
          '(mabo3n/csharp--get-function-region t)
          '(421 582))

(run-test (generate-scenario-description
           :given (generate-sentences :cursor-line   'blank
                                      :cursor-column 'beg-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(581 582)))
          '(buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
          '(mabo3n/csharp--get-function-region nil)
          '(581 582))

(run-test (generate-scenario-description
           :given (generate-sentences :cursor-line   'blank
                                      :cursor-column 'beg-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(581 628)))
          '(buffer-setup "./fixtures/Entity.cs" "^\n\s *int OneLiner()" t)
          '(mabo3n/csharp--get-function-region t)
          '(581 628))

(run-test (generate-scenario-description
           :given (generate-sentences :scope-type    'expression
                                      :cursor-line   'body
                                      :cursor-column 'succeeding-blank)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(582 628)))
          '(buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
          '(mabo3n/csharp--get-function-region nil)
          '(582 628))

(run-test (generate-scenario-description
           :given (generate-sentences :scope-type    'expression
                                      :cursor-line   'body
                                      :cursor-column 'succeeding-blank)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(582 630)))
          '(buffer-setup "./fixtures/Entity.cs" "3 \\+ 4 \\+ 5; $" nil)
          '(mabo3n/csharp--get-function-region t)
          '(582 630))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'multi-line
                                      :scope-type    'expression
                                      :scope-lf      t
                                      :cursor-line   'header
                                      :cursor-column 'preceding-blank)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(630 768)))
          '(buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
          '(mabo3n/csharp--get-function-region nil)
          '(630 768))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'multi-line
                                      :scope-type    'expression
                                      :scope-lf      t
                                      :cursor-line   'header
                                      :cursor-column 'preceding-blank)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(630 769)))
          '(buffer-setup "./fixtures/Entity.cs" "level = default\n\\s *)" nil)
          '(mabo3n/csharp--get-function-region t)
          '(630 769))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'brackets
                                      :cursor-line   'body
                                      :cursor-column 'end-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(769 888)))
          '(buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
          '(mabo3n/csharp--get-function-region nil)
          '(769 888))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'brackets
                                      :cursor-line   'body
                                      :cursor-column 'end-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(769 891)))
          '(buffer-setup "./fixtures/Entity.cs" "return a \\+ b;" nil)
          '(mabo3n/csharp--get-function-region t)
          '(769 891))

(run-test (generate-scenario-description
           :given (generate-sentences :cursor-line   'blank
                                      :cursor-column 'beg-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(888 1010)))
          '(buffer-setup "./fixtures/Entity.cs" "\n\n\\s +public IEnumerable" t)
          '(mabo3n/csharp--get-function-region t)
          '(888 1010))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'expression
                                      :cursor-line   'body
                                      :cursor-column 'end-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region nil))
           :then (format "%s should be returned" '(891 1010)))
          '(buffer-setup "./fixtures/Entity.cs" "=> this" nil)
          '(mabo3n/csharp--get-function-region nil)
          '(891 1010))

(run-test (generate-scenario-description
           :given (generate-sentences :header        'single-line
                                      :scope-type    'expression
                                      :cursor-line   'body
                                      :cursor-column 'end-of-line)
           :when (format "I call %s" '(mabo3n/csharp--get-function-region t))
           :then (format "%s should be returned" '(888 1010)))
          '(buffer-setup "./fixtures/Entity.cs" "=> this" nil)
          '(mabo3n/csharp--get-function-region t)
          '(888 1010))
