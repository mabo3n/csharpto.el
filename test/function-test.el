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

(require 'ert)
(require 'mabo3n/csharp-text-objects--function)

;; single header
;;   arrow function
;;     =>
;;       4x
;;     \n=>
;;       4x
;;   normal function
;;     {
;;       4x
;;     \n{
;;       4x
;; multiline header
;;   arrow function
;;     =>
;;       4x
;;     \n=>
;;       4x
;;   normal function
;;     {
;;       4x
;;     \n{
;;       4x
;;   middle of header

;; beg first line of header
;; end of line of header

;; singlefun
;;   4x
;; singlefun with spaces around
;;   4x
;; previousfun
;;   4x
;; previousfun with spaces before
;;   4x
;; afterfun
;;   4x
;; afterfun with spaces after
;;   4x

;; empty line before function
;;   4x
;; empty line after function
;;   4x
;; empty line between function
;;   4x

(defun test-log-message (format-string &rest args)
  "Log FORMAT-STRING with ARGS into the test run buffer."
  (save-excursion
    (let ((test-buffer "*Test run*"))
      (unless (get-buffer test-buffer)
        (set-buffer (get-buffer-create test-buffer))
        (insert "\n\n*** Test run ***\n\n")
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
  (-let* (((begs . rest) (apply '-zip-lists regions))
          (ends (car (last rest)))
          (highest-beginning    (apply 'max begs))
          (lowest-end           (apply 'min ends)))
    (and (< highest-beginning lowest-end)
         (list highest-beginning lowest-end))))

(defun test-log-code-snippet (point region expected-region)
  "Outputs a properly formatted code-snippet.

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

(defun run-test (fixture-path search-string goto-beg-of-match fcall expected-region)
  "TODO docstring"

  ;; setup buffer
  (with-temp-buffer
    (insert-file-contents fixture-path)
    (search-forward search-string)
    (when goto-beg-of-match
      (goto-char (match-beginning 0)))

    (test-log-message "\nGIVEN ...\n WHEN ...\n THEN ...\n\n")
    (let* ((returned-region (eval fcall))
           (regions-match-p (equal returned-region expected-region)))
      (cond
       (regions-match-p (test-log-message "Pass\n"))
       (returned-region (test-log-message "Fail: region mismatch\n"))
       (t               (test-log-message "Fail: nil region\n")))
      (unless nil ;; regions-match-p
        (test-log-message "\n%s\n" fcall)
        (test-log-code-snippet (point) returned-region expected-region)))))

(run-test "./fixtures/Entity.cs"
          "Id = Guid.NewGuid()"
          t
          '(mabo3n/csharp--get-function-region nil)
          '(421 581))
