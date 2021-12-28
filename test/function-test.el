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

(defmacro with-replica-buffer (original-buffer &rest body)
  "Execute BODY in a temp buffer with same contents as ORIGINAL-BUFFER."
  (declare (indent 1) (debug t))
  `(let ((buf ,original-buffer))
     (with-temp-buffer
       (insert-buffer-substring buf)
       (progn ,@body))))

(defun buffer-fancy-substring (region &optional delimiter context-lines)
  ""
  (with-replica-buffer (current-buffer)
    (let ((delimiter     (or delimiter "█"))
          (context-lines (or context-lines 1)))
      (let ((beg (save-excursion
                   (goto-char (car region))
                   ;; (unless (eq (point) (point-at-eol))
                   ;;   (delete-char 1))
                   (insert delimiter)
                   (beginning-of-line (1+ (- context-lines)))
                   (point)))
            (end (save-excursion
                   (goto-char (cadr region))
                   ;; (when (eq (point) (point-at-eol))
                   ;;   (goto-char (1- (point))))
                   (insert delimiter)
                   (end-of-line (1+ context-lines))
                   (point))))
        (buffer-substring beg end)))))

(defun run-test (fixture-path search-string goto-beg-of-match fcall expected-region)
  "docstring"

  ;; setup buffer
  (with-temp-buffer
    (insert-file-contents fixture-path)
    ;; go to intended point
    (search-forward search-string)
    (when goto-beg-of-match
      (goto-char (match-beginning 0)))

    ;; print buffer section with point and expected region
    ;; (insert "█")
    (message
     "%s\n...\n%s\n...\n"
     fcall
     (buffer-fancy-substring expected-region "█"))
    ;; (delete-char 1)

    ;; check if returned region equals expected
    ;;   if not, print buffer section with point and returned region
    (if-let ((returned-region (eval fcall)))
        (if (equal returned-region expected-region)
            (message "Pass")
          (message "Failed: unexpected region\n...\n%s\n...\n"
                   (buffer-fancy-substring expected-region "?")))
      (message "Failed: returned nil region"))
    ))

(run-test "./fixtures/Entity.cs"
          "Id = Guid.NewGuid()"
          t
          '(mabo3n/csharp--get-function-region nil)
          '(421 582))
