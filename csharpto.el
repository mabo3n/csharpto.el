;;; csharpto.el --- C# text objects -*- lexical-binding: t -*-

;; Author: mabo3n <dolly.marcel@gmail.com>
;; Maintainer: mabo3n
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (evil "1"))
;; Homepage: https://github.com/mabo3n/csharpto
;; Keywords: convenience, c, evil

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


;;; Commentary:

;; Add evil text objects for some constructs in the C# language.
;; Currently supported:
;;  - [iF] `csharpto-i-FUNCTION': from first to last character of current function.
;;  - [aF] `csharpto-a-FUNCTION': lines spamming current function + surrounding blank-lines.
;;  - [iS] `csharpto-i-SCOPE': from first to last character of current statement with a scope.
;;  - [aS] `csharpto-a-SCOPE': lines spamming current statement with a scope + surrounding blank lines.

;;; Code:

(require 'csharpto-function)
(require 'evil)

(evil-define-text-object csharpto-i-FUNCTION (count &optional beg end type)
  "From first to last character of current function."
  :type nil
  (csharpto--get-function-range nil nil))

(evil-define-text-object csharpto-a-FUNCTION (count &optional beg end type)
  "Lines spamming current function + surrounding blank-lines."
  :type nil
  (csharpto--get-function-range t nil))

(evil-define-text-object csharpto-i-SCOPE (count &optional beg end type)
  "From first to last character of current statement with a scope."
  :type nil
  (csharpto--get-function-range nil t))

(evil-define-text-object csharpto-a-SCOPE (count &optional beg end type)
  "Lines spamming current statement with a scope + surrounding blank lines."
  :type nil
  (csharpto--get-function-range t t))


;;; Bindings

(defvar csharpto-default-bindings-alist
  '(("iF" . csharpto-i-FUNCTION)
    ("aF" . csharpto-a-FUNCTION)
    ("iS" . csharpto-i-SCOPE)
    ("aS" . csharpto-a-SCOPE))
  "Default bindings for the text objects.")

(defun csharpto-bind-keys (&optional keymaps)
  "Set default text object bindings.

Bind according to variable `csharpto-default-bindings-alist'
in each keymap of KEYMAPS. If KEYMAPS is nil, use evil's global
keymaps (`evil-operator-state-map' and `evil-visual-state-map')."
  (interactive)
  (let ((keymaps (or keymaps (list evil-operator-state-map
                                   evil-visual-state-map))))
    (dolist (kmap keymaps)
      (unless (keymapp kmap)
        (if (keymapp (symbol-value kmap))
            (setq kmap (symbol-value kmap))
          (error "Not a keymap: %s" kmap)))
      (dolist (binding csharpto-default-bindings-alist)
        (define-key kmap (car binding) (cdr binding))))))

(defun csharpto-bind-keys-locally ()
  "Set default text object bindings locally.

Call function `csharpto-bind-keys' with evil's local keymaps
\(`evil-operator-state-local-map' and `evil-visual-state-local-map'\)."
  (interactive)
  (csharpto-bind-keys (list evil-operator-state-local-map
                            evil-visual-state-local-map)))

(defun csharpto--set-default-bindings-for-mode (mode &optional disable)
  "Make default text object bindings available for MODE.

If DISABLE is non-nil, disable this behavior instead."
  (let ((mode-hook-sym
         (intern (concat (symbol-name mode) "-hook"))))
    (unless (boundp mode-hook-sym)
      (error "Mode hook not bound: %s" mode-hook-sym))
    (funcall (if disable #'remove-hook #'add-hook)
             mode-hook-sym
             #'csharpto-bind-keys-locally)))

(defun csharpto-use-default-bindings-in-csharp-mode (&optional disable)
  "Make default text object bindings available for `csharp-mode' only.

When called interactively with `\\[universal-argument]' prefix,
DISABLE is set to t, which disables the behavior instead."
  (interactive "P")
  (csharpto--set-default-bindings-for-mode 'csharp-mode disable))

(provide 'csharpto)

;;; csharpto.el ends here
