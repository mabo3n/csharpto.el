;;; csharpto.el --- C# text objects -*- lexical-binding: t -*-

;; Author: mabo3n <https://github.com/mabo3n/>
;; Maintainer: mabo3n
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (evil "1"))
;; Homepage: https://github.com/mabo3n/csharpto
;; Keywords: convenience, c, evil

;;; License:

;; This file is not part of GNU Emacs
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Add evil text objects for some constructs in the C# language.

;; Currently supported:
;;   - [af] `csharpto-a-function': from first to last character of current function
;;   - [aF] `csharpto-a-FUNCTION': lines spamming current function + surrounding blank lines
;;   - [if] `csharpto-i-function': from first to last character of current function's body
;;   - [iF] `csharpto-i-FUNCTION': lines spamming current function's body + spaces until "{ }"
;;   - [as] `csharpto-a-scope': from first to last character of current statement with a scope
;;   - [aS] `csharpto-a-SCOPE': lines spamming current statement with a scope + surrounding blank lines
;;   - [is] `csharpto-i-scope': from first to last character of current statement's scope
;;   - [iS] `csharpto-i-SCOPE': lines spamming current statement's scope + spaces until "{ }"

;;; Code:

(require 'csharpto-function)
(require 'evil)

(evil-define-text-object csharpto-a-function (count &optional beg end type)
  "From first to last character of current function."
  :type nil
  (csharpto--get-function-range nil nil nil))

(evil-define-text-object csharpto-a-FUNCTION (count &optional beg end type)
  "Lines spamming current function + surrounding blank-lines."
  :type nil
  (csharpto--get-function-range t nil nil))

(evil-define-text-object csharpto-i-function (count &optional beg end type)
  "From first to last character of current function's body."
  :type nil
  (csharpto--get-function-range nil t nil))

(evil-define-text-object csharpto-i-FUNCTION (count &optional beg end type)
  "Lines spamming current function's body + spaces until \"{ }\"."
  :type nil
  (csharpto--get-function-range t t nil))

(evil-define-text-object csharpto-a-scope (count &optional beg end type)
  "From first to last character of current statement with a scope."
  :type nil
  (csharpto--get-function-range nil nil t))

(evil-define-text-object csharpto-a-SCOPE (count &optional beg end type)
  "Lines spamming current statement with a scope + surrounding blank lines."
  :type nil
  (csharpto--get-function-range t nil t))

(evil-define-text-object csharpto-i-scope (count &optional beg end type)
  "From first to last character of current statement's scope."
  :type nil
  (csharpto--get-function-range nil t t))

(evil-define-text-object csharpto-i-SCOPE (count &optional beg end type)
  "Lines spamming current statement's scope + spaces until \"{ }\"."
  :type nil
  (csharpto--get-function-range t t t))

;; Bindings
(defvar csharpto-default-bindings-alist
  '(("af" . csharpto-a-function)
    ("aF" . csharpto-a-FUNCTION)
    ("if" . csharpto-i-function)
    ("iF" . csharpto-i-FUNCTION)
    ("as" . csharpto-a-scope)
    ("aS" . csharpto-a-SCOPE)
    ("is" . csharpto-i-scope)
    ("iS" . csharpto-i-SCOPE))
  "Default bindings for the text objects.")

(defun csharpto--bind-keys (keymaps)
  "Set default text object bindings in each keymap of KEYMAPS.

Bind according to variable `csharpto-default-bindings-alist'."
  (interactive)
  (dolist (kmap keymaps)
    (unless (keymapp kmap)
      (if (keymapp (symbol-value kmap))
          (setq kmap (symbol-value kmap))
        (error "Not a keymap: %s" kmap)))
    (dolist (binding csharpto-default-bindings-alist)
      (define-key kmap (car binding) (cdr binding)))))

(defun csharpto-bind-keys-globally ()
  "Set default text object bindings in Evil's global keymaps.

\(`evil-operator-state-map' and `evil-visual-state-map'\)."
  (interactive)
  (csharpto--bind-keys (list evil-operator-state-map
                             evil-visual-state-map)))

(defun csharpto-bind-keys-locally ()
  "Set default text object bindings in Evil's local keymaps.

\(`evil-operator-state-local-map' and `evil-visual-state-local-map'\)."
  (interactive)
  (csharpto--bind-keys (list evil-operator-state-local-map
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
