;;; csharpto.el --- C# text objects -*- lexical-binding: t -*-

;; Author: mabo3n <dolly.marcel@gmail.com>
;; Maintainer: mabo3n
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/mabo3n/csharpto
;; Keywords: convenience, c

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

;; Add evil text object definitions for some constructs in the C# language.
;; Currently supported:
;;  - function

;;; Code:

(require 'csharpto-function)
(require 'evil)

(evil-define-text-object evil-inner-csharpto-function (count &optional beg end type)
  (csharpto-get-function-range nil))

(evil-define-text-object evil-a-csharpto-function (count &optional beg end type)
  (csharpto-get-function-range t))

(provide 'csharpto)

;;; csharpto.el ends here
