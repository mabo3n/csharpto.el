;;; -*- Mode: Emacs-Lisp -*-

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
