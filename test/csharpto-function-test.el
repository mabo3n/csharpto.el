;;; csharpto-function-test.el --- Tests for csharpto-function.el -*- lexical-binding: t -*-

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

;; Evaluate to run tests.
;; Press <return> over a expectation (:test entry) to visualize it.

;;; Code:

(require 'csharpto-function)
(require 'csharpto-test)

(setq
 csharpto--test-sentences-alist
 '((:cursor-line   . ((preceding-blank    . "The cursor lies in blank lines before the function")
                      (comments     . "The cursor lies in comments above the function")
                      (attributes   . "The cursor lies in attribute declarations before the function")
                      (signature    . "The cursor lies in the function signature")
                      (beg-of-scope . "The cursor lies in the beginning of function scope")
                      (body         . "The cursor lies in the function body")
                      (end-of-scope . "The cursor lies in the end of function scope")
                      (succeeding-blank  . "The cursor lies in blank lines succeeding function")
                      (blank             . "The cursor lies in blank lines between functions")))
   (:cursor-column . ((beg-of-line       . "The cursor lies in the beginning of line")
                      (preceding-blank   . "The cursor lies in the indentation")
                      (text              . "The cursor lies in the text")
                      (succeeding-blank  . "The cursor lies in the blank spaces ending the line")
                      (end-of-line       . "The cursor lies in the end of line")))
   (:signature     . ((single-line  . "The function has a single-line signature")
                      (multi-line   . "The function has a multi-line signature")))
   (:scope-type    . ((expression   . "The function is expression-bodied =>")
                      (brackets     . "The function body is defined with brackets { }")))
   (:scope-lf      . "There's a line feed before opening the function scope")
   (:attributes    . ((single-preceding   . "The function has a preceding attribute")
                      (single-inline      . "The function has an inline attribute")
                      (multiple-preceding . "The function has multiple preceding attributes")
                      (multiple-inline    . "The function has multiple inline attributes")))
   (:class-only    . "The file starts with the class definition (no namespace)")
   (:no-blank-lines   . "There's no blank lines!")
   (:item-before   . ((none       . "There's nothing before the cursor inside the function")
                      (lambda-exp . "There's a lambda expression before the cursor inside the function")))
   (:item-under    . ((none       . "There's nothing under the cursor inside the function")
                      (lambda-exp . "There's a lambda expression under the cursor inside the function")))
   (:item-after    . ((none       . "There's nothing after the cursor inside the function")
                      (lambda-exp . "There's a lambda expression after the cursor inside the function")))
   (:generic-type     . ((none      . "The function does not specifies generic types")
                         (single    . "The function specifies a single generic type")
                         (multiple  . "The function specifies multiple generic types")))
   (:type-constraints . ((none      . "The function has no type constraints")
                         (single    . "The function specifies a constraint for a generic type")
                         (multiple  . "The function specifies multiple constraints for generic types")))
   (:comment-line  . ((before           . "There's a line comment \"//\" before the function")
                      (above-attributes . "There's a line comment \"//\" above the function attribute(s)")
                      (attributes       . "There's a line comment \"//\" within the function attribute(s)")
                      (above            . "There's a line comment \"//\" above the function")
                      (signature        . "There's a line comment \"//\" within the function signature")
                      (beg-of-scope     . "There's a line comment \"//\" in the beginning of scope")
                      (end-of-scope     . "There's a line comment \"//\" in the end of scope")
                      (below            . "There's a line comment \"//\" below the function")))
   (:comment-block . ((before           . "There's a block comment \"/**/\" before the function")
                      (above-attributes . "There's a block comment \"/**/\" above the function attribute(s)")
                      (attributes       . "There's a block comment \"/**/\" within the function attribute(s)")
                      (above            . "There's a block comment \"/**/\" above the function")
                      (signature        . "There's a block comment \"/**/\" within the function signature")
                      (beg-of-scope     . "There's a block comment \"/**/\" in the beginning of scope")
                      (end-of-scope     . "There's a block comment \"/**/\" in the end of scope")
                      (below            . "There's a block comment \"/**/\" below the function")))))

;; Press enter in a expectation below to visualize it
(define-key evil-normal-state-local-map (kbd "<return>")
  #'csharpto--test-visualize-range-at-point)

(csharpto--test-reset-buffer)

(csharpto--test-run
 :id 'F1
 :setup (list :file "./fixtures/Entity.cs"
              :find "public MyEntity(string name)"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (429 580))
         ((csharpto--get-function-range t   nil) (421 582))
         ((csharpto--get-function-range nil   t) (480 570))
         ((csharpto--get-function-range t     t) (467 579)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'text))

(csharpto--test-run
 :id 'F2
 :setup (list :file "./fixtures/Entity.cs"
              :find "^\n\s *int OneLiner()"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (581 582))
         ((csharpto--get-function-range t   nil) (581 628))
         ((csharpto--get-function-range nil   t) ())
         ((csharpto--get-function-range t     t) ()))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto--test-run
 :id 'F3
 :setup (list :file "./fixtures/Entity.cs"
              :find "\\+ 5; $"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (590 627))
         ((csharpto--get-function-range t   nil) (582 630))
         ((csharpto--get-function-range nil   t) (608 626))
         ((csharpto--get-function-range t     t) (607 626)))
 :props (list :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'succeeding-blank))

(csharpto--test-run
 :id 'F4
 :setup (list :file "./fixtures/Entity.cs"
              :find "level = default\n\\s *)"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (638 767))
         ((csharpto--get-function-range t   nil) (630 769))
         ((csharpto--get-function-range nil   t) (731 767))
         ((csharpto--get-function-range t     t) (730 767)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'preceding-blank))

(csharpto--test-run
 :id 'F5
 :setup (list :file "./fixtures/Entity.cs"
              :find "return a \\+ b;"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (777 887))
         ((csharpto--get-function-range t   nil) (769 891))
         ((csharpto--get-function-range nil   t) (827 876))
         ((csharpto--get-function-range t     t) (814 886)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto--test-run
 :id 'F6
 :setup (list :file "./fixtures/Entity.cs"
              :find "\n\n.+IEnumerable"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (888 891))
         ((csharpto--get-function-range t   nil) (888 1010))
         ((csharpto--get-function-range nil   t) ())
         ((csharpto--get-function-range t     t) ()))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto--test-run
 :id 'F7
 :setup (list :file "./fixtures/Entity.cs"
              :find "=> this"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (899 1009))
         ((csharpto--get-function-range t   nil) (888 1010))
         ((csharpto--get-function-range nil   t) (953 1009))
         ((csharpto--get-function-range t     t) (952 1009)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto--test-run
 :id 'F8
 :setup (list :file "./fixtures/ClassWithSingleFunction.cs"
              :find "^.+SomeFunction"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (136 289))
         ((csharpto--get-function-range t   nil) (127 290))
         ((csharpto--get-function-range nil   t) (188 279))
         ((csharpto--get-function-range t     t) (175 288)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'beg-of-line))

(csharpto--test-run
 :id 'F9
 :setup (list :file "./fixtures/Attributes.cs"
              :find "ChangeName() {"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (668 982))
         ((csharpto--get-function-range t   nil) (659 983))
         ((csharpto--get-function-range nil   t) (731 972))
         ((csharpto--get-function-range t     t) (718 981)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :attributes    'single-inline
              :cursor-line   'signature
              :cursor-column 'end-of-line))

(csharpto--test-run
 :id 'F10
 :setup (list :file "./fixtures/Attributes.cs"
              :find "^.+\\[Theory\\]"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (185 658))
         ((csharpto--get-function-range t   nil) (177 660))
         ((csharpto--get-function-range nil   t) (448 658))
         ((csharpto--get-function-range t     t) (447 658)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'attributes
              :cursor-column 'beg-of-line))

(csharpto--test-run
 :id 'F11
 :setup (list :file "./fixtures/Attributes.cs"
              :find "() => new"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (185 658))
         ((csharpto--get-function-range t   nil) (177 660))
         ((csharpto--get-function-range nil   t) (448 658))
         ((csharpto--get-function-range t     t) (447 658)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'body
              :cursor-column 'text
              :item-under    'lambda-exp))

(csharpto--test-run
 :id 'F12
 :setup (list :file "./fixtures/ClassOnlyNoImports.cs"
              :find "Hello"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (39 128))
         ((csharpto--get-function-range t   nil) (35 129))
         ((csharpto--get-function-range nil   t) (73 122))
         ((csharpto--get-function-range t     t) (64 127)))
 :props (list :cursor-line   'signature))

(csharpto--test-run
 :id 'F13
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "MinValue\n"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (290 615))
         ((csharpto--get-function-range t   nil) (282 617))
         ((csharpto--get-function-range nil   t) (337 605))
         ((csharpto--get-function-range t     t) (323 614)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :item-before  'lambda-exp))

(csharpto--test-run
 :id 'F14
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "        \.First"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (625 794))
         ((csharpto--get-function-range t   nil) (617 796))
         ((csharpto--get-function-range nil   t) (670 794))
         ((csharpto--get-function-range t     t) (669 794)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'preceding-blank
              :item-before   'lambda-exp))

(csharpto--test-run
 :id 'F15
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "(owner),"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (1234 1516))
         ((csharpto--get-function-range t   nil) (1225 1517))
         ((csharpto--get-function-range nil   t) (1351 1506))
         ((csharpto--get-function-range t     t) (1338 1515)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :attributes    'single-preceding
              :cursor-line   'body
              :cursor-column 'end-of-line
              :item-before   'lambda-exp
              :item-after    'lambda-exp))

(csharpto--test-run
 :id 'F16
 :setup (list :file "./fixtures/Generics.cs"
              :find "=> default"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (71 96))
         ((csharpto--get-function-range t   nil) (63 98))
         ((csharpto--get-function-range nil   t) (85 96))
         ((csharpto--get-function-range t     t) (84 96)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'text
              :generic-type  'single))

(csharpto--test-run
 :id 'F17
 :setup (list :file "./fixtures/Generics.cs"
              :find "T: new"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (106 196))
         ((csharpto--get-function-range t   nil) (97  197))
         ((csharpto--get-function-range nil   t) (171 186))
         ((csharpto--get-function-range t     t) (158 195)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'text
              :generic-type  'multiple
              :type-constraint 'single))

(csharpto--test-run
 :id 'F18
 :setup (list :file "./fixtures/Comments.cs"
              :find "Id = "
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (429 715))
         ((csharpto--get-function-range t   nil) (421 717))
         ((csharpto--get-function-range nil   t) (521 705))
         ((csharpto--get-function-range t     t) (507 714)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'text
              :comment-line  'above))

(csharpto--test-run
 :id 'F19
 :setup (list :file "./fixtures/Comments.cs"
              :find "OneLiner"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (725 804))
         ((csharpto--get-function-range t   nil) (717 807))
         ((csharpto--get-function-range nil   t) (743 761))
         ((csharpto--get-function-range t     t) (742 761)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F20
 :setup (list :file "./fixtures/Comments.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (815  1069))
         ((csharpto--get-function-range t   nil) (807  1071))
         ((csharpto--get-function-range nil   t) (1033 1069))
         ((csharpto--get-function-range t     t) (1032 1069)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'signature))

(csharpto--test-run
 :id 'F21
 :setup (list :file "./fixtures/Comments.cs"
              :find "name=\"a\""
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1123 1489))
         ((csharpto--get-function-range t   nil) (1115 1491))
         ((csharpto--get-function-range nil   t) (1408 1457))
         ((csharpto--get-function-range t     t) (1395 1467)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'comments
              :cursor-column 'text
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F22
 :setup (list :file "./fixtures/Comments.cs"
              :find (rx "x++")
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1499 1737))
         ((csharpto--get-function-range t   nil) (1491 1739))
         ((csharpto--get-function-range nil   t) (1677 1726))
         ((csharpto--get-function-range t     t) (1664 1736)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'text
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto--test-run
 :id 'F23
 :setup (list :file "./fixtures/Comments.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1747 1924))
         ((csharpto--get-function-range t   nil) (1738 1925))
         ((csharpto--get-function-range nil   t) (1846 1914))
         ((csharpto--get-function-range t     t) (1845 1914)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :comment-line  'above
              :comment-line  'signature
              :comment-block 'signature
              :comment-line  'beg-of-scope
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F24
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "This is a"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (436 675))
         ((csharpto--get-function-range t   nil) (428 677))
         ((csharpto--get-function-range nil   t) (575 665))
         ((csharpto--get-function-range t     t) (562 674)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'comments
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'above-attributes))

(csharpto--test-run
 :id 'F25
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "//Comment$"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil nil) (685 742))
         ((csharpto--get-function-range t   nil) (677 744))
         ((csharpto--get-function-range nil   t) (710 728))
         ((csharpto--get-function-range t     t) (709 728)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'end-of-line
              :attributes    'single-inline
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F26
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (752 958))
         ((csharpto--get-function-range t   nil) (744 960))
         ((csharpto--get-function-range nil   t) (922 958))
         ((csharpto--get-function-range t     t) (921 958)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'signature))

(csharpto--test-run
 :id 'F27
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "end of SomeMethod"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1012 1231))
         ((csharpto--get-function-range t   nil) (1004 1233))
         ((csharpto--get-function-range nil   t) (1150 1199))
         ((csharpto--get-function-range t     t) (1137 1209)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'end-of-scope
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'before
              :comment-line  'above-attributes
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F28
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "\n +/\\* Block"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1299 1300))
         ((csharpto--get-function-range t   nil) (1299 1558))
         ((csharpto--get-function-range nil   t) ())
         ((csharpto--get-function-range t     t) ()))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'preceding-blank
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-block 'before
              :comment-block 'above-attributes
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto--test-run
 :id 'F29
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) (1567 1955))
         ((csharpto--get-function-range t   nil) (1558 1956))
         ((csharpto--get-function-range nil   t) (1882 1950))
         ((csharpto--get-function-range t     t) (1881 1950)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :attributes    'multiple-preceding
              :comment-block 'above-attribute
              :comment-line  'above-attribute
              :comment-block 'above
              :comment-line  'signature
              :comment-block 'signature
              :comment-line  'end-of-scope))

(csharpto--test-run
 :id 'F30
 :setup (list :file "./fixtures/Entity.cs"
              :find " +}\n}\n"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil nil) ())
         ((csharpto--get-function-range t   nil) ())
         ((csharpto--get-function-range nil   t) ())
         ((csharpto--get-function-range t     t) ()))
 :props (list :cursor-line   'succeeding-blank
              :no-blank-lines t))


(provide 'csharpto-function-test)

;;; csharpto-function-test.el ends here
