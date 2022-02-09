;;; csharpto-function-test.el --- Tests for csharpto-function.el -*- lexical-binding: t -*-

;;; Commentary:

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

(csharpto--test-reset-buffer)

(csharpto-test-run
 :id 'F1
 :setup (list :file "./fixtures/Entity.cs"
              :find "public MyEntity(string name)"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (421 581))
        ((csharpto--get-function-range t)    (421 582)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'text))

(csharpto-test-run
 :id 'F2
 :setup (list :file "./fixtures/Entity.cs"
              :find "^\n\s *int OneLiner()"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (581 582))
         ((csharpto--get-function-range t)   (581 628)))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 'F3
 :setup (list :file "./fixtures/Entity.cs"
              :find "3 \\+ 4 \\+ 5; $"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (582 628))
         ((csharpto--get-function-range t)   (582 630)))
 :props (list :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'succeeding-blank))

(csharpto-test-run
 :id 'F4
 :setup (list :file "./fixtures/Entity.cs"
              :find "level = default\n\\s *)"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (630 768))
         ((csharpto--get-function-range t)   (630 769)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'signature
              :cursor-column 'preceding-blank))

(csharpto-test-run
 :id 'F5
 :setup (list :file "./fixtures/Entity.cs"
              :find "return a \\+ b;"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (769 888))
         ((csharpto--get-function-range t)   (769 891)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 'F6
 :setup (list :file "./fixtures/Entity.cs"
              :find "\n\n.+IEnumerable"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (888 891))
         ((csharpto--get-function-range t)   (888 1010)))
 :props (list :cursor-line   'blank
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 'F7
 :setup (list :file "./fixtures/Entity.cs"
              :find "=> this"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (891 1010))
         ((csharpto--get-function-range t)   (888 1010)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 'F8
 :setup (list :file "./fixtures/ClassWithSingleFunction.cs"
              :find "^.+SomeFunction"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (128 290))
         ((csharpto--get-function-range t)   (127 290)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 'F9
 :setup (list :file "./fixtures/Attributes.cs"
              :find "ChangeName() {"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (660 983))
         ((csharpto--get-function-range t)   (659 983)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :attributes    'single-inline
              :cursor-line   'signature
              :cursor-column 'end-of-line))

(csharpto-test-run
 :id 'F10
 :setup (list :file "./fixtures/Attributes.cs"
              :find "^.+\\[Theory\\]"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (177 659))
         ((csharpto--get-function-range t)   (177 660)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'attributes
              :cursor-column 'beg-of-line))

(csharpto-test-run
 :id 'F11
 :setup (list :file "./fixtures/Attributes.cs"
              :find "() => new"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (177 659))
         ((csharpto--get-function-range t)   (177 660)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :scope-lf      t
              :attributes    'multiple-preceding
              :cursor-line   'body
              :cursor-column 'text
              :item-under    'lambda-exp))

(csharpto-test-run
 :id 'F12
 :setup (list :file "./fixtures/ClassOnlyNoImports.cs"
              :find "Hello"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (035 129))
         ((csharpto--get-function-range t)   (035 129)))
 :props (list :cursor-line   'signature))

(csharpto-test-run
 :id 'F13
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "MinValue\n"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (282 616))
         ((csharpto--get-function-range t)   (282 617)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :item-before  'lambda-exp))

(csharpto-test-run
 :id 'F14
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "        \.First"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (617 795))
         ((csharpto--get-function-range t)   (617 796)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'preceding-blank
              :item-before   'lambda-exp))

(csharpto-test-run
 :id 'F15
 :setup (list :file "./fixtures/BlogRepository.cs"
              :find "(owner),"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (1226 1517))
         ((csharpto--get-function-range t)   (1225 1517)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :attributes    'single-preceding
              :cursor-line   'body
              :cursor-column 'end-of-line
              :item-before   'lambda-exp
              :item-after    'lambda-exp))

(csharpto-test-run
 :id 'F16
 :setup (list :file "./fixtures/Generics.cs"
              :find "=> default"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (63 97))
         ((csharpto--get-function-range t)   (63 98)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'text
              :generic-type  'single))

(csharpto-test-run
 :id 'F17
 :setup (list :file "./fixtures/Generics.cs"
              :find "T: new"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (98 197))
         ((csharpto--get-function-range t)   (97 197)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'signature
              :cursor-column 'text
              :generic-type  'multiple
              :type-constraint 'single))

(csharpto-test-run
 :id 'F18
 :setup (list :file "./fixtures/Comments.cs"
              :find "Id = "
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (421 716))
         ((csharpto--get-function-range t)   (421 717)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'body
              :cursor-column 'text
              :comment-line  'above))

(csharpto-test-run
 :id 'F19
 :setup (list :file "./fixtures/Comments.cs"
              :find "OneLiner"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (717 805))
         ((csharpto--get-function-range t)   (717 807)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 'F20
 :setup (list :file "./fixtures/Comments.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (807 1070))
         ((csharpto--get-function-range t)   (807 1071)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :comment-line  'signature))

(csharpto-test-run
 :id 'F21
 :setup (list :file "./fixtures/Comments.cs"
              :find "name=\"a\""
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (1115 1490))
         ((csharpto--get-function-range t)   (1115 1491)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'comments
              :cursor-column 'text
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 'F22
 :setup (list :file "./fixtures/Comments.cs"
              :find (rx "x++")
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (1491 1738))
         ((csharpto--get-function-range t)   (1491 1739)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'body
              :cursor-column 'text
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto-test-run
 :id 'F23
 :setup (list :file "./fixtures/Comments.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (1739 1925))
         ((csharpto--get-function-range t)   (1738 1925)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'body
              :cursor-column 'beg-of-line
              :comment-line  'above
              :comment-line  'signature
              :comment-block 'signature
              :comment-line  'beg-of-scope
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 'F24
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "This is a"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (428 676))
         ((csharpto--get-function-range t)   (428 677)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :scope-lf      t
              :cursor-line   'comments
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'above-attributes))

(csharpto-test-run
 :id 'F25
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "//Comment$"
              :goto-beginning-of-match nil)
 :test '(((csharpto--get-function-range nil) (677 743))
         ((csharpto--get-function-range t)   (677 744)))
 :props (list :signature     'single-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'end-of-line
              :attributes    'single-inline
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 'F26
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "void Log"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (744 959))
         ((csharpto--get-function-range t)   (744 960)))
 :props (list :signature     'multi-line
              :scope-type    'expression
              :cursor-line   'signature
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'signature))

(csharpto-test-run
 :id 'F27
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "end of SomeMethod"
              :goto-beginning-of-match t)
 :test '(((csharpto--get-function-range nil) (1004 1232))
         ((csharpto-get-function-range t)   (1004 1233)))
 :props (list :signature     'single-line
              :scope-type    'brackets
              :cursor-line   'end-of-scope
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-line  'before
              :comment-line  'above-attributes
              :comment-line  'above
              :comment-line  'end-of-scope))

(csharpto-test-run
 :id 'F28
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find "\n +/\\* Block"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1299 1300))
         ((csharpto-get-function-range t)   (1299 1558)))
 :props (list :signature     'multi-line
              :scope-type    'brackets
              :cursor-line   'preceding-blank
              :cursor-column 'text
              :attributes    'multiple-preceding
              :comment-block 'before
              :comment-block 'above-attributes
              :comment-block 'above
              :comment-block 'beg-of-scope))

(csharpto-test-run
 :id 'F29
 :setup (list :file "./fixtures/CommentsAndAttributes.cs"
              :find ".+ToString"
              :goto-beginning-of-match t)
 :test '(((csharpto-get-function-range nil) (1559 1956))
         ((csharpto-get-function-range t)   (1558 1956)))
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

(provide 'csharpto-function-test)

;;; csharpto-function-test.el ends here
