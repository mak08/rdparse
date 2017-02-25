# rdparse

rdparse is recursive descent parser written in Common Lisp. 

## Installation

```
(asdf:load-system :rdparse)
```
## Usage example
```
> (use-package :rdparse)
T

;;; Defining a VERY simple expression parser:
> (defparser expr
     :rules ((sum (:alt
                   (:seq :numeric "+" sum)
                   :numeric))))
EXPR

> (expr "1 + 2")
(SUM ([NUMERIC 1] '+' (SUM [NUMERIC 2])))
NIL
NIL

;;; Using a parse tree function:
;;; Parse tree function need to be defined before the DEFPARSER form is evaluated
> (defun sum (symbol tree level)
    (if (token-p tree)
        (read-from-string (token-value tree))
        (+ (read-from-string (token-value (first tree)))
           (third tree))))
SUM

> (defparser expr 
     :rules ((sum (:alt 
                   (:seq :numeric "+" sum) 
                   :numeric))))
EXPR

> (EXPR "1 + 2")
3
NIL
NIL
```
