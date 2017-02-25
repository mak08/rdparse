;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-02-24 00:33:04>

(defsystem "rdparse"
  :description "A recursive-descent parser generator"
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "rdparse-package")
               (:file "charseq")
               (:file "lexer")
               (:file "rdparse")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

