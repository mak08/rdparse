;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2017-02-25 21:07:49>

(defpackage "RDPARSE"
  (:use "COMMON-LISP")
  (:export "DEFPARSER"
           "TOKEN-P"
           "TOKEN-VALUE"
           "DEFLINECOMMENT"
           "DEFBLOCKCOMMENT"
           "DEFLITERAL"
           "DEFCHARBAG"
           "DEFCHARBAG*"
           "DEFDELIMITED"
           "*WHITESPACE*"
           "*STRING-DELIMITER*"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
