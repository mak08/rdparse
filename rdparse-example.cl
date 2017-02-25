;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-02-25 21:23:47>

(use-package :rdparse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
(defparser parse-alnum
    :line-comment-start "//"
    :block-comment-start "/*"
    :block-comment-end "*/"
    :tokens ((number (:seq
                       (:opt (:alt "+" "-"))
                       :numeric
                       (:opt (:seq "." :numeric))))
             (identifier (:seq :alpha (:rep (:alt :alpha :numeric)))))
    :rules ((s (:rep alnum))
            (alnum
             (:alt number identifier))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum (symbol tree level)
  (if (token-p tree)
      (read-from-string (token-value tree))
      (+ (read-from-string (token-value (first tree)))
         (third tree))))

(defparser expr
    :rules ((sum (:alt
                  (:seq :numeric "+" sum)
                  :numeric))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
