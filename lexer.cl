;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-02-25 16:47:05>

(in-package "RDPARSE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens
;;;
;;; The TOKEN struct holds instances of tokens of any class.
;;; TOKEN-TYPE corresponds to the NAME of the TOKEN-CLASS.
;;; Tokens must be named by :keywords. The parser assumes that keywords name
;;; predefined tokens.

(defstruct token
  source
  type
  (start 0)
  (end 0))

(defmethod token-value (token)
  (subseq (charseq-chars (token-source token))
          (token-start token)
          (token-end token)))

(defmethod print-object ((thing token) stream)
  (let ((value (token-value thing)))
    (case (token-type thing)
      (:literal
       (format stream "'~a'" value))
      (:string
       (format stream "~s" value))
      (t
       (format stream "[~a ~a]" (token-type thing) value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokenizer interface

(defvar *defined-tokens*
  (make-hash-table))

(defvar *whitespace*
  #(#\space #\tab #\return #\newline))

(defun skip-whitespace (charseq &key
                        (whitespace *whitespace*)
                        (comments ()))
  (loop
     :while (and (not (eof charseq))
                 (position (charseq-peek charseq) *whitespace*))
     :do (charseq-advance charseq))
  (loop
     :while (some (lambda (c)
                    (unless (eof charseq)
                      (let ((token-type (gethash c *defined-tokens*)))
                        (read-token token-type charseq))))
                  comments)
     :do (loop
            :while (and (not (eof charseq))
                        (position (charseq-peek charseq) *whitespace*))
            :do (charseq-advance charseq))))

(defun get-token (charseq spec &key
                  (skip-whitespace t)
                  (whitespace *whitespace*) 
                  (comments ()))
  (when skip-whitespace
    (skip-whitespace charseq :whitespace whitespace :comments comments))
  (when (not (eof charseq))
    (etypecase spec
      (string
       (when
           (and
            (not (eof charseq))
            (eql (search spec (charseq-chars charseq) :start2 (charseq-position charseq))
                 (charseq-position charseq)))
         (prog1
             (make-token :source charseq
                         :type :literal
                         :start (charseq-position charseq)
                         :end (+ (charseq-position charseq) (length spec)))
           (charseq-advance charseq
                            (length spec)))))
      (keyword
       (let ((token-type (gethash spec *defined-tokens*)))
         (read-token token-type charseq))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Classes
;; The token classes provide convenient ways to define tokens. The token class
;; also determines how a token type is recognized

(defstruct token-class name)

(defstruct (comment (:include token-class))
  start
  end)

(defstruct (line-comment (:include comment)))
(defstruct (block-comment (:include comment)))

(defstruct (delimited (:include token-class))
  start-delim
  end-delim
  escape-char
  valid-char-p (constantly t))

(defstruct (literal (:include token-class))
  value)

(defstruct (charbag (:include token-class))
  valid-chars
  valid-char-p (constantly nil))

(defstruct (charbag* (:include charbag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recognizer methods

(defgeneric read-token (type charseq))

(defmethod read-token ((type line-comment) charseq)
  (when
      (eql (search (comment-start type) (charseq-chars charseq) :start2 (charseq-position charseq))
           (charseq-position charseq))
    (let ((end
           (or (position #\newline (charseq-chars charseq) :start (charseq-position charseq))
               (charseq-length charseq))))
      (setf (charseq-position charseq) (min (1+ end) (charseq-length charseq))))))

(defmethod read-token ((type block-comment) charseq)
  (when
      (eql (search (comment-start type) (charseq-chars charseq)
                   :start2 (charseq-position charseq))
           (charseq-position charseq))
    (let ((end
           (search (comment-end type) (charseq-chars charseq)
                   :start2 (+ (charseq-position charseq) (length (comment-start type))))))
      (if end
          (setf (charseq-position charseq) (min (+ end (length (comment-end type))) (charseq-length charseq)))
          (error "Unterminated block comment starting at position ~a" (charseq-position charseq))))))

(defmethod read-token ((type delimited) charseq)
  (unless (char= (charseq-peek charseq)
                 (delimited-start-delim type))
    (return-from read-token nil))
  (let ((saved-position (charseq-position charseq))
        (endpos (charseq-length charseq)))
    (do* ((eof nil
               (= (charseq-position charseq) endpos))
          (end nil
               (and (not eof)
                    (not (char= (charseq-peek charseq -1)
                                (delimited-escape-char type)))
                    (char= (charseq-peek charseq)
                           (delimited-end-delim type)))))
         ((or eof end)
          (cond
            (end
             (charseq-advance charseq)
             (make-token :source charseq
                         :type (token-class-name type)
                         :start saved-position
                         :end (charseq-position charseq)))
            (t
             (setf (charseq-position charseq) saved-position)
             nil)))
      (charseq-advance charseq 1))))

(defmethod read-token ((type literal) charseq)
  (when
      (eql (search (literal-value type) (charseq-chars charseq) :start2 (charseq-position charseq))
           (charseq-position charseq))
    (prog1
        (make-token :source charseq
                    :type (literal-name type)
                    :start (charseq-position charseq)
                    :end (+ (charseq-position charseq) (length (literal-value type))))
      (charseq-advance charseq
                       (length (literal-value type))))))

(defmethod read-token ((type charbag) charseq)
  (let ((start (charseq-position charseq)))
    (when (charbag-match type (charseq-peek charseq))
      (charseq-advance charseq)
      (make-token :source charseq
                  :type (charbag-name type)
                  :start start
                  :end (charseq-position charseq)))))

(defmethod read-token ((type charbag*) charseq)
  (let ((start (charseq-position charseq)))
    (loop
       :while (and
               (not (eof charseq))
               (charbag-match type (charseq-peek charseq)))
       :do (charseq-advance charseq))
    (when (> (charseq-position charseq) start)
      (prog1
          (make-token :source charseq
                      :type (charbag*-name type)
                      :start start
                      :end (charseq-position charseq))))))

(defun charbag-match (charbag char)
  (position char (charbag-valid-chars charbag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining token types

(defun add-token-class (token-class)
  (assert (keywordp (token-class-name token-class)))
  (when (gethash (token-class-name token-class) *defined-tokens*)
    (warn "Redefining token type ~a" (token-class-name token-class)))
  (setf (gethash (token-class-name token-class) *defined-tokens*)
        token-class)
  (token-class-name token-class))

(defmacro deflinecomment (name start)
  `(add-token-class (make-line-comment :name ',name :start ,value)))

(defmacro defblockcomment (name start end)
  `(add-token-class (make-block-comment :name ',name :start ,start :end ,end)))

(defmacro defliteral (name value)
  `(add-token-class (make-literal :name ',name :value ,value)))

(defmacro defliteral (name value)
  `(add-token-class (make-literal :name ',name :value ,value)))

(defmacro defcharbag (name chars)
  `(add-token-class (make-charbag :name ',name :valid-chars ,chars)))

(defmacro defcharbag* (name chars)
  `(add-token-class (make-charbag* :name ',name :valid-chars ,chars)))

(defmacro defdelimited (name &key start-delim end-delim escape-char)
  `(add-token-class (make-delimited :name ',name :start-delim ,start-delim :end-delim ,end-delim :escape-char ,escape-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined token types

(defcharbag* :whitespace *whitespace*)

(defliteral :semicolon ";")
(defliteral :dot ".")
(defliteral :comma ",")
(defliteral :underscore "_")

(defdelimited :string :start-delim #\' :end-delim #\' :escape-char #\\)

(defcharbag :digit "0123456789")
(defcharbag* :numeric "0123456789")

(defcharbag :hexdigit "0123456789abcdefABCDEF")
(defcharbag* :hexnum "0123456789abcdefABCDEF")

(defcharbag :letter "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defcharbag* :alpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
 
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
