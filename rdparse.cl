;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Recursive-descent parsers
;;;                2nd version of rd-parser  
;;;                Includes token definition & new tokenizer algorithm
;;; Created        2009-04-24 16:21:20 16:21:20
;;; Last Modified  <michael 2017-02-25 20:14:45>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "RDPARSE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface

(defvar *trace* nil)

(define-condition syntax-error (error)
  ((argument :reader argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "Parse error: ~a"
                     (argument condition)))))


(defmacro defparser (name &key line-comment-start block-comment-start block-comment-end tokens rules)
  ;; Comments
  ;;    :LINE-COMMENT-START   
  ;;    :BLOCK-COMMENT-START  
  ;;    :BLOCK-COMMENT-END  
  ;;    Within token rules, comments and whitespace are read at the start of each rule.
  ;;    Within syntax rules, comments and whitespace are read at the start of each lexer token. 
  ;; Parsed tokens
  ;;    Parsed tokens need to be defined in a single rule, otherwise the resulting tokens may inlude whitespace. 
  (let* ((parsed-tokens ())
         (token-parsers
          (mapcar (lambda (rule)
                    (push (car rule) parsed-tokens)
                    (translate-token-rule rule))
                  tokens))
         (nonterm-parsers
          (mapcar #'translate-rule
                  rules))
         (start-symbol (caar nonterm-parsers))
         (comments ()))
    (declare (special parsed-tokens))
    (when line-comment-start
      (push (add-token-class (make-line-comment :name :line-comment :start line-comment-start))
            comments))
    (when block-comment-start
      (assert block-comment-end)
      (push (add-token-class (make-block-comment :name :block-comment
                                                 :start block-comment-start
                                                 :end block-comment-end))
            comments))
    `(defun ,name (string)
       (let ((charseq (make-charseq :chars string))
             (expected-tokens ())
             (error-location 0)
             (comments ',comments))
         (declare (special comments expected-tokens error-location))
         (labels
             (,@token-parsers
              ,@nonterm-parsers)
           (let ((result (,start-symbol 0)))
             (skip-whitespace charseq :comments comments)
             (values (when
                         (eof charseq)
                       result)
                     (unless (eof charseq)
                       error-location)
                     (unless (eof charseq)
                       (format-error charseq)))))))))

(defun read-file (fn)
  (with-open-file (f fn :element-type 'character :external-format #+clisp charset:utf-8 #+ccl :utf-8 #+sbcl :utf-8)
    (let ((s (make-array (file-length f) :element-type 'character)))
      (let ((pos (read-sequence s f)))
        (values s
                pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation

(defun translate-rule (rule)
  (if (caddr rule)
    `(,(makesym (car rule)) (level)
       (trace-print "~a ~a @ ~a" level ',(car rule) (charseq-position charseq))
       (let ((parse-tree ,@(translate-ebnf (cadr rule))))
         (cond
             ((funcall ,(caddr rule) parse-tree)
              (funcall ',(parse-tree-constructor (car rule) nil)
                       ',(car rule)
                       parse-tree
                       level))
             (t
              (error 'syntax-error :argument "validation error")))))
    `(,(makesym (car rule)) (level)
       (trace-print "~a ~a @ ~a" level ',(car rule) (charseq-position charseq))
       (let ((parse-tree ,@(translate-ebnf (cadr rule))))
         (funcall ',(parse-tree-constructor (car rule) nil)
                  ',(car rule)
                  parse-tree
                  level)))))

(defun translate-token-rule (rule)
  `(,(makesym (car rule)) (level)
     (declare (special comments))
     (trace-print "~a ~a @ ~a" level ',(car rule) (charseq-position charseq))
     (skip-whitespace charseq :comments comments)
     (let ((parse-tree ,@(translate-ebnf (cadr rule) nil)))
       (funcall ',(parse-tree-constructor (car rule) 'concatenate-token)
                ',(car rule)
                parse-tree
                level))))

(defun translate-ebnf (term &optional (skip-whitespace t))
  (declare (special parsed-tokens))
  (cond
    ((stringp term)
     ;; strings denote keywords and other literals in the client grammar
     `((expect ,term charseq :skip-whitespace ,skip-whitespace)))
    ((and
      (keywordp term)
      (not (member term parsed-tokens)))
     ;; keywords denote predefined token types 
     `((expect ,term charseq :skip-whitespace ,skip-whitespace)))
    ((symbolp term)
     ;; symbols denote nonterminals
     `((,(makesym term) (1+ level))))
    ((listp term)
     (ecase (car term)
       (:seq
        `((list ,@(mapcan (lambda (term) (translate-ebnf term skip-whitespace)) (cdr term)))))
       (:alt
        `(,(alternative term skip-whitespace)))
       (:opt
        `(,(optional term skip-whitespace)))
       (:rep
        `(,(repetition term skip-whitespace)))))))

(defun alternative (form &optional (skip-whitespace t))
  (let ((clauses (loop
                    :for body :on (mapcar (lambda (form)
                                            (translate-ebnf form skip-whitespace))
                                          (cdr form))
                    :if (cdr body)
                    :collect (try 'alternative (car body))
                    :else :collect (caar body))))
    `(cl:block alternative
       (let ((curpos 
              (charseq-position charseq)))
         ,@clauses))))

(defun optional (form &optional (skip-whitespace t))
  (let ((body (try 'optional (translate-ebnf (cadr form) skip-whitespace))))
    `(cl:block optional
       (let ((curpos 
               (charseq-position charseq)))
         ,body))))

(defun repetition (form &optional (skip-whitespace t))
  (let ((body (try 'optional (translate-ebnf (cadr form) skip-whitespace))))
    `(loop
        :for result = (cl:block optional
                        (let ((curpos 
                                (charseq-position charseq)))
                          ,body))
        :while result
        :collect result)))

(defun makesym (name)
  (intern (format () "PARSE-~a" name)))
  
(defun try (tag body)
  `(multiple-value-bind (result error)
       (handler-case
           (progn ,@body)
         (syntax-error (c) (values nil c)))
     (cond
       ((null error)
        (trace-print "Success")
        (return-from ,tag result))
       (t
        (trace-print "Backtrace to ~a" curpos)
        (setf (charseq-position charseq) curpos)
        (values nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aux functions

(defun expect (expected charseq &key skip-whitespace)
  (declare (special comments))
  (trace-print "Expecting: ~a" expected)
  (let ((token
         (get-token charseq expected :skip-whitespace skip-whitespace :comments comments)))
    (cond
      ((null token)
       (record-error charseq expected)
       (trace-print "     Fail: ~a" token)
       (error 'syntax-error :argument (format-error charseq)))
      (t
       (trace-print "  Success: ~a" token)
       token))))

(defun record-error (charseq expected)
  (declare (special error-location expected-tokens))
  (cond
    ((= (charseq-position charseq) error-location)
     (pushnew expected expected-tokens :test #'equalp))
    ((> (charseq-position charseq) error-location)
     (setf error-location (charseq-position charseq)
           expected-tokens (list expected))))
  expected-tokens)

(defun format-error (charseq)
  (declare (special error-location expected-tokens))
  (format () "At position ~a: expected ~{'~a'~^, ~}"
          (charseq-position charseq)
          expected-tokens))

(defun parse-tree-constructor (symbol default)
  (cond
    ((fboundp symbol)
     symbol)
    ((fboundp default)
     default)
    (t
     'make-default-parse-tree)))

(defun make-default-parse-tree (symbol tree level)
  (trace-print "~a ~a: Success" level symbol)
  (list symbol tree))

(defun concatenate-token (type tree &optional level)
  (etypecase tree
    (token tree)
    (list
     (let ((tokens
            (mapcar (lambda (tree) (concatenate-token type tree)) tree))
           (charseq)
           (start nil)
           (end nil))
       (dolist (token tokens)
         (when (token-p token)
           (setf charseq (or charseq (token-source token)))
           (setf start (if start (min start (token-start token)) (token-start token)))
           (setf end (if end (max end (token-end token)) (token-end token)))))
       (when charseq
         (make-token :source charseq
                     :type type
                     :start start
                     :end end))))))

(defun trace-print (format &rest args)
  (format *trace* "~?~%" format args))

;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
