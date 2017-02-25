;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-02-25 21:35:40>

(use-package :rdparse)

(defstruct url scheme authority path query fragment)
(defstruct authority userinfo host port)

(defun url-host (url)
  (authority-host (url-authority url)))
(defun url-port (url)
  (authority-port (url-authority url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse tree constructors

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun url (symbol tree level)
  tree)

(defun _url (symbol tree level)
  (make-url :scheme (when (first tree)
                      (token-value (car (first tree))))
            :authority (car (second tree))
            :path (cadr (second tree))
            :query (third tree)
            :fragment (when (fourth tree)
                        (subseq (token-value (fourth tree)) 1))))

(defun hier-part (symbol tree level)
  (list (cadar tree) (cadr tree)))

(defun authority (symbol tree level)
  (make-authority :userinfo (first (first tree))
                  :host (second tree)
                  :port (when (third tree)
                          (subseq (token-value (third tree)) 1))))
                  
(defun host (symbol tree level)
  (when tree (token-value tree)))

(defun query (symbol tree level)
  (second tree))

(defun query-pairs (symbol tree level)
  (cons (car tree)
        (mapcar #'second (cadr tree))))

(defun query-pair (symbol tree leve)
  (etypecase tree
    (list
     (list (if (first tree) (token-value (first tree)) "")
           (if (third tree) (token-value (third tree)) "")))
    (rdparse::token
     (list (token-value tree)))))

(defun path (symbol tree level)
  (cond
    ((null tree)
     "")
    ((and (null (car tree))
          (null (cadr tree)))
     "")
    (t
     (let ((rest
            (mapcar (lambda (tree)
                      (if (null tree) ""
                          (if (null (second tree)) ""
                              (token-value (second tree)))))
                    (cadr tree))))
       (if (car tree)
           (cons (token-value (car tree)) rest)
           rest)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple tokens

(defcharbag :gen-delims ":/?#[]@")
(defcharbag :sub-delims "!$&'()*+,;=")
(defcharbag :sub-delims-q "!$'()*+,;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser

(defparser parse-url
    :rules (
            (url _url))

    :tokens (
             (_url (:seq (:opt (:seq scheme ":")) hier-part (:opt query) (:opt fragment)))

             (hier-part (:seq (:opt (:seq "//" authority)) path))

             (scheme (:seq :letter (:rep (:alt :letter :digit "+" "-" "."))))
             
             ;;  authority   = [ userinfo "@" ] host [ ":" port ]
             (authority (:seq (:opt (:seq userinfo "@")) host (:opt port)))
             (port (:seq ":" :numeric))
             ;;  userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
             (userinfo (:rep (:alt unreserved pct-encoded :sub-delims ":")))

             (pct-encoded (:seq "%" :hexdigit :hexdigit))

             (unreserved (:alt :letter :digit "-" "." "_" "~"))

             (host (:alt ipv4address reg-name))
             (ipv4address (:seq dec-octet "." dec-octet "." dec-octet "." dec-octet))
             (dec-octet (:seq :digit (:opt :digit) (:opt :digit)))
             (reg-name (:rep (:alt unreserved pct-encoded :sub-delims)))

             (path (:seq (:opt segment-nz)
                         (:rep (:seq "/" segment))))

             (segment (:rep pchar))
             (segment-nz (:seq pchar (:rep pchar)))

             (pchar (:alt unreserved pct-encoded :sub-delims ":" "@"))
             (qchar (:alt unreserved pct-encoded :sub-delims-q ":" "@"))
             (qword (:rep (:alt qchar "/")))
             
             (query (:seq "?" query-pairs))
             (query-pairs (:opt (:seq query-pair  (:rep (:seq "&" query-pair)))))
             (query-pair (:alt (:seq qword "=" qword)
                               _qftoken))

             (fragment (:seq "#" _qftoken))
             (_qftoken (:rep (:alt pchar "/" "?")))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
