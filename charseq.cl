;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-03-22 23:26:16>

(in-package "RDPARSE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character sequences

(defstruct charseq chars (position 0) (column 0))

(defmethod print-object ((charseq charseq) (stream t))
  (format stream "<charseq ~a/~a>"
          (charseq-position charseq)
          (length (charseq-chars charseq))))

;;; Using a vector with fill-pointer seems easier and faster:
;;; (defun make-charseq (&key chars)
;;;  (make-array (length chars)
;;;              :fill-pointer 0
;;;              :element-type 'character
;;;              :displaced-to chars
;;;              :displaced-index-offset 0))
;;; but requires 
;;; (defun charseq-chars (charseq)
;;;   ;; Perhaps the original idea of using a structure to wrap the string was better...
;;;   (make-array (array-total-size charseq)
;;;               :element-type 'character
;;;               :displaced-to charseq
;;;               :displaced-index-offset 0))
;;; which is inefficient.

(defun charseq-peek (charseq &optional (offset 0))
  (when (not (eof charseq))
    (aref (charseq-chars charseq)
          (+ (charseq-position charseq) offset))))

(defun charseq-length (charseq)
  (length (charseq-chars charseq)))

(defun charseq-subseq (charseq &key (start (charseq-position charseq)) (end (charseq-position charseq)))
  (subseq (charseq-chars charseq) start end))

(defun charseq-match (charseq prefix)
  (let ((m (mismatch prefix (charseq-chars charseq) :start2 (charseq-position charseq))))
    (or (not m)
        (eql m (length prefix)))))

(defun charseq-advance (charseq &optional (offset 1))
  (dotimes (k offset)
    (let ((current-char (charseq-peek charseq)))
      (when (member current-char '(#\return #\newline))
        (setf (charseq-column charseq) -1))
      (incf (charseq-column charseq))
      (incf (charseq-position charseq)))))

(defun eof (charseq)
  (= (charseq-position charseq)
     (length (charseq-chars charseq))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
