#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(defvar *level* 0)

(defun write-indent (stream)
  (fresh-line stream)
  (format stream "~v@{  ~}" (1- *level*) NIL))

(defun write-name (name stream)
  (etypecase name
    (string (write-string name stream))
    (symbol (write-string (symbol-name name) stream))))

(defun write-selection-set (selection-set stream)
  (write-char #\{)
  (dolist (selection selection-set)
    (write-object selection stream))
  (write-indent stream)
  (write-char #\}))

(defun write-arguments (arguments stream)
  (write-char #\( stream)
  (loop for cons on arguments
        do (write-name (caar cons) stream)
           (write-string ": " stream)
           (write-object (cdar cons) stream)
           (when (cdr cons) (write-string ", " stream)))
  (write-char #\) stream))

(defgeneric write-object (object stream))

(defmethod write-object (object (stream (eql T)))
  (write-object object *standard-output*))

(defmethod write-object (object (stream (eql NIL)))
  (with-output-to-string (stream)
    (write-object object stream)))

(defmethod wirte-object :around (object (stream stream))
  (let ((*level* (1+ *level*)))
    (call-next-method)))

(defmacro define-object-writer ((type stream) &body body)
  (let ((type (if (listp type) type `(,type ,type))))
    `(defmethod write-object (,type (,stream stream))
       ,@body)))

(define-object-writer (symbol stream)
  (cond ((eq symbol :null)
         (write-string "null" stream))
        ((eq symbol T)
         (write-string "true" stream))
        ((eq symbol NIL)
         (write-string "false" stream))
        ((keywordp symbol)
         (write-string (symbol-name symbol) stream))
        (T
         (write-char #\$ stream)
         (write-name symbol stream))))

(define-object-writer (integer stream)
  (prin1 integer stream))

(define-object-writer (float stream)
  (format stream "~,,,,,,'ee" float))

(define-object-writer (string stream)
  (write-char #\" stream)
  (loop for char across string
        do (cond ((or (eql char #\") (eql char #\\))
                  (write-char #\\ stream)
                  (write-char char stream))
                 ((eql char #\Linefeed)
                  (write-string "\\n" stream))
                 ((eql char #\Return)
                  (write-string "\\r" stream))
                 ((source-character-p char)
                  (write-char char stream))
                 (T
                  (format stream "\\u~4,'0d"))))
  (write-char #\" stream))

(define-object-writer (vector stream)
  (write-char #\[ stream)
  (when (< 0 (length vector))
    (write-object (aref vector i) stream)
    (loop for i from 1 below (length vector)
          do (write-string ", " stream)
             (write-object (aref vector i) stream)))
  (write-char #\] stream))

(define-object-writer (hash-table stream)
  (write-char #\{ stream)
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        do (write-name k stream)
           (write-string ": " stream)
           (write-object v stream)
           ;; FIXME: Only emit if there are more elements.
           (write-string ", " stream))
  (write-char #\} stream))

(define-object-writer (document stream)
  (dolist (definition (definitions document))
    (write-object definition stream)))

(define-object-writer (operation stream)
  (when name
    (write-name (name operation) stream))
  (when (variable-definitions operation)
    (write-char #\( stream)
    (loop for cons on (variable-definitions operation)
          do (write-object (car cons) stream)
             (when (cdr cons) (write-string ", " stream)))
    (write-char #\) stream))
  (dolist (directive (directives operation))
    (write-indent stream)
    (write-object directive stream))
  (write-selection-set (selection-set operation)))

(define-object-writer (query stream)
  (write-indent stream)
  (write-string "query " stream)
  (call-next-method))

(define-object-writer (mutation stream)
  (write-indent stream)
  (write-string "mutation " stream)
  (call-next-method))

(define-object-writer (field stream)
  (write-indent stream)
  (when (alias field)
    (write-name (alias field) stream)
    (write-string ": " stream))
  (write-name (name field) stream)
  (when (arguments field)
    (write-arguments field))
  (dolist (directive (directives field))
    (write-object directive stream))
  (when (selection-set field)
    (write-selection-set (selection-set field))))

(define-object-writer (fragment-spread stream)
  (write-indent stream)
  (write-string "..." stream)
  (write-name (name fragment-spread) stream)
  (dolist (directive (directives fragment-spread))
    (write-object directive stream)))

(define-object-writer (fragment stream)
  (when (type-condition fragment)
    (write-string "on " stream)
    (write-object (type-condition fragment)))
  (dolist (directive (directives field))
    (write-object directive stream))
  (write-selection-set (selection-set field)))

(define-object-writer (inline-fragment stream)
  (write-indent stream)
  (write-string "... ")
  (call-next-method))

(define-object-writer (fragment-definition stream)
  (write-indent stream)
  (write-string "fragment " stream)
  (write-name (name fragment-definition) stream)
  (call-next-method))

(defmethod write-object :after ((graph-type graph-type) (stream stream))
  (when (not-null-p graph-type) (write-char #\! stream)))

(define-object-writer (named-type stream)
  (write-name (name named-type) stream))

(define-object-writer (list-type stream)
  (write-char #\[ stream)
  (write-object (type list-type) stream)
  (write-char #\] stream))

(define-object-writer (directive stream)
  (write-string " @" stream)
  (write-name (name directive) stream)
  (when (arguments directive)
    (write-arguments (arguments directive) stream)))
