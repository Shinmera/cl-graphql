#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(defun name-p (string)
  (and (< 0 (length string))
       (find (char string 0) "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
       (loop for i from 1 below (length string)
             always (find (char string i) "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345679"))))

(defun fragment-name-p (string)
  (and (name-p string)
       (string/= string "on")))

(defun source-character-p (char)
  (let ((code (char-code char)))
    (or (= code #x0009)
        (= code #x000A)
        (= code #x000D)
        (<= #x0020 code #xFFFF))))

(defun whitespace-p (char)
  (let ((code (char-code char)))
    (or (= code #x0009)
        (= code #x0020))))

(defun ignorable-p (char)
  (let ((code (char-code char)))
    (or (= code #x0020)
        (= code #x0009)
        ;; Line end
        (= code #x000A)
        (= code #x000D)
        ;; Comma
        (= code #x002C))))

(defun gql-symbol (name)
  (find-symbol name '#:org.shirakumo.graphql.symbols))

(defun gql-intern (name)
  (intern name '#:org.shirakumo.graphql.symbols))

(defun peek (stream &optional (eof-error T))
  (peek-char NIL stream eof-error))

(defun translate-name (name)
  (etypecase name
    (string name)
    (symbol (with-output-to-string (out)
              (loop with upcase = T
                    for char across (symbol-name name)
                    do (cond ((eql char #\-) (setf upcase T))
                             (upcase (write-char (char-upcase char) out))
                             (T (write-char (char-downcase char) out))))))))

(defun schema-package-name (schema)
  (format NIL "ORG.SHIRAKUMO.GRAPHQL.SCHEMA.~@(a~)" schema))

(defun schema-package (schema &key (if-does-not-exist :error))
  (let* ((name (schema-package-name schema))
         (package (find-package name)))
    (if package
        package
        (ecase if-does-not-exist
          (:error (error "FIXME (No such schema ~a)" schema))
          (:create (make-package name :use ()))
          ((NIL) NIL)))))

(defun schema-name (schema name &key (if-does-not-exist :error))
  (intern (string name) (schema-package schema :if-does-not-exist if-does-not-exist)))
