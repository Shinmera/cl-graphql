#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(deftype name ()
  `(satisfies name-p))

(deftype fragment-name ()
  `(satisfies fragment-name-p))

(deftype value ()
  `(or symbol       ; Variable
       integer      ; IntValue
       float        ; FloatValue
       string       ; StringValue
       boolean      ; BooleanValue
       (eql :null)  ; NullValue
       keyword      ; EnumValue
       vector       ; ListValue
       hash-table)) ; ObjectValue

(defmacro define-representation (name direct-superclasses direct-slots &body options)
  (let* ((slots (loop for slot in direct-slots
                      collect (destructuring-bind (name &key (accessor name) (initarg (intern (string name) "KEYWORD")) type required &allow-other-keys) slot
                                `(,name :accessor ,accessor :initarg ,initarg :type ,type :required ,required))))
         (value (gensym "VALUE"))
         (names (gensym "NAMES"))
         (null (gensym "NULL")))
    `(progn
       (defclass ,name ,direct-superclasses
         ,(loop for (slot . options) in slots
                collect `(,slot :accessor ,(getf options :accessor) :initform NIL))
         ,@options)

       (defmethod initialize-instance :before ((,name ,name) &key ,@(loop for (slot . options) in slots
                                                                          when (and (getf options :initarg) (getf options :required))
                                                                          collect `(,(intern (string (getf options :initarg))) ',null)))
         ,@(loop for (slot . options) in slots
                 when (and (getf options :initarg) (getf options :required))
                 collect `(when (eq ,(intern (string (getf options :initarg))) ',null)
                            (error ,(format NIL "~a must be provided for ~as." (getf options :initarg) name)))))

       (defmethod shared-initialize :after ((,name ,name) ,names &key ,@(loop for (slot . options) in slots
                                                                              when (getf options :initarg)
                                                                              collect (intern (string (getf options :initarg)))))
         (declare (ignore ,names))
         ,@(loop for (slot . options) in slots
                 when (getf options :initarg)
                 collect `(setf (,(getf options :accessor) ,name) ,(intern (string (getf options :initarg))))))

       ,@(loop for (slot . options) in slots
               when (getf options :type)
               collect `(defmethod (setf ,(getf options :accessor)) :before (,value (,name ,name))
                          (check-type ,value ,(getf options :type)))))))

(define-representation document ()
    ((definitions :type list)))

;;; Abstract
(define-representation definition ()
    ())

;;; Abstract
(define-representation operation (definition)
    ((name :type name :required T)
     (variable-definitions :type list)
     (directives :type list)
     (selection-set :type list :required T)))

(define-representation query (operation)
    ())

(define-representation mutation (operation)
    ())

;;; Abstract
(define-representation selection ()
    ())

(define-representation field (selection)
    ((alias :type name)
     (name :type name :required T)
     (arguments :type list)
     (directives :type list)
     (selection-set :type list)))

(define-representation fragment-spread (selection)
    ((name :type fragment-name :required T)
     (directives :type list)))

;;; Abstract
(define-representation fragment ()
    ((type-condition :type named-type)
     (directives :type list)
     (selection-set :type list :required T)))

(define-representation inline-fragment (selection fragment)
    ())

(define-representation fragment-definition (definition fragment)
    ((name :type fragment-name :required T)))

(define-representation variable-definition (definition)
    ((name :type name :required T)
     (value-type :type graph-type :required T)
     (default-value :type value)))

;;; Abstract
(define-representation graph-type ()
    ((not-null-p)))

(define-representation named-type (graph-type)
    ((name :type name :required T)))

(define-representation list-type (graph-type)
    ((item-type :type graph-type :required T)))

(define-representation directive ()
    ((name :type name :required T)
     (arguments :type list)))
