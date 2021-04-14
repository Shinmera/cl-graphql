#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(defclass object-class (representation-class)
  ((name :initarg :type-name :accessor type-name)
   (interfaces :initarg :interfaces :accessor interfaces :initform ())
   (directives :initarg :directives :accessor directives :initform ())))

(defmethod initialize-instance :after ((class object-class) &key type-name)
  (unless type-name
    (error "TYPE-NAME required.")))

(defclass field-slot (representation-slot)
  ((arguments :initarg :arguments :accessor arguments :initform ())
   (directives :initarg :directives :accessor directives :initform ())))

(defclass direct-field-slot (field-slot c2mop:direct-slot-definition)
  ())

(defclass effective-field-slot (field-slot c2mop:effective-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class object-class) &rest _)
  (declare (ignore _))
  (find-class 'direct-field-slot))

(defmethod c2mop:effective-slot-definition-class ((class object-class) &rest _)
  (declare (ignore _))
  (find-class 'effective-field-slot))

(defmethod c2mop:compute-effective-slot-definition ((class object-class) name dslots)
  (let* ((direct (find name dslots :key #'c2mop:slot-definition-name))
         (effective (call-next-method)))
    (setf (arguments effective) (arguments direct))
    (setf (directives effective) (directives direct))
    effective))

(defclass object () ())
(defclass interface () ())
(defclass union () ())
(defclass enum () ())
(defclass input-object () ())

(defun field->slot-definition (schema field)
  (destructuring-bind (name type &rest options/args) field
    (let ((options ())
          (arguments ())
          (directives ())
          (name (schema-name schema name)))
      (loop for thing in options/args
            do (cond ((keywordp thing)
                      (push thing options))
                     ((eql '@ (first thing))
                      (push (rest thing) directives))
                     (T
                      (push thing arguments))))
      (list name :initarg (intern name "KEYWORD")
                 :accessor name
                 :required (find :required options)
                 :list (listp type)
                 :element-type (if (listp type) (car type) type)
                 :arguments arguments
                 :directives directives))))

(defmacro define-type ((schema symbol &optional name) implements &body options/fields)
  (let ((symbol (schema-name schema symbol))
        (options ())
        (fields ())
        (directives ()))
    (loop for thing in options/args
          do (cond ((keywordp (first thing))
                    (push thing options))
                   ((eql '@ (first thing))
                    (push (rest thing) directives))
                   (T
                    (push (field->slot-definition schema thing) fields))))
    `(defclass ,symbol (,@implements type)
       ,fields
       ,@options
       (:metaclass type-class)
       (:type-name ,(or name (translate-name symbol)))
       (:directives ,@options))))

(trivial-indent:define-indentation define-type (4 6 &rest (&whole 2 4 &body)))

(defmacro define-interface ((schema symbol &optional name) &body fields))
(defmacro define-union ((schema symbol &optional name) &body types))
(defmacro define-enum ((schema symbol &optional name) &body values))
(defmacro define-input ((schema symbol &optional name) &body fields))

(defgeneric parse-from (data type))

(define-type (graphql schema "__Schema") ()
  (types (type) :required)
  (query-type type :required)
  (mutation-type type)
  (directives (directive :required) :required))

(define-type (graphql type "__Type") ()
  (kind type-kind :required)
  (name string)
  (description string)
  (fields (field)
    (include-deprecated boolean NIL))
  (interfaces (type))
  (possible-types (type :required))
  (enum-values (enum-value)
    (include-deprecated boolean NIL))
  (input-fields (input-value :required))
  (of-type type))

(define-type (graphql field "__Field") ()
  (name string :required)
  (description string)
  (args (input-value) :required)
  (type type :required)
  (is-deprecated boolean :required)
  (deprecation-reason string))

(define-type (graphql input-value "__InputValue") ()
  (name string :required)
  (description string)
  (type type :required)
  (default-value string))

(define-type (graphql enum-value "__EnumValue") ()
  (name string :required)
  (description string)
  (is-deprecated boolean :required)
  (deprecation-reason string))

(define-enum (graphql type-kind "__TypeKind")
  scalar
  object
  interface
  union
  enum
  input-object
  list
  non-null)

(define-type (graphql directive "__Directive") ()
  (name string :required)
  (description string)
  (locations (directive-location) :required)
  (args (input-value) :required))

(define-enum (graphql directive-location "__DirectiveLocation")
  query
  mutation
  field
  fragment-definition
  fragment-spread
  inline-fragment)
