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

(defclass representation-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((a representation-class) (b T)) NIL)
(defmethod c2mop:validate-superclass ((a representation-class) (b standard-class)) T)
(defmethod c2mop:validate-superclass ((a standard-class) (b representation-class)) NIL)

(defclass representation-slot (c2mop:standard-slot-definition)
  ((required-p :initarg :required :accessor required-p :initform NIL :type boolean)
   (list-p :initarg :list :accessor list-p :initform NIL :type boolean)
   (element-type :initarg :element-type :accessor element-type :initform (error "ELEMENT-TYPE required."))))

(defclass direct-representation-slot (representation-slot c2mop:direct-slot-definition)
  ())

(defclass effective-representation-slot (representation-slot c2mop:effective-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class representation-class) &rest initargs)
  (if (getf initargs :element-type)
      (find-class 'direct-representation-slot)
      (find-class 'c2mop:standard-direct-slot-definition)))

(defmethod c2mop:effective-slot-definition-class ((class representation-class) &rest _)
  (declare (ignore _))
  (find-class 'effective-representation-slot))

(defmethod c2mop:compute-effective-slot-definition ((class representation-class) name dslots)
  (let* ((direct (find name dslots :key #'c2mop:slot-definition-name))
         (effective (call-next-method)))
    (cond ((typep direct 'representation-slot)
           (setf (required-p effective) (required-p direct))
           (setf (list-p effective) (list-p direct))
           (setf (element-type effective) (element-type direct)))
          (T
           (change-class effective 'c2mop:standard-effective-slot-definition)))
    effective))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class representation-class) object (slot representation-slot))
  (cond ((list-p slot)
         (unless (typep value 'list)
           (error "FIXME (bad value type for slot)"))
         (unless (or (null value) (typep (car value) (element-type slot)))
           (error "FIXME (bad element type for slot)")))
        (T
         (unless (typep value (element-type slot))
           (error "FIXME (bad value type for slot)")))))

(defmethod c2mop:slot-makunbound-using-class :before ((class representation-class) object (slot representation-slot))
  (when (required-p slot)
    (error "FIXME (required slot cannot be unbound)")))

(defmacro define-representation (name direct-superclasses direct-slots &rest options)
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass representation-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(define-representation document ()
    ((definitions :element-type definition :list T)))

;;; Abstract
(define-representation definition ()
    ())

;;; Abstract
(define-representation operation (definition)
    ((name :element-type name :required T)
     (variable-definitions :element-type variable-definition :list T)
     (directives :element-type directive :list T)
     (selection-set :element-type selection :list T :required T)))

(define-representation query (operation)
    ())

(define-representation mutation (operation)
    ())

;;; Abstract
(define-representation selection ()
    ())

(define-representation field (selection)
    ((alias :element-type name)
     (name :element-type name :required T)
     (arguments :element-type argument :list T)
     (directives :element-type directive :list T)
     (selection-set :element-type selection :list T)))

(define-representation fragment-spread (selection)
    ((name :element-type fragment-name :required T)
     (directives :element-type directive :list T)))

;;; Abstract
(define-representation fragment ()
    ((type-condition :element-type named-type)
     (directives :element-type directive :list T)
     (selection-set :element-type selection :list T :required T)))

(define-representation inline-fragment (selection fragment)
    ())

(define-representation fragment-definition (definition fragment)
    ((name :element-type fragment-name :required T)))

(define-representation variable-definition (definition)
    ((name :element-type name :required T)
     (value-type :element-type graph-type :required T)
     (default-value :element-type value)))

;;; Abstract
(define-representation graph-type ()
    ((not-null-p)))

(define-representation named-type (graph-type)
    ((name :element-type name :required T)))

(define-representation list-type (graph-type)
    ((item-type :element-type graph-type :required T)))

(define-representation directive ()
    ((name :element-type name :required T)
     (arguments :element-type argument :list T)))

(define-representation argument ()
    ((name :element-type name :required T)
     (value :element-type value :required T)))
