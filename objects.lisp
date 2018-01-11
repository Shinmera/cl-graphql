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

(defclass document ()
  (definitions)) ; => definition*

;;; Abstract
(defclass definition ()
  ())

;;; Abstract
(defclass operation (definition)
  (name  ; => name?
   variable-definitions ; => variable-definition*?
   directives ; => directive*?
   selection-set)) ; => selection*

(defclass query (operation)
  ())

(defclass mutation (operation)
  ())

;;; Abstract
(defclass selection ()
  ())

(defclass field (selection)
  (alias ; => name?
   name ; => name
   arguments ; => (name -> value)?
   directives ; => directive*?
   selection-set)) ; => selection*?

(defclass fragment-spread (selection)
  (name ; => fragment-name
   directives)) ; => directive*?

;;; Abstract
(defclass fragment ()
  (type-condition ; => named-type?
   directives ; => directive*?
   selection-set)) ; => selection*

(defclass inline-fragment (selection fragment)
  ())

(defclass fragment-definition (definition fragment)
  (name)) ; => fragment-name

(defclass variable-definition (definition)
  (name ; => name
   type ; => graph-type
   default-value)) ; => value?

;;; Abstract
(defclass graph-type ()
  (not-null-p))

(defclass named-type (graph-type)
  (name)) ; => name

(defclass list-type (graph-type)
  (type)) ; => graph-type

(defclass directive ()
  (name
   arguments)) ; (name -> value)?
