#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(defmacro define-alias (type alias))
(defmacro define-enum (type &body values))
(defmacro define-type (type implements &body fields))
(defmacro define-field ((type name) &body args))

(trivial-indent:define-indentation define-type (4 6 &rest (&whole 2 4 &body)))

(define-type (:schema "__Schema") ()
  (types (:type! !))
  (query-type :type!)
  (mutation-type :type)
  (directives (:directive! !)))

(define-type (:type "__Type") ()
  (kind :type-kind!)
  (name string)
  (description string)
  (fields (:field!))
  (include-deprecated boolean false)
  (interfaces (:type!))
  (possible-types (:type!))
  (enum-values (:enum-value!))
  (include-deprecated boolean false)
  (input-fields (:input-value!))
  (of-type :type))

(define-type (:field "__Field") ()
  (name string!)
  (description string)
  (args (:input-value! !))
  (type :type!)
  (is-deprecated boolean!)
  (deprecation-reason string))

(define-type (:input-value "__InputValue") ()
  (name string!)
  (description string)
  (type :type!)
  (default-value string))

(define-type (:enum-value "__EnumValue") ()
  (name string!)
  (description string)
  (is-deprecated boolean!)
  (deprecation-reason string))

(define-enum (:type-kind "__TypeKind")
  scalar
  object
  interface
  union
  enum
  input-object
  list
  non-null)

(define-type (:directive "__Directive") ()
  (name string!)
  (description string)
  (locations (:directive-location! !))
  (args (:input-value! !)))

(define-enum (:directive-location "__DirectiveLocation")
  query
  mutation
  field
  fragment-definition
  fragment-spread
  inline-fragment)
