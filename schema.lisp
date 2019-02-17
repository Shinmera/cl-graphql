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
  (types (:type :required) :required)
  (query-type :type :required)
  (mutation-type :type)
  (directives (:directive :required) :required))

(define-type (:type "__Type") ()
  (kind :type-kind :required)
  (name string)
  (description string)
  (fields (:field :required))
  (include-deprecated boolean false)
  (interfaces (:type :required))
  (possible-types (:type :required))
  (enum-values (:enum-value :required))
  (include-deprecated boolean false)
  (input-fields (:input-value :required))
  (of-type :type))

(define-type (:field "__Field") ()
  (name string :required)
  (description string)
  (args (:input-value :required) :required)
  (type :type :required)
  (is-deprecated boolean :required)
  (deprecation-reason string))

(define-type (:input-value "__InputValue") ()
  (name string :required)
  (description string)
  (type :type :required)
  (default-value string))

(define-type (:enum-value "__EnumValue") ()
  (name string :required)
  (description string)
  (is-deprecated boolean :required)
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
  (name string :required)
  (description string)
  (locations (:directive-location :required) :required)
  (args (:input-value :required) :required))

(define-enum (:directive-location "__DirectiveLocation")
  query
  mutation
  field
  fragment-definition
  fragment-spread
  inline-fragment)
