#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-graphql
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A client library for the GraphQL API protocol."
  :homepage "https://github.com/Shinmera/cl-graphql"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "representations")
               (:file "printer")
               (:file "reader")
               (:file "schema")
               (:file "generate")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-indent
               :closer-mop))
