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

