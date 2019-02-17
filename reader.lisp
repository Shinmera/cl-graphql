#|
 This file is a part of cl-graphql
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.graphql)

(defun consume-line (stream)
  (loop for prev = #\Nul then next
        for next = (read-char stream NIL NIL)
        while next
        do (cond ((char= next #\Linefeed)
                  (return))
                 ((char= prev #\Return)
                  (unread-char next stream)
                  (return)))))

(defun read-name (stream)
  (let ((buffer (make-string-output-stream)))
    (loop for char = (read-char stream NIL)
          while (and char (or (char= #\_ char)
                              (char<= #\a char #\z)
                              (char<= #\A char #\Z)
                              (char<= #\0 char #\9)))
          do (write-char char buffer)
          finally (when char (unread-char char stream)))
    (gql-intern (get-output-stream-string buffer))))

(defun read-unicode-escaped (stream)
  (let ((hex (make-string 4)))
    (unless (= 4 (read-sequence hex stream))
      (error 'end-of-file :stream stream))
    (code-char (parse-integer hex :radix 16))))

(defun read-base-string (stream)
  (with-output-to-string (buffer)
    (loop for char = (read-char stream)
          do (case char
               (#\"
                (return))
               (#\\
                (let ((next (read-char stream)))
                  (if (char= #\u next)
                      (read-unicode-escaped stream)
                      (write-char next buffer))))
               ((#\Return #\Linefeed)
                (error "FIXME (unexpected newline)"))
               (T
                (write-char char buffer))))))

(defun read-block-string (stream)
  (with-output-to-string (buffer)
    (loop for char = (read-char stream)
          do (case char
               (#\\
                (write-char (read-char stream) buffer)
                (when 
                  (write-char (read-char stream) buffer)
                  (when (char= #\" (peek stream))
                    (write-char (read-char stream) buffer))))
               (#\"
                (when (char= #\" (peek stream))
                  (read-char stream)
                  (when (char= #\" (peek stream))
                    (return))
                  (write-char #\" stream))
                (write-char #\" stream))
               (T
                (write-char char buffer))))))

(defun read-string (stream)
  (if (char= #\" (read-char stream))
      (cond ((eql #\" (peek stream NIL))
             (read-char stream)
             (read-block-string stream))
            (T
             ""))
      (read-base-string stream)))

(defun read-integer (stream)
  (let ((neg NIL))
    (case (peek stream)
      (#\-
       (read-char stream)
       (setf neg T))
      (#\+
       (read-char stream)))
    (flet ((char-integer (char)
             (- (char-code char) (char-code #\0))))
      (let ((num 0) (count 0))
        (loop for char = (read-char stream NIL)
              while char
              do (cond ((char<= #\0 char #\9)
                        ;; Primitive, but better than consing a string.
                        (incf count)
                        (setf num (+ (* 10 num) (char-integer char))))
                       (T
                        (unread-char char stream)
                        (return))))
        (values (if neg (- num) num) count)))))

(defun read-float (int-part stream)
  (let ((float int-part))
    (if (char= #\. (read-char stream))
        (multiple-value-bind (decimal digits) (read-integer stream)
          (setf decimal (/ decimal (expt 10 digits)))
          (setf float (+ float decimal)))
        (unread-char #\e stream))
    (when (find (peek stream NIL) "eE")
      (read-char stream)
      (setf float (* float (expt 10 (read-integer stream)))))
    (float float 0d0)))

(defun read-number (stream)
  (let ((int-part (read-integer stream))
        (next (peek stream NIL)))
    (case next
      ((#\. #\e #\E)
       (read-float int-part stream))
      (T
       int-part))))

(defun lex-char (char stream)
  (cond ;; Igonarable
    ((ignorable-p char))
    ;; Comment
    ((char= char #\#)
     (consume-line stream))
    ;; Punctuator
    ((find char "!$():=@[]{}|")
     (intern (string char) "KEYWORD"))
    ;; Triple-dot punctuator
    ((char= char #\.)
     (unless (and (char= #\. (read-char stream))
                  (char= #\. (read-char stream)))
       (error "FIXME (loose dot)"))
     :|...|)
    ;; Name
    ((or (char= #\_ char)
         (char<= #\a char #\z)
         (char<= #\A char #\Z))
     (unread-char char stream)
     (read-name stream))
    ;; Number
    ((or (char= #\- char)
         (char<= #\0 char #\9))
     (unread-char char stream)
     (read-number stream))
    ;; String
    ((char= #\" char)
     (read-string stream))
    ;; Failure
    (T
     (error "FIXME (invalid char)"))))

(defun lex (stream)
  (loop for char = (read-char stream NIL NIL)
        for el = (when char (lex-char char stream))
        while char
        when el collect el))
