#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivia.emilie2006.test
  (:use :cl
        :trivia
        :trivia.emilie2006
        :alexandria
        :fiveam))
(in-package :trivia.emilie2006.test)

(defun e (clause)
  ;; expand
  (match clause
    ((list* pattern body)
     (list* (pattern-expand-all pattern) body))))

(defun e* (clauses)
  (mapcar #'e clauses))

(def-suite :trivia.emilie2006)
(in-suite :trivia.emilie2006)

;; run test with (run! test-name) 
;;   test as you like ...

(test fuse
  ;; because the two clauses are merged
  (is-true
   (print
    (fuse
     (e*
      '(((cons
          (guard1 x (= 1 x))
          (guard1 y (null y))) body1)
        ((cons
          (guard1 x (stringp x))
          (guard1 y (null y))) body2)))))))

(test interleave
  (is-true
   (print
    (interleave
     (e '((cons 1 2) body1))
     (e '((null) body2))
     'list)))

  (is-true
   (print
    (interleave
     ;; under (not string), two patterns are incompatible and can be
     ;; interleaved
     (e '((type (or fixnum string)) body1))
     (e '((type (or float string)) body2))
     '(not string))))

  ;; however, without this constraint, two clauses are not compatible
  (is-false
   (interleave
    (e '((type (or fixnum string)) body1))
    (e '((type (or float string)) body2)))))

(test swap
  (is-true
   (xor
    (swappable
     (e '((type (or fixnum string)) body1))
     (e '((type (or float string)) body2))
     '(not string))
    (swappable
     (e '((type (or float string)) body2))
     (e '((type (or fixnum string)) body1))
     '(not string))))
  
  (is-false
   (swappable
    (e '((type (or fixnum string)) body1))
    (e '((type (or float string)) body2))
    t))
  (is-false
   (swappable
    (e '((type (or float string)) body2))
    (e '((type (or fixnum string)) body1))
    t)))

(eval-when (:load-toplevel :execute)
  (run! :trivia.emilie2006))




