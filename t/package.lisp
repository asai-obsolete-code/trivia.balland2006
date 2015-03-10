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
        :fiveam
        :type-r))
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
     (e '((null) body2)))))

  (is-true
   (print
    (interleave
     ;; under (not string), two patterns are incompatible and can be
     ;; interleaved
     (e '((type fixnum) body1))
     (e '((type float) body2)))))

  ;; however, without this constraint, two clauses are not compatible
  (is-false
   (interleave
    (e '((type (or fixnum string)) body1))
    (e '((type (or float string)) body2)))))

(test swap
  (is-true
   (xor
    (swappable
     (e '((type fixnum) body1))
     (e '((type float) body2)))
    (swappable
     (e '((type float) body2))
     (e '((type fixnum) body1)))))
  
  (is-false
   (swappable
    (e '((type (or fixnum string)) body1))
    (e '((type (or float string)) body2))))
  (is-false
   (swappable
    (e '((type (or float string)) body2))
    (e '((type (or fixnum string)) body1)))))

(test run
  (finishes
    (print
     (macroexpand
      `(match '(double-float 0.0d0 1.0d0)
         ((general-real-type low high) (list low high))))))
  (finishes
    (print
     (let ((*optimizer* :emilie2006))
       (macroexpand
        `(match '(double-float 0.0d0 1.0d0)
           ((general-real-type low high) (list low high))))))))

#+nil
(print
 (let ((*optimizer* :emilie2006))
   (macroexpand
    `(match '(double-float 0.0d0 1.0d0)
       ((general-real-type low high) (list low high))))))

#+nil
(print
 (let ((*optimizer* :emilie2006))
   (macroexpand
    `(match '(double-float 0.0d0 1.0d0)
       ((cons 0 b) b)
       ((cons 1 b) b)))))

(eval-when (:load-toplevel :execute)
  (run! :trivia.emilie2006))




