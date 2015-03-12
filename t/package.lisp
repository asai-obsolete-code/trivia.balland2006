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
        :type-i
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

(test fuse1
  ;; because the two clauses are merged
  (is-true
   (fusiblep
    (e '((cons
          (guard1 x (= 1 x))
          (guard1 y (null y))) body1))
    (e '((cons
          (guard1 x (stringp x))
          (guard1 y (null y))) body2)))))

(test interleave1
  (is-true
    (interleave
     (e '((cons 1 2) body1))
     (e '((null) body2))
    'list))

  ;; ;; rational and float are the exhaustive partition of real
  ;; ;; IN MOST IMPLEMENTATIONS. see CLHS Issue REAL-NUMBER-TYPE:X3J13-MAR-89 Summary
  ;; (is-true
  ;;  (print
  ;;   (interleave
  ;;    (e '((type rational) body1))
  ;;    (e '((type float) body2))
  ;;    'real)))
  ;; 
  ;; ;; without assumption of `real', these are not exhaustive
  ;; (is-false
  ;;  (interleave
  ;;   (e '((type rational) body1))
  ;;   (e '((type float) body2))))
  
  ;; these are not disjoint
  (is-false
   (interleave
    (e '((type (or simple-vector simple-string)) body1))
    (e '((type (or simple-vector simple-bit-vector)) body2))
    'simple-array))
  
  ;; these are not exhaustive
  (is-false
   (interleave
    (e '((type simple-string) body1))
    (e '((type simple-bit-vector) body2))
    'simple-array)))

(test swap1
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

(def-fixture emilie2006 ()
  (let ((*optimizer* :emilie2006))
    (&body)))

(test (or-pattern :fixture emilie2006)
  ;; test to see if or-pattern is grounded
  (finishes
     (macroexpand
      `(match '(double-float 0.0d0 1.0d0)
         ((or (cons 1 b) (cons 0 a)) (vector a b))
        ((string a) a)))))


(test (fuse2 :fixture emilie2006)
  ;; test to see if or-pattern is grounded
  (finishes
     (macroexpand
        `(match '(double-float 0.0d0 1.0d0)
           ((or (cons 1 b) (cons 0 a)) (vector a b))
        ((string a) a)))))

(test (run :fixture emilie2006)
  (finishes
     (macroexpand
      `(match '(double-float 0.0d0 1.0d0)
         ((cons 0 b) b)
        ((cons 1 b) b)))))

(defvar *twice* nil)

(deftype once-cons ()
  'cons)
(defun once-consp (x)
  (when *twice* (error "evaluated twice!"))
  (setf *twice* t)
  (consp x))

(defvar form
    '(match :does-not-match
      ((guard1 it (once-consp it) 1 a) a)
      ((guard1 it (once-consp it) 2 b) b)))

(test twice
  (pprint (macroexpand form))
  (let ((*twice* nil))
    (signals error (eval form))))

(test (strict-once :fixture emilie2006)
  (pprint (macroexpand form))
  (let ((*twice* nil))
    (finishes (eval form))))

#+nil
(match2+ x real
  ((type rational) body1)
  ((type float) body2))


#+nil
(test run-big
  (finishes
    (macroexpand
     `(match '(double-float 0.0d0 1.0d0)
        ((general-real-type low high) (list low high)))))

  (match '(double-float 0.0d0 1.0d0)
    ((general-real-type low high)
     (is (= 0.0d0 low))
     (is (= 1.0d0 high))))

  (finishes
    (let ((*optimizer* :emilie2006))
      (macroexpand
       `(match '(double-float 0.0d0 1.0d0)
          ((general-real-type low high) (list low high))))))
  
  (in-optimizer :emilie2006)
  (unwind-protect
      (eval '(match '(double-float 0.0d0 1.0d0)
              ((general-real-type low high)
               (is (= 0.0d0 low))
               (is (= 1.0d0 high)))))
    (in-optimizer :trivial)))

#+nil
(print
 (let ((*optimizer* :emilie2006))
   (macroexpand
    `(match '(double-float 0.0d0 1.0d0)
       ((general-real-type low high) (list low high))))))

(eval-when (:load-toplevel :execute)
  (run! :trivia.emilie2006))




