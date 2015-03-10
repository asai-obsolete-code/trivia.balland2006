#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivia.emilie2006.test
  (:use :cl
        :trivia
        :trivia.emilie2006
        :fiveam))
(in-package :trivia.emilie2006.test)



(def-suite :trivia.emilie2006)
(in-suite :trivia.emilie2006)

;; run test with (run! test-name) 
;;   test as you like ...

(test trivia.emilie2006
  ;; because the two clauses are merged
  (is (= 1
         (length
          (if-fusion
           `((cons
              (guard1 x (= 1 x))
              (guard1 y (null y))) body1)
           `((cons
              (guard1 x (stringp x))
              (guard1 y (null y))) body2)))))

  (is (= 1
         (length
          (print
           (interleaving
            `((cons 1 2) body1)
            `((null) body2)
            'list)))))


  (is (= 1
         (length
          (print
           (interleaving
            ;; under (not string), two patterns are incompatible and can be
            ;; interleaved
            `((type (or fixnum string)) body1)
            `((type (or float string)) body2)
            '(not string)))))))

(eval-when (:load-toplevel :execute)
  (run! :trivia.emilie2006))




