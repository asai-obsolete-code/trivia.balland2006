#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage trivia.emilie2006-asd
  (:use :cl :asdf))
(in-package :trivia.emilie2006-asd)


(defsystem trivia.emilie2006
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :type-i :iterate :alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "optimizer"))
  :serial t
  :description "Optimizer for Trivia based on (Emilie 2006)"
  :in-order-to ((test-op (load-op trivia.emilie2006.test))))
