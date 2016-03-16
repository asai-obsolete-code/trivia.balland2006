#|
  This file is a part of trivia.balland2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage trivia.balland2006-asd
  (:use :cl :asdf))
(in-package :trivia.balland2006-asd)


(defsystem trivia.balland2006
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :type-i :iterate :alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "optimizer")
               (:file "column-swapping"))
  :serial t
  :description "Optimizer for Trivia based on (Balland 2006)"
  :in-order-to ((test-op (test-op trivia.balland2006.test))))
