#|
  This file is a part of trivia.balland2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivia.balland2006.enabled.test-asd
  (:use :cl :asdf))
(in-package :trivia.balland2006.enabled.test-asd)


(defsystem trivia.balland2006.enabled.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system for trivia.balland2006.enabled"
  :license "LLGPL"
  :depends-on (:trivia.balland2006.enabled)
  :perform (load-op :after (op c) (test-system :trivia)))
