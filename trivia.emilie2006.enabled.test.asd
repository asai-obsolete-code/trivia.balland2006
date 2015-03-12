#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivia.emilie2006.enabled.test-asd
  (:use :cl :asdf))
(in-package :trivia.emilie2006.enabled.test-asd)


(defsystem trivia.emilie2006.enabled.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.emilie2006.enabled)
  :perform (load-op :after (op c) (test-system :trivia)))
