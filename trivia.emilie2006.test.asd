#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivia.emilie2006.test-asd
  (:use :cl :asdf))
(in-package :trivia.emilie2006.test-asd)


(defsystem trivia.emilie2006.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.emilie2006
               :fiveam
               :type-r)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) ))
