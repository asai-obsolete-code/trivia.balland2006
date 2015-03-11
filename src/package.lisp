#|
  This file is a part of trivia.emilie2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivia.emilie2006
  (:use :cl :trivia :alexandria :type-i :iterate)
  (:export
   #:apply-fusion
   #:fusiblep
   #:fuse
   #:apply-interleaving
   #:interleave
   #:apply-swapping
   #:swappable)
  (:shadowing-import-from :trivia :next))
(in-package :trivia.emilie2006)

;; blah blah blah.
