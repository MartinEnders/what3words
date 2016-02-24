;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders

(defpackage #:what3words
  (:use #:cl)
  (:export #:get-languages
	   #:position-to-three-words
	   #:*key*))

