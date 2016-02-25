;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.

(defpackage #:what3words
  (:use #:cl)
  (:export #:get-languages
	   #:position-to-three-words
	   #:three-words-to-position
	   #:*key*))

