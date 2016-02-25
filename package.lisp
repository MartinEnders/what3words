;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.

(defpackage #:what3words
  (:use #:cl)
  (:export #:get-languages
	   #:position-to-three-words
	   #:three-words-to-position
	   #:*key*
	   #:w3w-api-error
	   #:text
	   #:data))


(defpackage #:what3words-test
  (:use #:cl #:what3words)
  (:export #:test))

