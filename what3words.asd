;;;; what3words.asd
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.

(asdf:defsystem #:what3words
  :description "Describe what3words here"
  :author "Martin R. Enders"
  :license "BSD"
  :depends-on (#:drakma #:jsown #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "what3words")
	       (:file "what3words-test")))

