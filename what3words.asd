;;;; what3words.asd
;;;;
;;;; Copyright (c) 2016 Martin R. Enders

(asdf:defsystem #:what3words
  :description "Describe what3words here"
  :author "Martin R. Enders"
  :license "BSD"
  :depends-on (#:drakma #:jsown)
  :serial t
  :components ((:file "package")
               (:file "what3words")))

