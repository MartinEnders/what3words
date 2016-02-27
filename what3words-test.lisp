;;;; what3words-test.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.


(in-package :what3words-test)

(defun test (key)
  (let ((*key* key))
    (format t "STARTING TEST three-words-to-position~%--------------------------------------~%")
    (dolist (output (list (multiple-value-list (three-words-to-position (list "prom" "cape" "pump")))
			  (multiple-value-list (three-words-to-position "prom.cape.pump"))
			  (multiple-value-list (three-words-to-position (list "prom" "cape" "pump") :language "de"))
			  (multiple-value-list (three-words-to-position (list "prom" "cape" "pump") :corners t))))
      (format t "~{
position:    ~A
type:        ~A
three words: ~A
language:    ~A
corners:     ~A~%~}~%" output))


    (format t "STARTING TEST position-to-three-words~%--------------------------------------~%")
    (dolist (output (list (multiple-value-list (position-to-three-words 51.484463 -0.195405))
			  (multiple-value-list (position-to-three-words 51.484463 -0.195405 :language "de"))
			  (multiple-value-list (position-to-three-words 51.484463 -0.195405 :corners t))))
      (format t "~{
three words: ~A
position:    ~A
language:    ~A
corners:     ~A~%~}~%" output))

    (format t "STARTING TEST get-languages~%------------------------------------~%")
    (dolist (output (list (multiple-value-list (get-languages))
			  (multiple-value-list (get-languages :codes-only nil))))
      (format t "~{languages: ~A~%~}" output)))
  
  (format t "Test API-error-message handling~%")
  (format t "---------------------------------~%")
  (let ((*key* "invalid-key-test"))
    
    (handler-case
	(three-words-to-position (list "prom" "cape" "pump") :raise-error t)
      (w3w-api-error (e) 
	(format t "
:RAISE-ERROR t
Error ID:      ~S
Error message: ~S~%" (text e) (data e))))

    (format t "~{
:RAISE-ERROR nil
Return value:  ~S
Error ID:      ~S
Error Message: ~S~}~%" (multiple-value-list (three-words-to-position (list "prom" "cape" "pump") :raise-error nil)))))

  
