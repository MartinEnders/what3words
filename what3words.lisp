;;;; what3words.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.


(in-package #:what3words)

;;; "what3words" goes here. Hacks and glory await!

(defparameter *key* nil)
(defparameter *get-languages-url* "https://api.what3words.com/get-languages")
(defparameter *position-url* "https://api.what3words.com/position")
(defparameter *w3w-url* "https://api.what3words.com/w3w")

(define-condition w3w-api-error (error)
  ((text :initarg :text :reader text)
   (data :initarg :data :reader data)))

(defun check-error (data raise-error)
  "raise or return error message from w3w API"
  (let ((w3w-error-id  (jsown:val data "error"))
	(w3w-error-msg (jsown:val data "message")))
    (if raise-error
	(error 'w3w-api-error :text (format nil "w3w error [~A]: ~A" w3w-error-id w3w-error-msg) :data (list w3w-error-id w3w-error-msg))
	(values w3w-error-id w3w-error-msg))))

(defun three-words-to-position (three-words &key (language nil) (corners nil) (key *key*) (raise-error nil))
  "three-words: list of three words or string of three words with dots `.` 
language: nil for default language or language-code (see get-languages function); use only if you want to return 3 words in a different language then the language to the language submitted (can be used for translation of '3 words'
corners: true for the coordinates of the w3w square, false for the southwest and northeast coordinates of the square
key: api-key

multiple-return-values: three words (list), position (list), type(string) language (language-code, string), corners (positions of southwest and northeast corners or nil)
raise-error: if true raise an error, if nil then return the errormessage from w3w
"
  (let* ((w3w-words (if (stringp three-words)
			three-words
			(format nil "~{~A~^.~}" three-words)))
	 (w3w-corners (if corners "true" "false"))
	 (json-string (flexi-streams:octets-to-string
		       (drakma:http-request *w3w-url* :method :get :parameters (append
										(list (cons "string" w3w-words) (cons "key" key))
										(if corners  (list (cons "corners" w3w-corners)) nil)
										(if language (list (cons "lang" language))       nil)))))
	 (return-data (jsown:parse json-string)))
    (if (member "error" (jsown:keywords return-data) :test #'string=)
	(check-error return-data raise-error)
	(values (jsown:val return-data "position")
		(jsown:val return-data "type")
		(jsown:val return-data "words")
		(jsown:val return-data "language")
		(if corners (jsown:val return-data "corners") nil)))))


  


(defun position-to-three-words (latitude longitude &key (language nil) (corners nil) (key *key*) (raise-error nil))
  "latitude: latitude in degrees
longitude: longitude in degrees
language: nil for default language or language-code (see get-languages function)
corners: true for the coordinates of the w3w square, false for the southwest and northeast coordinates of the square
key: api-key

multiple-return-values: three words (list), position (list), language (language-code, string), corners (positions of southwest and northeast corners or nil)
raise-error: if true raise an error, if nil then return the errormessage from w3w
"
  (let* ((w3w-position (format nil "~A,~A" latitude longitude))
	 (w3w-corners (if corners "true" "false"))
	 (json-string (flexi-streams:octets-to-string
		       (drakma:http-request *position-url* :method :get :parameters (append
										     (list (cons "position" w3w-position) (cons "key" key))
										     (if corners (list (cons "corners" w3w-corners)) nil)
										     (if language (list (cons "lang" language)) nil)))))
	 (return-data (jsown:parse json-string)))


     (if (member "error" (jsown:keywords return-data) :test #'string=)
	 (check-error return-data raise-error)
	 (values (jsown:val return-data "words")
		 (jsown:val return-data "position")
		 (jsown:val return-data "language")
		 (if corners (jsown:val return-data "corners") nil)))))




(defun get-languages (&key (codes-only t) (key *key*) (raise-error nil))
  "codes-only: if true return a list of language codes, if nil return a-list of language-codes and language-names
key: api-key
raise-error: if true raise an error, if nil then return the errormessage from w3w
"

  (let* ((json-string (flexi-streams:octets-to-string
		       (drakma:http-request *get-languages-url* :method :get :parameters (list (cons "key" key)))))
	 (return-data (jsown:parse json-string)))
    (if (member "error" (jsown:keywords return-data) :test #'string=)
	(check-error return-data raise-error)
	(let* ((json-languages (jsown:val (jsown:parse json-string) "languages"))
	       (language-codes (jsown:filter json-languages map "code"))
	       (language-names (jsown:filter json-languages map "name_display")))
	  (if codes-only
	      language-codes
	      (mapcar (lambda (code name) (cons code name)) language-codes language-names))))))
