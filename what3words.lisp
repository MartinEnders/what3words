;;;; what3words.lisp
;;;;
;;;; Copyright (c) 2016 Martin R. Enders
;;;; Please see the file LICENSE in the distribution.


(in-package #:what3words)

;;; "what3words" goes here. Hacks and glory await!

(defparameter *key* nil)
(defparameter *get-languages-url* "https://api.what3words.com/v2/languages")
(defparameter *position-url* "https://api.what3words.com/v2/reverse")
(defparameter *w3w-url* "https://api.what3words.com/v2/forward")

(setf drakma:*drakma-default-external-format* :utf-8)

(define-condition w3w-api-error (error)
  ((text :initarg :text :reader text)
   (data :initarg :data :reader data)))

(defun ensure-json-key (data key default)
  (if (member key (jsown:keywords data) :test #'string=)
			     (jsown:val data key)
			     default))

(defun get-api-status (data)
  "The structure of the status within the response from w3w is not always the same"
  (let ((status "")
	(code "")
	(message ""))
    ;; check if data has status attribute
    (let ((w3w-status (ensure-json-key data "status" nil)))
      (if w3w-status
	  ;; status-key exists in w3w response
	  (progn
	    (setf status  (ensure-json-key w3w-status "status" 200))
	    (setf code    (ensure-json-key w3w-status "code" 200))
	    (setf message (ensure-json-key w3w-status "message" "")))
	    
	  ;; status-key does not exist
	  (progn
	    (setf code    (ensure-json-key data "code" 200))
	    (setf message (ensure-json-key data "message" "")))))
    (values status code message)))



(defun handle-w3w-error-status (data raise-error)
  "raise or return error message from w3w API"
  (multiple-value-bind (status code message) (get-api-status data)
    (declare (ignore status))
    (if raise-error
	(error 'w3w-api-error :text (format nil "w3w error [~A]: ~A" code message) :data (list code message))
	(values nil code message))))

(defun w3w-error-p (w3w-return-data)
  "Check status from w3w api"
  (multiple-value-bind (status code) (get-api-status w3w-return-data)
    (if (and (equal status 200)
	     (equal code 200))
	nil
	t)))
    

(defun w3w-to-list (w3w)
  "Convert What 3 Words dot notation to list"
  (cond ((stringp w3w)
	 (cl-ppcre:split "\\." w3w))
	((listp w3w)
	 w3w)
	(t nil)))


(defun create-return-data (return-data corners raise-error first-value)
  "Create returndata for the exported functions from the parsed json objects"
  (if (w3w-error-p return-data)
      (handle-w3w-error-status return-data raise-error)
      (let ((coords (list (jsown:filter return-data "geometry" "lat")
			  (jsown:filter return-data "geometry" "lng")))
	    (words (w3w-to-list (jsown:val return-data "words")))
	    (lang  (jsown:val return-data "language"))
	    (corners (if corners (list (list (jsown:filter return-data "bounds" "southwest" "lat")
					     (jsown:filter return-data "bounds" "southwest" "lng"))
				       (list (jsown:filter return-data "bounds" "northeast" "lat")
					     (jsown:filter return-data "bounds" "northeast" "lng")))
			 nil)))

	(cond ((eq first-value 'coords)
	       (values coords "3 words" words lang corners))
	      ((eq first-value 'words)
	       (values words "3 words" coords lang corners))))))

(defun three-words-to-position (three-words &key (language nil) (corners nil) (key *key*) (raise-error nil))
  "three-words: list of three words or string of three words with dots `.` 
language: nil for default language or language-code (see get-languages function); use only if you want to return 3 words in a different language then the language to the language submitted (can be used for translation of '3 words'
corners: true for the coordinates of the w3w square, false for the southwest and northeast coordinates of the square
key: api-key

multiple-return-values: three words (list), position (list), type(string) language (language-code, string), corners (positions of southwest and northeast corners or nil)
raise-error: if true raise an error if one occurs, if nil then return nil and the errormessage from w3w as multiple return values
"
  (let* ((w3w-words (if (stringp three-words)
			three-words
			(format nil "~{~A~^.~}" three-words)))
	 (w3w-corners (if corners "true" "false"))
	 (json-string (flexi-streams:octets-to-string
		       (drakma:http-request *w3w-url* :method :get :parameters (append
										(list (cons "addr" w3w-words) (cons "key" key))
										;(if corners  (list (cons "corners" w3w-corners)) nil)
										(if language (list (cons "lang" language))       nil)))))
	 (return-data (jsown:parse json-string)))
    (create-return-data return-data corners raise-error 'coords)))


(defun position-to-three-words (latitude longitude &key (language nil) (corners nil) (key *key*) (raise-error nil))
  "latitude: latitude in degrees
longitude: longitude in degrees
language: nil for default language or language-code (see get-languages function)
corners: true for the coordinates of the w3w square, false for the southwest and northeast coordinates of the square
key: api-key

multiple-return-values: three words (list), position (list), language (language-code, string), corners (positions of southwest and northeast corners or nil)
raise-error: if true raise an error if one occurs, if nil then return nil and the errormessage from w3w as multiple return values
"
  (let* ((w3w-position (format nil "~A,~A" latitude longitude))
	 (w3w-corners (if corners "true" "false"))
	 (json-string (flexi-streams:octets-to-string
		       (drakma:http-request *position-url* :method :get :parameters (append
										     (list (cons "coords" w3w-position) (cons "key" key))
										     ;(if corners (list (cons "corners" w3w-corners)) nil)
										     (if language (list (cons "lang" language)) nil)))))
	 (return-data (jsown:parse json-string)))
    (create-return-data return-data corners raise-error 'words)))



(defun get-languages (&key (codes-only t) (key *key*) (raise-error nil))
  "codes-only: if true return a list of language codes, if nil return a-list of language-codes and language-names
key: api-key
raise-error: if true raise an error if one occurs, if nil then return nil and the errormessage from w3w as multiple return values
"

  (let* ((json-string (flexi-streams:octets-to-string
		       (drakma:http-request *get-languages-url* :method :get :parameters (list (cons "key" key)))))
	 (return-data (jsown:parse json-string)))
    (if (w3w-error-p return-data)
	(handle-w3w-error-status return-data raise-error)
	(let* ((json-languages (jsown:val (jsown:parse json-string) "languages"))
	       (language-codes (jsown:filter json-languages map "code"))
	       (language-names (jsown:filter json-languages map "native_name")))
	  (if codes-only
	      language-codes
	      (mapcar (lambda (code name) (cons code name)) language-codes language-names))))))
