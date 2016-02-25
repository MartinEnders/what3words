Common Lisp API for http://what3words.com/
=========================================

Implementation of the api-functions described at http://developer.what3words.com/api/

Status
---------
Work in progress (2016-02-25)

Existing functions:

* three-words-to-position
* position-to-three-words
* get-languages


Dependencies
------------
* drakma ```(ql:quickload :drakma)```
* jsown ```(ql:quickload :jsown)```


Test
------------

Run the following function after loading the library:
```lisp
(what3words-test:test "your-w3w-key")
```


Remarks
-------

To try the examples you have to 'sign up for API' on http://developer.what3words.com/ to get your API-key

You can pass the key to the functions through the ```:key``` parameter or set the *key* variable:

```lisp
(setf what3words:*key* "your-w3w-key")
```



Convert 3 words to position
----------------------------

```lisp
(defun three-words-to-position (three-words &key (language nil) (corners nil) (key *key*) (raise-error nil))
```
* `three-words` List of three words or string of three words with dots `.` 
* `language` set to NIL (default) for default language or set language-code as provided from `get-languages`; use only if you want to return 3 words in a different language then the language of the 3 words you submitted (can be used for translation of '3 words')
* `corners` set to NIL (default)for the center-coordinates of the w3w square, set to true for the southwest and northeast coordinates of the square
* `key` your w3w api-key, `key` is set to what3words:*key* by default
* `raise-error` set to true if you want to raise errors based on the error messages from w3w, set to NIL (default) to get w3w error messages as return value of the function

returns (multiple return values):
* position (list)
* type (string)
* three words (list)
* language (language-code, string)
* corners (positions of southwest and northeast corners or nil)

API-URL: https://api.what3words.com/w3w

Examples:

three-words-to-position returns multiple values (coordinates, type, 3-words, corners)

```lisp
WHAT3WORDS> (three-words-to-position (list "prom" "cape" "pump"))
(51484463/1000000 -39081/200000)
"3 words"
("prom" "cape" "pump")
"en"
NIL
WHAT3WORDS> (three-words-to-position (list "prom" "cape" "pump") :language "de")
(51484463/1000000 -39081/200000)
"3 words"
("scholle" "lohn" "gleichheit") ;; Translation of prom cape pump
"de"
NIL
WHAT3WORDS> (three-words-to-position (list "prom" "cape" "pump") :corners t)
(51484463/1000000 -39081/200000)
"3 words"
("prom" "cape" "pump")
"en"
((51484449/1000000 -97713/500000) (12871119/250000 -195383/1000000))
```


Convert position to 3 words
----------------------------

```lisp
(defun position-to-three-words (latitude longitude &key (language nil) (corners nil) (key *key*) (raise-error nil))
```
* `latitude` in degrees
* `longitude` in degrees
* `language` set to NIL for default language or to language-code (see `get-languages`)
* `corners: true for the coordinates of the w3w square, false for the southwest and northeast coordinates of the square
* `key` your w3w api-key, `key` is set to what3words:*key* by default
* `raise-error` set to true if you want to raise errors based on the error messages from w3w, set to NIL (default) to get w3w error messages as return value of the function

returns (multiple return values):
* three words (list)
* position (list)
* language (language-code, string)
* corners (positions of southwest and northeast corners or nil)



API-URL: https://api.what3words.com/position

Examples:

position-to-three-words returns multiple values

```lisp
WHAT3WORDS> (position-to-three-words 51.484463 -0.195405)
("prom" "cape" "pump")
(51484463/1000000 -39081/200000)
"en"
NIL

WHAT3WORDS> (position-to-three-words 51.484463 -0.195405 :language "de")
("scholle" "lohn" "gleichheit")
(51484463/1000000 -39081/200000)
"de"
NIL

WHAT3WORDS> (position-to-three-words 51.484463 -0.195405 :corners t)
("prom" "cape" "pump")
(51484463/1000000 -39081/200000)
"en"
((51484449/1000000 -97713/500000) (12871119/250000 -195383/1000000))

WHAT3WORDS> (format t "~{~A~%~}" (position-to-three-words 51.484463 -0.195405))
prom
cape
pump

WHAT3WORDS> (format t "https://map.what3words.com/~{~A~^+~}" (position-to-three-words 51.484463 -0.195405))
https://map.what3words.com/prom+cape+pump
```




Get list of available 3 word languages
---------------------------------------

```lisp
(defun get-languages (&key (codes-only t) (key *key*) (raise-error nil))
```
* `codes-only` if true return a list of language codes, if nil return a-list of language-codes and language-names
* `key` your w3w api-key, `key` is set to what3words:*key* by default
* `raise-error` set to true if you want to raise errors based on the error messages from w3w, set to NIL (default) to get w3w error messages as return value of the function

API-URL: https://api.what3words.com/get-languages

```lisp
WHAT3WORDS> (get-languages)
("de" "en" "es" "fr" "it" "pt" "ru" "sv" "sw" "tr")

WHAT3WORDS> (get-languages :codes-only nil)
(("de" . "Deutsch (beta)") ("en" . "English") ("es" . "Español (beta)")
 ("fr" . "Français (beta)") ("it" . "Italiano (beta)")
 ("pt" . "Português (beta)") ("ru" . "Русский (beta)")
 ("sv" . "Svenska (beta)") ("sw" . "Kiswahili (beta)") ("tr" . "Türkçe (beta)"))
 ```

Exception handling
----------------------

Every API-Function has an optional `raise-error` attribute.
Errors which are controlled by the `raise-error` attribute are the errors listed in the w3w 'Error code list' (http://developer.what3words.com/additional-reference-docs/#apierrorcodes)
If `raise-error` is true then a w3w-api-error is thrown:

```lisp
WHAT3WORDS> (handler-case (let ((*key* "testtest"))
			    (get-languages :raise-error t))
	      (w3w-api-error (e) 
		(print 'error-handling)
		(print e)
		(print (text e))
		(print (data e))))
	      

ERROR-HANDLING 
#<W3W-API-ERROR {1006A60963}> 
"w3w error [X1]: Missing or invalid key" 
("X1" "Missing or invalid key") 
("X1" "Missing or invalid key")
; compiling (DEFPACKAGE #:WHAT3WORDS ...)
```

If `raise-error` parameter is NIL (default value) then the error-message from the w3w API is returned as mulitple values (id and description)
```lisp
WHAT3WORDS> (let ((*key* "testtest"))
	      (get-languages :raise-error nil))
"X1"
"Missing or invalid key"
WHAT3WORDS> 
```

I use `drakma` for the https requests and `jsown` for JSON-parsing so if one of this libraries runs into an exception they will be not handeled by the what3words library.


Development environment
-----------------------
SBCL on Debian GNU/Linux

License
----------
Please see the file LICENSE in the distribution.