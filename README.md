Common Lisp API for http://what3words.com/
=========================================

Implementation of the api-functions described at http://developer.what3words.com/api/

Status
---------
Work in progress (2016-02-24)

Existing functions:

* three-words-to-position
* position-to-three-words
* get-languages

Currently there is no exception handling implemented.



Dependencies
------------
* drakma
* jsown

Remarks
-------

To try the examples you have to 'sign up for API' on http://developer.what3words.com/ to get your API-key

You can pass the key to the functions through the ```:key``` parameter or set the *key* variable:

```lisp
(setf what3words:*key* "BLABLABLA")
```


Convert 3 words to position
----------------------------

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

URL: https://api.what3words.com/get-languages

```lisp
WHAT3WORDS> (get-languages)
("de" "en" "es" "fr" "it" "pt" "ru" "sv" "sw" "tr")

WHAT3WORDS> (get-languages :codes-only nil)
(("de" . "Deutsch (beta)") ("en" . "English") ("es" . "Español (beta)")
 ("fr" . "Français (beta)") ("it" . "Italiano (beta)")
 ("pt" . "Português (beta)") ("ru" . "Русский (beta)")
 ("sv" . "Svenska (beta)") ("sw" . "Kiswahili (beta)") ("tr" . "Türkçe (beta)"))
 ```

Development environment
-----------------------
SBCL on Debian GNU/Linux