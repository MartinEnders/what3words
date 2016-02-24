Common Lisp API for http://what3words.com/
=========================================

Implementation of the api-functions described at http://developer.what3words.com/api/

To try the examples you have to 'sign up for API' on http://developer.what3words.com/ to get your API-key

You can pass the key to the functions through the ```:key``` parameter or set the *key* variable:

```lisp
(setf what3words:*key* "BLABLABLA")
```


Convert 3 words to position
----------------------------

TODO



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

WHAT3WORDS> (format t "~{~A~%~}"(position-to-three-words 51.484463 -0.195405))
prom
cape
pump
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

Status
---------
Work in progress

Development environment
-----------------------
SBCL on Debian GNU/Linux