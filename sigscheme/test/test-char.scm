(load "./test/unittest.scm")

;; check char?
(assert-true "alphabet char" (char? #\a))
(assert-true "space 1"       (char? #\space))
(assert-true "space 2"       (char? #\ ))
(assert-true "tab"           (char? #\	))
(assert-true "newline 2"     (char? #\newline))
(assert-true "newline 2"     (char? #\
))
(assert-true "hiragana char" (char? #\дв))
(assert-true "( char"        (char? #\())
(assert-true ") char"        (char? #\)))
(assert-true "\\ char"       (char? #\\))

(total-report)
