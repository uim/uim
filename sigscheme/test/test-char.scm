(load "test/unittest.scm")

;; check char?
(assert "alphabet char" (char? #\a))
(assert "space 1"       (char? #\space))
(assert "space 2"       (char? #\ ))
(assert "newline"       (char? #\newline))
(assert "hiragana char" (char? #\дв))
(assert "( char"        (char? #\())
(assert ") char"        (char? #\)))
(assert "\\ char"       (char? #\\))

(total-report)
