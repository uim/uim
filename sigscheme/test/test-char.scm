(load "test/unittest.scm")

;; check char?
(assert "alphabet char" (char? #\a))
(assert "space"         (char? #\space))
(assert "newline"       (char? #\newline))
(assert "hiragana char" (char? #\дв))

(total-report)
