;; -*- euc-jp -*-
(load "./test/unittest.scm")

(assert-equal? "string 1" "美人には" (string #\美 #\人 #\に #\は))
(assert-equal? "list->string 1" "3日で" (list->string '(#\3 #\日 #\で)))
(assert-equal? "string->list 1" '(#\ぁ #\き #\る) (string->list "ぁきる"))

(assert-equal? "string-ref 1" #\歩  (string-ref "歯hi歩ﾍ歩" 3))
(assert-equal? "make-string 1" "歩歩歩歩歩"   (make-string 5 #\歩))
(assert-equal? "string-copy 1"     "金銀香"   (string-copy "金銀香"))
(assert-equal? "string-set! 1"     "金桂玉"   (string-set!
                                               (string-copy "金桂と")
                                               2
                                               #\玉))


(define str1 "あﾋャah暴\\暴n!☆錳◎!")
(define str1-list '(#\あ #\ﾋ #\ャ #\a #\h #\暴 #\\ #\暴 #\n #\! #\☆ #\錳 #\◎ #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))

(total-report)
