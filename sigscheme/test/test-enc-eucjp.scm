;; -*- euc-jp -*-
(load "./test/unittest.scm")

(assert-equal? "string 1" "Èş¿Í¤Ë¤Ï" (string #\Èş #\¿Í #\¤Ë #\¤Ï))
(assert-equal? "list->string 1" "3Æü¤Ç" (list->string '(#\3 #\Æü #\¤Ç)))
(assert-equal? "string->list 1" '(#\¤¡ #\¤­ #\¤ë) (string->list "¤¡¤­¤ë"))

(assert-equal? "string-ref 1" #\Êâ  (string-ref "»õhiÊâÍÊâ" 3))
(assert-equal? "make-string 1" "ÊâÊâÊâÊâÊâ"   (make-string 5 #\Êâ))
(assert-equal? "string-copy 1"     "¶â¶ä¹á"   (string-copy "¶â¶ä¹á"))
(assert-equal? "string-set! 1"     "¶â·Ë¶Ì"   (string-set!
                                               (string-copy "¶â·Ë¤È")
                                               2
                                               #\¶Ì))


;; The character after ¡ù is from JIS X 0212.  The one after ¡ı is
;; from JIS X 0213 plane 2.  This violates all known standards, but
;; souldn't be a real problem.
(define str1 "¤¢Ë¥ãahË½\\Ë½n!¡ùää¡ı¡¢!")
(define str1-list '(#\¤¢ #\Ë #\¥ã #\a #\h #\Ë½ #\\ #\Ë½ #\n #\! #\¡ù #\ää #\¡ı #\¡¢ #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))
