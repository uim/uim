(load "./test/unittest.scm")

;; This file provides a fallback test unit for all EUC systems.  It's
;; just a copy of test-enc-eucjp.scm with EUCJP-specific character
;; sequences removed, so some characters may be undefined in other EUC
;; systems.
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

(define str1 "¤¢¥ãahË½\\Ë½n!¡ù¡ı!")
(define str1-list '(#\¤¢ #\¥ã #\a #\h #\Ë½ #\\ #\Ë½ #\n #\! #\¡ù #\¡ı #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))
