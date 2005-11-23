;; -*- euc-jp -*-
(load "./test/unittest.scm")

;; string?
(assert-true "string? check" (string? "あいうえお"))

;; make-string
(assert-true "alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert-true "hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))
(assert-equal? "make-string 1" "歩歩歩歩歩"   (make-string 5 #\歩))

;; string-ref
(assert-equal? "hiragana string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))
(assert-equal? "string-ref 1" #\歩  (string-ref "歯hi歩ﾍ歩" 3))


;; string-set!
(assert-true"hiragana string-set! check" (string=? "あいかえお"
						   (begin
						     (define str (string-copy "あいうえお"))
						     (string-set! str 2 #\か)
						     str)))
(assert-true"mixed string-set! check" (string=? "aiueo"
						(begin
						  (define str (string-copy "aiうeo"))
						  (string-set! str 2 #\u)
						  str)))
(assert-equal? "string-set! 1"     "金桂玉"   (string-set!
                                               (string-copy "金桂と")
                                               2
                                               #\玉))


;; string-length
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))

;; string=?
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))

;; substring
(assert-true"hiragana substring check" (string=? "いう" (substring (string-copy "あいうえお") 1 3)))
(assert-true"mixed substring check"    (string=? "いu"  (substring (string-copy "aいuえo") 1 3)))

;; string-append
(assert-true "hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert-true "hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert-true "hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert-true "mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert-true "mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))


;; string->list
(assert-true "string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))
(assert-equal? "string->list 1" '(#\ぁ #\き #\る) (string->list "ぁきる"))

;; list->string
(assert-equal? "list->string check" "あaい" (list->string '(#\あ #\a #\い)))
(assert-equal? "list->string 1" "3日で" (list->string '(#\3 #\日 #\で)))

;; string-fill!
(assert-true"hiragana string-fill! check" (string=? "あああああ" (begin
								   (define str (string-copy "aiueo"))
								   (string-fill! str #\あ)
								   str)))
(assert-true"mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								       (define str (string-copy "aいうえo"))
								       (string-fill! str #\a)
								       str)))
(assert-true"mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									    (define str (string-copy "aいうえo"))
									    (string-fill! str #\い)
									    str)))

;; string
(assert-equal? "string 1" "美人には" (string #\美 #\人 #\に #\は))

;; string-copy
(assert-equal? "string-copy 1"     "金銀香"   (string-copy "金銀香"))


;; The character after ☆ is from JIS X 0212.  The one after ◎ is
;; from JIS X 0213 plane 2.  This violates all known standards, but
;; souldn't be a real problem.
(define str1 "あﾋャah暴\\暴n!☆錳◎�、!")
(define str1-list '(#\あ #\ﾋ #\ャ #\a #\h #\暴 #\\ #\暴 #\n #\! #\☆ #\錳 #\◎ #\�、 #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))
