(load "./test/unittest.scm")

;; check string?
(assert "string? check" (string? "aiueo"))

;; check make-string
(assert "null make-string" (string? (make-string 6)))
(assert "alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert "hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))

;; check string-ref
(assert-equal? "alphabet string-ref check" #\o  (string-ref "aiueo" 4))
(assert-equal? "hiragena string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))

;; check string-set!
(assert "alphabet string-set! check" (string=? "aikeo"
					       (begin
						 (define str "aiueo")
						 (string-set! str 2 #\k)
						 str)))
(assert "hiragana string-set! check" (string=? "あいかえお"
					       (begin
						 (define str "あいうえお")
						 (string-set! str 2 #\か)
						 str)))
(assert "mixed string-set! check" (string=? "aiueo"
					    (begin
					      (define str "aiうeo")
					      (string-set! str 2 #\u)
					      str)))

;; check string-length
(assert-equal? "alphabet string-length check" 5 (string-length "aiueo"))
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))

;; string=? check
(assert-equal? "alphabet string=? check" #t (string=? "aiueo" "aiueo"))
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))


;; substring check
(assert "alphabet substring check" (string=? "iu"   (substring "aiueo" 1 3)))
(assert "hiragana substring check" (string=? "いう" (substring "あいうえお" 1 3)))
(assert "mixed substring check"    (string=? "いu"  (substring "aいuえo" 1 3)))


;; string-append check
(assert "alphabet 1 string-append check" (string=? "a"   (string-append "a")))
(assert "alphabet 2 string-append check" (string=? "ai"  (string-append "a" "i")))
(assert "alphabet 3 string-append check" (string=? "aiu" (string-append "a" "i" "u")))
(assert "hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert "hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert "hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert "mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert "mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))

;; string->list
(assert "string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))


;; list->string
(assert "list->string check" (string=? "あaい" (list->string '(#\あ #\a #\い))))


;; string-fill!
(assert "alphabet string-fill! check" (string=? "jjjjj" (begin
							  (define str "aiueo")
							  (string-fill! str #\j)
							  str)))
(assert "hiragana string-fill! check" (string=? "あああああ" (begin
							       (define str "aiueo")
							       (string-fill! str #\あ)
							       str)))
(assert "mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								   (define str "aいうえo")
								   (string-fill! str #\a)
								   str)))
(assert "mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									(define str "aいうえo")
									(string-fill! str #\い)
									str)))

(total-report)
