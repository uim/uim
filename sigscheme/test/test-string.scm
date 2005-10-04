(load "./test/unittest.scm")

;; check string?
(assert-true"string? check" (string? "aiueo"))

;; check make-string
(assert-true"null make-string" (string? (make-string 6)))
(assert-true"alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert-true"hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))

;; check string-ref
(assert-equal? "alphabet string-ref check" #\o  (string-ref "aiueo" 4))
(assert-equal? "hiragena string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))

;; check string-set!
(assert-true"alphabet string-set! check" (string=? "aikeo"
					       (begin
						 (define str "aiueo")
						 (string-set! str 2 #\k)
						 str)))
(assert-true"hiragana string-set! check" (string=? "あいかえお"
					       (begin
						 (define str "あいうえお")
						 (string-set! str 2 #\か)
						 str)))
(assert-true"mixed string-set! check" (string=? "aiueo"
					    (begin
					      (define str "aiうeo")
					      (string-set! str 2 #\u)
					      str)))

;; check string-length
(assert-equal? "alphabet string-length check" 5 (string-length "aiueo"))
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))
(assert-equal? "backslash string-length check" 1 (string-length "\\"))
(assert-equal? "backslash string-length check" 2 (string-length "\\\\"))
(assert-equal? "backslash string-length check" 3 (string-length "\\\\\\"))

;; string=? check
(assert-equal? "alphabet string=? check" #t (string=? "aiueo" "aiueo"))
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))

;; substring check
(assert-true"alphabet substring check" (string=? "iu"   (substring "aiueo" 1 3)))
(assert-true"hiragana substring check" (string=? "いう" (substring "あいうえお" 1 3)))
(assert-true"mixed substring check"    (string=? "いu"  (substring "aいuえo" 1 3)))


;; string-append check
(assert-true"alphabet 1 string-append check" (string=? "a"   (string-append "a")))
(assert-true"alphabet 2 string-append check" (string=? "ai"  (string-append "a" "i")))
(assert-true"alphabet 3 string-append check" (string=? "aiu" (string-append "a" "i" "u")))
(assert-true"hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert-true"hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert-true"hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert-true"mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert-true"mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))

;; string->list
(assert-true"string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))
(assert-equal? "string->list check" '(#\\)             (string->list "\\"))
(assert-equal? "string->list check" '(#\\ #\\)         (string->list "\\\\"))
(assert-equal? "string->list check" '(#\\ #\\ #\\)     (string->list "\\\\\\"))
;;(assert-equal? "string->list check" '(#\tab)           (string->list "\t"))
(assert-equal? "string->list check" '(#\	)      (string->list "\t"))
;;(assert-equal? "string->list check" '(#\return)        (string->list "\r"))
(assert-equal? "string->list check" '(#\)            (string->list "\r"))
(assert-equal? "string->list check" '(#\ #\)       (string->list "\r\r"))
(assert-equal? "string->list check" '(#\newline)           (string->list "\n"))
(assert-equal? "string->list check" '(#\newline #\newline) (string->list "\n\n"))
(assert-equal? "string->list check" '(#\space)         (string->list " "))
(assert-equal? "string->list check" '(#\space #\space) (string->list "  "))
(assert-equal? "string->list check" '(#\")             (string->list "\""))
(assert-equal? "string->list check" '(#\" #\")         (string->list "\"\""))

;; list->string
(assert-equal? "list->string check" "あaい" (list->string '(#\あ #\a #\い)))
(assert-equal? "list->string check" "\\"     (list->string '(#\\)))
(assert-equal? "list->string check" "\\\\"   (list->string '(#\\ #\\)))
(assert-equal? "list->string check" "\\\\\\" (list->string '(#\\ #\\ #\\)))
(assert-equal? "list->string check" "\t" (list->string '(#\	)))
;;(assert-equal? "list->string check" "\t" (list->string '(#\tab)))
(assert-equal? "list->string check" "\r" (list->string '(#\)))
;;(assert-equal? "list->string check" "\r" (list->string '(#\return)))
(assert-equal? "list->string check" "\n" (list->string '(#\
)))
(assert-equal? "list->string check" "\n" (list->string '(#\newline)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" " " (list->string '(#\space)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" "\"" (list->string '(#\")))
(assert-equal? "list->string check" "\"a\"" (list->string '(#\" #\a #\")))

;; string-fill!
(assert-true"alphabet string-fill! check" (string=? "jjjjj" (begin
							  (define str "aiueo")
							  (string-fill! str #\j)
							  str)))
(assert-true"hiragana string-fill! check" (string=? "あああああ" (begin
							       (define str "aiueo")
							       (string-fill! str #\あ)
							       str)))
(assert-true"mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								   (define str "aいうえo")
								   (string-fill! str #\a)
								   str)))
(assert-true"mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									(define str "aいうえo")
									(string-fill! str #\い)
									str)))

(total-report)
