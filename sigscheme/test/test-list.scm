(load "test/unittest.scm")

; pair?
(assert "pair? test1" (pair? '(a . b)))
(assert "pair? test2" (pair? '(a b c)))
(assert-equal? "pair? test3" #f (pair? '()))
(assert-equal? "pair? test4" #f (pair? '#(a b)))

; cons
(assert-equal? "cons test1" '(a) (cons 'a '()))
(assert-equal? "cons test2" '((a) b c d) (cons '(a) '(b c d)))
(assert-equal? "cons test3" '(a . 3) (cons 'a 3))
(assert-equal? "cons test4" '((a b) . c) (cons '(a b) 'c))

; car
(assert-equal? "car test1" 'a (car '(a b c)))
(assert-equal? "car test2" '(a) (car '((a) b c)))
(assert-equal? "car test3" 1 (car '(1 . 2)))

; cdr
(assert-equal? "cdr test1" '(b c d) (cdr '((a) b c d)))
(assert-equal? "cdr test2" 2 (cdr '(1 . 2)))

; null?
(assert "null? test1" (null? '()))
(assert-equal? "null? test2" #f (null? "aiueo"))

; list?
(assert "list? test1" (list? '(a b c)))
(assert "list? test2" (list? '()))
(assert-equal? "list? test3" #f (list? '(a . b)))
; TODO : check finite length of the list!
;(assert-equal? "list? test4" #f (let ((x (list 'a)))
;			       (set-cdr! x x)
;			       (list? x)))

; list
(assert-equal? "list test1" '(a 7 c) (list 'a (+ 3 4) 'c))
(assert-equal? "list test2" '() (list))

; length
(assert-equal? "length test1" 3 (length '(a b c)))
(assert-equal? "length test2" 3 (length '(a (b) (c d e))))
(assert-equal? "length test2" 0 (length '()))

; append
(assert-equal? "append test1" '(x y) (append '(x) '(y)))
(assert-equal? "append test2" '(a b c d) (append '(a) '(b c d)))
(assert-equal? "append test3" '(a (b) (c)) (append '(a (b)) '((c))))
(define w '(n o))
(define x '(d o))
(define y '(car))
(define z '(why))
(assert-equal? "append test4" '(n o d o car why . ta) (append w x y () z 'ta))
(assert-equal? "append test5" '(n o) w)	; test non-destructiveness
(assert-eq? "append test6" x (cdr (append '((Calpis hosi-)) x))) ; share last

; reverse
(assert-equal? "reverse test1" '(c b a) (reverse '(a b c)))
(assert-equal? "reverse test2" '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

; list-tail
(assert-equal? "list-tail test1" '(a b c) (list-tail '(a b c) 0))
(assert-equal? "list-tail test2" '(b c) (list-tail '(a b c) 1))
(assert-equal? "list-tail test3" '(c) (list-tail '(a b c) 2))
(assert-equal? "list-tail test4" '() (list-tail '(a b c) 3))

; list-ref
(assert-equal? "list-ref test1" 'c (list-ref '(a b c d) 2))

; memq
(assert-equal? "memq test1" '(a b c) (memq 'a '(a b c)))
(assert-equal? "memq test2" '(b c) (memq 'b '(a b c)))
(assert-equal? "memq test3" #f (memq 'a '(b c d)))
(assert-equal? "memq test4" #f (memq (list 'a) '(b (a) c)))

; member
(assert-equal? "member test1" '((a) c) (member (list 'a) '(b (a) c)))

; assq
(define e '((a 1) (b 2) (c 3)))
(assert-equal? "assq test1" '(a 1) (assq 'a e))
(assert-equal? "assq test2" '(b 2) (assq 'b e))
(assert-equal? "assq test3" #f (assq 'd e))
(assert-equal? "assq test4" #f (assq (list 'a) '(((a)) ((b)) ((c)))))

; assoc
(assert-equal? "assoc test1" '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(total-report)
