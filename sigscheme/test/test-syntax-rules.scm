;;  FileName : test-syntax-rules.scm
;;  About    : unit test for R5RS hygienic macro
;;
;;  Copyright (C) 2006 Jun Inoue <jun.lambda@gmail.com>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(load "./test/unittest.scm")


;; Uncomment these if the interpreter crashes somewhere

; (define-syntax assert-equal?
;   (syntax-rules ()
;     ((_ name expected expr)
;      (begin
;        (display name)(newline)
;        (assert name name (equal? expected expr))))))

; (define-syntax assert-eq?
;   (syntax-rules ()
;     ((_ name expected expr)
;      (begin
;        (display name)(newline)
;        (assert name name (eq? expected expr))))))

(assert-eq? "syntax-rules unwrap in quote"
            'syntax-rules
            (let-syntax
                ((macro (syntax-rules ()
                          ((_) 'syntax-rules))))
              (macro)))

(assert-equal? "syntax-rules unwrap in quote inside recursive macro"
               '(,#(bad) ,(bar . baz) ,foo)
               (letrec-syntax
                   ((foo (syntax-rules ()
                           ((_ datum) (bar datum))
                           ((_ (parts ...) arg args ...)
                            (foo (,arg parts ...) args ...))))
                    (bar (syntax-rules ()
                           ((_ datum) 'datum))))
                 (foo () foo (bar . baz) #(bad))))

(assert-equal? "syntax-rules unwrap in quasiquote (lists)"
               '(sym (sym . sym)
                     (quasiquote ((unquote sym) unquote sym))
                     ((quasiquote (unquote-splicing sym)))
                     (0 0 . 0))
               (let ((sym 0))
                 (let-syntax
                     ((macro
                          (syntax-rules ()
                            ((_) (list `sym `(sym . sym)
                                       ``(,sym . ,sym) `(`,@sym)
                                       `(,@(list sym) ,sym . ,sym))))))
                   (macro))))

(assert-equal? "syntax-rules unwrap in quasiquote (vectors)"
               '(#() #(sym) #(sym 0) #((quasiquote (unquote sym)) 0))
               (let ((sym 0))
                 (let-syntax
                     ((macro
                          (syntax-rules ()
                            ((_) (list `#() `#(sym) `#(sym ,sym)
                                       `#(`,sym ,@(list sym)))))))
                   (macro))))

(assert-equal? "syntax-rules unwrap in case"
               'ok
               (let ((f  (lambda () 'f)))
                 (let-syntax
                     ((macro (syntax-rules ()
                               ((_ key)
                                (case key
                                  ((0 f) 'ok)
                                  (else 'nok))))))
                   (macro (f)))))

(assert-equal? "syntax-rules unwrap in case (else clause)"
               'ok
               (letrec ((f (lambda () f)))
                 (let-syntax
                     ((macro (syntax-rules ()
                               ((_ key)
                                (case key
                                  ((0 f) 'nok)
                                  (else 'ok))))))
                   (macro (f)))))


(assert-equal? "syntax-rules pattern matching"
               '(match match match match2
                 match3 match3 match3)
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ #() "abc" def ...)
                              'match)
                             ((_ #(b ...) "abc" (1))
                              'match2)
                             ((_ a ...)
                              'match3)
                             ((_ . _)
                              'mismatch))))
                 (list (macro #() "abc" 0 1 a b 2)
                       (macro #() "abc")
                       (macro #() "abc" (1))
                       (macro #("") "abc" (1))
                       (macro 0 1 2)
                       (macro #() ())
                       (macro))))

(assert-equal? "syntax-rules pattern matching 2"
               '(match mismatch match mismatch mismatch)
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ (a ...)) 'match)
                             ((_ . _) 'mismatch))))
                 (list (macro (0 1 2))
                       (macro (3 4 . 5))
                       (macro ())
                       (macro #())
                       (macro))))

(assert-equal? "syntax-rules repeatable subpattern"
               '(((0 3) (1 4) (2 5) 0 1 2) ((6 8) (7 9) 6 7) ())
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ ((a ...) (b ...)) ...)
                              '(((a b) ... a ...) ...)))))
                 (macro ((0 1 2) (3 4 5)) ((6 7) (8 9)) (() ()))))

(assert-equal? "syntax-rules repeatable subpattern 2"
               '(((1 2 1 2 0) (4 4 3)) ((6 7 6 7 5) (8)))
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ ((b a ...) ...) ...)
                              '(((a ... a ... b) ...) ...)))))
                 (macro ((0 1 2) (3 4)) ((5 6 7) (8)))))

(assert-equal? "syntax-rules repeatable subpattern matched against improper list"
               'mismatch
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ (a ...)) 'match)
                             ((_ . _) 'mismatch))))
                 (macro (0 1 . 2))))

;; Pop quiz!  What does LISP stand for?
(assert-equal? "syntax-rules deeply nested ellipses"
               '(
                 (())
                 ((((((((((((((((((((((1 2) (1 2)) (() ()) (#(3) #(3)) (4 4) (5 5) 0))))))))))))))))))))
                 #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(((1 2) (1 2)) (#(3) #(3)) (4 4) (5 5) 0))))))))))))))))))))
                 mismatch mismatch mismatch mismatch
                 )
               (let-syntax
                   ((macro (syntax-rules ()
                             ; Make the nests deeper if you've increased
                             ; DEFAULT_INDEX_BUF_SIZE in macro.c.
                             ((_ (((((((((((((((((((a b ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...)...) ; 20 deep
                              '(((((((((((((((((((((b b) ... a) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                             ((_ #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(a b ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                              '#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#((b b) ... a) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                             ((_ . _) 'mismatch)
                             )))
                 (list
                  (macro ())
                  (macro (((((((((((((((((((0 (1 2) () #(3) 4 5))))))))))))))))))))
                  (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 (1 2) #(3) 4 5)))))))))))))))))))))
                  (macro ((((((((((((((((((0 (1 2) () #(3) 4 5))))))))))))))))))) ; too shallow
                  (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 (1 2) #(3) 4 5))))))))))))))))))))
                  (macro (((((((((((((((((((0 (1 2) () #(3) 4 5) 6))))))))))))))))))) ; 6 doesn't match
                  (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 (1 2) #(3) 4 5) 6))))))))))))))))))))
                  )
                 ))


;; The buffer is re-allocated twice for these
(assert-equal? "syntax-rules more deeply nested ellipses"
               '(()
                 (((((((((((((((((((((((((((((((((((1 1) ((2 . 3) (2 . 3)) (#(4) #(4)) (() ()) 0)))) ())))))))))))))) ()))))))))))))))))
                 #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#((1 1) ((2 . 3) (2 . 3)) (#(4) #(4)) (() ()) 0)) #())))))))))))))))) #()))))))))))))))))
                 mismatch mismatch mismatch mismatch)
               (let-syntax
                   ((macro (syntax-rules ()
                             ;; Should be at least
                             ;; DEFAULT_INDEX_BUF_SIZE * 2 + 1 levels
                             ((_ ((((((((((((((((((((((((((((((((((a b ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...)) ; 33 deep
                              '(((((((((((((((((((((((((((((((((((b b) ... a) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                             ((_ #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(a b ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                              '#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#((b b) ... a) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...) ...))
                             ((_ . _) 'mismatch)
                             )))
                 (list (macro ())
                       (macro ((((((((((((((((((((((((((((((((((0 1 (2 . 3) #(4) ())))) ())))))))))))))) ())))))))))))))))))
                       (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 1 (2 . 3) #(4) ())) #())))))))))))))))) #())))))))))))))))))
                       (macro (((((((((((((((((((((((((((((((((0 1 (2 . 3) #(4) ()))))))))))))))))) ()))))))))))))))))) ; too shallow
                       (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 1 (2 . 3) #(4) ())) #()))))))))))))))) #())))))))))))))))))
                       (macro ((((((((((((((((((((((((((((((((((0 1 (2 . 3) #(4) ()) 5))) ())))))))))))))) ()))))))))))))))))) ; 5 doesn't match
                       (macro #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(0 1 (2 . 3) #(4) ()) 5) #())))))))))))))))) #())))))))))))))))))
                         )))

(assert-equal? "syntax-rules repeatable subpattern inside vector"
               '(((0 3) (1 4) (2 5)) ())
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ #((a ...) (b ...))  ...)
                              '(((a b) ...) ...))
                             ((_ . else) 'mismatch))))
                 (macro #((0 1 2) (3 4 5)) #(() ()))))

(assert-equal? "syntax-rules ability to discern #(a ...) and #((a ...))"
               '(a-1 a-2 b-1 b-2)
               (let-syntax
                   ((macro-a (syntax-rules ()
                               ((_ #(0 ...)) 'a-1)
                               ((_ #((0 ...))) 'a-2)
                               ((_ . else) 'mismatch)))
                    (macro-b (syntax-rules ()
                               ((_ #((0 ...))) 'b-1)
                               ((_ #(0 ...)) 'b-2)
                               ((_ . else) 'mismatch))))
                 (list (macro-a #(0))
                       (macro-a #((0)))
                       (macro-b #((0)))
                       (macro-b #(0)))))

(assert-equal? "syntax-rules basic hygiene"
               '(0 1 2)
               (let ((x 0))
                 (let-syntax
                     ((macro (syntax-rules ()
                               ((_ arg)
                                (cons
                                 x
                                 (let ((x 1))
                                   (list x arg)))))))
                   (let ((x 2))
                     (macro x)))))

(assert-equal? "syntax-rules literals"
               '(ex mismatch)
               (let ((x 'ex) (y 'why))
                 (let-syntax
                     ((macro (syntax-rules (x y)
                               ((_ x) x)
                               ((_ y) y)
                               ((_ a) 'mismatch))))
                   (let ((y 1))
                     (list (macro x)
                           (macro y))))))

(define-syntax tl-macro
  (syntax-rules (x)
    ((_ x) 'match)
    ((_ y) 'mismatch)))

(assert-equal? "syntax-rules literals 2"
               '(match mismatch mismatch (match mismatch))
               (list (tl-macro x)
                     (tl-macro z)
                     (let ((x 0))
                       (tl-macro x))
                     (let-syntax
                         ((foo (syntax-rules ()
                                 ((_) (tl-macro x))))
                          (bar (syntax-rules ()
                                 ((_) (tl-macro z)))))
                       (list (foo)
                             (bar)))))

(assert-equal? "syntax-rules literals in nested macro"
               'mismatch
               (let-syntax
                   ((foo
                     (syntax-rules ()
                       ((_)
                        (let-syntax
                            ((insert
                              (syntax-rules ()
                                ((_ ex)
                                 (let ((x 0))
                                   (let-syntax
                                       ((macro
                                            (syntax-rules (x)
                                              ((_ x) 'match)
                                              ((_ _) 'mismatch))))
                                     (macro ex)))))))
                          (insert x))))))
                 (foo)))

(assert-equal? "syntax-rules literals in nested macro 2"
               'match
               ;; the y in (foo y) and x in (_ x) are both timestamped
               ;; once but at different times
               (let-syntax
                   ((baz (syntax-rules ()
                           ((_ arg)
                            (let-syntax
                                ((bar (syntax-rules ()
                                        ((_ y)
                                         (let-syntax
                                             ((foo (syntax-rules (x)
                                                     ((_ x) 'match)
                                                     ((_ _) 'mismatch))))
                                           (foo y))))))
                              (bar arg))))))
                 (baz x))
               )

(assert-equal? "syntax-rules hygiene in simple recursion"
               '(0 1 2)
               (let ((x 2))
                 (letrec-syntax
                     ((foo (syntax-rules ()
                             ((_ x) (cons x (let ((x 1)) (bar x))))
                             ((_ y %) (list y))))
                      (bar (syntax-rules ()
                             ((_ arg) (cons arg (foo x %))))))
                   (let ((x 0))
                     (foo x)))))

(assert-equal? "syntax-rules binding of symbol passed to submacro via vector"
               '(0 1 2)
               (let ((y 0))
                 (let-syntax
                     ((foo (syntax-rules ()
                             ((_ x)
                              (let-syntax
                                  ((bar (syntax-rules ()
                                          ((_ #(a b c)) (list a b c)))))
                                (let ((t 2))
                                  (bar #(y x t))))))))
                   (let ((z 1))
                     (foo z)))))

(assert-equal? "syntax-rules binding of symbol passed to submacro via literal expression"
               '(0 1)
               (let-syntax
                   ((foo (syntax-rules ()
                           ((_ a)
                            (let-syntax
                                ((bar (syntax-rules ()
                                        ((_ '(b c)) (list b c)))))
                              (let ((x 1))
                                (bar '(a x))))))))
                 (let ((x 0))
                   (foo x))))

(assert-equal? "syntax-rules heavy recursion"
               '(0 1 2 3 4)
               (let ((x 0))
                 (letrec-syntax
                     ((foo (syntax-rules (x z)
                             ((_ x) (cons x (bar 1)))
                             ((_ z) 'bindings-disregarded)
                             ((_ y) (cons y (bar x)))
                             ((_) (cons 4 (bar)))
                             ((_ . _) 'foo-mismatch)))
                      (bar (syntax-rules (x)
                             ((_ x) (cons 3 (foo)))
                             ((_ y) (cons y (let ((z 2))
                                              (foo z))))
                             ((_) '()))))
                   (foo x))))

(assert-equal? "syntax-rules whether literal identifier comparison is sensitive to binding"
               '(0 #f)
               (let ((sym 0))
                 (let-syntax
                     ((macro (syntax-rules (sym)
                               ((_ sym) sym)
                               ((_ . *) #f))))
                   (list
                    (macro sym)
                    (let ((sym 1))
                      (macro sym))))))

(assert-equal? "syntax-rules renamed identifier in <literals>"
               '(mismatch match mismatch match match mismatch)
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ a b c)
                              (let ((x 0) (b 0))
                                (let-syntax
                                    ((lx (syntax-rules (x)
                                           ((_ x) 'match)
                                           ((_ s) 'mismatch)))
                                     (ly (syntax-rules (y)
                                           ((_ y) 'match)
                                           ((_ s) 'mismatch)))
                                     (lz (syntax-rules (z)
                                           ((_ z) 'match)
                                           ((_ s) 'mismatch))))
                                  (let ((z 0))
                                    (list (lx a) (lx x)
                                          (ly b) (ly y)
                                          (lz c) (lz z)
                                          ))))
                              )
                             )))
                 (macro x y z)))

(assert-equal? "syntax-rules doubly time-stamped identifier in <literals>"
               'match
               (let-syntax
                   ((foo (syntax-rules ()
                           ((_ a)
                            (let-syntax
                                ((bar (syntax-rules ()
                                        ((_ b)
                                         (let-syntax
                                             ((baz (syntax-rules (x)
                                                     ((_ x) 'match)
                                                     ((_ s) 'mismatch))))
                                           (baz b))))))
                              (bar a))))))
                 (foo x)))

(assert-equal? "syntax-rules identifier from macro use in <literals>"
               '(ay ex ay ex)
               (let-syntax
                   ((foo (syntax-rules ()
                           ((_ a)
                            (let-syntax
                                ((bar (syntax-rules (a)
                                        ((_ a) 'ay)
                                        ((_ x) 'ex)
                                        ((_ y) 'mismatch))))
                              (list (bar a)
                                    (let ((a 0)) (bar a))
                                    (bar x)
                                    (let ((x 0)) (bar x)))
                              )))))
                 (foo x)))

;; An identifier in <literals> and another in <pattern> are considered
;; identical only if they're eq? assuming the renaming rules outlined
;; in src/macro.c.
(assert-equal? "syntax-rules comparing id in <literals> with another in pattern"
               '(match match)
               (let-syntax
                   ((foo (syntax-rules ()
                           ((_ a)
                            (let-syntax
                                ((ax (syntax-rules (a)
                                       ((_ x) 'match)
                                       ((_ s) 'mismatch)))
                                 (xa (syntax-rules (x)
                                       ((_ a) 'match)
                                       ((_ s) 'mismatch))))
                              (list (ax !?) (xa !?)))))))
                 (foo x)))

(assert-equal? "syntax-rules pattern in <pattern> with same name as one in <template> but different origins or bindings"
               '(0 0 1 0 0 3 1 3)
               (let ((x 0) (y 0) (z 0) (t 0))
                 (let-syntax
                     ((macro (syntax-rules ()
                               ((_ a b c d)
                                (let ((y 1) (t 1))
                                  (let-syntax
                                      ((ax (syntax-rules ()
                                             ((_ a) x)))
                                       (xa (syntax-rules ()
                                             ((_ x) a)))
                                       (by (syntax-rules ()
                                             ((_ b) y))) ; 1
                                       (yb (syntax-rules ()
                                             ((_ y) b)))
                                       (cz (syntax-rules ()
                                             ((_ c) z)))
                                       (zc (syntax-rules ()
                                             ((_ z) c))) ; 3
                                       (dt (syntax-rules ()
                                             ((_ d) t))) ; 1
                                       (td (syntax-rules ()
                                             ((_ t) d))) ; 3
                                       )
                                    (list (ax 2) (xa 2) (by 2) (yb 2)
                                          (cz 2) (zc 2) (dt 2) (td 2))
                                    ))))))
                   (let ((z 3) (t 3))
                     (macro x y z t)))))

; assert-no-error
(assert-equal? "syntax-rules duplicate pattern variables with different bindings"
               #t
               (let-syntax
                   ((macro (syntax-rules ()
                             ((_ a)
                              (let-syntax
                                  ((foo (syntax-rules ()
                                          ((_ a x) #t))))
                                (foo 0 1))))))
                 (macro x)))

(assert-equal? "syntax-rules simple memoization interference check"
               '(macro)
               (letrec-syntax
                   ((macro (syntax-rules ()
                           ((_ form)
                            (macro form form))
                           ((_ form copy)
                            (begin
                              form ;; this invocation may mutate the input form
                              'copy)) ;; which shouldn't alter this copy here
                           ((_) #t))))
                 (macro (macro))))

;; From Al Petrofski's "An Advanced Syntax-rules Primer for the Mildly Insane"
(assert-equal? "syntax-rules mildly insane"
               1
               (let ((x 1))
                 (let-syntax
                     ((foo (syntax-rules ()
                             ((_ y) (let-syntax
                                        ((bar (syntax-rules ()
                                                ((_) (let ((x 2)) y)))))
                                      (bar))))))
                   (foo x))))

;; From "JRM's Syntax-rules Primer for the Merely Eccentric"
(assert-equal? "syntax-rules bug in MzScheme < v207"
               'citations
               (let-syntax
                   ((please (syntax-rules ()
                              ((please . forms) forms))))
                 (please quote citations)))


;; Invalid forms.  And of course we're using macros to save typing
;; (which we can't in other tests because macros are optional).
(define assert-error-orig assert-error)
(define-syntax assert-error
  (syntax-rules ()
    ((_ name expr)
     (assert-error-orig name (lambda () expr)))))

(assert-error "macro use is improper list [can fail depending on config]"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ . _) 'blah))))
                (macro a . b)))

(assert-error "syntax-rules misplaced ellipsis 1"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ ...) #f))))
                (macro)))

(assert-error "syntax-rules misplaced ellipsis 2"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ #(...)) #f))))
                (macro)))

(assert-error "syntax-rules misplaced ellipsis 3"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ a ... b) #f))))
                (macro 0 1 2)))

(assert-error "syntax-rules misplaced ellipsis 4"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a . ...)) #f))))
                (macro (0 . 1))))

(assert-error "syntax-rules misplaced ellipsis 5"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a ... . b)) #f))))
                (macro (0 . 1))))

(assert-error "syntax-rules missing ellipsis"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a ...) ...)
                             (list a ...)))))
                (macro () (0) (1 2))))

(assert-error "syntax-rules misplaced ellipsis"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a ... ...)) #f))
                     (macro ())))))

(assert-error "syntax-rules too many ellipses"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a) ...)
                             (list (a ...) ...)))))
                ; (macro () (0) (1 2)) -- shouldn't be necessary
                #f
                ))

(assert-error "syntax-rules too many ellipses in vector"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a) ...)
                             (list #(a ...) ...)))))
                ; (macro () (0) (1 2)) -- shouldn't be necessary
                #f
                ))

(assert-error "syntax-rules too few ellipses"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a ...) ...)
                             '(a ...)))))
                ; (macro (() 0) (1 2))
                #f
                ))

(assert-error "syntax-rules too few ellipses in vector"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_ (a ...) ...)
                             '#(a ...)))))
                ; (macro (() 0) (1 2))
                #f
                ))

(assert-error "syntax-rules constant template repetition"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_) #(a ...)))))
                #f))

(assert-error "syntax-rules constant template repetition in vector"
              (let-syntax
                  ((macro (syntax-rules ()
                            ((_) #(a ...)))))
                #f))



; Some corner cases.  Don't know what to do with these.  The
; assertions listed here show behaviors that *seem* reasonable to me.

; (assert-equal? "binding shadowed by let-syntax frame"
;                'foo
;                (let ((foo 0))
;                  (let-syntax
;                      ((macro
;                           (syntax-rules (foo)
;                             ((_ foo) foo)
;                             ((_ bar) #f))))
;                    (let-syntax
;                        ((foo (syntax-rules () ((_) -1))))
;                      (macro foo)
;                      ))))

; ; Or this can be quite useful if it produces (0 1 3 4).
; (assert-error "flattening of match tree"
;               (let-syntax
;                   ((macro (syntax-rules ()
;                             ((_ (a ...) ...)
;                              '(a ... ...)))))
;                 (macro (0 1) (3 4))))

; (assert-wreck-havoc? "ellipsis rebound"
;                      (let ((... 0))
;                        (let-syntax
;                            ((macro (syntax-rules ()
;                                      ((_ a ...) '(a ...))
;                                      ((_ . _) #f))))
;                          (macro 0 1 2))))

; ; Guile (and presumably Chez) chokes on this.
; (assert-equal? "one pvar exhausts earlier than another in template"
;                '((0 3) (1 4))
;                (let-syntax
;                    ((macro (syntax-rules ()
;                              ((_ (foo ...) (bar ...))
;                               '((foo bar) ...))
;                              ((_ . _) 'mismatch))))
;                  (macro (0 1 2) (3 4))))

(total-report)

