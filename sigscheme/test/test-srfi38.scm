;; No assertive tests for now, just print something and see if we bloat.                                                              ;(load "test/unittest.scm")
(use srfi-38)

(let* ((s "abc")
       (convolution `(,s 1 #(,s b) (2) () ,s)))
  ; go crazy with mutators
  (set-car! (cdr convolution) convolution)
  (vector-set! (caddr convolution) 1 (cddr convolution))
  (set-cdr! (cadddr convolution) (cdr convolution))
  (write-with-shared-structure convolution))
(display " <-- computed output\n")
(display "#1=(#2=\"abc\" . #3=(#1# . #4=(#(#2# #4#) (2 . #3#) () #2#))) <-- expected output\n")

(let* ((a-pair '(kar . kdr))
            (convolution (eval (list 'lambda () a-pair) (scheme-report-environment 5))))
       (set-cdr! a-pair convolution)
       (write-with-shared-structure convolution))
(display " <-- computed output\n")
(display "#1=#<closure:(() (kar . #1#))> <-- expected output\n")

;(total-report)
