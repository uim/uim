#!/usr/bin/env gosh

(use gauche.interactive)
(use file.util)
(use test.unit)

(add-load-path ".")

(if (symbol-bound? 'main)
    (define _main main))

(define (main args)
  (let ((dir (sys-dirname (car args))))
    (for-each (lambda (test-script)
                (load (string-join (list dir test-script) "/")))
              (directory-list dir
                              :filter (lambda (x)
                                        (rxmatch #/^test-.+\.scm$/ x))))
    (if (symbol-bound? '_main)
        (_main `(,(car args) "-vp" ,@(cdr args)))
        (run-all-test))))
