(use gauche.process)
(use gauche.selector)
(use gauche.version)
(use srfi-13)
(use test.unit)

(sys-putenv "LIBUIM_SCM_FILES" "./scm")
(sys-putenv "LIBUIM_VERBOSE" "1")  ;; must be 1
(sys-putenv "LIBUIM_VANILLA" "1")

(set! (port-buffering (current-output-port)) :none)

(define *uim-sh-process* #f)
(define *uim-sh-selector* (make <selector>))

(define (uim-sh-select port . timeout)
  (selector-add! *uim-sh-selector*
                 port
                 (lambda (port flag)
                   (selector-delete! *uim-sh-selector* port #f #f))
                 '(r))
  (not (zero? (apply selector-select *uim-sh-selector* timeout))))

(define (uim-sh-write sexp out)
  (set! (port-buffering out) :none)
  (with-output-to-port out
    (lambda ()
      (write sexp)
      (newline)
      (flush))))

(define (uim-sh-read in)
  (set! (port-buffering in) :none)
  (uim-sh-select in)
  (let ((uim-sh-output (with-error-handler
                         (lambda (err)
                           ;; (report-error err)
                           (read-line in) ;; ignore read error
                           #f)
                         (lambda ()
                           (read in)))))
      (if (and (eq? 'ERROR: uim-sh-output)
               (uim-sh-select in 3))
        (error (string-trim-both (read-block 10000 in)))
        uim-sh-output)))

(define (uim sexp)
  (uim-sh-write sexp (process-input *uim-sh-process*))
  (uim-sh-read (process-output *uim-sh-process*)))

(define (uim-bool sexp)
  (not (null? (uim sexp))))

(eval
 (if (version>=? *gaunit-version* "0.0.6")
   '(begin
      (define (*uim-sh-setup-proc*)
        (set! *uim-sh-process* (run-process "uim/uim-sh"
                                            "-b"
                                            :input :pipe
                                            :output :pipe)))
      (define (*uim-sh-teardown-proc*)
        (close-input-port (process-input *uim-sh-process*))
        (set! *uim-sh-process* #f))

      (define-syntax define-uim-test-case
        (syntax-rules ()
          ((_ arg ...)
           (begin
             (gaunit-add-default-setup-proc! *uim-sh-setup-proc*)
             (gaunit-add-default-teardown-proc! *uim-sh-teardown-proc*)
             (define-test-case arg ...)
             (gaunit-delete-default-setup-proc! *uim-sh-setup-proc*)
             (gaunit-delete-default-teardown-proc! *uim-sh-teardown-proc*))))))

   '(begin
      (define (**default-test-suite**)
        (with-module test.unit *default-test-suite*))
      (define <test-case>
        (with-module test.unit <test-case>))
      (define make-tests
        (with-module test.unit make-tests))
      (define add-test-case!
        (with-module test.unit add-test-case!))

      (define (make-uim-sh-setup-proc . args)
        (let-optionals* args ((additional-setup-proc (lambda () #f)))
          (lambda ()
            (set! *uim-sh-process* (run-process "uim/uim-sh"
                                                "-b"
                                                :input :pipe
                                                :output :pipe))
            (additional-setup-proc))))

      (define (make-uim-sh-teardown-proc . args)
        (let-optionals* args ((additional-teardown-proc (lambda () #f)))
          (lambda ()
            (close-input-port (process-input *uim-sh-process*))
            (set! *uim-sh-process* #f)
            (additional-teardown-proc))))

      (define-syntax define-uim-test-case
        (syntax-rules ()
          ((_ name) #f)
          ((_ name rest ...)
           (add-test-case! (**default-test-suite**)
                           (make-uim-test-case name rest ...)))))

      (define-syntax make-uim-test-case
        (syntax-rules (setup teardown)
          ((_ name (setup setup-proc) (teardown teardown-proc) test ...)
           (make <test-case>
             :name name
             :setup (make-uim-sh-setup-proc setup-proc)
             :teardown (make-uim-sh-teardown-proc teardown-proc)
             :tests (make-tests test ...)))
          ((_ name (setup proc) test ...)
           (make <test-case>
             :name name
             :setup (make-uim-sh-setup-proc proc)
             :teardown (make-uim-sh-teardown-proc)
             :tests (make-tests test ...)))
          ((_ name (teardown proc) test ...)
           (make <test-case>
             :name name
             :setup (make-uim-sh-setup-proc)
             :teardown (make-uim-sh-teardown-proc proc)
             :tests (make-tests test ...)))
          ((_ name test ...)
           (make <test-case>
             :name name
             :setup (make-uim-sh-setup-proc)
             :teardown (make-uim-sh-teardown-proc)
             :tests (make-tests test ...)))))))
 (current-module))

(provide "test/uim-test-utils")
