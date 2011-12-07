;; JSON implementation for Scheme
;; See http://www.json.org/ or http://www.crockford.com/JSON/index.html
;;
;; Copyright (c) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; JSON Structures are represented as vectors: #((symbol . value) (symbol . value) ...)
;; JSON Arrays are lists
;;

(require-extension (srfi 69))

(require "packrat.scm")
(require "json-parser-expanded.scm")

(define (hashtable->vector ht)
  (list->vector (hash-table->alist ht)) )

(define json-write
  (let ()
    (define (write-ht vec p)
      (display "{" p)
      (do ((need-comma #f #t)
           (i 0 (+ i 1)))
          ((= i (vector-length vec)))
        (if need-comma
            (display ", " p)
            (set! need-comma #t))
        (let* ((entry (vector-ref vec i))
               (k (car entry))
               (v (cdr entry)))
          (cond
           ((symbol? k) (write (symbol->string k) p))
           ((string? k) (write k p)) ;; for convenience
           (else (error "Invalid JSON table key in json-write" k)))
          (display ": " p)
          (write-any v p)))
      (display "}" p))

    (define (write-array a p)
      (display "[" p)
      (let ((need-comma #f))
        (for-each (lambda (v)
                    (if need-comma
                        (display ", " p)
                        (set! need-comma #t))
                    (write-any v p))
                  a))
      (display "]" p))

    (define (write-any x p)
      (cond
       ((hash-table? x) (write-ht (hashtable->vector x) p))
       ((vector? x) (write-ht x p))
       ((list? x) (write-array x p))
       ((symbol? x) (write (symbol->string x) p)) ;; for convenience
       ((or (string? x)
            (number? x)) (write x p))
       ((boolean? x) (display (if x "true" "false") p))
       ((eq? x (void)) (display "null" p))
       (else (error "Invalid JSON object in json-write" x))))

    (lambda (x . maybe-port)
      (write-any x (if (pair? maybe-port) (car maybe-port) (current-output-port))))))

(define json-read
  (let ()
    (define (generator p)
      (let ((ateof #f)
            (pos (top-parse-position "<?>")))
        (lambda ()
          (if ateof
              (values pos #f)
              (let ((x (read-char p)))
                (if (eof-object? x)
                    (begin
                      (set! ateof #t)
                      (values pos #f))
                    (let ((old-pos pos))
                      (set! pos (update-parse-position pos x))
                      (values old-pos (cons x x)))))))))

    (define parser
      expanded:json-parser)

    (define (read-any p)
      (let ((result (parser (base-generator->results (generator p)))))
        (if (parse-result-successful? result)
            (parse-result-semantic-value result)
            (error "JSON Parse Error"
                   (let ((e (parse-result-error result)))
                     (list 'json-parse-error
                           (parse-position->string (parse-error-position e))
                           (parse-error-expected e)
                           (parse-error-messages e)))))))

    (lambda maybe-port
      (read-any (if (pair? maybe-port) (car maybe-port) (current-input-port))))))

