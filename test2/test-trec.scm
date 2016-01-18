;;  test-trec.scm: Unit tests for trec.scm
;;
;;; Copyright (c) 2008-2013 uim Project https://github.com/uim/uim
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

(require-extension (unittest))

(require "trec.scm")

(define romaji-ruleset '((("a")             . ("A"))
			 (("i")             . ("I"))
			 (("k")             . ("K"))
			 (("k" "a")         . ("KA"))
			 (("u")             . ("U"))
			 (("k" "k" "y" "a") . ("KKYA"))
			 (("k" "k" "a")     . ("KKA"))
			 (("k" "k" "y")     . ("KKY"))
			 (("k" "k")         . ("KK"))))
(define romaji-ruletree (trec-parse-ruleset string=? #f romaji-ruleset))

(test-begin "trec-node")
(test-equal #f (trec-node-key romaji-ruletree))
(test-equal #f (trec-node-val romaji-ruletree))
(test-true  (pair? (trec-node-branches romaji-ruletree)))
(test-true  (trec-node-root? romaji-ruletree))
(test-false (trec-node-leaf? romaji-ruletree))
(test-end)

(test-begin "trec-route")
(define initial (trec-route-new romaji-ruletree))
(test-eq    romaji-ruletree
	    (car (trec-route-point-node initial)))
(test-eq    romaji-ruletree
	    (trec-route-last-node initial))
(test-true  (trec-route-initial? initial))
(test-eq    romaji-ruletree
	    (car (trec-route-initial initial)))
(test-true  (trec-route-root? initial))
(test-eq    initial
	    (trec-route-last-root initial))
(test-false (trec-route-goal? initial))
(test-true  (pair? (trec-route-next-descendants initial)))
(test-eq    #f (trec-route-last-key initial))

;; filter
(test-equal '()
	    (filter-map-trec-route (lambda (rt)
				     (trec-route-last-key rt))
				   initial))
(test-equal '((#f))
	    (filter-map-trec-route (lambda (rt)
				     (list (trec-route-last-key rt)))
				   initial))

;; keys
(test-equal '()
	    (trec-route-filter-keys initial values))
(test-equal '()
	    (trec-route-filter-keys initial (lambda (k) #t)))
(test-error (trec-route-nth-key initial 0 values))
(test-error (trec-route-nth-key initial 0 (lambda (k) #t)))

;; values
(test-eq    #f (trec-route-value initial))
(test-equal '()
	    (trec-route-values initial))
(test-end)

(test-begin "trec-route advanced once")
;;(define rtr-string=? (trec-router-std-advance-new string=?))
(define rtr-string=? (trec-router-vanilla-advance-new string=?))
(define rt1.rej (trec-route-advance initial rtr-string=? "k"))
(define rt1 (car rt1.rej))
(test-eq    '()
	    (cdr rt1.rej))
(test-false (trec-node-root? (trec-route-last-node rt1)))
(test-false (trec-node-leaf? (trec-route-last-node rt1)))
(test-false (trec-route-initial? rt1))
(test-eq    romaji-ruletree
	    (car (trec-route-initial rt1)))
(test-false (trec-route-root? rt1))
(test-eq    initial
	    (trec-route-last-root rt1))
(test-false (trec-route-goal? rt1))
(test-true  (pair? (trec-route-next-descendants rt1)))
(test-equal "k"
	    (trec-route-last-key rt1))
;; keys
(test-equal '("k")
	    (trec-route-filter-keys rt1 values))
(test-equal '("k")
	    (trec-route-filter-keys rt1 (lambda (k) #t)))
(test-equal "k"
	    (trec-route-nth-key rt1 0 values))
(test-equal "k"
	    (trec-route-nth-key rt1 0 (lambda (k) #t)))

;; values
(test-equal '("K")
	    (trec-route-value rt1))
(test-equal '(("K"))
	    (trec-route-values rt1))
(test-end)

(test-begin "trec-route-route")
(test-equal '("KKYA")
	    (trec-route-value
	     (car
	      (trec-route-route initial rtr-string=? '("k" "k" "y" "a")))))
(test-equal '()
	    (cdr
	     (trec-route-route initial rtr-string=? '("k" "k" "y" "a"))))
(test-equal '("KKYA")
	    (trec-route-value
	     (car
	      (trec-route-route initial rtr-string=? '("k" "k" "y" "a" "f")))))
(test-equal '("f")
	    (cdr
	      (trec-route-route initial rtr-string=? '("k" "k" "y" "a" "f"))))
(test-equal '("f" "o" "o")
	    (cdr
	      (trec-route-route initial rtr-string=? '("k" "k" "y" "a" "f" "o" "o"))))
(test-end)

(test-begin "trec-route-backtrack")
(define k    (car (trec-route-advance initial rtr-string=? "k")))
(define kk   (car (trec-route-advance k       rtr-string=? "k")))
(define kka  (car (trec-route-advance kk      rtr-string=? "a")))
(define kky  (car (trec-route-advance kk      rtr-string=? "y")))
(define kkya (car (trec-route-advance kky     rtr-string=? "a")))
(test-eq    initial
	    (car (trec-route-backtrack initial)))
(test-equal #f
	    (cdr (trec-route-backtrack initial)))
(test-eq    initial
	    (car (trec-route-backtrack k)))
(test-equal "k"
	    (cdr (trec-route-backtrack k)))
(test-eq    k
	    (car (trec-route-backtrack kk)))
(test-equal "k"
	    (cdr (trec-route-backtrack kk)))
(test-eq    kk
	    (car (trec-route-backtrack kky)))
(test-equal "y"
	    (cdr (trec-route-backtrack kky)))
(test-eq    kky
	    (car (trec-route-backtrack kkya)))
(test-equal kka
	    (car (trec-route-advance
		  (car (trec-route-backtrack
			(car (trec-route-backtrack kkya))))
		  rtr-string=? "a")))
(test-end)

(test-begin "trec-route-keys")
(test-equal '()
	    (trec-route-keys initial))
(test-equal '("k")
	    (trec-route-keys k))
(test-equal '("k" "k")
	    (trec-route-keys kk))
(test-equal '("k" "k" "y")
	    (trec-route-keys kky))
(test-equal '("k" "k" "y" "a")
	    (trec-route-keys kkya))
(test-end)

(test-begin "trec-route-values")
(test-equal '()
	    (trec-route-values initial))
(test-equal '(("K"))
	    (trec-route-values k))
(test-equal '(("KK"))
	    (trec-route-values kk))
(test-equal '(("KKY"))
	    (trec-route-values kky))
(test-equal '(("KKYA"))
	    (trec-route-values kkya))
(test-end)

(test-report-result)
