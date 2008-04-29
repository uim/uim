#!/usr/bin/env gosh

;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;; This file is tested with revision * of new repository

(use test.unit)

(require "test/uim-test-utils")

;;(define-uim-test-case "testcase trec-node"
;;  (setup
;;   (lambda ()
;;     (uim '(require "trec.scm"))))
;;
;;  ("test trec-node-new"
;;   (assert-equal (uim ''(#f #f . ()))
;;		 (uim '(trec-node-new)))
;;   (assert-equal (uim ''("a" #f . ()))
;;		 (uim '(trec-node-new "a")))
;;   (assert-equal (uim ''("a" ("A") . ()))
;;		 (uim '(trec-node-new "a" '("A")))))
;;
;;  ("test trec-node-branches"
;;   (assert-equal ()
;;		 (uim '(trec-node-branches (trec-node-new)))))
;;
;;  ("test trec-node-set-branches!"
;;   (uim '(define test-node (trec-node-new "a" '("A"))))
;;   (assert-equal ()
;;		 (uim '(trec-node-branches test-node)))
;;   ;; insert a node
;;   (uim '(trec-node-set-branches! test-node (list (trec-node-new))))
;;   (assert-equal (uim ''((#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . ((#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; insert another node
;;   (uim '(trec-node-set-branches! test-node (list (trec-node-new)
;;						  (trec-node-new "b" '("B")))))
;;   (assert-equal (uim ''((#f #f . ())
;;			 ("b" ("B") . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . ((#f #f . ())
;;				      ("b" ("B") . ()))))
;;		 (uim 'test-node)))
;;
;;  ("test trec-node-terminal?"
;;   (assert-true  (uim-bool '(trec-node-terminal? (trec-node-new))))
;;
;;   (uim '(define test-node (trec-node-new "a" '("A"))))
;;   (assert-true  (uim-bool '(trec-node-terminal? test-node)))
;;   (uim '(trec-node-set-branches! test-node (list (trec-node-new))))
;;   (assert-false (uim-bool '(trec-node-terminal? test-node))))
;;
;;  ("test test-node-key-match?"
;;   (uim '(define test-node (trec-node-new "a" '("A"))))
;;   (assert-true  (uim-bool '(trec-node-key-match? test-node string=? "a")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "b")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "A")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "B")))
;;
;;   (uim '(define test-node (trec-node-new)))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "a")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "b")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "A")))
;;   (assert-false (uim-bool '(trec-node-key-match? test-node string=? "B"))))
;;
;;  ("test trec-node-insert-branch!"
;;   (uim '(define test-node (trec-node-new "a" '("A"))))
;;   (assert-equal ()
;;		 (uim '(trec-node-branches test-node)))
;;   ;; insert a node
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new)))
;;   (assert-equal (uim ''((#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . ((#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; insert another node
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new "b" '("B"))))
;;   (assert-equal (uim ''(("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . (("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; insert more another node
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new "c" '("C"))))
;;   (assert-equal (uim ''(("c" ("C") . ())
;;			 ("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . (("c" ("C") . ())
;;				      ("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; there is no overwrite check
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new "c" '("C"))))
;;   (assert-equal (uim ''(("c" ("C") . ())
;;			 ("c" ("C") . ())
;;			 ("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . (("c" ("C") . ())
;;				      ("c" ("C") . ())
;;				      ("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; there is no overwrite check
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new "b" '("B"))))
;;   (assert-equal (uim ''(("b" ("B") . ())
;;			 ("c" ("C") . ())
;;			 ("c" ("C") . ())
;;			 ("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . (("b" ("B") . ())
;;				      ("c" ("C") . ())
;;				      ("c" ("C") . ())
;;				      ("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; contents of target node affects nothing about branches
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new "a" '("A"))))
;;   (assert-equal (uim ''(("a" ("A") . ())
;;			 ("b" ("B") . ())
;;			 ("c" ("C") . ())
;;			 ("c" ("C") . ())
;;			 ("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . (("a" ("A") . ())
;;				      ("b" ("B") . ())
;;				      ("c" ("C") . ())
;;				      ("c" ("C") . ())
;;				      ("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node))
;;   ;; there is no treatment about branch content
;;   (uim '(trec-node-insert-branch! test-node (trec-node-new)))
;;   (assert-equal (uim ''((#f #f . ())
;;			 ("a" ("A") . ())
;;			 ("b" ("B") . ())
;;			 ("c" ("C") . ())
;;			 ("c" ("C") . ())
;;			 ("b" ("B") . ())
;;			 (#f #f . ())))
;;		 (uim '(trec-node-branches test-node)))
;;   (assert-equal (uim ''("a" ("A") . ((#f #f . ())
;;				      ("a" ("A") . ())
;;				      ("b" ("B") . ())
;;				      ("c" ("C") . ())
;;				      ("c" ("C") . ())
;;				      ("b" ("B") . ())
;;				      (#f #f . ()))))
;;		 (uim 'test-node)))
;;
;;  ("test trec-node-find-subtree"
;;   (uim '(define test-root (trec-node-new "k" '("K"))))
;;   (uim '(define test-node-kk (trec-node-new "k" '("KK"))))
;;   (uim '(define test-node-ky (trec-node-new "y" '("KY"))))
;;   (uim '(define test-node-kky (trec-node-new "y" '("KKY"))))
;;   (uim '(define test-node-kz (trec-node-new "z" '("KZ"))))
;;   (uim '(trec-node-insert-branch! test-root (trec-node-new "a" '("KA"))))
;;   (uim '(trec-node-insert-branch! test-root (trec-node-new "i" '("KI"))))
;;   (uim '(trec-node-insert-branch! test-root (trec-node-new "u" '("KU"))))
;;   (uim '(trec-node-insert-branch! test-node-kk (trec-node-new "a" '("KKA"))))
;;   (uim '(trec-node-insert-branch! test-node-kk (trec-node-new "i" '("KKI"))))
;;   (uim '(trec-node-insert-branch! test-node-kk (trec-node-new "u" '("KKU"))))
;;   (uim '(trec-node-insert-branch! test-node-kky (trec-node-new "a" '("KKYA"))))
;;   (uim '(trec-node-insert-branch! test-node-kky (trec-node-new "i" '("KKYI"))))
;;   (uim '(trec-node-insert-branch! test-node-kky (trec-node-new "u" '("KKYU"))))
;;   (uim '(trec-node-insert-branch! test-node-ky (trec-node-new "a" '("KYA"))))
;;   (uim '(trec-node-insert-branch! test-node-ky (trec-node-new "i" '("KYI"))))
;;   (uim '(trec-node-insert-branch! test-node-ky (trec-node-new "u" '("KYU"))))
;;   (uim '(trec-node-insert-branch! test-node-kz (trec-node-new "a" '("KZA"))))
;;   (uim '(trec-node-insert-branch! test-node-kz (trec-node-new "i" '("KZI"))))
;;   (uim '(trec-node-insert-branch! test-node-kz (trec-node-new "u" '("KZU"))))
;;   ;; direct children
;;   (assert-equal (uim ''("k" ("K") . (("u" ("KU") . ())
;;				      ("i" ("KI") . ())
;;				      ("a" ("KA") . ()))))
;;		 (uim 'test-root))
;;   (assert-equal '(("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "u")))
;;   (assert-equal '(("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "i")))
;;   (assert-equal '(("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "a")))
;;   (assert-false (uim-bool '(trec-node-find-subtree test-root string=? "z")))
;;   ;; with descendants
;;   (uim '(trec-node-insert-branch! test-root test-node-kk))
;;   (uim '(trec-node-insert-branch! test-root test-node-ky))
;;   (uim '(trec-node-insert-branch! test-root test-node-kz))
;;   (assert-equal (uim ''("k" ("K") . (("z" ("KZ") . (("u" ("KZU") . ())
;;						     ("i" ("KZI") . ())
;;						     ("a" ("KZA") . ())))
;;				      ("y" ("KY") . (("u" ("KYU") . ())
;;						     ("i" ("KYI") . ())
;;						     ("a" ("KYA") . ())))
;;				      ("k" ("KK") . (("u" ("KKU") . ())
;;						     ("i" ("KKI") . ())
;;						     ("a" ("KKA") . ())))
;;				      ("u" ("KU") . ())
;;				      ("i" ("KI") . ())
;;				      ("a" ("KA") . ()))))
;;		 (uim 'test-root))
;;   (assert-equal '(("z" ("KZ") . (("u" ("KZU") . ())
;;				  ("i" ("KZI") . ())
;;				  ("a" ("KZA") . ())))
;;		   ("y" ("KY") . (("u" ("KYU") . ())
;;				  ("i" ("KYI") . ())
;;				  ("a" ("KYA") . ())))
;;		   ("k" ("KK") . (("u" ("KKU") . ())
;;				  ("i" ("KKI") . ())
;;				  ("a" ("KKA") . ())))
;;		   ("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "z")))
;;   (assert-equal '(("y" ("KY") . (("u" ("KYU") . ())
;;				  ("i" ("KYI") . ())
;;				  ("a" ("KYA") . ())))
;;		   ("k" ("KK") . (("u" ("KKU") . ())
;;				  ("i" ("KKI") . ())
;;				  ("a" ("KKA") . ())))
;;		   ("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "y")))
;;   (assert-equal '(("k" ("KK") . (("u" ("KKU") . ())
;;				  ("i" ("KKI") . ())
;;				  ("a" ("KKA") . ())))
;;		   ("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "k")))
;;   (assert-equal '(("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "u")))
;;   (assert-equal '(("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "i")))
;;   (assert-equal '(("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "a")))
;;   (assert-false (uim-bool '(trec-node-find-subtree test-root string=? "x")))
;;   ;; with more subordinate descendants
;;   (uim '(trec-node-insert-branch! test-node-kk test-node-kky))
;;   (assert-equal (uim ''("k" ("K") . (("z" ("KZ") . (("u" ("KZU") . ())
;;						     ("i" ("KZI") . ())
;;						     ("a" ("KZA") . ())))
;;				      ("y" ("KY") . (("u" ("KYU") . ())
;;						     ("i" ("KYI") . ())
;;						     ("a" ("KYA") . ())))
;;				      ("k" ("KK") . (("y" ("KKY") . (("u" ("KKYU") . ())
;;								     ("i" ("KKYI") . ())
;;								     ("a" ("KKYA") . ())))
;;						     ("u" ("KKU") . ())
;;						     ("i" ("KKI") . ())
;;						     ("a" ("KKA") . ())))
;;				      ("u" ("KU") . ())
;;				      ("i" ("KI") . ())
;;				      ("a" ("KA") . ()))))
;;		 (uim 'test-root))
;;   (assert-equal '(("z" ("KZ") . (("u" ("KZU") . ())
;;				  ("i" ("KZI") . ())
;;				  ("a" ("KZA") . ())))
;;		   ("y" ("KY") . (("u" ("KYU") . ())
;;				  ("i" ("KYI") . ())
;;				  ("a" ("KYA") . ())))
;;		   ("k" ("KK") . (("y" ("KKY") . (("u" ("KKYU") . ())
;;						  ("i" ("KKYI") . ())
;;						  ("a" ("KKYA") . ())))
;;				  ("u" ("KKU") . ())
;;				  ("i" ("KKI") . ())
;;				  ("a" ("KKA") . ())))
;;		   ("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "z")))
;;   (assert-equal '(("k" ("KK") . (("y" ("KKY") . (("u" ("KKYU") . ())
;;						  ("i" ("KKYI") . ())
;;						  ("a" ("KKYA") . ())))
;;				  ("u" ("KKU") . ())
;;				  ("i" ("KKI") . ())
;;				  ("a" ("KKA") . ())))
;;		   ("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "k")))
;;   (assert-equal '(("u" ("KU") . ())
;;		   ("i" ("KI") . ())
;;		   ("a" ("KA") . ()))
;;		 (uim '(trec-node-find-subtree test-root string=? "u"))))
;;
;;  ("test trec-node-merge-rule!"
;;   ;; root
;;   (uim '(define test-root (trec-node-new)))
;;   (assert-equal (uim ''(#f #f . ()))
;;		 (uim 'test-root))
;;   ;; make a node by a rule
;;   (uim '(trec-node-merge-rule! test-root string=? '(("a") ("A"))))
;;   (assert-equal (uim ''(#f #f . (("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make another node by a rule
;;   (uim '(trec-node-merge-rule! test-root string=? '(("i") ("I"))))
;;   (assert-equal (uim ''(#f #f . (("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make node k
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k") ("K"))))
;;   (assert-equal (uim ''(#f #f . (("k" ("K") . ())
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make subnode ka
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k" "a") ("KA"))))
;;   (assert-equal (uim ''(#f #f . (("k" ("K") . (("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make node u
;;   (uim '(trec-node-merge-rule! test-root string=? '(("u") ("U"))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make subnode kkya without subnode kk and kky
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k" "k" "y" "a") ("KKYA"))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" #f . (("y" #f . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; make subnode kka
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k" "k" "a") ("KKA"))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" #f . (("a" ("KKA") . ())
;;							   ("y" #f . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; complement subnode kky
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k" "k" "y") ("KKY"))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" #f . (("a" ("KKA") . ())
;;							   ("y" ("KKY") . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; complement subnode kk
;;   (uim '(trec-node-merge-rule! test-root string=? '(("k" "k") ("KK"))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" ("KK") . (("a" ("KKA") . ())
;;							       ("y" ("KKY") . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root)))
;;
;;  ("test trec-node-merge-ruleset!"
;;   ;; initial
;;   (uim '(define test-root (trec-node-new)))
;;   (assert-equal (uim ''(#f #f . ()))
;;		 (uim 'test-root))
;;   ;; parse a ruleset
;;   (uim '(trec-node-merge-ruleset! test-root
;;				   string=?
;;				   '((("a")             ("A"))
;;				     (("i")             ("I"))
;;				     (("k")             ("K"))
;;				     (("k" "a")         ("KA"))
;;				     (("u")             ("U"))
;;				     (("k" "k" "y" "a") ("KKYA")))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" #f . (("y" #f . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root))
;;   ;; incrementally parse additional ruleset
;;   (uim '(trec-node-merge-ruleset! test-root
;;				   string=?
;;				   '((("k" "k" "a")     ("KKA"))
;;				     (("k" "k" "y")     ("KKY"))
;;				     (("k" "k")         ("KK")))))
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" ("KK") . (("a" ("KKA") . ())
;;							       ("y" ("KKY") . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim 'test-root)))
;;
;;  ("test trec-parse-ruleset"
;;   ;; null ruleset
;;   (assert-equal (uim ''(#f #f . ()))
;;		 (uim '(trec-parse-ruleset string=? ())))
;;   ;; a ruleset contains single rule
;;   (assert-equal (uim ''(#f #f . (("a" ("A") . ()))))
;;		 (uim '(trec-parse-ruleset string=? '((("a") ("A"))))))
;;   ;; a full ruleset
;;   (assert-equal (uim ''(#f #f . (("u" ("U") . ())
;;				  ("k" ("K") . (("k" ("KK") . (("a" ("KKA") . ())
;;							       ("y" ("KKY") . (("a" ("KKYA") . ())))))
;;						("a" ("KA") . ())))
;;				  ("i" ("I") . ())
;;				  ("a" ("A") . ()))))
;;		 (uim '(trec-parse-ruleset string=?
;;					   '((("a")             ("A"))
;;					     (("i")             ("I"))
;;					     (("k")             ("K"))
;;					     (("k" "a")         ("KA"))
;;					     (("u")             ("U"))
;;					     (("k" "k" "y" "a") ("KKYA"))
;;					     (("k" "k" "a")     ("KKA"))
;;					     (("k" "k" "y")     ("KKY"))
;;					     (("k" "k")         ("KK"))))))))

(define-uim-test-case "testcase trec-context"
  (setup
   (lambda ()
     (uim
      '(begin
	 (require "trec.scm")
	 (define string-ci=?
	   (lambda (a b)
	     (= (char-downcase (string->char a))
		(char-downcase (string->char b)))))

	 (define test-full-ruleset '((("a")             . ("A"))
				     (("i")             . ("I"))
				     (("k")             . ("K"))
				     (("k" "a")         . ("KA"))
				     (("u")             . ("U"))
				     (("k" "k" "y" "a") . ("KKYA"))
				     (("k" "k" "a")     . ("KKA"))
				     (("k" "k" "y")     . ("KKY"))
				     (("k" "k")         . ("KK"))))
	 (define test-romaji-basic-ruleset
	   '((("a")             . ("あ"))
	     (("i")             . ("い"))
	     (("u")             . ("う"))
	     (("e")             . ("え"))
	     (("o")             . ("お"))

	     (("k" "a")         . ("か"))
	     (("k" "i")         . ("き"))
	     (("k" "u")         . ("く"))
	     (("k" "e")         . ("け"))
	     (("k" "o")         . ("こ"))

	     (("k" "k")         . ("っ" "k"))
	     (("k" "k" "a")     . ("っ" "か"))
	     (("k" "k" "i")     . ("っ" "き"))
	     (("k" "k" "u")     . ("っ" "く"))
	     (("k" "k" "e")     . ("っ" "け"))
	     (("k" "k" "o")     . ("っ" "こ"))

	     (("k" "y" "a")     . ("き" "ゃ"))
	     (("k" "y" "i")     . ("き" "ぃ"))
	     (("k" "y" "u")     . ("き" "ゅ"))
	     (("k" "y" "e")     . ("き" "ぇ"))
	     (("k" "y" "o")     . ("き" "ょ"))

	     ;;(("k" "k" "y")     . ("っ" "k" "y"))
	     (("k" "k" "y" "a") . ("っ" "き" "ゃ"))
	     (("k" "k" "y" "i") . ("っ" "き" "ぃ"))
	     (("k" "k" "y" "u") . ("っ" "き" "ゅ"))
	     (("k" "k" "y" "e") . ("っ" "き" "ぇ"))
	     (("k" "k" "y" "o") . ("っ" "き" "ょ"))

	     ;; another solutions
	     (("A")             . ("A"))
	     (("K" "K" "U")     . ("ッ" "ク"))
	     (("k" "k" "U")     . ("っ" "ク"))
	     ))
	 (define test-romaji-basic-root
	   (trec-route-new (trec-parse-ruleset string=?
					       test-romaji-basic-ruleset)))

	 (define test-null-ruletree (trec-parse-ruleset string=? ()))
	 (define test-single-ruletree (trec-parse-ruleset string=?
							  '((("a") . ("A")))))
	 (define test-full-ruletree (trec-parse-ruleset string=?
							test-full-ruleset))
	 (define test-romaji-basic-ruletree
	   (trec-parse-ruleset string=? test-romaji-basic-ruleset))

	 (define test-null-root (trec-route-new test-null-ruletree))
	 (define test-single-root (trec-route-new test-single-ruletree))
	 (define test-full-root (trec-route-new test-full-ruletree))
	 (define test-romaji-basic-root
	   (trec-route-new test-romaji-basic-ruletree))
	 ))))

  ("test trec-context-new"
   (assert-error (lambda ()
		   (uim '(trec-context-new))))
   (assert-equal (uim ''((((#f #f . ())))))
		 (uim '(trec-context-new test-null-root)))
   (assert-equal (uim ''((((#f #f . (("a" ("A") . ())))))))
		 (uim '(trec-context-new test-single-root)))
   (assert-equal (uim ''((((#f #f . (("u" ("U") . ())
				     ("k" ("K") . (("k" ("KK") . (("a" ("KKA") . ())
								  ("y" ("KKY") . (("a" ("KKYA") . ())))))
						   ("a" ("KA") . ())))
				     ("i" ("I") . ())
				     ("a" ("A") . ())))))))
		 (uim '(trec-context-new test-full-root))))

  ("test trec-context-reset!"
   (uim '(define test-tc (trec-context-new test-full-root)))
   ;; input
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "y")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "a")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-true  (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal (uim ''("a" "y" "k" "k" #f))
		 (uim '(map (compose trec-node-key car)
			    (trec-context-route test-tc))))
   (assert-equal '("KKYA")
		 (uim '(trec-context-value test-tc)))
   ;; flush
   (uim '(trec-context-reset! test-tc))
   (assert-true  (uim-bool '(trec-context-initial? test-tc)))
   (assert-false (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal (uim ''((((#f #f . (("u" ("U") . ())
				     ("k" ("K") . (("k" ("KK") . (("a" ("KKA") . ())
								  ("y" ("KKY") . (("a" ("KKYA") . ())))))
						   ("a" ("KA") . ())))
				     ("i" ("I") . ())
				     ("a" ("A") . ())))))))
		 (uim '(trec-context-new test-full-root))))

  ("test trec-context-advance!"
   (uim '(define test-tc (trec-context-new test-romaji-basic-root)))
   (assert-true  (uim-bool '(trec-context-initial? test-tc)))
   (assert-false (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal "z"
		 (uim '(trec-context-advance! test-tc string=? "z")))
   ;; input "k"
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-false (uim-bool '(trec-context-goal? test-tc)))
   (assert-false (uim-bool '(trec-context-value test-tc)))
   ;; invalid input
   (assert-equal "z"
		 (uim '(trec-context-advance! test-tc string=? "z")))
   ;; input "a"
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "a")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-true  (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal '("か")
		 (uim '(trec-context-value test-tc)))
   ;; invalid input
   (assert-equal "a"
		 (uim '(trec-context-advance! test-tc string=? "a")))
   ;; long input sequence
   (uim '(define test-tc (trec-context-new test-romaji-basic-root)))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "y")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "a")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-true  (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal '("っ" "き" "ゃ")
		 (uim '(trec-context-value test-tc))))

  ("test trec-context-backtrack!"
   (uim '(define test-tc (trec-context-new test-romaji-basic-root)))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "y")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "a")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-true  (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal (uim ''("a" "y" "k" "k" #f))
		 (uim '(map (compose trec-node-key car)
			    (trec-context-route test-tc))))
   (assert-equal '("っ" "き" "ゃ")
		 (uim '(trec-context-value test-tc)))
   ;; undo to '("k" "k" "y")
   (assert-equal "a"
		 (uim '(trec-context-backtrack! test-tc)))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-false (uim-bool '(trec-context-goal? test-tc)))
   (assert-false (uim-bool '(trec-context-value test-tc)))
   ;; undo to '("k" "k")
   (assert-equal "y"
		 (uim '(trec-context-backtrack! test-tc)))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-false (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal '("っ" "k")
		 (uim '(trec-context-value test-tc)))
   ;; input into '("k" "k" "a")
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "a")))
   (assert-false (uim-bool '(trec-context-initial? test-tc)))
   (assert-true  (uim-bool '(trec-context-goal? test-tc)))
   (assert-equal '("っ" "か")
		 (uim '(trec-context-value test-tc))))

  ("test trec-context-reroute!"
   (uim '(define test-tc (trec-context-new test-romaji-basic-root)))
   ;; kk
   (assert-false (uim-bool '(trec-context-advance! test-tc string-ci=? "k")))
   (assert-false (uim-bool '(trec-context-advance! test-tc string-ci=? "k")))
   (assert-equal '("K" "K")
		 (uim '(trec-context-keys test-tc)))
   (assert-false (uim-bool '(trec-context-value test-tc)))
;;   (assert-equal '("っ" "k")
;;		 (uim '(trec-context-value test-tc)))
   ;; kku
   (assert-false (uim-bool '(trec-context-advance! test-tc string-ci=? "u")))
   (assert-equal '("K" "K" "U")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("ッ" "ク")
		 (uim '(trec-context-value test-tc)))
   ;; kku (2nd route)
   (assert-false (uim-bool '(trec-context-reroute! test-tc string-ci=? ())))
   (assert-equal '("k" "k" "U")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("っ" "ク")
		 (uim '(trec-context-value test-tc)))
   ;; kku (3rd route)
   (assert-false (uim-bool '(trec-context-reroute! test-tc string-ci=? ())))
   (assert-equal '("k" "k" "u")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("っ" "く")
		 (uim '(trec-context-value test-tc)))
   ;; kku (4th route)
   (assert-equal '("k" "k" "u")
		 (uim '(trec-context-reroute! test-tc string-ci=? ())))
   (assert-equal '("k" "k" "u")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("っ" "く")
		 (uim '(trec-context-value test-tc)))))

(define-uim-test-case "testcase trec utilities"
  (setup
   (lambda ()
     (uim
      '(begin
	 (require "trec.scm")
	 ;; This hepburn style romaji "oh" composition ruleset is only
	 ;; prepared to test the
	 ;; trec-route-split-longest-tail. Production composer will
	 ;; use another ruleset.
	 (define test-tail-ruleset '((("o")         . ("お"))
				     (("o" "h")     . ("おお"))
				     (("h" "o")     . ("ほ"))
				     (("h" "h" "o") . ("っほ"))
				     (("t" "a")     . ("た"))))
	 (define test-tail-root
	   (trec-route-new (trec-parse-ruleset string=? test-tail-ruleset)))
	 ))))

  ("test trec-context-new"
   (uim '(define test-tc (trec-context-new test-tail-root)))
   ;; o
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "o")))
   (assert-equal '("o")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("お")
		 (uim '(trec-context-value test-tc)))
   (assert-false (uim-bool '(trec-route-split-longest-tail
			     (trec-context-route test-tc) string=? ())))
   ;; oh
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "h")))
   (assert-equal '("o" "h")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("おお")
		 (uim '(trec-context-value test-tc)))
   (assert-false (uim-bool '(trec-route-split-longest-tail
			     (trec-context-route test-tc) string=? ())))
   ;; oho
   (assert-equal "o"
		 (uim '(trec-context-advance! test-tc string=? "o")))
   (assert-equal '("o" "h")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("おお")
		 (uim '(trec-context-value test-tc)))
   (assert-equal '("o")
		 (uim '(trec-route-keys
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("o"))))))
   (assert-equal '("お")
		 (uim '(trec-route-value
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("o"))))))
   (assert-equal '("h" "o")
		 (uim '(trec-route-keys
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("o"))))))
   (assert-equal '("ほ")
		 (uim '(trec-route-value
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("o"))))))
   ;; ohho
   (assert-equal "h"
		 (uim '(trec-context-advance! test-tc string=? "h")))
   (assert-equal '("o" "h")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("おお")
		 (uim '(trec-context-value test-tc)))
   (assert-equal '("o")
		 (uim '(trec-route-keys
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("h" "o"))))))
   (assert-equal '("お")
		 (uim '(trec-route-value
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("h" "o"))))))
   (assert-equal '("h" "h" "o")
		 (uim '(trec-route-keys
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("h" "o"))))))
   (assert-equal '("っほ")
		 (uim '(trec-route-value
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("h" "o"))))))
   ;; oht
   (assert-equal "t"
		 (uim '(trec-context-advance! test-tc string=? "t")))
   (assert-equal '("o" "h")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("おお")
		 (uim '(trec-context-value test-tc)))
   (assert-false (uim-bool '(trec-route-split-longest-tail
			     (trec-context-route test-tc) string=? '("t"))))
   ;; ohta
   (assert-equal "t"
		 (uim '(trec-context-advance! test-tc string=? "t")))
   (assert-equal '("o" "h")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("おお")
		 (uim '(trec-context-value test-tc)))
   (assert-equal '("o" "h")
		 (uim '(trec-route-keys
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("おお")
		 (uim '(trec-route-value
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("t" "a")
		 (uim '(trec-route-keys
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("た")
		 (uim '(trec-route-value
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   ;; ota
   (uim '(define test-tc (trec-context-new test-tail-root)))
   (assert-false (uim-bool '(trec-context-advance! test-tc string=? "o")))
   (assert-equal "t"
		 (uim '(trec-context-advance! test-tc string=? "t")))
   (assert-equal '("o")
		 (uim '(trec-context-keys test-tc)))
   (assert-equal '("お")
		 (uim '(trec-context-value test-tc)))
   (assert-equal '("o")
		 (uim '(trec-route-keys
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("お")
		 (uim '(trec-route-value
			(car (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("t" "a")
		 (uim '(trec-route-keys
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a"))))))
   (assert-equal '("た")
		 (uim '(trec-route-value
			(cdr (trec-route-split-longest-tail
			      (trec-context-route test-tc) string=? '("t" "a")))))))
  )
