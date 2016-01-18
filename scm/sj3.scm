;;; sj3.scm: SJ3 for uim.
;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

(require-extension (srfi 1 2 6 23 34 48))

(require "ustr.scm")
(require "japanese.scm")
(require "generic-predict.scm")
(require "sj3v2-socket.scm")
(require-custom "generic-key-custom.scm")
(require-custom "sj3-custom.scm")
(require-custom "sj3-key-custom.scm")


;;; implementations

;;
;; canna emulating functions
;;
(define (sj3-lib-init server)
  (if sj3-use-remote-server?
      (sj3-lib-open server sj3-user)
      (sj3-lib-open "" sj3-user)))

;; error recovery
(define (sj3-connect-wait sc w)
  (let ((stime (time)))
    (let loop ((now (time)))
      (let ((diff (- w (string->number (difftime now stime)))))
        (if (< diff 0)
            #t
            (begin
              (im-clear-preedit sc)
              (im-pushback-preedit
               sc preedit-reverse
               (format #f (N_ "[Please wait ~asec...]") diff))
              (im-update-preedit sc)
              (sleep 1)
              (loop (time))))))))
(define (sj3-connect-retry sc)
  (set! sj3-init-lib-ok? #f)
  (sj3-cancel-conv sc)
  (let loop ((fib1 1) (fib2 1))
    (uim-notify-info (N_ "Reconnecting to sj3 server."))
    (guard (err (else
                 (uim-notify-info (N_ "Reconnecting to sj3 server."))
                 (im-clear-preedit sc)
                 (im-pushback-preedit
                  sc preedit-reverse
                  (N_ "[Reconnecting...]"))
                 (im-update-preedit sc)
                 (sleep 1)
                 (sj3-connect-wait sc fib1)
                 (loop fib2 (+ fib1 fib2))))
           (sj3-lib-init sj3-server-name))
    (set! sj3-init-lib-ok? #t)))
(define (sj3-lib-funcall sc f . args)
  (or
   (guard (err (else #f))
          (apply f args))
   (begin
     (if sj3-init-lib-ok?
         (guard (close-err
                 (else
                  #f))
                (sj3-lib-close)))
     (sj3-connect-retry sc)
     (apply f args))))

(define (sj3-lib-alloc-context)
  #t)
(define (sj3-make-map-from-kana-string str)
  (map (lambda (x) (apply string-append x))
       (apply zip (map (lambda (c)
                         (ja-find-kana-list-from-rule ja-rk-rule-basic c))
                       (reverse (string-to-list str))))))
(define (sj3-getdouon sc str)
  (let ((douon (sj3-lib-funcall sc sj3-lib-getdouon str))
        (kana-list (sj3-make-map-from-kana-string str)))
    (append douon
            (map list kana-list))))
(define (sj3-get-nth-yomi sc nth)
  (let ((sc-ctx (sj3-context-sc-ctx sc)))
    (car (list-ref sc-ctx nth))))
(define (sj3-lib-get-nth-candidate-without-muhenkan sc seg nth)
  (let* ((yomi (sj3-get-nth-yomi sc seg))
         (cnt (sj3-lib-funcall sc sj3-lib-douoncnt yomi)))
    (if (and (< nth cnt)
             (<= 0 nth))
        ;; henkan
        (cons (sj3-lib-get-nth-candidate sc seg nth) #t)
        ;; muhenkan
        (cons (sj3-lib-get-nth-candidate sc seg nth) #f))))
(define (sj3-lib-get-nth-candidate sc seg nth)
  (let* ((yomi (sj3-get-nth-yomi sc seg))
         (cnt (sj3-lib-funcall sc sj3-lib-douoncnt yomi)))
    (cond ((< nth 0)
           (list-ref (sj3-make-map-from-kana-string yomi) 0))
          ((< nth cnt)
           (car (sj3-lib-funcall sc sj3-lib-get-nth-douon yomi nth)))
          (else
           (list-ref (sj3-make-map-from-kana-string yomi) (- nth cnt))))))
(define (sj3-lib-release-context sc)
  #t)
(define (sj3-lib-get-unconv-candidate sc seg-idx)
  (sj3-get-nth-yomi sc seg-idx))
(define (sj3-lib-get-nr-segments sc)
  (let ((sc-ctx (sj3-context-sc-ctx sc)))
    (length sc-ctx)))
(define (sj3-get-nr-douon sc str)
  (+ (sj3-lib-funcall sc sj3-lib-douoncnt str)
     (length (sj3-make-map-from-kana-string str))))
(define (sj3-lib-get-nr-candidates sc seg)
  (sj3-get-nr-douon sc (sj3-get-nth-yomi sc seg)))
(define (sj3-lib-resize-segment sc seg cnt)
  (let* ((sc-ctx (sj3-context-sc-ctx sc))
         (kana-str (sj3-get-nth-yomi sc seg))
         (kana-list (reverse (string-to-list kana-str))))
    (cond ((and (< cnt 0) ;; shrink segment
                (< 1 (length kana-list)))
           (let* ((not-edited-head (if (< 0 seg)
                                       (take sc-ctx seg)
                                       '()))
                  (edited-head (list (list (apply string-append (drop-right kana-list (* -1 cnt))))))
                  (edited-tail (if (= (+ 1 seg) (length sc-ctx)) ;; end of segments
                                   (list (take-right kana-list (* -1 cnt)))
                                   (let* ((next-char (car (take-right kana-list (* -1 cnt))))
                                          (kana-next-str (sj3-get-nth-yomi sc (+ 1 seg))))
                                     (list (list (string-append next-char kana-next-str))))))
                  (not-edited-tail (if (= (+ 1 seg) (length sc-ctx))
                                       '()
                                       (drop sc-ctx (+ seg 2)))))
             (sj3-context-set-sc-ctx!
              sc
              (append not-edited-head edited-head edited-tail not-edited-tail)))
           #t)
          ((and (< 0 cnt) ;; stretch segment
                (< (+ seg 1) (length sc-ctx))
                (< 0 (length (string-to-list (sj3-get-nth-yomi sc (+ seg 1))))))
           (let* ((next-str (sj3-get-nth-yomi sc (+ seg 1)))
                  (next-kana-list (reverse (string-to-list next-str)))
                  (not-edited-head (if (< 0 seg)
                                       (take sc-ctx seg)
                                       '()))
                  (edited-head (list (list (apply string-append
                                                  (append kana-list
                                                          (take next-kana-list cnt))))))
                  (edited-tail (if (= 1 (length next-kana-list))
                                   '()
                                   (list (list (apply string-append (drop next-kana-list cnt))))))
                  (not-edited-tail (if (< (length sc-ctx) 2)
                                       '()
                                       (drop sc-ctx (+ 2 seg)))))
             (sj3-context-set-sc-ctx!
              sc
              (append not-edited-head edited-head edited-tail not-edited-tail)))
           #t)
          (else
           #t))))
(define (sj3-lib-begin-conversion sc str)
  (let ((ret (sj3-lib-funcall sc sj3-lib-getkan str)))
    (cond ((list? ret)
           (sj3-context-set-sc-ctx! sc (cdr ret))
           (- (length ret) 1))
          (else
           (sj3-context-set-sc-ctx! sc (list (sj3-make-map-from-kana-string str))) ; XXX hack
           1))))
(define (sj3-lib-commit-segment sc seg delta)
  ;; segment learnining
  (if (sj3-lib-opened?)
      (let* ((yomi (sj3-get-nth-yomi sc seg))
             (douon (sj3-getdouon sc yomi)))
        (if (< delta (length douon))
            (let ((entry (list-ref douon delta)))
              (if (= 2 (length entry))
                  (begin
                    (predict-meta-commit (sj3-context-prediction-ctx sc) yomi (list-ref entry 0) "")
                    (sj3-lib-funcall sc sj3-lib-gakusyuu (list-ref entry 1)))))))
      #f))

;; return alist of cons'ed offset and length
;; ("abc" "d" "efgh") => ((3 . 3) (4 . 1) (8 . 4))
(define (sj3-get-seg-offset l)
  (let loop ((l l)
             (offset 0)
             (rest '()))
    (if (null? l)
        (reverse rest)
        (let ((len (string-length (car l))))
          (loop (cdr l) (+ offset len) (cons (cons (+ offset len) len) rest))))))
(define (sj3-find-index pred l)
  (let loop ((l l) (ref 0))
    (cond ((null? l) #f)
          ((pred (car l)) ref)
          (else (loop (cdr l) (+ 1 ref))))))
(define (sj3-filter-split-segment l1 l2)
  (let loop ((l1 l1)
             (rest '()))
    (if (null? l1)
        (reverse rest)
        (let ((idx (sj3-find-index (lambda (x) (= (caar l1) (car x))) l2)))
          (if (and (number? idx)
                   (< 0 idx)
                   (= (cdar l1)
                      (+ (cdr (list-ref l2 (- idx 1)))
                         (cdr (list-ref l2 idx)))))
              (loop (cdr l1) (cons (- idx 1) rest))
              (loop (cdr l1) rest))))))
(define (sj3-filter-merge-segment l1 l2)
  (let loop ((l1 l1)
             (last 0)
             (rest '()))
    (if (null? l1)
        (reverse rest)
        (let ((idx (sj3-find-index (lambda (x) (= (caar l1) (car x))) l2)))
          (if (and (number? idx)
                   (<= 0 idx)
                   (= (+ last (cdar l1))
                      (cdr (list-ref l2 idx))))
              (loop (cdr l1) (cdar l1) (cons idx rest))
              (loop (cdr l1) (cdar l1) rest))))))
(define (sj3-filter-move-segment l1 l2)
  (let loop ((l1 l1)
             (last 0)
             (rest '()))
    (if (null? l1)
        (reverse rest)
        (let ((idx (sj3-find-index (lambda (x) (= (caar l1) (car x))) l2)))
          (if (and (number? idx)
                   (< 0 idx)
                   (not (= last (cdr (list-ref l2 (- idx 1)))))
                   (= (+ (cdar l1)
                         last)
                      (+ (cdr (list-ref l2 (- idx 1)))
                         (cdr (list-ref l2 idx)))))
              (loop (cdr l1) (cdar l1) (cons (- idx 1) rest))
              (loop (cdr l1) (cdar l1) rest))))))

;;
;; XXX: call this function _after_ sj3-lib-commit-segment
;;
(define (sj3-lib-commit-segments sc segs)
  (define (restore-segments)
    (map (lambda (idx seg)
           (if (cdr seg)
               (cons (sj3-get-nth-yomi sc idx)
                     (cdr (sj3-lib-funcall sc sj3-lib-get-nth-douon
                                           (sj3-get-nth-yomi sc idx)
                                           ;; gakusyuu1 has been called yet. always 0
                                           0)))
               (list (car seg) #f)))
         (iota (length segs))
         segs))
  ;; segment-length learnining
  (if (sj3-lib-opened?)
      (let* ((sc-ctx (sj3-context-sc-ctx sc))
             ;; revert from hell
             (new-segments (restore-segments))
             (orig (apply string-append (map car sc-ctx)))
             ;; revert too
             (old-string-list (map car (cdr (sj3-lib-funcall sc sj3-lib-getkan orig)))))
        (receive (new-string-list new-dcid-list)
                 (unzip2 new-segments)
          (let ((old-offset (sj3-get-seg-offset old-string-list))
                (new-offset (sj3-get-seg-offset new-string-list)))
            ;; split case
            (for-each (lambda (l)
                        (let ((yomi1 (list-ref new-string-list l))
                              (yomi2 (list-ref new-string-list (+ 1 l)))
                              (dcid  (list-ref new-dcid-list   (+ 1 l))))
                          (if dcid
                              (sj3-lib-gakusyuu2 yomi1 yomi2 dcid))))
                      (sj3-filter-split-segment old-offset new-offset))
            ;; merge case
            (for-each (lambda (l)
                        (let ((yomi1 (list-ref new-string-list l))
                              (yomi2 #f)
                              (dcid  #f))
                          (if dcid
                              (sj3-lib-gakusyuu2 yomi1 yomi2 dcid))))
                      (sj3-filter-merge-segment old-offset new-offset))
            ;; move case
            (for-each (lambda (l)
                        (let ((yomi1 (list-ref new-string-list l))
                              (yomi2 (list-ref new-string-list (+ 1 l)))
                              (dcid  (list-ref new-dcid-list   (+ 1 l))))
                          (if dcid
                              (sj3-lib-gakusyuu2 yomi1 yomi2 dcid))))
                      (sj3-filter-move-segment old-offset new-offset))
            ))))
  #t)

(define (sj3-lib-reset-conversion sc)
  #f)

(define sj3-init-lib-ok? #f)

(define sj3-type-direct	   ja-type-direct)
(define sj3-type-hiragana	   ja-type-hiragana)
(define sj3-type-katakana	   ja-type-katakana)
(define sj3-type-halfkana	   ja-type-halfkana)
(define sj3-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define sj3-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define sj3-input-rule-roma 0)
(define sj3-input-rule-kana 1)
(define sj3-input-rule-azik 2)
(define sj3-input-rule-act 3)
(define sj3-input-rule-kzik 4)

(define sj3-candidate-type-katakana -2)
(define sj3-candidate-type-hiragana -3)
(define sj3-candidate-type-halfkana -4)
(define sj3-candidate-type-halfwidth-alnum -5)
(define sj3-candidate-type-fullwidth-alnum -6)
(define sj3-candidate-type-upper-halfwidth-alnum -7)
(define sj3-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key sj3-space-key? '(" "))

(define sj3-prepare-input-rule-activation
  (lambda (sc)
    (cond
     ((sj3-context-state sc)
      (sj3-do-commit sc))
     ((sj3-context-transposing sc)
      (im-commit sc (sj3-transposing-text sc)))
     ((and
       (sj3-context-on sc)
       (sj3-has-preedit? sc))
      (im-commit
       sc (sj3-make-whole-string sc #t (sj3-context-kana-mode sc)))))
    (sj3-flush sc)
    (sj3-update-preedit sc)))

(define sj3-prepare-input-mode-activation
  (lambda (sc new-mode)
    (let ((old-kana (sj3-context-kana-mode sc)))
      (cond
       ((sj3-context-state sc)
	(sj3-do-commit sc))
       ((sj3-context-transposing sc)
	(im-commit sc (sj3-transposing-text sc))
	(sj3-flush sc))
       ((and
	 (sj3-context-on sc)
	 (sj3-has-preedit? sc)
	 (not (= old-kana new-mode)))
	(im-commit
	 sc (sj3-make-whole-string sc #t (sj3-context-kana-mode sc)))
	(sj3-flush sc)))
      (sj3-update-preedit sc))))

(register-action 'action_sj3_hiragana
		 (lambda (sc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (sc) ;; activity predicate
		   (and (sj3-context-on sc)
		        (not (sj3-context-alnum sc))
			(= (sj3-context-kana-mode sc)
			   sj3-type-hiragana)))

		 (lambda (sc) ;; action handler
		   (sj3-prepare-input-mode-activation sc sj3-type-hiragana)
		   (sj3-context-set-on! sc #t)
		   (sj3-context-set-alnum! sc #f)
		   (sj3-context-change-kana-mode! sc sj3-type-hiragana)))

(register-action 'action_sj3_katakana
		 (lambda (sc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (sc)
		   (and (sj3-context-on sc)
		        (not (sj3-context-alnum sc))
			(= (sj3-context-kana-mode sc)
			   sj3-type-katakana)))
		 (lambda (sc)
		   (sj3-prepare-input-mode-activation sc sj3-type-katakana)
		   (sj3-context-set-on! sc #t)
		   (sj3-context-set-alnum! sc #f)
		   (sj3-context-change-kana-mode! sc sj3-type-katakana)))

(register-action 'action_sj3_halfkana
		 (lambda (sc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (sc)
		   (and (sj3-context-on sc)
			(not (sj3-context-alnum sc))
			(= (sj3-context-kana-mode sc) sj3-type-halfkana)))
		 (lambda (sc)
		   (sj3-prepare-input-mode-activation sc sj3-type-halfkana)
		   (sj3-context-set-on! sc #t)
		   (sj3-context-set-alnum! sc #f)
		   (sj3-context-change-kana-mode! sc sj3-type-halfkana)))

(register-action 'action_sj3_halfwidth_alnum
		 (lambda (sc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (sc) ;; activity predicate
		   (and (sj3-context-on sc)
			(sj3-context-alnum sc)
			(= (sj3-context-alnum-type sc)
			   sj3-type-halfwidth-alnum)))
		 (lambda (sc) ;; action handler
		   (sj3-prepare-input-mode-activation
		    sc (sj3-context-kana-mode sc))
		   (sj3-context-set-on! sc #t)
		   (sj3-context-set-alnum! sc #t)
		   (sj3-context-set-alnum-type!
		    sc sj3-type-halfwidth-alnum)))

(register-action 'action_sj3_direct
		 (lambda (sc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (sc)
		   (not (sj3-context-on sc)))
		 (lambda (sc)
		   (sj3-prepare-input-mode-activation sc sj3-type-direct)
		   (sj3-context-set-on! sc #f)))

(register-action 'action_sj3_fullwidth_alnum
		 (lambda (sc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (sc)
		   (and (sj3-context-on sc)
			(sj3-context-alnum sc)
			(= (sj3-context-alnum-type sc)
			   sj3-type-fullwidth-alnum)))
		 (lambda (sc)
		   (sj3-prepare-input-mode-activation
		    sc (sj3-context-kana-mode sc))
		   (sj3-context-set-on! sc #t)
		   (sj3-context-set-alnum! sc #t)
		   (sj3-context-set-alnum-type!
		    sc sj3-type-fullwidth-alnum)))

(register-action 'action_sj3_roma
		 (lambda (sc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (sc)
		   (= (sj3-context-input-rule sc)
		      sj3-input-rule-roma))
		 (lambda (sc)
		   (sj3-prepare-input-rule-activation sc)
		   (rk-context-set-rule! (sj3-context-rkc sc)
					 ja-rk-rule)
		   (sj3-context-set-input-rule! sc sj3-input-rule-roma)))

(register-action 'action_sj3_kana
		 (lambda (sc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (sc)
		   (= (sj3-context-input-rule sc)
		      sj3-input-rule-kana))
		 (lambda (sc)
		   (sj3-prepare-input-rule-activation sc)
                   (require "japanese-kana.scm")
		   (sj3-context-set-input-rule! sc sj3-input-rule-kana)
                   (sj3-context-change-kana-mode!
                     sc (sj3-context-kana-mode sc))
		   (sj3-context-set-alnum! sc #f)))

(register-action 'action_sj3_azik
		 (lambda (sc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (sc)
		   (= (sj3-context-input-rule sc)
		      sj3-input-rule-azik))
		 (lambda (sc)
		   (sj3-prepare-input-rule-activation sc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (sj3-context-rkc sc)
					 ja-azik-rule)
		   (sj3-context-set-input-rule! sc sj3-input-rule-azik)))

(register-action 'action_sj3_kzik
		 (lambda (sc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (sc)
		   (= (sj3-context-input-rule sc)
		      sj3-input-rule-kzik))
		 (lambda (sc)
		   (sj3-prepare-input-rule-activation sc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (sj3-context-rkc sc)
					 ja-kzik-rule)
		   (sj3-context-set-input-rule! sc sj3-input-rule-kzik)))

(register-action 'action_sj3_act
		 (lambda (sc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (sc)
		   (= (sj3-context-input-rule sc)
		      sj3-input-rule-act))
		 (lambda (sc)
		   (sj3-prepare-input-rule-activation sc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (sj3-context-rkc sc)
					 ja-act-rule)
		   (sj3-context-set-input-rule! sc sj3-input-rule-act)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define sj3-configure-widgets
  (lambda ()
    (register-widget 'widget_sj3_input_mode
		     (activity-indicator-new sj3-input-mode-actions)
		     (actions-new sj3-input-mode-actions))

    (register-widget 'widget_sj3_kana_input_method
		     (activity-indicator-new sj3-kana-input-method-actions)
		     (actions-new sj3-kana-input-method-actions))
    (context-list-replace-widgets! 'sj3 sj3-widgets)))

(define sj3-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'sc-ctx             ()) ;; sj3-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'kana-mode          sj3-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      sj3-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         sj3-input-rule-roma)
    (list 'raw-ustr	      #f)
    (list 'prediction-ctx     '())
    (list 'prediction-word '())
    (list 'prediction-candidates '())
    (list 'prediction-appendix '())
    (list 'prediction-nr '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '()))))
(define-record 'sj3-context sj3-context-rec-spec)
(define sj3-context-new-internal sj3-context-new)

(define (sj3-predict sc str)
  (predict-meta-search
   (sj3-context-prediction-ctx sc)
   str))
(define (sj3-lib-set-prediction-src-string sc str)
  (let* ((ret (sj3-predict sc str))
         (word  (predict-meta-word? ret))
         (cands (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret)))
    (sj3-context-set-prediction-word! sc word)
    (sj3-context-set-prediction-candidates! sc cands)
    (sj3-context-set-prediction-appendix! sc appendix)
    (sj3-context-set-prediction-nr! sc (length cands)))
  #f)
(define (sj3-lib-get-nr-predictions sc)
  (sj3-context-prediction-nr sc))
(define (sj3-lib-get-nth-word sc nth)
  (let ((word (sj3-context-prediction-word sc)))
    (list-ref word nth)))
(define (sj3-lib-get-nth-prediction sc nth)
  (let ((cands (sj3-context-prediction-candidates sc)))
    (list-ref cands nth)))
(define (sj3-lib-get-nth-appendix sc nth)
  (let ((appendix (sj3-context-prediction-candidates sc)))
    (list-ref appendix nth)))
(define (sj3-lib-commit-nth-prediction sc nth)
  (predict-meta-commit
   (sj3-context-prediction-ctx sc)
   (sj3-lib-get-nth-word sc nth)
   (sj3-lib-get-nth-prediction sc nth)
   (sj3-lib-get-nth-appendix sc nth)))

(define (sj3-context-new id im)
  (let ((sc (sj3-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (sj3-context-set-sc-ctx! sc (if sj3-init-lib-ok?
;				      (sj3-lib-alloc-context) ()))
    (sj3-context-set-sc-ctx! sc (sj3-lib-alloc-context))
    (sj3-context-set-widgets! sc sj3-widgets)
    (sj3-context-set-rkc! sc rkc)
    (sj3-context-set-preconv-ustr! sc (ustr-new '()))
    (sj3-context-set-raw-ustr! sc (ustr-new '()))
    (sj3-context-set-segments! sc (ustr-new '()))
    (if sj3-use-prediction?
        (begin
          (sj3-context-set-prediction-ctx! sc (predict-make-meta-search))
          (predict-meta-open (sj3-context-prediction-ctx sc) "sj3")
          (predict-meta-set-external-charset! (sj3-context-prediction-ctx sc) "EUC-JP")))
    sc))

(define (sj3-commit-raw sc)
  (im-commit-raw sc)
  (sj3-context-set-commit-raw! sc #t))

(define (sj3-context-kana-toggle sc)
  (let* ((kana (sj3-context-kana-mode sc))
	 (opposite-kana (ja-opposite-kana kana)))
    (sj3-context-change-kana-mode! sc opposite-kana)))

(define sj3-context-alkana-toggle
  (lambda (sc)
    (let ((alnum-state (sj3-context-alnum sc)))
      (sj3-context-set-alnum! sc (not alnum-state)))))

(define sj3-context-change-kana-mode!
  (lambda (sc kana-mode)
    (if (= (sj3-context-input-rule sc)
           sj3-input-rule-kana)
        (rk-context-set-rule!
	 (sj3-context-rkc sc)
	 (cond
	  ((= kana-mode sj3-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode sj3-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode sj3-type-halfkana) ja-kana-halfkana-rule))))
    (sj3-context-set-kana-mode! sc kana-mode)))

(define sj3-make-whole-string
  (lambda (sc convert-pending-into-kana? kana)
    (let* ((rkc (sj3-context-rkc sc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (sj3-context-input-rule sc))
           (preconv-str (sj3-context-preconv-ustr sc))
           (extract-kana
            (if (= rule sj3-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule sj3-input-rule-kana)
	  (ja-make-kana-str
	   (ja-make-kana-str-list
	    (string-to-list
	     (string-append
	      (string-append-map-ustr-former extract-kana preconv-str)
	      (if convert-pending-into-kana?
		  (if residual-kana
                    (if (list? (car residual-kana))
                      (string-append-map extract-kana residual-kana)
		      (extract-kana residual-kana))
                    pending)
		  pending)
              (string-append-map-ustr-latter extract-kana preconv-str))))
	   kana)
          (string-append
	   (string-append-map-ustr-former extract-kana preconv-str)
           (if convert-pending-into-kana?
               (if residual-kana
                 (if (list? (car residual-kana))
                   (string-append-map extract-kana residual-kana)
                   (extract-kana residual-kana))
                 "")
               pending)
           (string-append-map-ustr-latter extract-kana preconv-str))))))

(define sj3-make-raw-string
  (lambda (raw-str-list wide? upper?)
    (if (not (null? raw-str-list))
	(if wide?
	    (string-append
	     (ja-string-list-to-wide-alphabet
	      (if upper?
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list)))))
		  (string-to-list (car raw-str-list))))
	     (sj3-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (sj3-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define sj3-make-whole-raw-string
  (lambda (sc wide? upper?)
    (sj3-make-raw-string (sj3-get-raw-str-seq sc) wide? upper?)))

(define (sj3-init-handler id im arg)
  (if (not sj3-init-lib-ok?)
      (begin
	(sj3-lib-init sj3-server-name)
	(set! sj3-init-lib-ok? #t)))
  (sj3-context-new id im))

(define (sj3-release-handler sc)
  (if sc
      (sj3-lib-release-context sc)))

(define (sj3-flush sc)
  (rk-flush (sj3-context-rkc sc))
  (ustr-clear! (sj3-context-preconv-ustr sc))
  (ustr-clear! (sj3-context-raw-ustr sc))
  (ustr-clear! (sj3-context-segments sc))
  (sj3-context-set-transposing! sc #f)
  (sj3-context-set-state! sc #f)
  (if (or (sj3-context-candidate-window sc)
          (sj3-context-prediction-window sc))
      (im-deactivate-candidate-selector sc))
  (sj3-context-set-candidate-window! sc #f)
  (sj3-context-set-prediction-window! sc #f)
  (sj3-context-set-candidate-op-count! sc 0))

(define (sj3-begin-input sc key key-state)
  (if (cond
       ((sj3-on-key? key key-state)
	#t)
       ((and
	 sj3-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((sj3-hiragana-key? key key-state)
	   (sj3-context-set-kana-mode! sc sj3-type-hiragana)
	   (sj3-context-set-alnum! sc #f)
	   #t)
	  ((sj3-katakana-key? key key-state)
	   (sj3-context-set-kana-mode! sc sj3-type-katakana)
	   (sj3-context-set-alnum! sc #f)
	   #t)
	  ((sj3-halfkana-key? key key-state)
	   (sj3-context-set-kana-mode! sc sj3-type-halfkana)
	   (sj3-context-set-alnum! sc #f)
	   #t)
	  ((sj3-halfwidth-alnum-key? key key-state)
	   (sj3-context-set-alnum-type! sc sj3-type-halfwidth-alnum)
	   (sj3-context-set-alnum! sc #t)
	   #t)
	  ((sj3-halfwidth-alnum-key? key key-state)
	   (sj3-context-set-alnum-type! sc sj3-type-fullwidth-alnum)
	   (sj3-context-set-alnum! sc #t)
	   #t)
	  ((sj3-kana-toggle-key? key key-state)
	   (sj3-context-kana-toggle sc)
	   (sj3-context-set-alnum! sc #f)
	   #t)
	  ((sj3-alkana-toggle-key? key key-state)
	   (sj3-context-alkana-toggle sc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(sj3-context-set-on! sc #t)
	(rk-flush (sj3-context-rkc sc))
	(sj3-context-set-state! sc #f)
	#t)
      #f))

(define (sj3-update-preedit sc)
  (if (not (sj3-context-commit-raw sc))
      (let ((segments (if (sj3-context-on sc)
			  (if (sj3-context-transposing sc)
			      (sj3-context-transposing-state-preedit sc)
			      (if (sj3-context-state sc)
				  (sj3-compose-state-preedit sc)
                                  (if (sj3-context-predicting sc)
                                      (sj3-predicting-state-preedit sc)
                                      (sj3-input-state-preedit sc))))
                          ())))
	(context-update-preedit sc segments))
      (sj3-context-set-commit-raw! sc #f)))

(define (sj3-begin-conv sc)
  (let ((sc-ctx (sj3-context-sc-ctx sc))
	(preconv-str (sj3-make-whole-string sc #t sj3-type-hiragana)))
    (if (and sc-ctx
             (> (string-length preconv-str) 0))
	(let ((num (sj3-lib-begin-conversion sc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (sj3-context-segments sc)
		 (make-list num 0))
		(sj3-context-set-state! sc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; sj3-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define sj3-cancel-conv
  (lambda (sc)
    (sj3-reset-candidate-window sc)
    (sj3-context-set-state! sc #f)
    (ustr-clear! (sj3-context-segments sc))
    (sj3-lib-reset-conversion sc)))

(define (sj3-proc-input-state-no-preedit sc key key-state)
  (let
      ((rkc (sj3-context-rkc sc))
       (direct (ja-direct (charcode->string key)))
       (rule (sj3-context-input-rule sc)))
    (cond
     ((and sj3-use-with-vi?
           (sj3-vi-escape-key? key key-state))
      (sj3-flush sc)
      (sj3-context-set-on! sc #f)
      (sj3-commit-raw sc))

     ((sj3-off-key? key key-state)
      (sj3-flush sc)
      (sj3-context-set-on! sc #f))

     ((sj3-backspace-key? key key-state)
      (sj3-commit-raw sc))
     
     ((sj3-delete-key? key key-state)
      (sj3-commit-raw sc))

     ((and
       (sj3-hiragana-key? key key-state)
       (not
        (and
	 (= (sj3-context-kana-mode sc) sj3-type-hiragana)
	 (not (sj3-context-alnum sc)))))
      (sj3-context-change-kana-mode! sc sj3-type-hiragana)
      (sj3-context-set-alnum! sc #f))

     ((and
       (sj3-katakana-key? key key-state)
       (not
        (and
	 (= (sj3-context-kana-mode sc) sj3-type-katakana)
	 (not (sj3-context-alnum sc)))))
      (sj3-context-change-kana-mode! sc sj3-type-katakana)
      (sj3-context-set-alnum! sc #f))
     
     ((and
       (sj3-halfkana-key? key key-state)
       (not
        (and
	 (= (sj3-context-kana-mode sc) sj3-type-halfkana)
	 (not (sj3-context-alnum sc)))))
      (sj3-context-change-kana-mode! sc sj3-type-halfkana)
      (sj3-context-set-alnum! sc #f))
     
     ((and
       (sj3-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (sj3-context-alnum-type sc) sj3-type-halfwidth-alnum)
	 (sj3-context-alnum sc))))
      (sj3-context-set-alnum-type! sc sj3-type-halfwidth-alnum)
      (sj3-context-set-alnum! sc #t))
     
     ((and
       (sj3-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (sj3-context-alnum-type sc) sj3-type-fullwidth-alnum)
	 (sj3-context-alnum sc))))
      (sj3-context-set-alnum-type! sc sj3-type-fullwidth-alnum)
      (sj3-context-set-alnum! sc #t))
     
     ((and
       (not (sj3-context-alnum sc))
       (sj3-kana-toggle-key? key key-state))
      (sj3-context-kana-toggle sc))

     ((sj3-alkana-toggle-key? key key-state)
      (sj3-context-alkana-toggle sc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (sj3-commit-raw sc))
     
     ;; direct key => commit
     (direct
      (im-commit sc direct))

     ;; space key
     ((sj3-space-key? key key-state)
      (if (sj3-context-alnum sc)
	  (im-commit sc (list-ref
			 ja-alnum-space
			 (- (sj3-context-alnum-type sc)
			    sj3-type-halfwidth-alnum)))
	  (im-commit sc (list-ref ja-space (sj3-context-kana-mode sc)))))

     ((symbol? key)
      (sj3-commit-raw sc))

     (else
      (if (sj3-context-alnum sc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (sj3-context-preconv-ustr sc)
			       (if (= (sj3-context-alnum-type sc)
				      sj3-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (sj3-context-raw-ustr sc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule sj3-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (sj3-context-preconv-ustr sc) res)
                    (ustr-insert-elem! (sj3-context-preconv-ustr sc) res))
	          (ustr-insert-elem! (sj3-context-raw-ustr sc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (sj3-commit-raw sc)))))))))

(define (sj3-has-preedit? sc)
  (or (not (ustr-empty? (sj3-context-preconv-ustr sc)))
      (> (string-length (rk-pending (sj3-context-rkc sc))) 0)))

(define sj3-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type sj3-type-halfwidth-alnum)
       (= state sj3-type-halfwidth-alnum))
      sj3-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type sj3-type-fullwidth-alnum)
       (= state sj3-type-fullwidth-alnum))
      sj3-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define sj3-proc-transposing-state
  (lambda (sc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (sj3-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons sj3-type-fullwidth-alnum rotate-list)))
      (if (sj3-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons sj3-type-halfwidth-alnum rotate-list)))
      (if (sj3-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons sj3-type-halfkana rotate-list)))
      (if (sj3-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons sj3-type-katakana rotate-list)))
      (if (sj3-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons sj3-type-hiragana rotate-list)))

      (if (sj3-context-transposing sc)
	  (let ((lst (member (sj3-context-transposing-type sc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (sj3-rotate-transposing-alnum-type
				 (sj3-context-transposing-type sc)
				 (car rotate-list))))))
	  (begin
	    (sj3-context-set-transposing! sc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state sj3-type-hiragana)
	      (= state sj3-type-katakana)
	      (= state sj3-type-halfkana)))
	(sj3-context-set-transposing-type! sc state))
       ((and state
	     (or
	      (= state sj3-type-halfwidth-alnum)
	      (= state sj3-candidate-type-upper-halfwidth-alnum)
	      (= state sj3-type-fullwidth-alnum)
	      (= state sj3-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (sj3-context-input-rule sc) sj3-input-rule-kana))
	    (sj3-context-set-transposing-type! sc state)))
       (else
	(and
	 ; commit
	 (if (sj3-commit-key? key key-state)
	     (begin
	       (im-commit sc (sj3-transposing-text sc))
	       (sj3-flush sc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (sj3-begin-conv-key? key key-state)
	     (begin
	       (sj3-context-set-transposing! sc #f)
	       (sj3-begin-conv sc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (sj3-cancel-key? key key-state)
	      (sj3-backspace-key? key key-state))
	     (begin
	       (sj3-context-set-transposing! sc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (sj3-prev-page-key? key key-state)
	      (sj3-next-page-key? key key-state)
	      (sj3-extend-segment-key? key key-state)
	      (sj3-shrink-segment-key? key key-state)
	      (sj3-next-segment-key? key key-state)
	      (sj3-beginning-of-preedit-key? key key-state)
	      (sj3-end-of-preedit-key? key key-state)
	      (sj3-next-candidate-key? key key-state)
	      (sj3-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit sc (sj3-transposing-text sc))
	   (sj3-flush sc)
	   (sj3-proc-input-state sc key key-state))))))))

(define (sj3-move-prediction sc offset)
  (let* ((nr (sj3-lib-get-nr-predictions sc))
         (idx (sj3-context-prediction-index sc))
         (n (if (not idx)
                0
                (+ idx offset)))
         (compensated-n (cond
                         ((>= n nr)
                          0)
                        ((< n 0)
                         (- nr 1))
                        (else
                         n))))
    (im-select-candidate sc compensated-n)
    (sj3-context-set-prediction-index! sc compensated-n)))

(define (sj3-move-prediction-in-page sc numeralc)
  (let* ((nr (sj3-lib-get-nr-predictions sc))
         (p-idx (sj3-context-prediction-index sc))
         (n (if (not p-idx)
                0
                p-idx))
         (cur-page (if (= sj3-nr-candidate-max 0)
                       0
                       (quotient n sj3-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page sj3-nr-candidate-max) compensated-pageidx))
         (compensated-idx (cond
                           ((>= idx nr)
                            #f)
                           (else
                            idx)))
         (selected-pageidx (if (not p-idx)
                               #f
                               (if (= sj3-nr-candidate-max 0)
                                   p-idx
                                   (remainder p-idx
                                              sj3-nr-candidate-max)))))
    (if (and
         compensated-idx
         (not (eqv? compensated-pageidx selected-pageidx)))
        (begin
          (sj3-context-set-prediction-index! sc compensated-idx)
          (im-select-candidate sc compensated-idx)
          #t)
       #f)))

(define (sj3-prediction-select-non-existing-index? sc numeralc)
  (let* ((nr (sj3-lib-get-nr-predictions sc))
         (p-idx (sj3-context-prediction-index sc))
         (cur-page (if (= sj3-nr-candidate-max 0)
                       0
                       (quotient p-idx sj3-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page sj3-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (sj3-prediction-keys-handled? sc key key-state)
  (cond
   ((sj3-next-prediction-key? key key-state)
    (sj3-move-prediction sc 1)
    #t)
   ((sj3-prev-prediction-key? key key-state)
    (sj3-move-prediction sc -1)
    #t)
   ((and
     sj3-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (sj3-move-prediction-in-page sc key))
   ((and
     (sj3-context-prediction-index sc)
     (sj3-prev-page-key? key key-state))
    (im-shift-page-candidate sc #f)
    #t)
   ((and
     (sj3-context-prediction-index sc)
     (sj3-next-page-key? key key-state))
    (im-shift-page-candidate sc #t)
    #t)
   (else
    #f)))

(define (sj3-proc-prediction-state sc key key-state)
  (cond
   ;; prediction index change
   ((sj3-prediction-keys-handled? sc key key-state))

   ;; cancel
   ((sj3-cancel-key? key key-state)
    (if (sj3-context-prediction-index sc)
        (sj3-reset-prediction-window sc)
        (begin
          (sj3-reset-prediction-window sc)
          (sj3-proc-input-state sc key key-state))))

   ;; commit
   ((and
     (sj3-context-prediction-index sc)
     (sj3-commit-key? key key-state))
    (sj3-do-commit-prediction sc))
   (else
    (if (and
         sj3-use-implicit-commit-prediction?
         (sj3-context-prediction-index sc))
        (cond
         ((or
           ;; check keys used in sj3-proc-input-state-with-preedit
           (sj3-begin-conv-key? key key-state)
           (sj3-backspace-key? key key-state)
           (sj3-delete-key? key key-state)
           (sj3-kill-key? key key-state)
           (sj3-kill-backward-key? key key-state)
           (and
            (not (sj3-context-alnum sc))
            (sj3-commit-as-opposite-kana-key? key key-state))
           (sj3-transpose-as-hiragana-key? key key-state)
           (sj3-transpose-as-katakana-key? key key-state)
           (sj3-transpose-as-halfkana-key? key key-state)
           (and
            (not (= (sj3-context-input-rule sc) sj3-input-rule-kana))
            (or
             (sj3-transpose-as-halfwidth-alnum-key? key key-state)
             (sj3-transpose-as-fullwidth-alnum-key? key key-state)))
           (sj3-hiragana-key? key key-state)
           (sj3-katakana-key? key key-state)
           (sj3-halfkana-key? key key-state)
           (sj3-halfwidth-alnum-key? key key-state)
           (sj3-fullwidth-alnum-key? key key-state)
           (and
            (not (sj3-context-alnum sc))
            (sj3-kana-toggle-key? key key-state))
           (sj3-alkana-toggle-key? key key-state)
           (sj3-go-left-key? key key-state)
           (sj3-go-right-key? key key-state)
           (sj3-beginning-of-preedit-key? key key-state)
           (sj3-end-of-preedit-key? key key-state)
           (and
            (modifier-key-mask key-state)
            (not (shift-key-mask key-state))))
          ;; go back to unselected prediction
          (sj3-reset-prediction-window sc)
          (sj3-check-prediction sc #f))
         ((and
           (ichar-numeric? key)
           sj3-select-prediction-by-numeral-key?
           (not (sj3-prediction-select-non-existing-index? sc key)))
          (sj3-context-set-predicting! sc #f)
          (sj3-context-set-prediction-index! sc #f)
          (sj3-proc-input-state sc key key-state))
         (else
          ;; implicit commit
          (sj3-do-commit-prediction sc)
          (sj3-proc-input-state sc key key-state)))
        (begin
          (sj3-context-set-predicting! sc #f)
          (sj3-context-set-prediction-index! sc #f)
          (sj3-proc-input-state sc key key-state))))))

(define (sj3-proc-input-state-with-preedit sc key key-state) 
  (define (check-auto-conv str)
    (and
      str
      sj3-auto-start-henkan?
      (string-find japanese-auto-start-henkan-keyword-list str)
      (begin
	(sj3-reset-prediction-window sc)
	(sj3-begin-conv sc))))
  (let ((preconv-str (sj3-context-preconv-ustr sc))
	(raw-str (sj3-context-raw-ustr sc))
	(rkc (sj3-context-rkc sc))
	(rule (sj3-context-input-rule sc))
	(kana (sj3-context-kana-mode sc)))
    (cond
     ;; begin conversion
     ((sj3-begin-conv-key? key key-state)
      (sj3-reset-prediction-window sc)
      (sj3-begin-conv sc))

     ;; prediction
     ((sj3-next-prediction-key? key key-state)
      (sj3-check-prediction sc #t))

     ;; backspace
     ((sj3-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (sj3-context-input-rule sc) sj3-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((sj3-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((sj3-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((sj3-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (sj3-context-alnum sc))
       (sj3-commit-as-opposite-kana-key? key key-state))
      (im-commit sc (sj3-make-whole-string sc #t (ja-opposite-kana kana)))
      (sj3-flush sc))

     ;; Transposing状態へ移行
     ((or (sj3-transpose-as-hiragana-key? key key-state)
	  (sj3-transpose-as-katakana-key? key key-state)
	  (sj3-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (sj3-context-input-rule sc) sj3-input-rule-kana))
	   (or
	    (sj3-transpose-as-halfwidth-alnum-key? key key-state)
	    (sj3-transpose-as-fullwidth-alnum-key? key key-state))))
      (sj3-reset-prediction-window sc)
      (sj3-proc-transposing-state sc key key-state))

     ((sj3-hiragana-key? key key-state)
      (if (not (= kana sj3-type-hiragana))
	  (begin
	    (im-commit sc (sj3-make-whole-string sc #t kana))
	    (sj3-flush sc)))
      (sj3-context-set-kana-mode! sc sj3-type-hiragana)
      (sj3-context-set-alnum! sc #f))

     ((sj3-katakana-key? key key-state)
      (if (not (= kana sj3-type-katakana))
	  (begin
	    (im-commit sc (sj3-make-whole-string sc #t kana))
	    (sj3-flush sc)))
      (sj3-context-set-kana-mode! sc sj3-type-katakana)
      (sj3-context-set-alnum! sc #f))

     ((sj3-halfkana-key? key key-state)
      (if (not (= kana sj3-type-halfkana))
	  (begin
	    (im-commit sc (sj3-make-whole-string sc #t kana))
	    (sj3-flush sc)))
      (sj3-context-set-kana-mode! sc sj3-type-halfkana)
      (sj3-context-set-alnum! sc #f))

     ((and
       (sj3-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (sj3-context-alnum-type sc) sj3-type-halfwidth-alnum)
	 (sj3-context-alnum sc))))
      (sj3-context-set-alnum-type! sc sj3-type-halfwidth-alnum)
      (sj3-context-set-alnum! sc #t))

     ((and
       (sj3-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (sj3-context-alnum-type sc) sj3-type-fullwidth-alnum)
	 (sj3-context-alnum sc))))
      (sj3-context-set-alnum-type! sc sj3-type-fullwidth-alnum)
      (sj3-context-set-alnum! sc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (sj3-context-alnum sc))
       (sj3-kana-toggle-key? key key-state))
      (im-commit sc (sj3-make-whole-string sc #t kana))
      (sj3-flush sc)
      (sj3-context-kana-toggle sc))

     ((sj3-alkana-toggle-key? key key-state)
      (sj3-context-alkana-toggle sc))

     ;; cancel
     ((sj3-cancel-key? key key-state)
      (sj3-flush sc))

     ;; commit
     ((sj3-commit-key? key key-state)
      (begin
	(im-commit
	 sc
	 (sj3-make-whole-string sc #t kana))
	(sj3-flush sc)))

     ;; left
     ((sj3-go-left-key? key key-state)
      (sj3-context-confirm-kana! sc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((sj3-go-right-key? key key-state)
      (sj3-context-confirm-kana! sc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((sj3-beginning-of-preedit-key? key key-state)
      (sj3-context-confirm-kana! sc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((sj3-end-of-preedit-key? key key-state)
      (sj3-context-confirm-kana! sc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (if (sj3-context-alnum sc)
          (let ((key-str (charcode->string key))
	        (pend (rk-pending rkc))
		(residual-kana (rk-peek-terminal-match rkc)))
	    (rk-flush rkc) ;; OK to reset rkc here.
	    (if residual-kana
	        (begin
                  (if (list? (car residual-kana))
                    (begin
                      (ustr-insert-seq! preconv-str residual-kana)
                      (ustr-insert-elem! raw-str (reverse
                                                   (string-to-list pend))))
                    (begin
                      (ustr-insert-elem! preconv-str residual-kana)
                      (ustr-insert-elem! raw-str pend)))))
	    (ustr-insert-elem! preconv-str
			       (if (= (sj3-context-alnum-type sc)
				      sj3-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str)
	    (check-auto-conv key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule sj3-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (pend (rk-pending rkc))
	         (res (rk-push-key! rkc key-str)))
	    (if (and res
		     (or (list? (car res))
		         (not (string=? (car res) ""))))
	        (let ((next-pend (rk-pending rkc)))
	          (if (list? (car res))
		      (ustr-insert-seq!  preconv-str res)
		      (ustr-insert-elem! preconv-str res))
	          (if (and next-pend
		           (not (string=? next-pend "")))
		      (ustr-insert-seq! raw-str
                                        (reverse (string-to-list pend)))
		      (if (list? (car res))
		          (begin
                            (if (member pend
                                        (map car
                                             ja-consonant-syllable-table))
                              ;; treat consonant having more than one
                              ;; charactear as one raw-str in this case
                              (ustr-insert-elem! raw-str pend)
                              (ustr-insert-elem! raw-str (reverse
                                                           (string-to-list
                                                             pend))))
                            ;; assume key-str as a vowel
			    (ustr-insert-elem! raw-str key-str))
		          (ustr-insert-elem!
		           raw-str
		           (string-append pend key-str))))))
	    (check-auto-conv (if res (car res) #f))))))))

(define sj3-context-confirm-kana!
  (lambda (sc)
    (if (= (sj3-context-input-rule sc)
	   sj3-input-rule-kana)
	(let* ((preconv-str (sj3-context-preconv-ustr sc))
	       (rkc (sj3-context-rkc sc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (sj3-reset-prediction-window sc)
  (if (sj3-context-prediction-window sc)
      (im-deactivate-candidate-selector sc))
  (sj3-context-set-predicting! sc #f)
  (sj3-context-set-prediction-window! sc #f)
  (sj3-context-set-prediction-index! sc #f))

(define (sj3-check-prediction sc force-check?)
  (if (and
       (not (sj3-context-state sc))
       (not (sj3-context-transposing sc))
       (not (sj3-context-predicting sc)))
      (let* ((use-pending-rk-for-prediction? #t)
	     (preconv-str
	      (sj3-make-whole-string
	       sc
	       (not use-pending-rk-for-prediction?)
	       (sj3-context-kana-mode sc)))
	     (preedit-len (+
			   (ustr-length (sj3-context-preconv-ustr sc))
			   (if (not use-pending-rk-for-prediction?)
			       0
			       (string-length (rk-pending
					       (sj3-context-rkc
						sc)))))))
	(if (or
	     (>= preedit-len sj3-prediction-start-char-count)
	     force-check?)
	    (begin
	      (sj3-lib-set-prediction-src-string sc preconv-str)
	      (let ((nr (sj3-lib-get-nr-predictions sc)))
		(if (and
		     nr
		     (> nr 0))
		    (begin
		      (im-activate-candidate-selector
		       sc nr sj3-nr-candidate-max)
		      (sj3-context-set-prediction-window! sc #t)
		      (sj3-context-set-predicting! sc #t))
		    (sj3-reset-prediction-window sc))))
	    (sj3-reset-prediction-window sc)))))

(define (sj3-proc-input-state sc key key-state)
  (if (sj3-has-preedit? sc)
      (sj3-proc-input-state-with-preedit sc key key-state)
      (sj3-proc-input-state-no-preedit sc key key-state))
  (if sj3-use-prediction?
      (sj3-check-prediction sc #f)))

(define sj3-separator
  (lambda (sc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if sj3-show-segment-separator?
	  (cons attr sj3-segment-separator)
	  #f))))

(define sj3-context-transposing-state-preedit
  (lambda (sc)
    (let ((transposing-text (sj3-transposing-text sc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define sj3-transposing-text
  (lambda (sc)
    (let ((transposing-type (sj3-context-transposing-type sc)))
      (cond
       ((or
	 (= transposing-type sj3-type-hiragana)
	 (= transposing-type sj3-type-katakana)
	 (= transposing-type sj3-type-halfkana))
	(sj3-make-whole-string sc #t transposing-type))
       ((= transposing-type sj3-type-halfwidth-alnum)
	(sj3-make-whole-raw-string sc #f #f))
       ((= transposing-type sj3-candidate-type-upper-halfwidth-alnum)
	(sj3-make-whole-raw-string sc #f #t))
       ((= transposing-type sj3-type-fullwidth-alnum)
	(sj3-make-whole-raw-string sc #t #f))
       ((= transposing-type sj3-candidate-type-upper-fullwidth-alnum)
	(sj3-make-whole-raw-string sc #t #t))))))

(define sj3-get-raw-str-seq
  (lambda (sc)
    (let* ((rkc (sj3-context-rkc sc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (sj3-context-raw-ustr sc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
	      right-str))))

(define sj3-get-raw-candidate
  (lambda (sc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (sj3-make-whole-string sc #t sj3-type-hiragana))))
	   (unconv-candidate (sj3-lib-get-unconv-candidate sc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (sj3-get-raw-str-seq sc))))
      (cond
       ((= cand-idx sj3-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx sj3-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) sj3-type-katakana))
       ((= cand-idx sj3-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) sj3-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (sj3-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx sj3-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       sj3-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx sj3-candidate-type-halfwidth-alnum)
			    (= cand-idx sj3-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (sj3-predicting-state-preedit sc)
  (if (or
       (not sj3-use-implicit-commit-prediction?)
       (not (sj3-context-prediction-index sc)))
      (sj3-input-state-preedit sc)
      (let ((cand (sj3-get-prediction-string sc)))
        (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (sj3-compose-state-preedit sc)
  (let* ((segments (sj3-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (sj3-separator sc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx sj3-candidate-type-katakana)
			(sj3-lib-get-nth-candidate sc seg-idx cand-idx)
			(sj3-get-raw-candidate sc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (sj3-input-state-preedit sc)
  (let* ((preconv-str (sj3-context-preconv-ustr sc))
	 (rkc (sj3-context-rkc sc))
	 (pending (rk-pending rkc))
	 (kana (sj3-context-kana-mode sc))
	 (rule (sj3-context-input-rule sc))
	 (extract-kana
	  (if (= rule sj3-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (sj3-has-preedit? sc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (sj3-get-commit-string sc)
  (let ((segments (sj3-context-segments sc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx sj3-candidate-type-katakana)
			     (sj3-lib-get-nth-candidate
			      sc seg-idx cand-idx)
			     (sj3-get-raw-candidate
			      sc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (sj3-commit-string sc)
  (let ((segments (sj3-context-segments sc)))
    (if sc
	(let ((save-segments (map (lambda (seg-idx cand-idx) ;; store segment data
                                    (sj3-lib-get-nth-candidate-without-muhenkan sc seg-idx cand-idx))
                                  (iota (ustr-length segments))
                                  (ustr-whole-seq segments))))
	  (for-each (lambda (seg-idx cand-idx)
		      (if (> cand-idx sj3-candidate-type-katakana)
			  (sj3-lib-commit-segment sc seg-idx cand-idx)))
		    (iota (ustr-length segments))
		    (ustr-whole-seq segments))
          ;; use stored segment
          ;; order of douon is overwritten by sj3-lib-commit-segment
          ;; converted index is alway 0
          (sj3-lib-commit-segments sc save-segments)
	  (if (every (lambda (x) (<= x sj3-candidate-type-katakana))
		     (ustr-whole-seq segments))
	      (sj3-lib-reset-conversion sc))))))

(define (sj3-do-commit sc)
    (im-commit sc (sj3-get-commit-string sc))
    (sj3-commit-string sc)
    (sj3-reset-candidate-window sc)
    (sj3-flush sc))

(define (sj3-get-prediction-string sc)
  (sj3-lib-get-nth-prediction
   sc
   (sj3-context-prediction-index sc)))

(define (sj3-learn-prediction-string sc)
  (sj3-lib-commit-nth-prediction
   sc
   (sj3-context-prediction-index sc)))

(define (sj3-do-commit-prediction sc)
  (im-commit sc (sj3-get-prediction-string sc))
  (sj3-learn-prediction-string sc)
  (sj3-reset-prediction-window sc)
  (sj3-flush sc))

(define sj3-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (sj3-move-segment sc dir)
  (sj3-reset-candidate-window sc)
  (let ((segments (sj3-context-segments sc)))
    (ustr-cursor-move! segments dir)
    (sj3-correct-segment-cursor segments)))

(define (sj3-resize-segment sc cnt)
  (let* ((segments (sj3-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments)))
    (sj3-reset-candidate-window sc)
    (sj3-lib-resize-segment sc cur-seg cnt)
    (let* ((resized-nseg (sj3-lib-get-nr-segments sc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (sj3-move-candidate sc offset)
  (let* ((segments (sj3-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (sj3-lib-get-nr-candidates sc cur-seg))
	 (n (if (< (ustr-cursor-frontside segments) 0) ;; segment-transposing
		0
		(+ (ustr-cursor-frontside segments) offset)))
	 (compensated-n (cond
			 ((>= n max)
			  0)
			 ((< n 0)
			  (- max 1))
			 (else
			  n)))
	 (new-op-count (+ 1 (sj3-context-candidate-op-count sc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (sj3-context-set-candidate-op-count! sc new-op-count)
    (if (and
	 (= (sj3-context-candidate-op-count sc)
	    sj3-candidate-op-count)
	 sj3-use-candidate-window?)
	(begin
	  (sj3-context-set-candidate-window! sc #t)
	  (im-activate-candidate-selector sc max sj3-nr-candidate-max)))
    (if (sj3-context-candidate-window sc)
	(im-select-candidate sc compensated-n))))

(define sj3-move-candidate-in-page
  (lambda (sc numeralc)
    (let* ((segments (sj3-context-segments sc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (sj3-lib-get-nr-candidates sc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= sj3-nr-candidate-max 0)
			 0
			 (quotient n sj3-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page sj3-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (sj3-context-candidate-op-count sc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (sj3-context-set-candidate-op-count! sc new-op-count)
      (im-select-candidate sc compensated-idx))))

(define (sj3-reset-candidate-window sc)
  (if (sj3-context-candidate-window sc)
      (begin
	(im-deactivate-candidate-selector sc)
	(sj3-context-set-candidate-window! sc #f)))
  (sj3-context-set-candidate-op-count! sc 0))

(define sj3-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx sj3-candidate-type-halfwidth-alnum)
       (= state sj3-candidate-type-halfwidth-alnum))
      sj3-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx sj3-candidate-type-fullwidth-alnum)
       (= state sj3-candidate-type-fullwidth-alnum))
      sj3-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define sj3-set-segment-transposing
  (lambda (sc key key-state)
    (let ((segments (sj3-context-segments sc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(sj3-reset-candidate-window sc)
	(sj3-context-set-candidate-op-count! sc 0)

	(if (sj3-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons sj3-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (sj3-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons sj3-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (sj3-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons sj3-candidate-type-halfkana
				    rotate-list)))
	(if (sj3-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons sj3-candidate-type-katakana
				    rotate-list)))
	(if (sj3-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons sj3-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx sj3-candidate-type-hiragana)
	     (= idx sj3-candidate-type-katakana)
	     (= idx sj3-candidate-type-halfkana)
	     (= idx sj3-candidate-type-halfwidth-alnum)
	     (= idx sj3-candidate-type-fullwidth-alnum)
	     (= idx sj3-candidate-type-upper-halfwidth-alnum)
	     (= idx sj3-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (sj3-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (sj3-proc-compose-state sc key key-state)
  (cond
   ((sj3-prev-page-key? key key-state)
    (if (sj3-context-candidate-window sc)
        (im-shift-page-candidate sc #f)))

   ((sj3-next-page-key? key key-state)
    (if (sj3-context-candidate-window sc)
        (im-shift-page-candidate sc #t)))

   ((sj3-commit-key? key key-state)
    (sj3-do-commit sc))

   ((sj3-extend-segment-key? key key-state)
    (sj3-resize-segment sc 1))

   ((sj3-shrink-segment-key? key key-state)
    (sj3-resize-segment sc -1))

   ((sj3-next-segment-key? key key-state)
    (sj3-move-segment sc 1))

   ((sj3-prev-segment-key? key key-state)
    (sj3-move-segment sc -1))

   ((sj3-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (sj3-context-segments sc))
      (sj3-reset-candidate-window sc)))

   ((sj3-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (sj3-context-segments sc))
      (sj3-correct-segment-cursor (sj3-context-segments sc))
      (sj3-reset-candidate-window sc)))

   ((sj3-backspace-key? key key-state)
    (sj3-cancel-conv sc))

   ((sj3-next-candidate-key? key key-state)
    (sj3-move-candidate sc 1))

   ((sj3-prev-candidate-key? key key-state)
    (sj3-move-candidate sc -1))

   ((or (sj3-transpose-as-hiragana-key? key key-state)
        (sj3-transpose-as-katakana-key? key key-state)
        (sj3-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (sj3-context-input-rule sc) sj3-input-rule-kana))
         (or
          (sj3-transpose-as-halfwidth-alnum-key? key key-state)
          (sj3-transpose-as-fullwidth-alnum-key? key key-state))))
    (sj3-set-segment-transposing sc key key-state))

   ((sj3-cancel-key? key key-state)
    (sj3-cancel-conv sc))

   ((and sj3-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (sj3-context-candidate-window sc))
    (sj3-move-candidate-in-page sc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (sj3-do-commit sc)
      (sj3-proc-input-state sc key key-state)))))

(define (sj3-press-key-handler sc key key-state)
  (if (ichar-control? key)
      (im-commit-raw sc)
      (if (sj3-context-on sc)
          (if (sj3-context-transposing sc)
              (sj3-proc-transposing-state sc key key-state)
              (if (sj3-context-state sc)
                  (sj3-proc-compose-state sc key key-state)
                  (if (sj3-context-predicting sc)
                      (sj3-proc-prediction-state sc key key-state)
                      (sj3-proc-input-state sc key key-state))))
	  (sj3-proc-raw-state sc key key-state)))
  (sj3-update-preedit sc))

;;;
(define (sj3-release-key-handler sc key key-state)
  (if (or (ichar-control? key)
	  (not (sj3-context-on sc)))
      (sj3-commit-raw sc)))
;;;
(define (sj3-reset-handler sc)
  (if (sj3-context-on sc)
      (begin
	(if (sj3-context-state sc)
            (sj3-lib-reset-conversion sc))
	(sj3-flush sc))))

;;;
(define (sj3-get-candidate-handler sc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (sj3-context-segments sc)))
         (cand (if (sj3-context-state sc)
                   (sj3-lib-get-nth-candidate sc cur-seg idx)
                   (sj3-lib-get-nth-prediction sc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (sj3-set-candidate-index-handler sc idx)
  (cond
   ((sj3-context-state sc)
    (ustr-cursor-set-frontside! (sj3-context-segments sc) idx)
    (sj3-update-preedit sc))
   ((sj3-context-predicting sc)
    (sj3-context-set-prediction-index! sc idx)
    (sj3-update-preedit sc))))

(define (sj3-proc-raw-state sc key key-state)
  (if (not (sj3-begin-input sc key key-state))
      (im-commit-raw sc)))

(sj3-configure-widgets)
(register-im
 'sj3
 "ja"
 "EUC-JP"
 sj3-im-name-label
 sj3-im-short-desc
 #f
 sj3-init-handler
 sj3-release-handler
 context-mode-handler
 sj3-press-key-handler
 sj3-release-key-handler
 sj3-reset-handler
 sj3-get-candidate-handler
 sj3-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
