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

(require-extension (srfi 95))

(require-custom "generic-key-custom.scm")
(require-custom "look-custom.scm")

(require "annotation.scm")

;; widgets
(define look-widgets '(widget_look_input_mode))

;; default activity for each widgets
(define default-widget_look_input_mode 'action_look_sleep)

;; actions of widget_look_input_mode
(define look-input-mode-actions
  '(action_look_sleep action_look_direct action_look_look))

;;; implementations

(register-action 'action_look_sleep
                 (lambda (lc)
                   (list
                    'look_sleep_input
                    "_"
                    (N_ "Sleep")
                    (N_ "Look Sleep Input Mode")))
                 (lambda (lc)
                   (not (look-context-on? lc)))
                 (lambda (lc)
                   (look-context-set-on! lc #f)))

(register-action 'action_look_direct
                 (lambda (lc)
                   (list
                    'look_direct_input
                    "-"
                    (N_ "Direct")
                    (N_ "Look Direct Input Mode")))
                 (lambda (lc)
                   (and (look-context-on? lc)
                        (not (look-context-look? lc))))
                 (lambda (lc)
                   (look-context-set-on! lc #t)
                   (look-context-set-look! lc #f)))

(register-action 'action_look_look
                 (lambda (lc)
                   (list
                    'look_input
                    "e" ;; do you like nethack?
                    (N_ "Look")
                    (N_ "Look Input Mode")))
                 (lambda (lc)
                   (and (look-context-on? lc)
                        (look-context-look? lc)))
                 (lambda (lc)
                   (look-context-set-on! lc #t)
                   (look-context-set-look! lc #t)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define (look-configure-widgets)
  (register-widget 'widget_look_input_mode
                   (activity-indicator-new look-input-mode-actions)
                   (actions-new look-input-mode-actions)))

(define look-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on         #f)
    (list 'look       #f)
    (list 'nth        0)
    (list 'candidates ())
    (list 'left       "")
    (list 'prev       ())    ; simple queue: ([string]prevword1 prevword2 ...)
    (list 'dict       #f)    ; list ((([string]prevword1 prevword2 ...)  . [alist]history) ...)
    (list 'dictlen    0))))
(define look-context-rec-spec look-context-rec-spec)
(define-record 'look-context look-context-rec-spec)
(define look-context-new-internal look-context-new)

;; XXX: fake R5RS functions
(define (look-internal:string->list s)
  (map (lambda (c)
         (string->symbol c))
       (reverse (string-to-list s))))
(define (look-internal:list->string l)
  (apply string-append
         (map (lambda (x)
                (symbol->string x))
              l)))
(define (look-internal:make-string n c)
  (apply string-append (map (lambda (x) (symbol->string c)) (iota n))))
(define (look-to-lower-string str)
  (apply string-append
         (map (lambda (c)
                (if (ichar-upper-case? (string->charcode c))
                    (charcode->string (ichar-downcase (string->charcode c)))
                    c))
              (reverse (string-to-list str)))))

(define (look-history-sort li lessf)
  ;;(map car li))
  (map car (sort! li (lambda (x y) (lessf (cdr x) (cdr y))))))

(define (look-history-eow? x)
  (eq? #t (car x)))
(define (look-init-history seedf)
  (list (cons #t (seedf))))
(define (look-make-eow stat)
  (cons #t stat))
(define (look-histroy-append str hist seedf eowf)
  (let ((cs (look-internal:string->list str)))
    (cond ((null? cs)
           (if (assq #t hist) ; eow?
               (map (lambda (x)
                      (if (look-history-eow? x)
                          (look-make-eow (eowf (cdr x)))
                          x))
                    hist)
               (append (list (look-make-eow (seedf))) hist)))
          ((and (not (null? hist))
                (assoc (car cs) hist))
           (map (lambda (x)
                  (if (equal? (car cs) (car x))
                      (cons (car cs)
                            (look-histroy-append
                             (look-internal:list->string (cdr cs))
                             (cdr x)
                             seedf eowf))
                      x))
                hist))
          (else
           (append (list (cons (car cs)
                               (look-histroy-append
                                (look-internal:list->string (cdr cs))
                                '()
                                seedf eowf)))
                   hist)))))
(define (look-history-search str hist)
  (define (skip str hist)
    (let ((cs (look-internal:string->list str)))
      (if (null? cs)
          hist
          (let ((c (assoc (car cs) hist)))
            (if c
                (skip (look-internal:list->string (cdr cs)) (cdr c))
                '())))))
  (define (connect-tree hist)
    (let loop ((hist hist) (rest ""))
      (cond ((null? hist)
             '())
            ((find (lambda (x) (not (look-history-eow? x))) hist)
             (apply
              append (map (lambda (l)
                            (let ((li (loop (cdr l)
                                            (string-append rest (look-internal:make-string 1 (car l))))))
                              (if (list? li)
                                  li
                                  (list li))))
                          (filter (lambda (x) (not (look-history-eow? x))) hist))))
            (else
             (cons rest (cdar hist))))))
  (connect-tree (filter (lambda (x) (not (look-history-eow? x)))
                        (skip str hist))))

;; accumulator
(define (look-history-stat-init)
  1)
(define (look-history-stat-inc x)
  (+ 1 x))
(define (look-history-stat-less x y)
  (> x y))

;; XXX: non-atomic functions
(define (look-save-personal-dict lc)
  (call-with-output-file look-personal-dict-filename
    (lambda (port)
      (im-clear-preedit lc)
      (im-pushback-preedit
       lc preedit-reverse
       "[saving...]")
      (im-update-preedit lc)
      (write (cons look-prepared-words
                   (look-context-dict lc))
             port)
      (im-clear-preedit lc)
      (im-update-preedit lc))))

(define (look-load-personal-dict lc)
  (if (file-readable? look-personal-dict-filename)
      (let ((dict (call-with-input-file look-personal-dict-filename
		    (lambda (port)
		      (im-clear-preedit lc)
		      (im-pushback-preedit
		       lc preedit-reverse
		       "[loading...]")
		      (im-update-preedit lc)
		      (guard (err
			      (else #f))
			(read port))))))
	(if (and dict
		 (not (null? dict))
		 (= (car dict) look-prepared-words))
	    (look-context-set-dict! lc (cdr dict)))))
  (im-clear-preedit lc)
  (im-update-preedit lc))

(define (look-learn lc)
  (define (histroy-append hist)
    (look-histroy-append (look-to-lower-string (look-context-left lc))
                         hist
                         look-history-stat-init
                         look-history-stat-inc))
  (cond ((= 0 look-prepared-words)
         (let ((hist (if (not (look-context-dict lc))
                         (look-init-history look-history-stat-init)
                         (look-context-dict lc))))
           (look-context-set-dict!
            lc
            (histroy-append hist))))
        ((< (length (look-context-prev lc)) look-prepared-words)
         #t)
        (else
         (if (not (look-context-dict lc))
             (look-context-set-dict!
              lc
              (cons (look-context-prev lc)
                    (histroy-append (look-init-history look-history-stat-init))))
             (if (assoc (look-context-prev lc)
                        (look-context-dict lc))
                 (look-context-set-dict!
                  lc
                  (map (lambda (x)
                         (if (equal? (look-context-prev lc)
                                     (car x))
                             (cons (car x)
                                   (histroy-append (cdr x)))
                             x))
                       (look-context-dict lc)))
                 (look-context-set-dict!
                  lc
                  (append (list (cons (look-context-prev lc)
                                      (histroy-append (look-init-history look-history-stat-init))))
                          (look-context-dict lc)))))))
  (if (< (length (look-context-prev lc)) look-prepared-words)
      (look-context-set-prev! lc (append (look-context-prev lc)
                                         (list (string->symbol (look-context-left lc)))))
      (if (= 0 look-prepared-words)
          #t
          (look-context-set-prev! lc (append (cdr (look-context-prev lc))
                                             (list (string->symbol (look-context-left lc))))))))

(define (look-search-learned lc str)
  (if (= 0 look-prepared-words)
      (if (look-context-dict lc)
          (look-history-sort
           (look-history-search (look-to-lower-string str)
                                (look-context-dict lc))
           look-history-stat-less)
          '())
      (let ((res (if (look-context-dict lc)
                     (assoc (look-context-prev lc) (look-context-dict lc))
                     #f)))
        (if res
            (look-history-sort
             (look-history-search (look-to-lower-string (look-context-left lc))
                                  (cdr res))
             look-history-stat-less)
            '()))))

(define look-context-on? look-context-on)
(define look-context-look? look-context-look)

(define (look-get-nth-candidate lc)
  (if (< 0 (length (look-context-candidates lc)))
      (nth (look-context-nth lc) (look-context-candidates lc))
      ""))

(define (look-get-length-left lc)
  (string-length (look-context-left lc)))

(define (look-append-left! lc str)
  (look-context-set-left! lc (string-append (look-context-left lc) str)))

(define (look-remove-last-char-from-left! lc)
  (let ((left (look-context-left lc)))
    (if (< 0 (look-get-length-left lc))
        (look-context-set-left! lc (apply string-append (reverse (cdr (string-to-list left)))))
        (look-context-set-left! lc ""))))

(define (look-append-char-from-candidate-to-left! lc)
  (let ((candidate (look-get-nth-candidate lc)))
    (if (< 0 (string-length candidate))
        (look-context-set-left! lc (string-append (look-context-left lc)
                                                  (car (reverse (string-to-list candidate))))))))

(define (look-append-from-candidate-to-left! lc)
  (look-context-set-left! lc (string-append (look-context-left lc)
                                            (look-get-nth-candidate lc)))
  (look-context-set-candidates! lc '()))

(define (look-context-new . args)
  (let ((lc (apply look-context-new-internal args)))
    (look-context-set-widgets! lc look-widgets)
    (if look-use-annotation?
        (annotation-init))
    lc))

(define (look-context-clean lc)
  (look-context-set-on! lc #f)
  (look-context-set-look! lc #f)
  (look-context-set-nth! lc 0)
  (look-context-set-candidates! lc '())
  (look-context-set-left! lc ""))

(define (look-context-flush lc)
  (look-learn lc)
  (im-commit lc (look-context-left lc))
  (look-context-set-look! lc #f)
  (look-context-set-nth! lc 0)
  (look-context-set-candidates! lc '())
  (look-context-set-left! lc ""))

(define (look-push-back-mode lc lst)
  (if (car lst)
      (begin
        (im-pushback-mode-list lc (caar lst))
        (look-push-back-mode lc (cdr lst)))))

(define (look-init-handler id im arg)
  (let ((lc (look-context-new id im)))
    (look-load-personal-dict lc)
    lc))

(define (look-release-handler lc)
  (if look-use-annotation?
      (annotation-release))
  #f)

(define (look-alphabetic-char? key state)
  (and (or (not (modifier-key-mask state))
           (shift-key-mask state))
       (ichar-alphabetic? key)))

(define (look-next-candidate! lc)
  (if (< (look-context-nth lc) (- (length (look-context-candidates lc)) 1))
      (look-context-set-nth! lc (+ (look-context-nth lc) 1))))

(define (look-prev-candidate! lc)
  (if (< 0 (look-context-nth lc))
      (look-context-set-nth! lc (- (look-context-nth lc) 1))))

(define (look-look lc look-dict str)
  (let* ((learned (look-search-learned lc str))
         (looked (look-lib-look #t #t look-candidates-max look-dict str)))
    (look-context-set-dictlen! lc (length learned))
    (append learned (if looked looked '()))))

(define (look-update lc)
  (let ((str (look-context-left lc)))
    (look-context-set-nth! lc 0)
    (if (<= look-beginning-character-length (string-length str))
        (look-context-set-candidates! lc (look-look lc look-dict str))
        (look-context-set-candidates! lc '()))))

(define (look-format-candidates lc)
  (let ((candidates (look-context-candidates lc)))
    (if (or (= 0 (string-length (look-context-left lc)))
            (<= (length candidates) (look-context-nth lc)))
        ""
        (string-append look-fence-left
                       (nth (look-context-nth lc) candidates)
                       look-fence-right))))

(define (look-format-candidates-nth lc)
  (if (or (= 0 (string-length (look-context-left lc)))
          (<= (length (look-context-candidates lc))
              (look-context-nth lc)))
      ""
      (let ((nth (if (< (look-context-nth lc)
                        (look-context-dictlen lc))
                     (+ 1 (look-context-nth lc))
                     (+ 1
                        (- (look-context-nth lc)
                           (look-context-dictlen lc)))))
            (candidates (if (< (look-context-nth lc)
                               (look-context-dictlen lc))
                            (look-context-dictlen lc)
                            (- (length (look-context-candidates lc))
                               (look-context-dictlen lc)))))
        (string-append "["
                       (number->string nth)
                       "/"
                       (number->string candidates)
                       "]"))))

(define (look-format-annotation lc)
  (define (annotation-format-entry str lines)
    (let loop ((l (string->list str))
             (lines lines)
             (rest '()))
      (cond ((or (null? l)
                 (= 0 lines))
             (list->string (reverse rest)))
            ((eq? #\newline (car l))
           (loop (cdr l) (- lines 1) (cons #\space rest)))
            (else
             (loop (cdr l) lines (cons (car l) rest))))))
  (let ((candidates (look-context-candidates lc)))
    (if (or (= 0 (string-length (look-context-left lc)))
            (<= (length candidates) (look-context-nth lc)))
        ""
        (annotation-format-entry (annotation-get-text (string-append
                                                       (look-context-left lc)
                                                       (nth (look-context-nth lc) candidates))
                                                      "UTF-8")
                                 look-annotation-show-lines))))


(define (look-update-preedit lc)
  (im-clear-preedit lc)
  (im-pushback-preedit
   lc preedit-none
   (look-context-left lc))
  (im-pushback-preedit
   lc preedit-cursor
   (look-format-candidates lc))
  (if (< (look-context-nth lc) (look-context-dictlen lc))
      (im-pushback-preedit
       lc preedit-none
       (look-format-candidates-nth lc))
      (im-pushback-preedit
       lc preedit-reverse
       (look-format-candidates-nth lc)))
  (if look-use-annotation?
      (im-pushback-preedit
       lc preedit-none
       (look-format-annotation lc)))
  (im-update-preedit lc))

(define (look-key-press-state-look lc key state)
  (cond ((look-off-key? key state)
         (look-context-clean lc)
         (look-update-preedit lc))
        ((look-alphabetic-char? key state)
         (look-append-left! lc (charcode->string key))
         (look-update lc)
         (look-update-preedit lc))
        ((look-completion-key? key state)
         (look-append-from-candidate-to-left! lc)
         (look-context-flush lc)
         (look-update-preedit lc))
        ((and (look-next-char-key? key state)
              (< 0 (look-get-length-left lc)))
         (look-append-char-from-candidate-to-left! lc)
         (look-update lc)
         (look-update-preedit lc))
        ((look-prev-char-key? key state)
         (cond ((<= (look-get-length-left lc) 0)
                (look-context-flush lc)
                ;; or (look-context-clean lc)
		(im-commit-raw lc))
               (else
                (look-remove-last-char-from-left! lc)))
         (look-update lc)
         (look-update-preedit lc))
        ((look-next-candidate-key? key state)
         (look-next-candidate! lc)
         (look-update-preedit lc))
        ((look-prev-candidate-key? key state)
         (look-prev-candidate! lc)
         (look-update-preedit lc))
        ((look-save-dict-key? key state)
         (look-save-personal-dict lc)
         (im-commit-raw lc)
         (look-context-flush lc)
         (look-update-preedit lc))
        ((look-load-dict-key? key state)
         (look-load-personal-dict lc)
         (im-commit-raw lc)
         (look-context-flush lc)
         (look-update-preedit lc))
        (else
         (im-commit-raw lc)
         (look-context-flush lc)
         (look-update-preedit lc))))

(define (look-key-press-state-direct lc key state)
  (cond ((look-off-key? key state)
         (look-context-clean lc)
         (look-update-preedit lc))
        ((look-alphabetic-char? key state)
         (look-context-set-left! lc (charcode->string key))
         (look-update lc)
         (look-update-preedit lc)
         (look-context-set-look! lc #t))
        ((look-save-dict-key? key state)
         (look-save-personal-dict lc)
         (im-commit-raw lc))
        ((look-load-dict-key? key state)
         (look-load-personal-dict lc)
         (im-commit-raw lc))
        (else
         (im-commit-raw lc))))

(define (look-key-press-state-sleep lc key state)
  (cond ((look-on-key? key state)
         (look-context-set-on! lc #t)
         (look-context-set-look! lc #f))
        (else
         (im-commit-raw lc))))

(define (look-key-press-handler lc key state)
  (if (look-context-on? lc)
      (if (look-context-look? lc)
          (look-key-press-state-look lc key state)
          (look-key-press-state-direct lc key state))
      (look-key-press-state-sleep lc key state)))

(define (look-key-release-handler lc key state)
  (im-commit-raw lc))

(define (look-reset-handler lc)
  #f)

;;(define (look-mode-handler lc mode)
;;  (create-context (look-context-id lc)
;;                  #f
;;                  (car (nth mode im-list)))
;;  #f)

(define (look-get-candidate-handler lc idx)
  #f)

(define (look-set-candidate-index-handler lc idx)
  #f)

(look-configure-widgets)

(register-im
 'look
 ""
 "UTF-8"
 (N_ "Look")
 (N_ "Tiny predictive input method")
 #f
 look-init-handler
 look-release-handler
 context-mode-handler
 look-key-press-handler
 look-key-release-handler
 look-reset-handler
 look-get-candidate-handler
 look-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
