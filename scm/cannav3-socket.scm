;;; cannav3-socket.scm: Canna protocol version 3 for uim.
;;;
;;; Copyright (c) 2009-2013 uim Project https://github.com/uim/uim
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

(use srfi-1)
(require "socket.scm")
(require "lolevel.scm")

;; canna protocol operators
(define canna-lib-initialize-op          #x1)
(define canna-lib-finalize-op            #x2)
(define canna-lib-create-context-op      #x3)
(define canna-lib-close-context-op       #x5)
(define canna-lib-get-dictionary-list-op #x6)
(define canna-lib-mount-dictionary-op    #x8)
(define canna-lib-unmount-dictionary-op  #x9)
(define canna-lib-begin-convert-op       #xf)
(define canna-lib-end-convert-op         #x10)
(define canna-lib-get-candidacy-list-op  #x11)
(define canna-lib-get-yomi-op            #x12)
(define canna-lib-resize-pause-op        #x1a)

(define (canna-var&user-fmt user)
  (format "3.3:~a" user))

(define (canna-lib-initialize socket user)
  (let* ((canna-var&user (canna-var&user-fmt user))
         (canna-var&user-len (+ 1 (string-length canna-var&user))))
    (file-write socket
                (u8list->string-buf
                 (u8list-pack '(u32 u32 s8)
                              canna-lib-initialize-op canna-var&user-len canna-var&user)))
    (call-with-u8list-unpack
     '(u16 u16) (string-buf->u8list (file-read socket 4))
     (lambda (major minor)
       (not (and (= major 65535) (= major 65535)))))))

(define (canna-lib-finalize socket)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16)
                            canna-lib-finalize-op 0 0)))
  (call-with-u8list-unpack
   '(u32 u8) (string-buf->u8list (file-read socket 5))
        (lambda (dummy result)
          (= result 0))))

(define (canna-lib-create-context socket)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16)
                            canna-lib-create-context-op 0 0)))
  (call-with-u8list-unpack
   '(u32 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy context-id)
     (and (not (= context-id 65535))
          context-id))))

(define (canna-lib-close-context socket context-id)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16)
                            canna-lib-close-context-op 0 2 context-id)))
  (call-with-u8list-unpack
   '(u32 u8) (string-buf->u8list (file-read socket 5))
   (lambda (dummy result)
     (not (= result 255)))))

(define (canna-lib-get-dictionary-list socket context-id)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16 u16)
                            canna-lib-get-dictionary-list-op 0 4 context-id 1024)))
  (call-with-u8list-unpack
   '(u32 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy result)
     (and (not (= result 65535))
          (call-with-u8list-unpack
           (make-list result 's8) (string-buf->u8list (file-read socket 1024))
           (lambda dict-list
             dict-list))))))

(define (canna-lib-mount-dictionary socket context-id dict mode)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u32 u16 s8)
                            canna-lib-mount-dictionary-op
                            0
                            (+ (string-length dict) 7)
                            mode context-id dict)))
  (call-with-u8list-unpack
   '(u32 u8) (string-buf->u8list (file-read socket 5))
   (lambda (dummy result)
     (not (= result 255)))))

(define (canna-lib-unmount-dictionary socket context-id dict mode)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u32 u16 s8)
                            canna-lib-unmount-dictionary-op
                            0
                            (+ (string-length dict) 7)
                            mode context-id dict)))
  (call-with-u8list-unpack
   '(u32 u8) (string-buf->u8list (file-read socket 5))
   (lambda (dummy result)
     (not (= result 255)))))

(define (canna-lib-begin-convert socket context-id yomi mode)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u32 u16 s16)
                            canna-lib-begin-convert-op
                            0
                            (+ (string-length yomi) 8)
                            mode context-id yomi)))
  (call-with-u8list-unpack
   '(u16 u16 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy len bunsetsu)
     (and (not (= bunsetsu 65535))
          (call-with-u8list-unpack
           (make-list bunsetsu 's16) (string-buf->u8list (file-read socket len))
           (lambda conv
             conv))))))

(define (canna-lib-end-convert socket context-id cands mode)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16 u16 u32 u16list)
                            canna-lib-end-convert-op
                            0
                            (+ (* 2 (length cands)) 8)
                            context-id (length cands) mode
                            cands)))
  (call-with-u8list-unpack
   '(u32 u8) (string-buf->u8list (file-read socket 5))
   (lambda (dummy result)
     (not (= result 255)))))

(define (canna-lib-get-candidacy-list socket context-id bunsetsu-pos)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16 u16 u16)
                            canna-lib-get-candidacy-list-op
                            0
                            6
                            context-id bunsetsu-pos 1024)))
  (call-with-u8list-unpack
   '(u16 u16 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy len cands)
     (call-with-u8list-unpack
      (make-list cands 's16) (string-buf->u8list (file-read socket len))
      (lambda cand-list
        cand-list)))))

(define (canna-lib-get-yomi socket context-id bunsetsu-pos)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16 u16 u16)
                            canna-lib-get-yomi-op
                            0
                            6
                            context-id bunsetsu-pos 1024)))
  (call-with-u8list-unpack
   '(u16 u16 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy len yomi-len)
     (call-with-u8list-unpack
      '(s16) (string-buf->u8list (file-read socket len))
      (lambda (conv)
        conv)))))

(define (canna-lib-resize-pause socket context-id yomi-length bunsetsu-pos)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u8 u8 u16 u16 u16 u16)
                            canna-lib-resize-pause-op
                            0
                            6
                            context-id bunsetsu-pos yomi-length)))
  (call-with-u8list-unpack
   '(u16 u16 u16) (string-buf->u8list (file-read socket 6))
   (lambda (dummy len bunsetsu)
     (and (not (= bunsetsu 65535))
          (let loop ((s16list (string-buf->u8list (file-read socket len)))
                     (rest '()))
            (if (equal? s16list '(0 0))
                (reverse rest)
                (let ((s16 (u8list-unpack '(s16) s16list)))
                  (loop (drop s16list (+ 2 (string-length (car s16))))
                        (cons (car s16) rest)))))))))

;;
;; RK compatible functions
;;
(define canna-lib-context-rec-spec
  (list
   (list 'id   #f)
   (list 'mode 0)
   (list 'nostudy #f)
   (list 'cands '())
   (list 'nth-cands '#())
   (list 'dic-list '())))
(define-record 'canna-lib-context canna-lib-context-rec-spec)
(define canna-lib-context-new-internal canna-lib-context-new)

(define *canna-lib-socket* #f)
(define *canna-lib-context-list* '())
(define canna-lib-cannaserver #f)

(define (canna-lib-open-with-server server)
  (let ((server-name (if (equal? server "")
                         "localhost"
                         server)))
    (if canna-server-name
        (tcp-connect server-name "canna")
        (unix-domain-socket-connect "/tmp/.iroha_unix/IROHA"))))

(define (canna-lib-init server)
  (set! canna-lib-cannaserver server)
  (and (not *canna-lib-socket*)
       (let ((s (canna-lib-open-with-server server)))
         (and s
              (begin
                (canna-lib-initialize s canna-user-name)
                (set! *canna-lib-socket* s)
                #t)))))

(define (canna-lib-alloc-context)
  (if (and (not *canna-lib-socket*)
           (not (canna-lib-init canna-lib-cannaserver)))
      (begin
        (uim-notify-fatal (N_ "Initialize failed."))
        #f)
      (and-let* ((cic (canna-lib-context-new-internal))
                 (id (canna-lib-create-context *canna-lib-socket*))
                 (dic-list (canna-lib-context-set-dic-list!
                            cic
                            (canna-lib-get-dictionary-list *canna-lib-socket* id)))
                 (mode 19))  ;; XXX: (RK_XFER << RK_XFERBITS) | RK_KFER
        (canna-lib-context-set-id! cic id)
        (canna-lib-context-set-mode! cic mode)
        (map (lambda (dict)
               (canna-lib-mount-dictionary *canna-lib-socket* id dict 0))
             dic-list)
        (set! *canna-lib-context-list*
              (cons cic *canna-lib-context-list*))
        cic)))

(define (canna-lib-release-context cic)
  (set! *canna-lib-context-list* (delete! cic *canna-lib-context-list* equal?))
  (canna-lib-close-context *canna-lib-socket*
                           (canna-lib-context-id cic)))

(define (canna-lib-begin-conversion cic str)
  (let ((cands (canna-lib-begin-convert *canna-lib-socket*
                                        (canna-lib-context-id cic)
                                        str
                                        (canna-lib-context-mode cic))))
    (canna-lib-context-set-cands! cic cands)
    (canna-lib-context-set-nth-cands! cic (make-vector (length cands) 0))
    (length cands)))

(define (canna-lib-get-nth-candidate cic seg nth)
  (vector-set! (canna-lib-context-nth-cands cic) seg nth)
  (list-ref (canna-lib-get-candidacy-list *canna-lib-socket*
                                          (canna-lib-context-id cic)
                                          seg)
            nth))

(define (canna-lib-get-unconv-candidate cic seg)
  (canna-lib-context-set-nth-cands! cic (vector-set! (canna-lib-context-nth-cands cic) seg 0))
  (canna-lib-get-yomi *canna-lib-socket*
                      (canna-lib-context-id cic)
                      seg))

(define (canna-lib-resize-segment cic seg delta)
  (let* ((direct (if (< 0 delta)
                    -1
                    -2))
         (new-cands (canna-lib-resize-pause *canna-lib-socket*
                                            (canna-lib-context-id cic) direct seg))
         (len (length new-cands))
         (new-nth-cands (make-vector (+ seg len) 0)))
    ;; save unconverted segments
    (for-each (lambda (n)
                (vector-set! new-nth-cands
                             n
                             (vector-ref (canna-lib-context-nth-cands cic) n)))
              (iota seg))
    (canna-lib-context-set-cands! cic new-cands)
    (canna-lib-context-set-nth-cands! cic new-nth-cands)
    #t))

(define (canna-lib-get-nr-segments cic)
  (vector-length (canna-lib-context-nth-cands cic)))

(define (canna-lib-get-nr-candidates cic seg)
  (length (canna-lib-get-candidacy-list *canna-lib-socket*
                                        (canna-lib-context-id cic)
                                        seg)))

(define (canna-lib-commit-segment cic seg nth)
  (let ((nth-cands (vector->list (canna-lib-context-nth-cands cic)))
        (learn (if (canna-lib-context-nostudy cic)
                   0
                   1)))
    (canna-lib-end-convert *canna-lib-socket*
                           (canna-lib-context-id cic)
                           nth-cands
                           learn)))

(define (canna-lib-reset-conversion cic)
  (let ((nth-cands (vector->list (canna-lib-context-nth-cands cic))))
    (canna-lib-end-convert *canna-lib-socket*
                           (canna-lib-context-id cic)
                           nth-cands
                           0)))
