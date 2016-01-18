;;; sj3v2-socket.scm: SJ3 protocol version 2 for uim.
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
(require "util.scm")
(require "i18n.scm")
(require "socket.scm")
(require "lolevel.scm")
(require "process.scm") ;; getpid

;; sj3v2 protocol operators
(define $SJ3_CONNECT        1)
(define $SJ3_DISCONNECT     2)
(define $SJ3_OPENDICT       11)
(define $SJ3_CLOSEDICT      12)
(define $SJ3_OPENSTDY       21)
(define $SJ3_CLOSESTDY      22)
(define $SJ3_STDYSIZE       23)
(define $SJ3_STUDY          61)
(define $SJ3_MAKEDICT       81)
(define $SJ3_MAKESTDY       82)
(define $SJ3_MAKEDIR        83)
(define $SJ3_ACCESS         84)
(define $SJ3_PH2KNJ_EUC     111)
(define $SJ3_CL2KNJ_ALL_EUC 115)
(define $SJ3_CL2KNJ_CNT_EUC 116)
(define $SJ3_CLSTUDY_EUC    117)

(define sj3-lib-error-str-alist
  `((-1  . ,(N_ "Internal server error."))    ;; SJ3_InternalError
    (0   . ,(N_ "No error."))                 ;; SJ3_NormalEnd
    (1   . ,(N_ "Serverdown."))               ;; SJ3_ServerDown
    (2   . ,(N_ "Cannot open socket."))       ;; SJ3_OpenSocket
    (3   . ,(N_ "Cannot connect socket."))    ;; SJ3_ConnectSocket
    (4   . ,(N_ "Unknown hostname."))         ;; SJ3_GetHostByName
    (5   . ,(N_ "Not opened."))               ;; SJ3_NotOpened
    (6   . ,(N_ "Not enough memory."))        ;; SJ3_NotEnoughMemory
    (7   . ,(N_ "Illegal command."))          ;; SJ3_IllegalCommand
    (11  . ,(N_ "Different version."))        ;; SJ3_DifferentVersion
    (12  . ,(N_ "No host name."))             ;; SJ3_NoHostName
    (13  . ,(N_ "No user name."))             ;; SJ3_NoUserName
    (14  . ,(N_ "User not allowd."))          ;; SJ3_NotAllowedUser
    (15  . ,(N_ "Already connected."))        ;; SJ3_AlreadyConnected
    (16  . ,(N_ "Not connected."))            ;; SJ3_NotConnected
    (21  . ,(N_ "Too long parameter."))       ;; SJ3_TooLongParameter
    (22  . ,(N_ "Illegal parameter."))        ;; SJ3_IllegalParameter
    (31  . ,(N_ "Bad dictionary ID."))        ;; SJ3_BadDictID
    (32  . ,(N_ "Illegal dictionary file."))  ;; SJ3_IllegalDictFile
    (33  . ,(N_ "Illegal study file."))       ;; SJ3_IllegalStdyFile
    (34  . ,(N_ "Incorrect password."))       ;; SJ3_IncorrectPasswd
    (35  . ,(N_ "File not exist."))           ;; SJ3_FileNotExist
    (36  . ,(N_ "Cannot access file."))       ;; SJ3_CannotAccessFile
    (37  . ,(N_ "Cannot open file."))         ;; SJ3_CannotOpenFile
    (38  . ,(N_ "Cannot create file."))       ;; SJ3_CannotCreateFile
    (39  . ,(N_ "File read error."))          ;; SJ3_FileReadError
    (40  . ,(N_ "File write error."))         ;; SJ3_FileWriteError
    (41  . ,(N_ "File seek error."))          ;; SJ3_FileSeekError
    (51  . ,(N_ "Study already opened."))     ;; SJ3_StdyAlreadyOpened
    (52  . ,(N_ "Study file not opened."))    ;; SJ3_StdyFileNotOpened
    (53  . ,(N_ "Too small study area."))     ;; SJ3_TooSmallStdyArea
    (61  . ,(N_ "Locked by other."))          ;; SJ3_LockedByOther
    (62  . ,(N_ "Not locked."))               ;; SJ3_NotLocked
    (71  . ,(N_ "No such dictionary."))       ;; SJ3_NoSuchDict
    (72  . ,(N_ "Dictionary is read only."))  ;; SJ3_ReadOnlyDict
    (73  . ,(N_ "Dictionary is locked."))     ;; SJ3_DictLocked
    (74  . ,(N_ "Yomi string is bad."))       ;; SJ3_BadYomiString
    (75  . ,(N_ "Kanji string is bad."))      ;; SJ3_BadKanjiString
    (76  . ,(N_ "Hinshi code is bad."))       ;; SJ3_BadHinsiCode
    (81  . ,(N_ "Add dictionary failed."))    ;; SJ3_AddDictFailed
    (82  . ,(N_ "Word is already exist."))    ;; SJ3_AlreadyExistWord
    (83  . ,(N_ "No more douon word."))       ;; SJ3_NoMoreDouonWord
    (84  . ,(N_ "No more user dictionary."))  ;; SJ3_NoMoreUserDict
    (85  . ,(N_ "No more index block"))       ;; SJ3_NoMoreIndexBlock
    (91  . ,(N_ "Delete dictionary failed.")) ;; SJ3_DelDictFailed
    (92  . ,(N_ "No such word."))             ;; SJ3_NoSuchWord
    (101 . ,(N_ "Directory already exist."))  ;; SJ3_DirAlreadyExist
    (102 . ,(N_ "Cannot create directory."))  ;; SJ3_CannotCreateDir
    (111 . ,(N_ "No more dictionary data."))  ;; SJ3_NoMoreDictData
    (121 . ,(N_ "User connected."))           ;; SJ3_UserConnected
    (131 . ,(N_ "Too long password."))        ;; SJ3_TooLongPasswd
    (132 . ,(N_ "Too long comment."))         ;; SJ3_TooLongComment
    (133 . ,(N_ "Cannot code convert."))))    ;; SJ3_CannotCodeConvert


(define sj3-protocol-version 2)

;;
;; sj3 protocol api
;;
(define (sj3-lib-connect socket user)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 u32 s8 s8 s8)
                            $SJ3_CONNECT sj3-protocol-version
                            "unix" user (format "~a.uim-sj3" (current-process-id)))))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= -2 (u32->s32 result)))))

(define (sj3-lib-disconnect socket)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32) $SJ3_DISCONNECT)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= 0 result))))

(define (sj3-lib-opendict socket dictionary-name passwd)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 s8) $SJ3_OPENDICT
                            dictionary-name passwd)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (and (= result 0)
          (call-with-u8list-unpack
           '(u32) (string-buf->u8list (file-read socket 4))
           (lambda (result)
             result))))))

(define (sj3-lib-closedict socket dict-id)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 u32) $SJ3_CLOSEDICT dict-id)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= 0 result))))

(define (sj3-lib-openstdy socket stdy-name)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 s8) $SJ3_OPENSTDY stdy-name "")))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     result)))

(define (sj3-lib-closestdy socket)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32) $SJ3_CLOSESTDY)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     result)))

(define (sj3-lib-stdy-size socket)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32) $SJ3_STDYSIZE)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (and (= result 0)
          (call-with-u8list-unpack
           '(u32) (string-buf->u8list (file-read socket 4))
           (lambda (result)
             result))))))

(define (sj3-lib-study socket stdy)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 u8list) $SJ3_STUDY stdy)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     result)))

(define (sj3-lib-makedict socket dictionary-name)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 u32 u32 u32) $SJ3_MAKEDICT
                            dictionary-name
                            2048  ; Index length
                            2048  ; Length
                            256   ; Number
                            )))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= 0 result))))

(define (sj3-lib-makestdy socket stdy-name)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 u32 u32 u32) $SJ3_MAKESTDY
                            stdy-name
                            2048  ; Number
                            1     ; Step
                            2048  ; Length
                            )))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= 0 result))))

(define (sj3-lib-makedir socket directory-name)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8) $SJ3_MAKEDIR directory-name)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     result)))

(define (sj3-lib-access? socket directory-name mode)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 u32) $SJ3_ACCESS
                            directory-name
                            mode)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (= 0 result))))

(define (sj3-lib-ph2knj-euc socket stdy-size yomi)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8) $SJ3_PH2KNJ_EUC yomi)))
  (call-with-u8list-unpack
   '(u32 u32) (string-buf->u8list (file-read socket 8))
   (lambda (result yomi-length)
     (and (= result 0)
          (let loop ((yomi-len (cons (car (string-buf->u8list (file-read socket 1)))
                                     '()))
                     (rest-stdy '())
                     (rest-kouho '()))
            (if (<= (car yomi-len) 0)
                (values (reverse yomi-len) (reverse rest-stdy) (reverse rest-kouho))
                (let* ((new-stdy (string-buf->u8list (file-read socket stdy-size)))
                       (new-kouho (file-read-string-with-terminate socket #\nul)))
                  (loop (cons (car (string-buf->u8list (file-read socket 1)))
                              yomi-len)
                        (cons new-stdy rest-stdy)
                        (cons new-kouho rest-kouho)))))))))

(define (sj3-lib-cl2knj-all-euc socket stdy-size len yomi)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 u32 s8) $SJ3_CL2KNJ_ALL_EUC len yomi)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (and (= result 0)
          (let loop ((yomi-len
                      (cons (u8list->u32 (string-buf->u8list (file-read socket 4)))
                            '()))
                     (rest-stdy '())
                     (rest-kouho '()))
            (if (<= (car yomi-len) 0)
                (values (reverse yomi-len) (reverse rest-stdy) (reverse rest-kouho))
                (let* ((new-stdy (string-buf->u8list (file-read socket stdy-size)))
                       (new-kouho (file-read-string-with-terminate socket #\nul)))
                  (loop (cons (u8list->u32 (string-buf->u8list (file-read socket 4)))
                              yomi-len)
                        (cons new-stdy rest-stdy)
                        (cons new-kouho rest-kouho)))))))))

(define (sj3-lib-cl2knj-cnt-euc socket stdy-size len yomi)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 u32 s8) $SJ3_CL2KNJ_CNT_EUC len yomi)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     (and (= result 0)
          (call-with-u8list-unpack
           '(u32) (string-buf->u8list (file-read socket 4))
           (lambda (result)
             result))))))

(define (sj3-lib-clstudy-euc socket yomi1 yomi2 stdy)
  (file-write socket
              (u8list->string-buf
               (u8list-pack '(u32 s8 s8 u8list) $SJ3_CLSTUDY_EUC
                            yomi1 yomi2 stdy)))
  (call-with-u8list-unpack
   '(u32) (string-buf->u8list (file-read socket 4))
   (lambda (result)
     result)))


;;
;; helper functions
;;
(define (sj3-lib-mkdir-p socket path)
  (let ((entries (string-split path "/")))
    (fold (lambda (acc rest)
            (let ((new-path (if (string=? rest "")
                                acc
                                (string-append rest "/" acc))))
              (if (not (sj3-lib-access? socket acc 0))
                  (sj3-lib-makedir socket new-path))
              new-path))
          ""
          entries)))

(define (sj3-lib-split-yomi yomi yomi-length-list)
  (let loop ((yomi yomi)
             (yomi-length-list yomi-length-list)
             (rest '()))
    (if (= (car yomi-length-list) 0)
        (reverse rest)
        (loop (substring yomi (car yomi-length-list) (string-length yomi))
              (cdr yomi-length-list)
              (cons (substring yomi 0 (car yomi-length-list)) rest)))))


;;
;; sj3lib compatible functions
;;

(define *sj3-lib-socket* #f)
(define *sj3-lib-stdy-size* 20)
(define *sj3-lib-main-dict* #f)
(define *sj3-lib-user-dict* #f)

(define (sj3-lib-get-private-path user-name)
  (format "user/~a" user-name))
(define (sj3-lib-get-private-dicionary-name user-name)
  (format "~a/private.dic" (sj3-lib-get-private-path user-name)))
(define (sj3-lib-get-private-study-name user-name)
  (format "~a/study.dat" (sj3-lib-get-private-path user-name)))

(define (sj3-lib-open-with-server server)
  (let ((server-name (if (equal? server "")
                         "localhost")))
    (if sj3-use-remote-server?
        (tcp-connect server-name 3086)
        (unix-domain-socket-connect sj3-unix-domain-socket-path))))

(define (sj3-lib-open server user-name)
  (set! *sj3-lib-socket* (sj3-lib-open-with-server server))
  (if *sj3-lib-socket*
      (begin
        (if (not (sj3-lib-connect *sj3-lib-socket* user-name))
            (raise (N_ "Cannot connect SJ3 server")))
        (set! *sj3-lib-main-dict* (sj3-lib-opendict *sj3-lib-socket* "sj3main.dic" ""))
        (if (not (sj3-lib-access? *sj3-lib-socket* (sj3-lib-get-private-path user-name) 0))
            (begin
              (sj3-lib-mkdir-p *sj3-lib-socket* (sj3-lib-get-private-path user-name))
              (sj3-lib-makedict *sj3-lib-socket* (sj3-lib-get-private-dicionary-name user-name))
              (uim-notify-info (N_ "SJ3: create new dictionary"))))
        (if (not (sj3-lib-access? *sj3-lib-socket* (sj3-lib-get-private-study-name user-name) 0))
            (sj3-lib-makestdy *sj3-lib-socket* (sj3-lib-get-private-study-name user-name)))
        (set! *sj3-lib-user-dict*
              (sj3-lib-opendict *sj3-lib-socket* (sj3-lib-get-private-dicionary-name user-name) ""))
        (sj3-lib-openstdy *sj3-lib-socket* (sj3-lib-get-private-study-name user-name))
        (set! *sj3-lib-stdy-size* (sj3-lib-stdy-size *sj3-lib-socket*)))
      (uim-notify-info (N_ "Cannot connect SJ3 server")))
  *sj3-lib-socket*)

(define (sj3-lib-opened?)
  *sj3-lib-socket*)

(define (sj3-lib-close)
  (if *sj3-lib-socket*
      (begin
        (sj3-lib-closestdy *sj3-lib-socket*)
        (sj3-lib-closedict *sj3-lib-socket* *sj3-lib-user-dict*)
        (sj3-lib-closedict *sj3-lib-socket* *sj3-lib-main-dict*)
        (sj3-lib-disconnect *sj3-lib-socket*)
        (file-close *sj3-lib-socket*))))

(define (sj3-lib-getkan yomi)
  (if *sj3-lib-socket*
      (receive (yomi-len stdy cands)
        (sj3-lib-ph2knj-euc *sj3-lib-socket* *sj3-lib-stdy-size* yomi)
        (cons (apply string-append cands)
             (zip (sj3-lib-split-yomi yomi yomi-len)
                   cands
                   stdy)))
      #f))

(define (sj3-lib-douoncnt yomi)
  (if *sj3-lib-socket*
      (sj3-lib-cl2knj-cnt-euc *sj3-lib-socket* *sj3-lib-stdy-size*
			      (length (string->list yomi)) ;; byte length
			      yomi)
      0))

(define (sj3-lib-getdouon yomi)
  (receive (yomi-len stdy cand)
      (sj3-lib-cl2knj-all-euc *sj3-lib-socket* *sj3-lib-stdy-size*
                              (length (string->list yomi)) ;; byte length
                              yomi)
    (zip cand stdy)))

(define (sj3-lib-get-nth-douon yomi nth)
  (receive (yomi-len stdy cand)
      (sj3-lib-cl2knj-all-euc *sj3-lib-socket* *sj3-lib-stdy-size*
                              (length (string->list yomi)) ;; byte length
                              yomi)
    (list (list-ref cand nth)
          (list-ref stdy nth))))

(define (sj3-lib-gakusyuu stdy)
  (sj3-lib-study *sj3-lib-socket* stdy))

(define (sj3-lib-gakusyuu2 yomi1 yomi2 stdy)
  (let ((new-yomi1 (or yomi1 ""))
        (new-yomi2 (or yomi2 "")))
    (sj3-lib-clstudy-euc *sj3-lib-socket*
                         new-yomi1 new-yomi2
                         stdy)))

