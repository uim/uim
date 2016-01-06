;;; xkb.scm: interface to X Keyboard Extension
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

(require-extension (srfi 1 34))

(define xkb-plugin-ready?
  (require-dynlib "xkb"))

;; us pc104 keymap from Xkb
(define xkb-fallback-map
  '((9 ESC (256))
    (10 AE01 (49 33))
    (11 AE02 (50 64))
    (12 AE03 (51 35))
    (13 AE04 (52 36))
    (14 AE05 (53 37))
    (15 AE06 (54 94))
    (16 AE07 (55 38))
    (17 AE08 (56 42))
    (18 AE09 (57 40))
    (19 AE10 (48 41))
    (20 AE11 (45 95))
    (21 AE12 (61 43))
    (22 BKSP (258 258))
    (23 TAB (257 257))
    (24 AD01 (113 81))
    (25 AD02 (119 87))
    (26 AD03 (101 69))
    (27 AD04 (114 82))
    (28 AD05 (116 84))
    (29 AD06 (121 89))
    (30 AD07 (117 85))
    (31 AD08 (105 73))
    (32 AD09 (111 79))
    (33 AD10 (112 80))
    (34 AD11 (91 123))
    (35 AD12 (93 125))
    (36 RTRN (261))
    (37 LCTL (32769))
    (38 AC01 (97 65))
    (39 AC02 (115 83))
    (40 AC03 (100 68))
    (41 AC04 (102 70))
    (42 AC05 (103 71))
    (43 AC06 (104 72))
    (44 AC07 (106 74))
    (45 AC08 (107 75))
    (46 AC09 (108 76))
    (47 AC10 (59 58))
    (48 AC11 (39 34))
    (49 TLDE (96 126))
    (50 LFSH (32768))
    (51 BKSL (92 124))
    (52 AB01 (122 90))
    (53 AB02 (120 88))
    (54 AB03 (99 67))
    (55 AB04 (118 86))
    (56 AB05 (98 66))
    (57 AB06 (110 78))
    (58 AB07 (109 77))
    (59 AB08 (44 60))
    (60 AB09 (46 62))
    (61 AB10 (47 63))
    (62 RTSH (32768))
    (64 LALT (32770 32771))
    (65 SPCE (32))
    (66 CAPS (36864))
    (67 FK01 (307 307 307 307))
    (68 FK02 (308 308 308 308))
    (69 FK03 (309 309 309 309))
    (70 FK04 (310 310 310 310))
    (71 FK05 (311 311 311 311))
    (72 FK06 (312 312 312 312))
    (73 FK07 (313 313 313 313))
    (74 FK08 (314 314 314 314))
    (75 FK09 (315 315 315 315))
    (76 FK10 (316 316 316 316))
    (77 NMLK (36865))
    (78 SCLK (36866))
    (94 LSGT (60 62 124 166))
    (95 FK11 (317 317 317 317))
    (96 FK12 (318 318 318 318))
    (98 KATA (281))
    (99 HIRA (280))
    (100 HENK (278))
    (101 HKTG (282))
    (102 MUHE (277))
    (105 RCTL (32769))
    (108 RALT (32770 32771))
    (110 HOME (268))
    (111 UP (263))
    (112 PGUP (266))
    (113 LEFT (262))
    (114 RGHT (264))
    (115 END (269))
    (116 DOWN (265))
    (117 PGDN (267))
    (118 INS (260))
    (119 DELE (259))
    (130 HNGL (292))
    (131 HJCV (295))
    (133 LWIN (32772))
    (134 RWIN (32772))
    (203 MDSW (275))
    (204 ALT (32770))
    (205 META (32771))
    (206 SUPR (32772))
    (207 HYPR (32773))))

(define xkb-map #f)

(define (xkb-get-map refresh?)
  (or (and (not refresh?) xkb-map)
      (begin (set! xkb-map
		   (or (and xkb-plugin-ready? (xkb-lib-get-map))
		       (and xkb-save-map?
			    (guard (err
				    (else #f))
				   (call-with-input-file xkb-map-path read)))
		       xkb-fallback-map))
	     xkb-map)))

(define (xkb-get-groups-wrap-control)
  (or (and xkb-plugin-ready? (xkb-lib-get-groups-wrap-control))
      '(wrap-into-range 1)))

(define (xkb-get-group)
  (or (and xkb-plugin-ready? (xkb-lib-get-group))
      0))

(define (xkb-expand-map m groups-wrap ngroups)
  (map (lambda (k)
	 (let ((code-name (take k 2))
	       (groups (cddr k)))
	   (map (lambda (group) (append code-name group))
		(if (eq? groups-wrap 'wrap-into-range)
		    (take (apply circular-list groups) ngroups)
		    (let ((fallback
			   (if (eq? groups-wrap 'clamp-into-range)
			       (last groups)
			       (guard (err
				       (else (car groups)))
				      (list-ref groups groups-wrap)))))
		      (append
		       groups
		       (make-list (- ngroups (length groups)) fallback)))))))
       m))

(define (xkb-index-single-group-map-by-ukey . group-map)
  (fold-right
   (lambda (k alst)
     (let ((keycode (car k))
	   (xkbname (cadr k))
	   (ukeys (cddr k)))
       (append
	(reverse
	 (fold
	  (lambda (ukey alst)
	    (let ((shift-level (if (null? alst) 0 (+ 1 (cadar alst)))))
	      (cons (list ukey shift-level keycode xkbname) alst)))
	  '() ukeys))
	alst)))
   '() group-map))

(define (xkb-index-map-by-ukey m groups-wrap-control)
  (let ((groups-wrap (car groups-wrap-control))
	(ngroups (cadr groups-wrap-control)))
    (apply map (cons xkb-index-single-group-map-by-ukey
		     (xkb-expand-map m groups-wrap ngroups)))))
