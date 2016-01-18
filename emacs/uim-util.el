;; 
;;  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or
;;  without modification, are permitted provided that the
;;  following conditions are met:
;;
;;  1. Redistributions of source code must retain the above
;;     copyright notice, this list of conditions and the
;;     following disclaimer.
;;  2. Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the
;;     following disclaimer in the documentation and/or other
;;     materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its
;;     contributors may be used to endorse or promote products
;;     derived from this software without specific prior written
;;     permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

;; 
;; Get vertical distance from beginning-of-line of the target point
;; to the line of target.
;;
(defun uim-relative-vertical-count (target)
  (let (base (i 1))
    (save-excursion
      (goto-char target)
      (beginning-of-line)
      (setq base (point))
      (catch 'rv-loop
	(while t 
	  (goto-char base)
	  ;; move i lines from beginning-of-line
	  (when (> i (vertical-motion i))
	      ;; reach end-of-buffer 
	    (setq i (- i 1))
	    (throw 'rv-loop t))
	  (when (= (point) target)
	      ;; reach the target point
	    (throw 'rv-loop t))
	  (when (> (point) target)
	      ;; over the target point
	    (setq i (- i 1))
	    (throw 'rv-loop t))
	  (setq i (+ i 1)))))
    i ;; vertical distance
    ))


;;
;; Get vertical distance from start to end.
;;  Two points should be in the same line
;;
(defun uim-vertical-distance (start end)
  (let (dist1 dist2)
    (setq dist1 (uim-relative-vertical-count start))
    (setq dist2 (uim-relative-vertical-count end))
    (- dist2 dist1)))


;; 
;; My vertical-motion
;;   workaround to avoid Emacs-21.x vertical-motion bug
;;
(defun uim-vertical-motion-exec (n)
  (let (start (i 1))
    (setq start (point))
    ;; get distance from physical line head
    (setq i (uim-relative-vertical-count start))
    (beginning-of-line)
    (- (vertical-motion (+ i n)) i)
    ))

;;
;; My vertical-motion wrapper
;;
(defun uim-vertical-motion (n)
  (if (and uim-emacs
	   (>= emacs-major-version 21))
      (uim-vertical-motion-exec n)
    (vertical-motion n)))

;;
;; Fill blank at right edge of the window with white space
;;   The blank appears when a double-width character is
;;   inserted into the last one-character-space of the
;;   screen-line. (Emacs-21.x with X11 only?)
;; 
(defun uim-pad-space (start end)
  (let (strtmp pad (cnt 0) wwidth)
    (if (and uim-emacs
             window-system
             (>= emacs-major-version 21))
        (setq wwidth (window-width))
      (setq wwidth (- (window-width) 1)))

    (save-excursion
      (while (> (uim-string-width (setq strtmp (buffer-substring start end)))
                wwidth)
        (setq strtmp (truncate-string-to-width strtmp wwidth))
        (setq pad (- wwidth (uim-string-width strtmp)))
        (setq cnt (+ cnt pad))
        (goto-char (+ start (length strtmp)))
	(insert-before-markers (make-string pad 32))
        (setq start (point))
        (setq end (+ end pad))
        ))
    cnt))



;;
;; Replace tabs to the same width white spaces.
;;
(defun uim-replace-tab (start end)
  (let (tpos (cnt 0) (ofs 0) tabwidth)
    (save-excursion
      (goto-char start)
      (while (setq tpos (search-forward "\t" end 1))

	(if uim-emacs
	    (setq ofs (string-width (buffer-substring start (- tpos 1)))))
	(if uim-xemacs
	    (save-excursion
	      (goto-char (- tpos 1))
	      (vertical-motion 0)
	      (setq ofs (string-width (buffer-substring (point) (- tpos 1))))))

	(setq tabwidth (- tab-width (% ofs tab-width)))
	(setq cnt (- (+ cnt tabwidth) 1))
	(goto-char (- tpos 1))
	;;(insert-and-inherit (make-string tabwidth 32))
	(insert (make-string tabwidth 32))
	(delete-char 1)
	(setq end (+ end tabwidth (- 1) ))
	))
    cnt)) 



;;
;; Replace tabs and spaces.
;;
(defun uim-tab-pad-space (start end)
  (let (org lstart lend (endorg end) tabspace)
    (setq org (point))
    (save-excursion
      (goto-char start)
      (catch 'line-loop
        (while (< (point) end)
          (setq lstart (point))
          (save-excursion
            (end-of-line)
            (if (> (point) end)
                (setq lend end)
              (setq lend (point))))
          (setq tabspace (uim-replace-tab lstart lend))
          (setq end (+ end tabspace ))
          (setq lend (+ lend tabspace))
          (setq end (+ end (uim-pad-space lstart lend)))
          ;; fix lstart
          (end-of-line)
          (if (and (> (point-max) (point)) ;; not end of buffer
                   (< (point) end)) ;; not goal
              (forward-char 1)
            (throw 'line-loop t)))))
    (- end endorg)))




;;
;; My string-width for XEmacs
;;   XEmacs returns the displayed width of each tab
;; 
(defun uim-string-width (str)
  (if (not uim-xemacs)
      (string-width str)
    (let ((start 0) (cnt 0))
      (while (setq start (string-match "\n" str start))
	(setq start (match-end 0))
	(setq cnt (- cnt 1)))
      (setq start 0)
      (while (setq start (string-match "\t" str start))
	(setq start (match-end 0))
	(setq cnt (+ cnt (- tab-width 1))))
      (+ (string-width str) cnt))))




;;
;; Add white spaces.
;;
(defun uim-format-string (str width &optional right)
  (if right
      (concat (make-string (- width (uim-string-width str)) 32) str)
    (concat str (make-string (- width (uim-string-width str)) 32))))


;;
;; Search "target" from "lst" and append "add" after it.
;;
(defun uim-list-search-append (lst target add)
  (let ((new '()))
    (while lst
      (if (nlistp lst)
	  (setq new (append new lst))
	(if (nlistp (car lst))
	    (if (eq (car lst) target)
		(setq new (append new (list target add)))
	      (setq new (append new (list (car lst)))))

	  (setq new (append new (list (uim-list-search-append 
				       (car lst) 
				       target 
				       add))))))
      (if (listp lst)
	  (setq lst (cdr lst))
	(setq lst nil)))
    new))


;;
;; Remove "target" from "lst"
;;
(defun uim-list-search-delete (lst target)
  (let ((new '()) newtmp)
    (while lst
      (if (nlistp lst)
          (setq new (append new lst))
        (if (nlistp (car lst))
            (unless (eq (car lst) target)
              (setq new (append new (list (car lst)))))
          (setq newtmp (uim-list-search-delete (car lst) target))
          (if (not (equal newtmp nil))
              (setq new (append new (list newtmp))))))
      (if (listp lst)
          (setq lst (cdr lst))
        (setq lst nil )))
    new))



(defun uim-get-window-offset ()
  (let ((i (or (and (= (window-start) (point)) 0)
	       (- (count-lines (window-start) (point)) 1)))
	goal
	dummy-space
	)

    ;; for Tab
    (save-excursion
      (when (and (char-after)
		 (= (char-after) ?\t))
	(insert-char 32 1)
	(setq dummy-space t)
	))

    (save-excursion
      (uim-vertical-motion 0)
      (setq goal (point)))

    (save-excursion
      (catch 'offset-loop
	(while (< i (window-height))
	  (goto-char (window-start))
	  (uim-vertical-motion i)
	  (when (>= (point) goal)
	    (throw 'offset-loop nil)
	    i)
	  (setq i (+ i 1)))
	)
      )

    (if dummy-space
	(delete-char 1))
    i
    ))


(defun uim-goto-char (pt)
  (set-window-point (selected-window) pt)
  )


(defun uim-get-vector-from-head (vec n)
  (if (and (<= n (length vec))
	   (> n 0))
      (let ((vlist (append vec nil))
	    (ret '()))
	(while (> n 0)
	  (setq ret (append ret (list (car vlist))))
	  (setq vlist (cdr vlist))
	  (setq n (- n 1)))
	(vconcat ret))))

(defun uim-get-vector-from-tail (vec n)
  (if (and (<= n (length vec))
	   (> n 0))
      (vconcat (nthcdr (- (length vec) n) (append vec nil)))))

(defun uim-cut-vector-from-head (vec n)
  (if (<= n (length vec))
      (uim-get-vector-from-tail vec (- (length vec) n))))


(defun uim-vector-cdr (vec)
  (if (> (length vec) 1)
      (uim-get-vector-from-tail vec (- (length vec) 1))))

(defun uim-vector-car (vec)
  (uim-get-vector-from-head vec 1))

(defun uim-delete-atom (list)
  (if (and list 
	   (not (atom list)))
      (if (and (car list) 
	       (atom (car list)))
	  (uim-delete-atom (cdr list))
	(cons (car list) (uim-delete-atom (cdr list))))
    list))

(provide 'uim-util)
