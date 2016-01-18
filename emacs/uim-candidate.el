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
;; Checks that the candidate list can be displayed inlinely on current window.
;;
(defun uim-check-candidate-space ()
  ;; don't show the candidate in mini-buffer
  ;; also thin window is not supported
  (and (= (minibuffer-depth) 0)
       (>= (window-width) uim-candidate-minimum-width)))


;; 
;; Merge formatted text into current buffer
;;
(defun uim-merge-candidate ()

  (let ((maxwidth (string-width (nth 1 (car uim-candidate-line-list))))
	offset mark-base mark-cursor mark-limit)

    ;; format page label
    (setq uim-candidate-page-label
	  (concat (make-string (- maxwidth 
				  (- (string-width uim-candidate-page-label) 1))
			       (if uim-candidate-display-frame ?- 32))
		  uim-candidate-page-label))

    ;; make mark for chasing
    (setq mark-cursor (point-marker))

    (goto-char uim-candidate-start)

    ;; mark current-point
    (setq mark-base (point-marker))

    ;; save font-lock face
    (when uim-xemacs
      (uim-overwrite-font-face uim-candidate-original-start
			       uim-candidate-original-end))

    ;; save original string
    (setq uim-candidate-original-str
	  (buffer-substring uim-candidate-original-start
			    uim-candidate-original-end))

    (save-excursion
      (goto-char uim-candidate-original-end)
      (setq mark-limit (point-marker)))

    ;; remove tabs/spaces
    (uim-tab-pad-space uim-candidate-original-start
		       uim-candidate-original-end)

 
    ;; update uim-candidate-start
    (let (base-ofs)
      (setq base-ofs (- (marker-position mark-base) uim-candidate-start))
      (setq uim-candidate-start (+ uim-candidate-start base-ofs)))

    (set-marker mark-base nil)

    (goto-char uim-candidate-start)

    (save-excursion
      (uim-vertical-motion 0)
      (setq offset 
	    (uim-string-width (buffer-substring (point) uim-candidate-start))))

    ;; if offset + maxwidth >= window-width then reduce offset
    (if (>= (+ offset maxwidth 2) (window-width))
	(setq offset (- (- (window-width) 1) 
			(+ maxwidth 2))))


    (save-excursion
      (if uim-show-candidate-upward
	  (uim-vertical-motion (- (+ (length uim-candidate-line-list) 1)))
	(uim-vertical-motion uim-candidate-vofs))

      (let ((rest t)
	    mergecount vhead linetmp padding overflow)
	
	(save-excursion
	  (setq mergecount (uim-vertical-motion 
			       (+ (length uim-candidate-line-list) 1))))


	(while rest
	  (let (candidx candstr candsel)
	    (if uim-candidate-line-list
		(progn
		  ;; normal candidate line
		  (setq candidx (nth 0 (car uim-candidate-line-list)))
		  (setq candstr 
			(uim-format-string (nth 1 (car uim-candidate-line-list))
					   maxwidth))
		  (setq candsel (nth 2 (car uim-candidate-line-list)))

		  (setq candstr
			(if uim-candidate-display-frame
			    (if candsel
				(concat ">" candstr "<")
			      (concat "|" candstr "|"))
			  (if candsel
			      (concat "[" candstr "]")
			    (concat " " candstr " "))))
		  )
	      ;; page label
	      (setq candidx 0)

	      (setq candstr
		    (if uim-candidate-display-frame
			(concat "+" uim-candidate-page-label )
		      (concat " " uim-candidate-page-label )))
	      (setq candsel nil)
	      )

	    (if (> mergecount 0)
		(progn
		  ;; go virtual head of the next line
		  (if (not uim-show-candidate-upward)
		      (uim-vertical-motion 1))

		  (setq vhead (point))

		  (if (<= (save-excursion
			    (end-of-line)
			    (point))
			  (marker-position mark-limit))
		  
		      ;; make new line
		      (save-excursion

			(end-of-line)
		    
			;; |ABCDEFGH\ + |___aaa__|
			;; |IJKLMN  |
			;;
			;; get region from vhead to physical end
			;;
			;; linetmp = ABCDEFGH
			;; 
			(setq linetmp 
			      (truncate-string-to-width (buffer-substring vhead (point))
							(window-width)))

			;; make padding by truncating linetmp to offset size
			;;  (add space as padding if shortage)
			;;   witdth of padding is equal to offset
			;; 
			;; padding = ABC
			;;
			(setq padding
			      (truncate-string-to-width linetmp offset nil 32))
		      
			;;  overflow = GH
			;;
			(let ((candwidth (+ maxwidth 2)))

			  (if (>= (uim-string-width linetmp) (+ offset candwidth))
			      (setq overflow 
				    (truncate-string-to-width linetmp 
							      (uim-string-width linetmp)
							      (+ offset candwidth) 32))
			    (setq overflow "")))

			;; index string selected appendix
			(save-excursion
			  (goto-char vhead)
			  (delete-region vhead (+ vhead (length linetmp)))
			
			  (when uim-xemacs
			    (insert " ")
			    (remove-text-properties (- (point) 1) (point)
						    '(face nil))
			    (goto-char (- (point) 1)))

			  (insert (concat padding 
					  candstr
					  overflow))

			  (when uim-xemacs
			    (delete-char 1))
		      
			  )
		
			(uim-set-candidate-face candidx candsel 
						(+ vhead (length padding))
						(length candstr))


			;; update merge-end
			(save-excursion
			  (end-of-line) 
			  (setq uim-candidate-end 
			      (if (> (marker-position mark-limit) (point))
				  (marker-position mark-limit)
				(point))))
	      
			(setq mergecount (- mergecount 1))
			)
		    )

		  (if uim-show-candidate-upward
		      (uim-vertical-motion 1))

		  )
	      ;; append 
	      (let (vhead)

		(goto-char (point-max))

		(setq vhead (+ (point) 1))

		(insert (concat "\n"
				(make-string offset 32)
				candstr))

		(uim-set-candidate-face candidx candsel 
					(+ vhead offset)
					(length candstr))

		(save-excursion
		  (end-of-line)
		  (setq uim-candidate-end (point)))

		)))
	    
	  (if uim-candidate-line-list
	      (setq uim-candidate-line-list
		    (cdr uim-candidate-line-list))
	    (setq rest nil))

	  )
	
	(force-mode-line-update)

	;; update cursor position
	(goto-char (marker-position mark-cursor))
	(setq uim-candidate-cursor (point))

	;; move to end of preedit
	(if uim-show-candidate-upward
	    (save-excursion
	      (end-of-line)
	      (setq uim-candidate-end (point))))

	(set-marker mark-cursor nil)

	))
    ))



(defun uim-echo-candidate (cand)

  ;; display candidate in minibuffer 
  (let* ((cands "")
	(selstart 0)
	(selend 0)
	(page-current (format "%d" (caar cand)))
	(page-total (format "%d" (cdar cand)))
	 (cand-height 0)
	 (displayed nil)
	 (echo-max-height 1)
	 (page-space (- (string-width page-total) 
			(string-width page-current)))
	 (page-label (concat (if (> page-space 0) 
				 (make-string page-space 32))
			     page-current  "/" page-total " "))
	 cands-tmp
	 (uim-candidate-in-echo-region t)
	)

    (setq cand (cdr cand))


    (mapcar 
     '(lambda (x)
	(let ((selected (nth 0 x))
	      (candlabel (nth 1 x))
	      (candstr (nth 2 x)))

	  (if selected
	      (setq selstart (length cands)))

	  (setq cands
		(if selected
		    (concat cands "[" candlabel "." candstr "]")
		  (concat cands " "  candlabel "." candstr " ")))

	  (if selected
	      (setq selend (length cands)))

	  )
	) cand)

    
    (setq cands-tmp (concat page-label cands))
    
    (if (and uim-allow-resize-echo-region
	     uim-emacs
	     (>= emacs-major-version 21))
	;; Emcas-21 or Emacs-22
	(let (message-log-max)

	  (save-excursion
	    (set-buffer (get-buffer-create uim-el-candidates-buffer-name))
	    (erase-buffer)
	    (insert cands-tmp)
	    (setq cand-height (+ (uim-vertical-distance 1 (buffer-end 1)) 1))
	    (let ((current-minibuf (window-buffer (minibuffer-window))))
	      (set-window-buffer (minibuffer-window) 
				 uim-el-candidates-buffer-name)
	      (set-buffer (window-buffer (minibuffer-window)))
	      ;; estimate height of candidates in mini buffer
	      (setq cand-height (+ (uim-vertical-distance 1 (buffer-end 1)) 1))
	      (set-window-buffer (minibuffer-window) current-minibuf))

	    (message cands-tmp)
	    (if (= (setq echo-max-height (window-height (minibuffer-window)))
		   cand-height)
		(setq displayed t)))))
    
    (if (not displayed)
	;; Emacs-20 or XEmacs or uim-allow-resize-echo-region is nil 
	;; or too small frame
	(let* ((trimmed "")
	     (page-width (string-width page-label))
	       (page-cands-width (string-width cands-tmp))
	     (cands-width (string-width cands))
	       (candreg-width (- (- (* (window-width) echo-max-height) 1)
				 page-width)))

	  (cond ((>= candreg-width cands-width)
		 (setq trimmed cands-tmp))

		((<= (window-width) page-width)
		 ;; this part might not be reached
		 (setq trimmed (truncate-string-to-width page-label
							 (- (window-width) 1))))

		((<= (window-width) (+ page-width 6))
		 (setq trimmed (concat page-label
				       (make-string 
					(- (window-width) page-width 1)

					?.))))

	      ((= selstart 0)
	       ;; | 10/134 [1.xxxxx] 2.yyyyy 3.zzzzz ...|
	       (setq cands 
		     (truncate-string-to-width cands
						 (if (> candreg-width 3)
						     (- candreg-width 3)
						 0)))
		 (setq trimmed
		     (concat page-label
			     cands
			       (make-string (- candreg-width
					     (string-width cands))
					  ?.)))

	       )

		((<= (string-width (substring cands selstart))
		     (- candreg-width 3)) ;; at least 1 dot
	       ;; | 10/134 ...yyy [3.zzzzz] 
	       (setq cands
		     (truncate-string-to-width cands
					       cands-width 
						 (+ (- cands-width 
						       candreg-width) 3)))

		 (setq trimmed
		     (concat page-label
			       (make-string (- candreg-width 
					     (string-width cands))
					  ?.)
			     cands)))
	      (t
	       ;; | 10/134 ...[3.zzzzz] ---- ...|
	       (setq cands 
		     (concat "..."
			     (truncate-string-to-width (substring cands selstart)
							 (if (> candreg-width 6)
							     (- candreg-width 6)
							   0))))
		 (setq trimmed
		     (concat page-label
			     cands
			       (make-string (- candreg-width 
					     (string-width cands))
					    ?.)))))
      (let (message-log-max)
	    (message trimmed))))
    )
  )


;;
;; Make candidate list to be displayed
(defun uim-make-candlist (cand) 
  ;; cand: (( <nil|t> "candlabel"  "candstr" ) ... )

  (let* ((i 1)
	 (truncwidth (min (- (window-width) 3)
			  (max (+ uim-max-candlabel uim-max-candstr 1)
			       (- (string-width uim-candidate-page-label) 1))))
	 (candlist '()))

    (mapcar 
     '(lambda (x)
	(let ((selected (nth 0 x))
	      (candlabel (nth 1 x))
	      (candstr (nth 2 x)))

	  (setq candlabel 
		(concat (make-string (- uim-max-candlabel 
					(string-width candlabel)) 32)
			candlabel))

	  (setq candstr (concat candlabel "." candstr))
	
	  (catch 'truncate-loop
	    (let (candlabelpad)
	      (while t
		(let* ((trunc (truncate-string-to-width candstr
							truncwidth nil))
		       (width-truncated (string-width trunc))
		       )
		
		  (setq candlist 
			(append candlist 
				(list 
				 (list i (concat trunc 
						 (make-string (- truncwidth
								 width-truncated)
							      32))
				       selected))))

		  (if (= width-truncated (string-width candstr))
		      (throw 'truncate-loop t))

		  (if (not candlabelpad)
		      (setq candlabelpad
			    (make-string (+ (string-width candlabel) 1) 32)))

		  (setq candstr
			(concat candlabelpad
				(truncate-string-to-width candstr
							  (string-width candstr)
							  width-truncated)))
		  ))))
	  )
	(setq i (+ i 1))
	) cand)

    (setq uim-candidate-line-list candlist)
    )
  )
  


;;
;; Put overlay
;;
(defun uim-set-candidate-face (index selected begin length)
  (let (face)
    (cond 
     (selected
      (setq face 'uim-candidate-selected-face))
     ((= index 0)
      (setq face 'uim-candidate-nth-face))
     ((= (% index 2) 1)
      (setq face 'uim-candidate-odd-face))
     ((= (% index 2) 0)
      (setq face 'uim-candidate-even-face)))

    (put-text-property begin (+ begin length) 'face face)

    ))




(defun uim-check-overlay (head tail)
  (if (> head (point-min))
      (setq head (- head 1)))

  (if (< tail (point-max))
      (setq tail (+ tail 1)))
  
  (overlays-in  head tail))


;;
;; Insert candidate string
;;
(defun uim-show-candidate (candidate)

  ;; separate appendix (for prime...)
  (setq candidate
	(cons (car candidate)
	      (mapcar
	       '(lambda (x)
		  (let ((selected (nth 0 x))
			(candlabel (nth 1 x))
			(candstr (nth 2 x))
			eom)
		    
		    ;; separate appendix (for prime...)
		    (if (not uim-candidate-display-appendix)
			(when (string-match "\t" candstr)
			  (setq candstr (substring candstr 0 
						   (match-beginning 0))))
		      (while (setq eom (string-match "\t" candstr))
			(setq candstr
			      (concat (substring candstr 0 eom)
				      " "
				      (substring candstr (+ eom 1))))))
   
		    (list selected candlabel candstr)
		    )
		  ) (cdr candidate))))
	
  (setq uim-max-candlabel 
	(eval (cons 'max 
		    (mapcar '(lambda (x) (string-width (nth 1 x)))
			    (cdr candidate)))))

  ;; get max width 
  (setq uim-max-candstr 
	(eval (cons 'max 
		    (mapcar '(lambda (x) (string-width (nth 2 x)))
			    (cdr candidate)))))

  (setq uim-candidate-page-label 
	(format "%d/%d" (caar candidate) (cdar candidate)))

  (setq uim-candidate-original-start nil)
  (setq uim-candidate-original-end nil)

  (let ((display-inline uim-candidate-display-inline)
	(upward-end-line nil))

    (if (>= (max (+ uim-max-candlabel 6) 
		 (+ (string-width uim-candidate-page-label) 2))
	    (window-width))
	(setq display-inline nil))

    (if (/= (window-hscroll) 0)
	(setq display-inline nil))

    (if display-inline
	(uim-make-candlist (cdr candidate)))
  

    (if display-inline
	(save-excursion
	  (goto-char uim-candidate-start)

	  (let ((winofs (save-excursion
			  (goto-char uim-candidate-cursor)
			  (uim-get-window-offset)))
		(candlines (+ 1 (length uim-candidate-line-list)))
		ol-down dhead dtail uhead utail)
	    
	    (if (and (not (setq ol-down (uim-check-overlay
					 (save-excursion
					   (beginning-of-line)
					   (setq dhead (point)))
					 (save-excursion
					   (uim-vertical-motion 
					    (+ uim-candidate-vofs 
					       candlines))
					   (end-of-line)
					   (setq dtail (point))))))
		     ;; 
		     (save-excursion
		       (goto-char uim-candidate-cursor)
		     (>= (- (- (window-height) 1) (+ winofs 1)) 
			   (+ candlines 1))))

		(progn
		  (setq uim-show-candidate-upward nil)
		  (setq uim-candidate-original-start dhead)		  
		  (setq uim-candidate-original-end dtail))

	      (if (and (>= winofs candlines)
		       (not (uim-check-overlay
			     (save-excursion
			       (uim-vertical-motion (- candlines))
			       (beginning-of-line)
			       (setq uhead (point)))
			     (save-excursion
			       (end-of-line)
			       (setq utail (point)))))
		       )
		  (progn
		    (save-excursion
		      (setq upward-end-line (uim-get-window-offset)))
		    (setq uim-show-candidate-upward t)
		    (setq uim-candidate-original-start uhead)
		    (setq uim-candidate-original-end utail))
		
		;; Scroll buffer to show all candidates
		(if (and (not ol-down)
			 (>= (- (window-height) 1) (+ candlines 1)))
		    (progn
		      (setq uim-show-candidate-upward nil)
		      (if (not uim-window-force-scrolled-original)
			  (setq uim-window-force-scrolled-original 
				(window-start)))
		      (setq uim-window-force-scrolled t)
		      (setq uim-candidate-original-start dhead)
		      (setq uim-candidate-original-end dtail)

			(save-excursion 
			  (goto-char uim-candidate-cursor)
			  (recenter (- (- (- (window-height) 1) candlines) 1))))
		  ;; disable inline display
		  (setq display-inline nil)
		  
		  ))))))

    (if display-inline
	;; inline candidate display mode
	(let ((inhibit-read-only t))
	  (uim-merge-candidate)
	  (if uim-show-candidate-upward 
	      (save-excursion
		(recenter upward-end-line)))
	  )
      ;; display in echo region
      (uim-echo-candidate candidate)
      )
    )
  )

;;
;; Remove candidate
;;
(defun uim-remove-candidate ()
  (if (and uim-candidate-display-inline
	   uim-candidate-original-start)
      (let ((inhibit-read-only t))

	;; delete region which includes candidate
	(delete-region uim-candidate-original-start 
		       uim-candidate-end)

	;; restore original region
	(save-excursion
	  (goto-char uim-candidate-original-start)
	  (insert uim-candidate-original-str))

	)

    ;; clear minibuffer
    (let (message-log-max)
      (message nil))
   
    )
  
  )


(provide 'uim-candidate)
