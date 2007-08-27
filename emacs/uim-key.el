;; 
;;  Copyright (c) 2005-2007 uim Project http://code.google.com/p/uim/
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
;; this-command-keys wrapper 
;;
(defun uim-this-command-keys-vector ()
  (if uim-xemacs
      (this-command-keys)
    (this-command-keys-vector)))


;;
;; translate XEmacs style key name
;;
(defun uim-translate-xemacs-keyname (keyname)
  (cond ((string= keyname "BS")  "backspace")
	((string= keyname "TAB") "tab")
	((string= keyname "LFD") "linefeed")
	((string= keyname "RET") "return")
	((string= keyname "ESC") "escape")
	((string= keyname "DEL") "delete")
	((string= keyname "SPC") "space")
	(t keyname)))


;; 
;; Get last one stroke from keyvector
;;
(defun uim-last-onestroke-key (keyvec)
  (let ((len (length keyvec)))
    (cond ((= len 1)
 	   keyvec 
	   )
 	  ((>= len 2)
 	   (let ( (last2 (aref keyvec (- len 2)) )
 		  (last (aref keyvec (- len 1)) ) )
 	     (if (eq last2 27)
 		 (vector last2 last)
 	       (vector last)))))))


;;
;; Get original key-mapped function
;;
(defun uim-getbind (keyvec &optional translation)
  (let (bind 
	(mode uim-mode))
    (unwind-protect
	(progn
	  (setq uim-mode nil)

	  (setq bind (key-binding (vector (aref keyvec 0))))

	  (if (not (memq bind (list 'universal-argument 
				    'digit-argument 
				    'negative-argument)))
	      (setq bind (key-binding keyvec)))

	  (if (and (or (not bind)
		       (integerp bind))
		   translation)
	      (setq bind (lookup-key key-translation-map keyvec))))
      (setq uim-mode mode))
    bind
    ))


;;
;; Stop combining
;;
(defun uim-flush-concat-undo ()
  (setq uim-undo-stacking nil)
  (undo-boundary)
  )


;;
;; Combine continuous self-insert-commands
;; 
(defun uim-concat-undo ()

  (if (and (listp buffer-undo-list)
	   (>= (length buffer-undo-list) 2))
      (if (not uim-undo-stacking)
	  (setq uim-undo-stacking t)
	(let (recent previous)
	  (setq recent (nth 0 buffer-undo-list))
	  (setq previous (nth 2 buffer-undo-list))

	  (if (and (consp recent) (consp previous)
		   (not (nth 1 buffer-undo-list))
		   (= (car recent) (cdr previous)))
	      (let ((left (car previous))
		    (right (cdr recent)))
		(if (and (integerp left) (integerp right)
			 (<= (- right left) 20))
		    (progn
		      (setcar buffer-undo-list (cons left right))
		      (setcdr buffer-undo-list (cdr (cddr buffer-undo-list)))
		      )
		  )
		))))
    (setq uim-undo-stacking nil))
  (undo-boundary) 
  )

(defun uim-backup-this-command-keys ()
  (when (not uim-this-command-keys-original)
    ;;(uim-debug "this-command-keys backup!")
    (setq uim-this-command-keys-original
          (symbol-function 'this-command-keys))))


(defun uim-this-command-keys-override ()
  (if (not uim-this-command-keys-original)
      (progn
        (uim-backup-this-command-keys)
        (defun this-command-keys ()
          (if (and (boundp 'uim-key-vector)
                   uim-key-vector)
              uim-key-vector
            (funcall uim-this-command-keys-original))))))


(defun uim-command-execute (uim-key-vector)
  (uim-debug (format "uim-command-execute: %s" uim-key-vector))
  (let ((mode uim-mode))

    (unwind-protect
	(progn
	  (setq uim-mode nil)
	  (setq this-command uim-key-vector)
	  (run-hooks 'pre-command-hook)
	  (command-execute this-command)
	  )
      (progn
	(setq uim-mode mode)))))


(defun uim-blink-match (char)

  (when 
      (= ?\) (char-syntax char))
    (blink-matching-open))
  )


(defun uim-process-mouse-event (event)
  (cond (uim-emacs
	 (let* ((mouse-event (car event))
		(bind (uim-getbind (vector mouse-event))))
	   (if (commandp bind)
	       (call-interactively bind nil (vector event))
	     (if (not (memq 'down (event-modifiers mouse-event)))
		 (undefined)))))
	(uim-xemacs
	 (let* ((bind (uim-getbind (vector event))))
	   (if (commandp bind)
	       (call-interactively bind nil (vector event))))))
  )


;;
;; Process the key vector returned from Uim.
;; 
(defun uim-process-keyvec (uim-key-vector &optional count)
  (let ((bind (uim-getbind uim-key-vector t))
	keyvectmp)
    (uim-debug (format "uim-process-keyvec"))
    
    (if uim-emacs
	;; for transient-mark-mode
	(setq deactivate-mark nil))
    
    (unwind-protect    
	(cond (count
	       (setq prefix-arg count)
	       (uim-command-execute
		(uim-getbind uim-key-vector t))
		;;(uim-getbind (uim-last-onestroke-key uim-key-vector)))
	       )
	      ((stringp bind)
	       (command-execute bind)
	       (uim-concat-undo)
	       )
	      ((commandp bind)

	       (if (eq bind 'self-insert-command)
		   (progn
		     (setq this-command bind)
		     (setq last-command-char (aref uim-key-vector 0))
		     (call-interactively bind)
		     (uim-concat-undo))
		 (setq this-command bind)
		 (uim-debug (format "this-command is %s" this-command))
		 (setq last-command-char (aref uim-key-vector 0))

		 (if uim-xemacs
		     (progn
		       (setq last-input-event uim-original-input-event)
		       (handle-pre-motion-command)))

		 (run-hooks 'pre-command-hook)
		 (command-execute this-command) 

		 (if uim-xemacs
		     (handle-post-motion-command))

		 (uim-flush-concat-undo)
		 )
	       )

	      ((or (and uim-emacs
			(= help-char (aref (uim-last-onestroke-key uim-key-vector) 0)))
		   (and uim-xemacs
			(equal (uim-xemacs-make-event 
				(uim-convert-char-to-symbolvector (key-description help-char)))
			       (aref (uim-last-onestroke-key uim-key-vector) 0))))
	       (uim-debug "help-char")
	       (let ((mode uim-mode))
		 (unwind-protect
		     (progn
		       (setq uim-mode nil)
		       (funcall prefix-help-command)
		       )
		   (progn
		     (setq uim-mode mode))))
	       )
	      (t
	       (uim-flush-concat-undo)
	       (if uim-xemacs
		   (error 'undefined-keystroke-sequence 
			  (uim-xemacs-make-event 
			   (uim-convert-char-to-symbolvector 
			    (key-description uim-key-vector))))
		 (undefined))
	       )
	      )
      (if uim-emacs
	  (setq uim-deactivate-mark deactivate-mark))
      )
    )
  )
  



(defun uim-xemacs-restore-menubar ()
  (if uim-menubar-temp
      (progn
	(setq current-menubar uim-menubar-temp)
	(setq uim-menubar-temp nil)
	)
    )
  )

(defun uim-xemacs-save-menubar ()
  (if (not uim-menubar-temp)
      (progn
	(setq uim-menubar-temp current-menubar)
	(setq current-menubar (mapcar '(lambda (x)
				     (if x
					 (list (car x) ["" nil :active nil])
				       nil))
				      current-menubar))
	))
  )



;;
;; convert keyvec to event for XEmacs
;;
(defun uim-xemacs-make-event (keyvec)
  (let* (button 
	 event
	 (keylist-tmp (aref keyvec 0))
	 (keylist (if (listp keylist-tmp) keylist-tmp
		    (list keylist-tmp)))
	 (lastkey (nth (- (length keylist) 1) keylist)))
    (cond ((setq button
		 (assoc lastkey
			'((button1 . 1) (button2 . 2) 
			  (button3 . 3) (button4 . 4)
			  (button5 . 5))))
	   ;; mouse press
	   (delq lastkey keylist)
	   (setq event 
		 (make-event 'button-press 
			     (list 'button (cdr button) 'modifiers keylist)))
	   )
	  ((setq button
		 (assoc lastkey
			'((button1up . 1) (button2up . 2) 
			  (button3up . 3) (button4up . 4)
			  (button5up . 5))))
	   ;; mouse up
	   (delq lastkey keylist)
	   (setq event 
		 (make-event 'button-release
			     (list 'button (cdr button) 'modifiers keylist)))
	   )
	  (t
	   ;; key
	   (setq keylist (nbutlast keylist))
	   (setq event 
		 (make-event 'key-press (list 'key lastkey 'modifiers keylist)))
	   )
	  )
    event))

;;
;; convert XEmacs style key sequence to symbol list
;;
(defun uim-convert-char-to-symbolvector (keychar)
  (let (symbol-vector keys ofs)
    (while keychar
      (if (setq ofs (string-match " " keychar))
	  (progn
	    (setq keys (substring keychar 0 ofs))
	    (setq keychar (substring keychar (+ ofs 1))))
	(setq keys keychar)
	(setq keychar nil))

      (let (symbol-list)
	(progn
	  (while (string-match "^\\(C\\|M\\|S\\|H\\|A\\|Sh\\)-" keys)
	    (let ((mod (match-string 0 keys)))
	      (cond ((string= mod "C-")
		     (setq symbol-list (cons 'control symbol-list)))
		    ((string= mod "M-")
		     (setq symbol-list (cons 'meta symbol-list)))
		    ((string= mod "S-")
		     (setq symbol-list (cons 'super symbol-list)))
		    ((string= mod "H-")
		     (setq symbol-list (cons 'hyper symbol-list)))
		    ((string= mod "A-")
		     (setq symbol-list (cons 'alt symbol-list)))
		    ((string= mod "Sh-")
		     (setq symbol-list (cons 'shift symbol-list)))))
	    (setq keys (substring keys (match-end 0))))

	  (setq keys (uim-translate-xemacs-keyname keys))


	  (setq symbol-list 
		(append symbol-list (list 
				     (if (= (length keys) 1)
					 (string-to-char keys)
				       (read keys)))))
	  )
	(setq symbol-vector (vconcat symbol-vector (vector symbol-list)))
	))
    symbol-vector
    ))




;;
;; remove Shift modifier from key vector
;;
(defun uim-remove-shift (input-vector)
  (vconcat (mapcar 
	   '(lambda (x)
  (cond (uim-emacs
		     (if (and (integerp x)
			      (/= (logand (lsh 1 25) x) 0))
			 (logand (lognot (lsh 1 25)) x)
		       (let ((key-str (format "%s" x)))
			 (if (string-match "S-" key-str)
			     (read (replace-match "" nil nil 
						  key-str))
			   x))))
	(uim-xemacs
		     (let ((key-str 
			    (key-description input-vector)))
		       (if (string-match "Sh-" key-str)
			   (uim-xemacs-make-event 
			      (uim-convert-char-to-symbolvector 
			     (replace-match "" nil nil key-str)))
			 x)))))
	   (append input-vector nil))))


;;
;; convert XEmacs style key sequence to FSF key vector
;;
(defun uim-convert-keystr-to-uimagent-vector (keystr)
  (let (symbol-vector keys ofs)
    (while keystr
      (if (setq ofs (string-match " " keystr))
	  (progn
	    (setq keys (substring keystr 0 ofs))
	    (setq keystr (substring keystr (+ ofs 1))))
	(setq keys keystr)
	(setq keystr nil))

      (let (symbol-str 
	    (symbol-val 0))
	(progn
	  (while (string-match "^\\(C\\|M\\|S\\|H\\|A\\|Sh\\)-" keys)
	    (let ((mod (match-string 0 keys)))
	      (cond ((string= mod "C-")
		     (setq symbol-val (+ symbol-val (lsh 1 26)))
		     (setq symbol-str (concat "C-" symbol-str)))
		    ((string= mod "M-")
		     (setq symbol-val (+ symbol-val (lsh 1 27)))
		     (setq symbol-str (concat "M-" symbol-str)))
		    ((string= mod "S-") 
		     ;; super
		     (setq symbol-val (+ symbol-val (lsh 1 23)))
		     (setq symbol-str (concat "s-" symbol-str)))
		    ((string= mod "H-")
		     (setq symbol-val (+ symbol-val (lsh 1 24)))
		     (setq symbol-str (concat "H-" symbol-str)))
		    ((string= mod "A-")
		     (setq symbol-val (+ symbol-val (lsh 1 22)))
		     (setq symbol-str (concat "A-" symbol-str)))
		    ((string= mod "Sh-")
		     ;; shift
		     (setq symbol-val (+ symbol-val (lsh 1 25)))
		     (setq symbol-str (concat "S-" symbol-str)))))

	    (setq keys (substring keys (match-end 0))))

	  (setq keys (uim-translate-xemacs-keyname keys))

	  (if (= (length keys) 1)
	      (if (not symbol-str)
		  (setq symbol-str (format "%d" (string-to-char keys)))
		(setq symbol-str (format "%d" (+ symbol-val
						 (string-to-char keys)))))
	    (setq symbol-str (concat symbol-str keys)))

	  (setq symbol-vector (vconcat symbol-vector 
				       (vector (read symbol-str))))
	  )))
    symbol-vector
    ))


(defun uim-is-single-escape (keyvec)
  (cond (uim-emacs
	 (or (equal keyvec [27])
	     (equal keyvec [escape])))
	(uim-xemacs
	 (or (equal keyvec (vector (uim-xemacs-make-event [escape])))
	     (and (eq (global-key-binding keyvec) esc-map)
		  (keymapp (uim-getbind keyvec)))))
	))
	 

(defun uim-is-start-with-escape (keyvec)
;  (uim-debug (format "uim-is-start-width-escape: %s" keyvec))
  (cond (uim-emacs
	 (uim-is-single-escape (vector (aref keyvec 0))))
	(uim-xemacs
	 (or (memq 'meta (aref (uim-convert-char-to-symbolvector (key-description keyvec)) 0))
	     (uim-is-single-escape (vector (aref keyvec 0)))))))


(defun uim-is-escape (keyvec)
;  (uim-debug (format "uim-is-escape %s" keyvec))
  (cond (uim-emacs
	 (if (or window-system
		 (and (not window-system) uim-use-single-escape-on-terminal))
	     (uim-is-single-escape keyvec)
	   (equal keyvec [27 27]))
	 )
	(uim-xemacs
	 (if (or window-system
		 (and (not window-system) uim-use-single-escape-on-terminal))
	     (uim-is-single-escape keyvec)
	   (or (equal keyvec (make-vector 2 (uim-xemacs-make-event [(escape)])))
	       (equal keyvec (vector (uim-xemacs-make-event (uim-convert-char-to-symbolvector "M-ESC"))))
	       )
	   )
	 )
	)
  )


;;
;; get this-command-keys with symbol lists vector
;; 
(defun uim-this-command-keys (with-arg)
  (let (keyvec replace-continue fmap-continue bind)

    (if uim-xemacs
	(setq keyvec (this-command-keys)))

    (if uim-emacs
	(setq keyvec (this-command-keys-vector)))

      (if uim-prefix-ignore-next
	  ;; avoid mysterious key event on Emacs-21 on terminal
	  ;;  ex. C-u 1 0 escape f 
	  (progn
	    (uim-debug "ignore this key vector")
	    (setq keyvec nil)
	    (setq uim-prefix-ignore-next nil)
	    )
	(if with-arg
	    ;; key with prefix arg
	    (let* ((rkeylist (reverse (append keyvec nil))))
	      (setq keyvec (uim-last-onestroke-key keyvec))
	      (if (= (length keyvec) 2)
		  ;; Only Emacs-21 returns two stroke keys at 1st time
		  (setq uim-prefix-arg-vector 
			(vconcat (reverse (cdr (cdr rkeylist)))))
		(setq uim-prefix-arg-vector 
		      (vconcat (reverse (cdr rkeylist)))))
	      ;; work around
	      (if (= (length keyvec) 2)
		  (setq uim-prefix-ignore-next t))
	    )))

    (uim-debug (format "keyvec %s" keyvec))

    ;; workaround for Emacs22
    ;;  detect and convert odd double key vector into single
    ;;  vector (ex. [1 1] to [1])
    (if (and  (>= (length  keyvec) 2)
	      (not (uim-getbind keyvec)))
	(progn
	  (uim-debug "*** wrong key vector detected (Emacs22's bug?)")
	  (setq keyvec (vector (aref keyvec 0)))))

    ;; translate key vector with function-key-map

    (let (fmap key replaced)
      (let* ((merged-vector (vconcat uim-stacked-key-vector keyvec))
	     (merged-list (append merged-vector nil))
	     (stacked-list nil)
	     merged-list-backup
	     done)
	(uim-debug (format "merged-list: %s" merged-list))

	(catch 'fmap-loop
	  (while merged-list

	    (setq merged-list-backup merged-list)


	    (setq bind (uim-getbind (vconcat merged-list)))
	  
	    (if (and bind
		     (not (integerp bind)))
		(progn
		  (uim-debug "skip function-key-map lookup")
		  (setq uim-stacked-key-vector 
			(vconcat stacked-list merged-list))
		  (throw 'fmap-loop t))

	  (setq fmap (lookup-key function-key-map (vconcat merged-list)))

	      (if (and (or (not fmap)
			   (integerp fmap))
		       (boundp 'local-function-key-map))
		  (setq fmap (lookup-key local-function-key-map 
					 (vconcat merged-list))))


	      (if  (or (not fmap) 
		       (integerp fmap))
		  (progn
		    (setq merged-list
			  (append (uim-remove-shift (vconcat merged-list)) nil))

		    (setq bind (uim-getbind (vconcat merged-list)))

		    (if (and bind
			     (not (integerp bind)))
			
			(progn
			  (uim-debug "skip function-key-map lookup (remove shift)")
			  (setq uim-stacked-key-vector
				(vconcat stacked-list merged-list))
			  (throw 'fmap-loop t))

		      ;; そうでなかったら，function-key-mapを引いてみる
		      (setq fmap (lookup-key function-key-map
					     (vconcat merged-list)))

	  (if (and (or (not fmap) 
		       (integerp fmap))
		   (boundp 'local-function-key-map))
	      (setq fmap (lookup-key local-function-key-map
				     (vconcat merged-list))))


		      (if (not fmap)
			  (setq merged-list merged-list-backup)))))

	      
	  (cond ((vectorp fmap)
		 ;; vector: replace
		 (uim-debug (format "vector: %s" (vconcat merged-list)))
		 (setq uim-stacked-key-vector 
		       (vconcat stacked-list fmap))
		     (throw 'fmap-loop t)
		 )
		((keymapp fmap)
		 ;; keymap: wait next
		 (uim-debug (format "keymap: %s" (vconcat merged-list)))
		 (setq fmap-continue t)
		 (setq uim-stacked-key-vector merged-vector)
		     (throw 'fmap-loop t)
		 )
		((functionp fmap)
		 (uim-debug (format "function: %s" (vconcat merged-list)))
		 (setq fmap (funcall fmap nil))
		 (if (vectorp fmap)
		     (setq uim-stacked-key-vector (vconcat stacked-list fmap))
		   )
		     (throw 'fmap-loop t)
		)
		    ))

	    (setq merged-list merged-list-backup)
	    
	  (setq stacked-list (append stacked-list (list (car merged-list))))
	  (setq merged-list (cdr merged-list))
	  )

	    (setq uim-stacked-key-vector merged-vector))))
      
    ;; Replate [escape escape] with [M-escape] on XEmacs
    ;;   Some special keys cannot be used with escape key on terminal
    (when (and uim-xemacs
	       (>= (length uim-stacked-key-vector) 2)
	       (equal (aref uim-stacked-key-vector 0) 
		      (uim-xemacs-make-event [(escape)])))
      ;; append meta
      (setq uim-stacked-key-vector
	    (vector
	     (uim-xemacs-make-event
	      (vector 
	       (cons 'meta 
		     (aref (uim-convert-char-to-symbolvector
			    (key-description (vconcat
					      (cdr (append 
						    uim-stacked-key-vector
						    nil)))))
			   0)))))))



    (uim-debug (format "stacked-key-vector: %s" uim-stacked-key-vector))

    (cond ((and uim-preedit-keymap-enabled
		(uim-is-escape uim-stacked-key-vector))
	   (uim-debug "stacked-key is Escape")
	   ;; Return escape key
	   (if uim-emacs
	       (setq keyvec [27]))
	   (if uim-xemacs
	       (setq keyvec (vector (uim-xemacs-make-event [escape]))))
	   (setq uim-stacked-key-vector nil)
	   )
	  ((or (and uim-preedit-keymap-enabled
		      (or 
		       (and (or (eq (car-safe (aref uim-stacked-key-vector 0))
				    'menu-bar)
				(eq (car-safe (aref uim-stacked-key-vector 0))
				    'tool-bar))
			    (keymapp (uim-getbind uim-stacked-key-vector)))

		    (and fmap-continue  ;; wait ESC- key vector
			    (uim-is-start-with-escape uim-stacked-key-vector))
		       ))
	       
	       (and (not uim-preedit-keymap-enabled)
		    (or (and fmap-continue 
			     (not (commandp (uim-getbind uim-stacked-key-vector t))))
			(keymapp (uim-getbind uim-stacked-key-vector t))))
	       uim-prefix-ignore-next ;; work around for Emacs-21 prefix arg
	       )
	   (uim-debug "wait next")
	   (setq keyvec nil))
	  (t
	   ;; No need to wait any more. Return current keys.
	   (setq keyvec uim-stacked-key-vector)
	   (setq uim-stacked-key-vector nil))
	  )
	
    keyvec
    )
  )


(provide 'uim-key)

