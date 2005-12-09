;; 
;;  Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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
;; Convert keyname 
;; 
(defun uim-replace-keyvec-with-functionkeymap (keyvec)
  (let ((keys kyevec))
    (while keys
      (let ((mapto (assq (car keys) function-key-map)))
	(if mapto
	    (setcar keys mapto)))))
  keyvec)


;;
;; Get original key-mapped function
;;
(defun uim-getbind (keyvec)
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
	  )
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


(defun uim-command-execute (keyvec)
  (let ((mode uim-mode))
    (unwind-protect
	(progn
	  (setq uim-mode nil)
	  (command-execute keyvec))
      (setq uim-mode mode))))


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
(defun uim-process-keyvec (keyvec &optional count)
  (let ((bind (uim-getbind keyvec))
	keyvectmp)

    (if uim-emacs
	;; for transient-mark-mode
	(setq deactivate-mark nil))
    
    (cond ((or (keymapp bind)
	       (and (not bind)
		    (setq keyvectmp (uim-remove-shift keyvec))
		    (setq keyvec keyvectmp)))

	   (if uim-xemacs
	       (progn
		 (setq uim-retry-keys keyvec)
		 (setq unread-command-events
		       (cons (aref keyvec 0) unread-command-events))))

	   (if uim-emacs
	       (progn
		 (setq uim-retry-keys keyvec)
		 (setq unread-command-events
		       (nconc (listify-key-sequence keyvec)
			      unread-command-events))))
	   )
	  (count
	   (setq prefix-arg count)
	   (uim-command-execute
	    (uim-getbind (uim-last-onestroke-key keyvec)))
	   )

	  ((commandp bind)
	   (if (eq bind 'self-insert-command)
	       (progn
		 (setq this-command bind)
		 (call-interactively bind)
		 (uim-concat-undo))
	     (setq this-command bind)
	     (command-execute bind) 
	     (uim-flush-concat-undo)
	     ))
	  (t
	   (uim-flush-concat-undo)
	   (if uim-xemacs
	       (error 'undefined-keystroke-sequence 
		      (uim-xemacs-make-event 
		       (uim-convert-char-to-symbolvector 
			(key-description keyvec))))
	     (undefined))
	   )
	  )

    (if uim-emacs
	(setq uim-deactivate-mark deactivate-mark))
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
			'((button1 . 1) (button2 . 2) (button3 . 3))))
	   ;; mouse press
	   (delq lastkey keylist)
	   (setq event 
		 (make-event 'button-press 
			     (list 'button (cdr button) 'modifiers keylist)))
	   )
	  ((setq button
		 (assoc lastkey
			'((button1up . 1) (button2up . 2) (button3up . 3))))
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
(defun uim-remove-shift (keyvec)
  (cond (uim-emacs
	 (let ((keyval (aref keyvec 0)))
	   (if (and (integerp keyval)
		    (/= (logand (lsh 1 25) keyval) 0))
	       (setq keyvec (vector (logand (lognot (lsh 1 25)) keyval)))
	     (let ((keystr (format "%s" keyval)))
	       (if (string-match "S-" keystr)
		   (setq keyvec 
			 (vector (read 
				  (replace-match "" nil nil keystr))))
		 (setq keyvec nil))))))
	(uim-xemacs
	 (let ((keystr (key-description keyvec)))
	   (if (string-match "Sh-" keystr)
	       (setq keyvec 
		     (vector (uim-xemacs-make-event 
			      (uim-convert-char-to-symbolvector 
			       (replace-match "" nil nil keystr)))))
	     (setq keyvec nil))))
	(t 
	 (setq keyvec nil))
	)
  keyvec
  )


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
(defun uim-this-command-keys ()
  (let (keyvec replace-continue)

    (if uim-xemacs
	(setq keyvec (this-command-keys)))

    (if uim-emacs
	(setq keyvec (this-command-keys-vector)))

    (if (or (not keyvec)
	    (and (vectorp keyvec)
		 (= (length keyvec) 0)))
	(setq keyvec uim-retry-keys))

;    (uim-debug (format "keyvec %s" keyvec))

    (let* ((newvec (vconcat uim-stacked-key-vector keyvec))
	   (keylist (append newvec nil))
	   prefix
	   replace)
      (catch 'replace-loop
	(while keylist
	  (when (or (not prefix)
		    (keymapp (uim-getbind prefix)))
	    (setq replace (uim-lookup-function-key-map (vconcat keylist)))
	    (cond ((vectorp replace)
		   (setq uim-stacked-key-vector (vconcat prefix replace))
		   (throw 'replace-loop nil))
		  ((keymapp replace)
		   (setq replace-continue t)
		   (setq uim-stacked-key-vector newvec)
		   (throw 'replace-loop nil)
		   )))
	  (setq prefix (vconcat prefix (vector (car keylist))))
	  (setq keylist (cdr keylist))
	  )
	;; cannot be replaced
	(setq uim-stacked-key-vector newvec)))

    
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
			   0))
	       )))))

    ;;(uim-debug (format "stacked-key-vector: %s" uim-stacked-key-vector))

    (cond ((and uim-preedit-keymap-enabled
		(uim-is-escape uim-stacked-key-vector)) ;; preedit ESC-ESC
	   (uim-debug "Escape")
	   ;; stop waiting and return ESC key
	   (if uim-emacs
	       (setq keyvec [27]))
	   (if uim-xemacs
	       (setq keyvec (vector (uim-xemacs-make-event [escape]))))
	   (setq uim-stacked-key-vector nil)
	   )
	  ((or (and uim-preedit-keymap-enabled
		    (and replace-continue ;; wait ESC- key vector
			 (uim-is-start-with-escape uim-stacked-key-vector)))
	       (and (not uim-preedit-keymap-enabled)
		    (or replace-continue ;; wait all
			(keymapp (uim-getbind uim-stacked-key-vector)))))
	   ;; wait next
	   (uim-debug "wait next")
	   (setq keyvec nil))
	  (t
	   ;; return keys
	   (setq keyvec uim-stacked-key-vector)
	   (setq uim-stacked-key-vector nil)))
	
    ;; convert keyvector with key-translation-map
    (if keyvec
	(let ((transvec (lookup-key key-translation-map keyvec)))
	  (if (vectorp transvec)
	      (setq keyvec transvec))))

    keyvec
    )
  )



(defun uim-lookup-function-key-map (keyvec)
  (let ((keylist (append keyvec nil))
	(ret nil)
	vec)
    (catch 'fkmap-loop
      (while keylist
	(setq vec (vconcat vec (vector (car keylist))))
	(if (commandp (uim-getbind vec))
	    (throw 'fkmap-loop nil)
	    )
	(setq keylist (cdr keylist)))
      (setq ret (lookup-key function-key-map keyvec))
      )
    ret))



(provide 'uim-key)

