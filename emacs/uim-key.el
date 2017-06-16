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
(defun uim-key-binding (vec &optional translation)
  (let (bind 
	(mode uim-mode))
    (unwind-protect
	(progn
	  (setq uim-mode nil)
	  (setq bind (key-binding vec))

	  (if (and (or (not bind)
		       (integerp bind))
		   translation)
	      (setq bind (lookup-key key-translation-map vec))))

      (setq uim-mode mode))

    ;; process autoload keymap
    (if (and (symbolp bind) (commandp bind))
	(let ((sf (symbol-function bind)))
	  (if (and (listp sf)
		   (eq 'keymap (nth 4 sf)))
	      (lookup-key bind [nil]))))

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


(defun uim-this-command-keys-override ()
  (if (not uim-this-command-keys-original)
      (let ((doc (documentation 'this-command-keys)))
	(setq uim-this-command-keys-original
	      (symbol-function 'this-command-keys))
	(eval
	 `(fset 'this-command-keys 
		'(lambda ()
		   ,doc
		   (if (and (boundp 'uim-key-vector) uim-key-vector)
		       uim-key-vector
		     (funcall uim-this-command-keys-original))))))))


(defun uim-this-command-keys-restore ()
  (if uim-this-command-keys-original
      (progn
	(fset 'this-command-keys uim-this-command-keys-original)
	(setq uim-this-command-keys-original nil))))


;; for Emacs21
(defun uim-read-char-exclusive-override ()
  (if (not uim-read-char-exclusive-original)
      (let ((doc (documentation 'read-char-exclusive)))
	(setq uim-read-char-exclusive-original
	      (symbol-function 'read-char-exclusive))
	(eval
	 `(fset 'read-char-exclusive
		'(lambda (&optional prompt inherit-input-method)
		   ,doc
		   (if (and (boundp 'uim-key-vector) uim-key-vector)
		       (let (executing-macro)
			 (funcall uim-read-char-exclusive-original 
				  prompt inherit-input-method))
		     (funcall uim-read-char-exclusive-original))))))))


;; for Emacs21
(defun uim-read-char-exclusive-restore ()
  (if uim-read-char-exclusive-original
      (progn
	(fset 'read-char-exclusive uim-read-char-exclusive-original)
	(setq uim-read-char-exclusive-original nil))))


(defun uim-command-execute (uim-key-vector &optional bind)

  (let (map
	(buffer (current-buffer)))

    (unwind-protect
	(progn

	  (if (and bind
		   (commandp bind))
	      (setq this-command bind)
	    (setq this-command uim-key-vector))


	  (when uim-xemacs
	    (setq last-input-event uim-original-input-event)
	    (handle-pre-motion-command))

	  (run-hooks 'pre-command-hook)

	  (setq last-command-char 
		(aref (uim-get-vector-from-tail uim-key-vector 1) 0))
	  
	  ;; backup current keymap of uim-mode
	  (setq map (uim-disable-keymap))

	  (if (or (and bind
		       (eq bind 'digit-argument))	  
		  (and uim-xemacs
		       (not (eventp last-command-char))))
	      (command-execute uim-key-vector)
	    (command-execute this-command))

	  (setq last-command this-command)
	  ;;(setq last-command-char (aref uim-key-vector 0))

	  (if uim-xemacs
	      (handle-post-motion-command))

	  (if (eq bind 'self-insert-command)
	      (uim-concat-undo)
	    (uim-flush-concat-undo)))

      (if (and buffer 
	       (buffer-live-p buffer))
	  (progn 
	    (set-buffer buffer)
	    ;; restore keymap of uim-mode
	    (uim-set-keymap map)
	    ))
      )))




(defun uim-blink-match (char)

  (when 
      (= ?\) (char-syntax char))
    (blink-matching-open))
  )


(defun uim-process-mouse-event (event)
  (cond (uim-emacs
	 (let* ((mouse-event (car event))
		(bind (uim-key-binding (vector mouse-event))))
	   (if (commandp bind)
	       (call-interactively bind nil (vector event))
	     (if (not (memq 'down (event-modifiers mouse-event)))
		 (undefined)))))
	(uim-xemacs
	 (let* ((bind (uim-key-binding (vector event))))
	   (if (commandp bind)
	       (call-interactively bind nil (vector event))))))
  )


;;
;; Process the key vector returned from Uim.
;; 
(defun uim-process-key-vector (key-vector &optional count)

  (let ((bind (uim-key-binding key-vector t))
	undef
	vtmp
	continue
	)
    
    (if uim-emacs
	;; for transient-mark-mode
	(setq deactivate-mark nil))

    (unwind-protect
	(cond ((keymapp bind)
	       (setq continue t)
	       )

	      ((stringp bind)
	       (if count
		   (setq prefix-arg count))
	       (uim-command-execute key-vector bind))

	      ((commandp bind)
	       (if count
		   (setq prefix-arg count))
	       (uim-command-execute key-vector bind))

	      ((or (and uim-emacs
			(setq vtmp (aref (uim-last-onestroke-key key-vector) 0))
			(integerp vtmp)
			(equal help-char vtmp))
		   (and uim-xemacs
			(setq vtmp (aref (uim-last-onestroke-key key-vector) 0))
			(equal (uim-xemacs-make-event 
				(uim-convert-char-to-symbolvector 
				 (key-description help-char)))
			       vtmp)))
	       (uim-command-execute key-vector))

	      (t
	       (setq undef key-vector)))
      
      (when undef
	(uim-flush-concat-undo)
	(if uim-xemacs
	    (error 'undefined-keystroke-sequence 
		   (uim-xemacs-make-event 
		    (uim-convert-char-to-symbolvector 
		     (key-description key-vector))))
	  )
	(when uim-emacs
	  (undefined)
	  (setq uim-keystroke-displaying nil)
	  (if (>= emacs-major-version 22)
	      (let (message-log-max)
		(message "%s is undefined" (key-description undef)))
	  )))

      (if uim-emacs
	  (setq uim-deactivate-mark deactivate-mark))
      )
    continue
    ))


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



(defun uim-check-shift (input-vector)
  (eval (cons 'or
	      (mapcar
	       '(lambda (x)
		  (or (and uim-emacs
			   (or (and (integerp x) (/= (logand (lsh 1 25) x) 0))
			       (string-match "S-" (format "%s" x))))
		      (and uim-xemacs
			   (string-match "Sh-" 
					 (key-description input-vector)))))
	       (append input-vector nil)))))


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
		  (keymapp (uim-key-binding keyvec)))))
	))
	 

(defun uim-is-start-with-escape (keyvec)
  (cond (uim-emacs
	 (uim-is-single-escape (vector (aref keyvec 0))))
	(uim-xemacs
	 (or (memq 'meta (aref (uim-convert-char-to-symbolvector (key-description keyvec)) 0))
	     (uim-is-single-escape (vector (aref keyvec 0)))))))


(defun uim-is-escape (keyvec)
  (cond (uim-emacs
	 (if (or window-system
		 (and (not window-system) uim-use-single-escape-on-terminal))
	     (uim-is-single-escape keyvec)
	   (equal keyvec [27 27]))
	 )
	(uim-xemacs
	 (if (or window-system
		 (and (not window-system) 
		      uim-use-single-escape-on-terminal))
	     (uim-is-single-escape keyvec)
	   (or (equal keyvec 
		      (make-vector 2 (uim-xemacs-make-event [(escape)])))
	       (equal keyvec 
		      (vector (uim-xemacs-make-event [(meta escape)]))))
	   ))))

(defun uim-separate-prefix-vector (key-vector)
  (let (key-vector-prefix key-vector-main)
    (setq key-vector-main (uim-last-onestroke-key key-vector))
    (setq key-vector-prefix 
	  (uim-get-vector-from-head key-vector
				    (- (length key-vector)
				       (length key-vector-main))))
    ;;(setq key-vector key-vector-main)
    ;;(setq uim-prefix-arg-vector key-vector-prefix)
    (list key-vector-main key-vector-prefix))
  )


;;
;; get this-command-keys with symbol lists vector
;; 
(defun uim-translate-key (input-vector)
  (let (translated-vector map 
	(input-vector-main input-vector)
	(input-vector-prefix nil)
	translated bind)

    (catch 'fmap-loop
      (while input-vector-main

	(setq bind (uim-key-binding input-vector-main))
	  
	(if (and bind
		 (not (integerp bind)))
	    (progn
	      ;;(setq translated-vector input-vector)
	      (setq translated-vector nil)
	      (throw 'fmap-loop t))

	  (if (boundp 'input-decode-map)
	      (setq translated (lookup-key input-decode-map
					   input-vector-main)))
	  
	  (if (or (not translated)
		  (integerp translated))
	      (setq translated (lookup-key function-key-map
					   input-vector-main)))

	  (if (and (or (not translated)
		       (integerp translated))
		   (boundp 'local-function-key-map))
	      (setq translated (lookup-key local-function-key-map 
					   input-vector-main)))

	  (if (and (symbolp translated) (commandp translated))
	      (let ((sf (symbol-function translated)))
		(if (and (listp sf)
			 (eq 'autoload (nth 0 sf))
			 (or (eq 'keymap (nth 4 sf))
			     (eq 'macro  (nth 4 sf))))
		    (load (nth 1 sf)))))

	  (cond ((not translated)
		 )

		((vectorp translated)
		 ;; vector ... replace immediately
		 (setq translated-vector 
		       (vconcat input-vector-prefix translated))
		 (throw 'fmap-loop t))

		((keymapp translated)
		 ;; keymap ... wait next input
		 (setq map translated)
		 (throw 'fmap-loop t))

		((functionp translated)
		 ;; function ... call immediately and use returned value
		 (if (not uim-keystroke-displaying)
		     (setq uim-keystroke-displaying (sit-for echo-keystrokes)))

		 (if uim-keystroke-displaying
		     (let (message-log-max)
		       (message (concat (key-description 
					 (vconcat uim-prefix-arg-vector
						  input-vector))
					"-")))
		   )

		 (setq translated-vector
		       (vconcat input-vector-prefix (funcall translated nil)))

		 (throw 'fmap-loop t))
		
		((stringp translated)
		 ;; string ... replace
		 (setq translated-vector
		       (char-to-string (aref translated 
					     (- (length translated) 1))))
		 (throw 'fmap-loop t))
		))


	(setq input-vector-prefix
	      (vconcat input-vector-prefix (uim-vector-car input-vector-main)))

	(setq input-vector-main
	      (uim-vector-cdr input-vector-main))) ;; end of while
      
      (setq translated-vector nil)) ;; end of catch
    
    (list translated-vector map))
  )



;; Replate [escape escape] with [M-escape] on XEmacs
;;   Some special keys cannot be used with escape key on terminal
(defun uim-translate-escape-meta (input-vector)
  (if (and uim-xemacs
	   (>= (length input-vector) 2)
	   (equal (aref input-vector 0)
		  (uim-xemacs-make-event [(escape)])))
      ;; append meta
      (vconcat 
       (vector 
	(uim-xemacs-make-event 
	 (vector (cons 'meta (aref (uim-convert-char-to-symbolvector
				   (key-description (uim-vector-cdr 
						     input-vector))) 0)
		      ))))
       (uim-cut-vector-from-head input-vector 2))
    input-vector))

(provide 'uim-key)

