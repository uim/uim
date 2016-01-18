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
;; Insert preedit
;;
(defun uim-insert-preedit (preedit)

  (when uim-preedit-display-fences
    (setq preedit (append (cons '(t "|") preedit) '((t "|")))))

  (setq uim-preedit-cursor nil)

  ;; disregard read-only temporarily

    (save-excursion
    (setq uim-preedit-current-sentence-start nil)
      
      (let ((preedit-blocks preedit) block-start-point)

	(mapcar
	 '(lambda (x)
	    (let ((preedit-flag (format "%s" (car x)))
		  (preedit-str (car (cdr x))))

	      ;; save point for candidate displaying before insertion
	      ;;  i.e. head of the block
	      (if (string-match "c" preedit-flag)
		  (if (> (length preedit-str) 0)
		    (setq uim-preedit-current-sentence-start (point))
		    ;; workaround for uim-prime 
		  (setq uim-preedit-current-sentence-start
			block-start-point)))
	    
	      (setq block-start-point (point))

	      (when (> (length preedit-str) 0)

		(insert preedit-str)

		(let ((face (cond
			     ((string-match "s" preedit-flag)
			      'uim-separator-face)
			     ((string-match "ru" preedit-flag)
			      'uim-preedit-highlight-underline-face)
			     ((string-match "u" preedit-flag)
			      'uim-preedit-underline-face)
			     ((string-match "r" preedit-flag)
			      'uim-preedit-highlight-face)
			     (t 
			      'uim-preedit-face))))

		  (put-text-property block-start-point (point) 'face face)

		  )
		)
	      

	      ;; save point for cursor displaying after insertion
	      (if (string-match "c" preedit-flag)
		  (setq uim-preedit-cursor (point)))

	      ;; update preedit-end
	      (setq uim-preedit-end (point))

	      ))
	 preedit-blocks)
	)
      )

    ;; if "c" flag not found in preedit 
  (if (not uim-preedit-current-sentence-start)
      (setq uim-preedit-current-sentence-start uim-preedit-start))
	  (if (not uim-preedit-cursor)
      (setq uim-preedit-cursor uim-preedit-end))

    ;; set vertical offset
  (setq uim-preedit-overlap
	(uim-vertical-distance uim-preedit-current-sentence-start
			       uim-preedit-cursor))


  )



;; Remove preedit
(defun uim-remove-preedit ()

  (goto-char uim-preedit-start)

    (save-excursion
      ;; remove preedit string
      (delete-region uim-preedit-start uim-preedit-end))
    )



(provide 'uim-preedit)

