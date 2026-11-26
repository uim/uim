;; -*- lexical-binding: t -*-
;;
;;  Copyright (c) 2026 uim Project https://github.com/uim/uim
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
;; Types into a buffer with uim-mode on, as a user would, and looks at
;; what uim.el leaves there.

(defun uim-test-type (im keys)
  "Type KEYS into a new buffer with uim-mode on and IM.
Return a plist of the buffer's text with its properties, whether
candidates were shown, and the text once uim-mode is off again."
  (setq uim-default-im-engine im)
  (ert-with-test-buffer-selected ()
    (uim-mode 1)
    (execute-kbd-macro (kbd keys))
    (let ((result (list :text (buffer-string)
                        :candidate-displayed (and uim-candidate-displayed t))))
      (uim-mode 0)
      (append result (list :text-after-off (buffer-string))))))

(ert-deftest uim-test-commit ()
  (should (equal uim-test-ka
                 (plist-get (uim-test-type "skk" "C-j k a") :text))))

(ert-deftest uim-test-preedit ()
  (let ((result (uim-test-type "skk" "C-j k")))
    (should (equal "k" (substring-no-properties (plist-get result :text))))
    (should (eq 'uim-preedit-underline-face
                (get-text-property 0 'face (plist-get result :text))))
    ;; The preedit is uim.el's, not the buffer's.
    (should (equal "" (plist-get result :text-after-off)))))

;; SKK starts with the input method off, so the key goes to Emacs.
(ert-deftest uim-test-unconsumed-key ()
  (should (equal "a" (plist-get (uim-test-type "skk" "a") :text))))

(ert-deftest uim-test-candidates ()
  (should (plist-get (uim-test-type "candidates" "a") :candidate-displayed)))

;; A minor mode that is defined after uim.el and binds a key with ESC
;; as inf-ruby-minor-mode does with C-M-x. It's put before uim-mode in
;; minor-mode-map-alist.
(defvar uim-test-esc-prefix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") #'ignore)
    map))
(define-minor-mode uim-test-esc-prefix-mode
  "A minor mode that binds a key with ESC.")

;; M-x must reach uim.el, which passes it to Emacs, not be undefined.
(ert-deftest uim-test-meta-key-with-esc-prefix-minor-mode ()
  (setq uim-default-im-engine "skk")
  (ert-with-test-buffer-selected ()
    (uim-test-esc-prefix-mode 1)
    (uim-mode 1)
    (unwind-protect
        (should (eq 'uim-process-input (key-binding [?\M-x] t)))
      (uim-mode 0))))
