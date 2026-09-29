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
;; what uim.el leaves there. uim.el talks to the uim-el-agent and
;; uim-el-helper-agent in UIM_EL_AGENT and UIM_EL_HELPER_AGENT; run
;; this with run.el.

(require 'ert)

(defvar uim-test-directory (make-temp-file "uim-el-test" t))

(add-hook 'kill-emacs-hook
          (lambda () (delete-directory uim-test-directory t)))

;; The agents read the user's uim configuration and look for
;; uim-helper-server under these, so the test keeps them to its own.
;; default-directory may start with the "~" that is about to move.
(setq default-directory (expand-file-name default-directory))
(setenv "HOME" uim-test-directory)
(setenv "XDG_RUNTIME_DIR" uim-test-directory)

(setenv "LIBUIM_USER_SCM_FILE"
        (expand-file-name "user.scm" uim-test-directory))
(with-temp-file (getenv "LIBUIM_USER_SCM_FILE")
  (insert (format "(load %S)\n"
                  (expand-file-name "candidates.scm"
                                    (file-name-directory load-file-name)))))

(setq uim-el-agent (getenv "UIM_EL_AGENT"))
(setq uim-el-helper-agent (getenv "UIM_EL_HELPER_AGENT"))

(require 'uim)

;; Hiragana ka, as an escape so that this file stays ASCII.
(defconst uim-test-ka "\u304b")

(defun uim-test-type (im keys)
  "Type KEYS into a new buffer with uim-mode on and IM.
Return a plist of the buffer's text with its properties, whether
candidates were shown, and the text once uim-mode is off again."
  (setq uim-default-im-engine im)
  (let ((buffer (generate-new-buffer "*uim-test*"))
        result)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (uim-mode 1)
          (execute-kbd-macro (kbd keys))
          (setq result (list :text (buffer-string)
                             :candidate-displayed
                             (and uim-candidate-displayed t)))
          (uim-mode 0)
          (append result (list :text-after-off (buffer-string))))
      (kill-buffer buffer))))

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
