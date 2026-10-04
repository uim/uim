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
;; Runs the tests in this directory. Run it with the build tree's
;; uim.el on the load path and UIM_EL_AGENT and UIM_EL_HELPER_AGENT
;; set:
;;
;;   emacs -Q --batch -L emacs -l emacs/test/run.el

(require 'ert)
(require 'ert-x)

;; uim.el blocks without a limit writing to an agent that stopped
;; reading. A hang must fail the tests, not hold the build.
(run-at-time 60 nil
             (lambda ()
               (let ((test (ert-running-test)))
                 (message "Timed out%s"
                          (if test
                              (format " in %S" (ert-test-name test))
                            "")))
               (kill-emacs 1)))

(defvar uim-test-directory (make-temp-file "uim-el-test" t))

(add-hook 'kill-emacs-hook
          (lambda () (delete-directory uim-test-directory t)))

;; The agents read the user's uim configuration and look for
;; uim-helper-server under these, so the tests keep them to their own.
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

;; uim-leim changes uim.el as it is loaded, so every test gets it.
(require 'uim)
(require 'uim-leim)

;; Hiragana ka, as an escape so that the tests stay ASCII.
(defconst uim-test-ka "\u304b")

(dolist (file (directory-files (file-name-directory load-file-name)
                               t "\\`test-.*\\.el\\'"))
  (load file nil t))

(ert-run-tests-batch-and-exit)
