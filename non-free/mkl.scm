;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2019 Inria

(define-module (non-free mkl)
  #:use-module (guix)
  #:use-module (guix inferior)
  #:use-module (guix memoization)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1))

(define channels
  ;; This is the old revision from which we want to
  ;; extract guile-json.
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit
          "c81457a5883ea43950eb2ecdcbb58a5b144bcd11"))))

(define inferior
  ;; An inferior representing the above revision.
  (mlambda ()
    (inferior-for-channels channels)))
