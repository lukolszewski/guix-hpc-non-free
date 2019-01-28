;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2019 Inria

(define-module (inria tainted hiepacs)
  #:use-module (guix)
  #:use-module (inria hiepacs)
  #:use-module (non-free mkl)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public chamelon/mkl
  ;; Chameleon linked against MKL.
  (package
    (inherit chameleon)
    (name "chameleon-mkl")
    (inputs `(("blas" ,mkl)
              ("lapack" ,mkl)
              ,@(fold alist-delete (package-inputs chameleon)
                      '("blas" "lapack"))))))
