;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020, 2022 Inria

(define-module (hacky suitesparse-mkl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-1)
  #:use-module (non-free mkl))

(define-public suitesparse-mkl
  (package
   (inherit suitesparse)
   (name "suitesparse-mkl")
   (inputs
    `(("mkl" ,mkl) ("libomp" ,libomp)
      ,@(alist-delete "openblas" (package-inputs suitesparse))))
   (arguments
    `(#:modules ((guix build utils) (guix build gnu-build-system) (srfi srfi-1))
      ,@(substitute-keyword-arguments
         (package-arguments suitesparse)
         ((#:make-flags flags)
          `(cons
            (string-append "MKLROOT=" (assoc-ref %build-inputs "mkl"))
            (lset-difference string=? ,flags
                             (list "BLAS=-lopenblas" "LAPACK=-lopenblas"))))
         )))
   (synopsis "Suite of sparse matrix software (compiled with Intel® MKL instead
of OpenBLAS)")))
