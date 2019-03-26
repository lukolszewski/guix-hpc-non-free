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
  #:use-module (inria storm)
  #:use-module (inria tainted storm)
  #:use-module (inria storm-pm2)
  #:use-module (non-free mkl)
  #:use-module (gnu packages mpi)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public chameleon/mkl
  ;; Chameleon linked against MKL.
  (package
    (inherit chameleon)
    (name "chameleon-mkl")
    (propagated-inputs `(("lapack" ,mkl)
              ,@(fold alist-delete (package-propagated-inputs chameleon)
                      '("lapack"))))))

(define-public chameleon/mkl+openmpi
  ;; Chameleon linked against MKL.
  (package
    (inherit chameleon/mkl)
    (name "chameleon-mkl-openmpi")
    (propagated-inputs `(("mpi" ,openmpi)
                         ("starpu" ,starpu+openmpi)
                         ,@(delete `("starpu" ,starpu) (package-propagated-inputs chameleon/mkl))))
     (arguments
      (substitute-keyword-arguments (package-arguments chameleon/mkl)
        ((#:configure-flags flags '())
         `(cons "-DCHAMELEON_USE_MPI=ON" ,flags))))))

 (define-public chameleon/mkl+madmpi
   (package
     (inherit chameleon/mkl)
     (name "chameleon-madmpi")
     (propagated-inputs `(("mpi" ,nmad-mini)
                          ("starpu" ,starpu+madmpi)
                          ,@(delete `("starpu" ,starpu) (package-propagated-inputs chameleon/mkl))))
     (arguments
      (substitute-keyword-arguments (package-arguments chameleon/mkl)
        ((#:configure-flags flags '())
         `(cons "-DCHAMELEON_USE_MPI=ON" ,flags))))))

 (define-public chameleon/mkl+nmad
   (package
     (inherit chameleon/mkl)
     (name "chameleon-nmad")
     (propagated-inputs `(("mpi" ,nmad)
                          ("starpu" ,starpu+nmad)
                          ,@(delete `("starpu" ,starpu) (package-propagated-inputs chameleon/mkl))))
     (arguments
      (substitute-keyword-arguments (package-arguments chameleon/mkl)
        ((#:configure-flags flags '())
         `(cons "-DCHAMELEON_USE_MPI=ON" ,flags))))))
