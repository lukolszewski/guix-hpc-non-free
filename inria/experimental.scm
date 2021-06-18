;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria experimental)
  #:use-module (cnrs irit)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wget)
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (non-free cuda)
  #:use-module (inria tainted storm)
  #:use-module (guix utils)
  )


(define-public qr_mumps+cuda
  (package
   (inherit qr_mumps)
   (name "qr_mumps-cuda")
   (arguments
    (substitute-keyword-arguments (package-arguments qr_mumps)
                                  ((#:configure-flags flags '())
                                   `(cons "-DQRM_WITH_CUDA=ON" ,flags))))
   (inputs
    `(("cuda" ,cuda)
      ,@(package-inputs chameleon)))
   (propagated-inputs `(("starpu" ,starpu+cuda)
                        ,@(delete `("starpu" ,starpu) (package-inputs qr_mumps))))))

