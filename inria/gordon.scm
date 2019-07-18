;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2018 Inria

(define-module (inria gordon)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (non-free mkl)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (inria hiepacs))


(define-public fmr
  (package
    (name "fmr")
    (version "55938b987201c89f007eae3cc321597536096c06")
    (home-page "https://gitlab.inria.fr/piblanch/fmr")
    (source #f)
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DFMR_USE_CHAMELEON=ON")

       ;; FIXME: don't know how to run FMR tests for now
       #:tests? #f))
    (inputs `(("lapack" ,openblas)
              ("hdf5" , hdf5)
              ("chameleon" ,chameleon)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)))
    (synopsis "Fast and accurate Methods for Randomized numerical linear algebra")
    (description
     "This project provides routines for performing low-rank matrix
approximations based on rando mized techniques.")
    (license license:cecill-c)))

(define-public diodon
  (package
    (name "diodon")
    (version "55938b987201c89f007eae3cc321597536096c06")
    (home-page "https://gitlab.inria.fr/afranc/diodon")
    (synopsis "Librairies for Multivariate Data Analysis and
Dimensionality Reduction for very large datasets")
    (description
     "Librairies for Multivariate Data Analysis and Dimensionality
Reduction for very large datasets.")
    (license license:cecill-c)
    (source #f)
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DDIODON_USE_INTERNAL_FMR=OFF"
                           (string-append "-DFMR_INCLUDE_DIR=" fmr "/include")
                           "-DDIODON_USE_CHAMELEON=ON")

       ;; FIXME: don't know how to run Diodon tests for now
       #:tests? #f))

    ;; FIXME: should use hdf5-parallel-openmpi ?
    (inputs `(("zlib" ,zlib)
              ("hdf5" , hdf5)
              ("lapack" ,openblas)
              ("chameleon" ,chameleon)
              ("fmr" ,fmr)))))
