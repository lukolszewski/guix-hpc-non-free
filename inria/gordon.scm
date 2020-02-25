;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2018, 2020 Inria

(define-module (inria gordon)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix git)                         ;for 'git-checkout'
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

(define-public chameleon+mkl+mt
  (package
   (inherit chameleon)
   (name "chameleon-mkl-mt")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon)
                                  ((#:configure-flags flags '())
                                   `(cons "-DBLA_VENDOR=Intel10_64lp" ,flags))))
   (inputs `(("lapack" ,mkl)
                        ,@(delete `("lapack" ,openblas) (package-inputs chameleon))))))

(define-public fmr
  (package
    (name "fmr")
    (version "0")
    (home-page "https://gitlab.inria.fr/piblanch/fmr")
    (source (git-checkout (url "git@gitlab.inria.fr:piblanch/fmr.git")
                          (branch "diodon")))     ;or (commit "1234abc")
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DFMR_USE_CHAMELEON=ON")

       ;; FIXME: don't know how to run FMR tests for now
       #:tests? #f))
    (inputs `(("zlib" , zlib)
              ("hdf5" , hdf5)
              ("lapack" ,mkl)
              ("chameleon" ,chameleon+mkl+mt)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)))
    (synopsis "Fast and accurate Methods for Randomized numerical linear algebra")
    (description
     "This project provides routines for performing low-rank matrix
approximations based on randomized techniques.")
    (license license:cecill-c)))

(define-public diodon
  (package
    (name "diodon")
    (version "0")
    (home-page "https://gitlab.inria.fr/afranc/diodon")
    (source (git-checkout (url "git@gitlab.inria.fr:afranc/diodon.git")
                          (branch "master")))     ;or (commit "1234abc")
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags `("-DBUILD_SHARED_LIBS=ON"
                           "-DDIODON_USE_INTERNAL_FMR=OFF"
                           "-DDIODON_USE_CHAMELEON=ON")
       #:phases (modify-phases %standard-phases
                               (add-after 'unpack 'chdir
                                          (lambda _
                                            (chdir "cpp"))))

       ;; FIXME: don't know how to run Diodon tests for now
       #:tests? #f))

    ;; FIXME: should use hdf5-parallel-openmpi ?
    (inputs `(("zlib" ,zlib)
              ("hdf5" , hdf5)
              ("lapack" ,mkl)
              ("chameleon" ,chameleon+mkl+mt)
              ("fmr" ,fmr)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)))

    (synopsis "Librairies for Multivariate Data Analysis and
Dimensionality Reduction for very large datasets")
    (description
     "Librairies for Multivariate Data Analysis and Dimensionality
Reduction for very large datasets.")
    (license license:cecill-c)))
