;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2019 Inria

(define-module (inria tainted hiepacs)
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
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (non-free cuda)
  #:use-module (non-free mkl)
  #:use-module (inria tainted storm))

(define-public chameleon+cuda
  (package
    (inherit chameleon)
    (name "chameleon-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments chameleon)
                                   ((#:configure-flags flags '())
                                    `(cons "-DCHAMELEON_USE_CUDA=ON" ,flags))))
    (inputs
     `(("cuda" ,cuda)
       ,@(package-inputs chameleon)))
    (propagated-inputs `(("starpu" ,starpu+cuda)
                         ,@(delete `("starpu" ,starpu) (package-inputs chameleon))))))

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
                          (branch "diodon")     ;or (commit "1234abc")
                          (recursive? #t)))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")

       ;; FIXME: don't know how to run FMR tests for now
       #:tests? #f))
    (inputs `(("zlib" , zlib)
              ("hdf5" , hdf5)
              ("lapack" ,mkl)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)))
    (synopsis "Fast and accurate Methods for Randomized numerical linear algebra")
    (description
     "This project provides routines for performing low-rank matrix
approximations based on randomized techniques.")
    (license license:cecill-c)))

(define-public fmr+mpi
  (package
    (inherit fmr)
    (name "fmr-mpi")
    (arguments
     (substitute-keyword-arguments (package-arguments fmr)
                                   ((#:configure-flags flags '())
                                    `(cons "-DFMR_USE_CHAMELEON=ON" ,flags))))
    (inputs `(("chameleon" ,chameleon+mkl+mt)
              ("hdf5" ,hdf5-parallel-openmpi)
              ,@(package-inputs fmr)))))

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
                           "-DDIODON_USE_INTERNAL_FMR=OFF")
       #:phases (modify-phases %standard-phases
                               (add-after 'unpack 'chdir
                                          (lambda _
                                            (chdir "cpp"))))

       ;; FIXME: don't know how to run Diodon tests for now
       #:tests? #f))

    (inputs `(("zlib" ,zlib)
              ("hdf5" , hdf5)
              ("lapack" ,mkl)
              ("fmr" ,fmr)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gfortran" ,gfortran)))

    (synopsis "Librairies for Multivariate Data Analysis and
Dimensionality Reduction for very large datasets")
    (description
     "Librairies for Multivariate Data Analysis and Dimensionality
Reduction for very large datasets.")
    (license license:cecill-c)))

(define-public diodon+mpi
  (package
    (inherit diodon)
    (name "diodon-mpi")
    (arguments
     (substitute-keyword-arguments (package-arguments diodon)
                                   ((#:configure-flags flags '())
                                    `(cons "-DDIODON_USE_CHAMELEON=ON" ,flags))))
    (inputs `(("chameleon" ,chameleon+mkl+mt)
              ("fmr" ,fmr+mpi)
              ("hdf5" ,hdf5-parallel-openmpi)
              ,@(delete `("fmr" ,fmr) (package-inputs diodon))))))
