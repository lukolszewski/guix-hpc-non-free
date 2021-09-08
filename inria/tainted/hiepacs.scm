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
  #:use-module (gnu packages ssh)
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
                                   `(cons "-DBLA_VENDOR=Intel10_64lp" (cons "-DCBLAS_MT=ON" (cons "-DLAPACKE_MT=ON" ,flags))))))
   (inputs `(("lapack" ,mkl)
             ,@(delete `("lapack" ,openblas) (package-inputs chameleon))))))

(define-public chameleon+cuda+mkl+mt
  (package
   (inherit chameleon+mkl+mt)
   (name "chameleon-cuda-mkl-mt")
   (arguments
    (substitute-keyword-arguments (package-arguments chameleon+mkl+mt)
                                  ((#:configure-flags flags '())
                                   `(cons "-DCHAMELEON_USE_CUDA=ON" ,flags))))
   (inputs
    `(("cuda" ,cuda)
      ,@(package-inputs chameleon+mkl+mt)))
   (propagated-inputs `(("starpu" ,starpu+cuda)
                        ,@(delete `("starpu" ,starpu) (package-inputs chameleon+mkl+mt))))))

(define-public pastix+cuda
  (package
   (inherit pastix)
   (name "pastix-cuda")
   (arguments
    (substitute-keyword-arguments (package-arguments pastix)
                                  ((#:configure-flags flags '())
                                   `(cons "-DPASTIX_WITH_CUDA=ON" ,flags))))
   (inputs
    `(("cuda" ,cuda)
      ("starpu" ,starpu+cuda)
      ,@(delete `("starpu" ,starpu) (package-inputs pastix))))))

(define-public fmr
  (package
   (name "fmr")
   (version "0")
   (home-page "https://gitlab.inria.fr/compose/oldstack/fmr")
   (source (git-checkout (url "git@gitlab.inria.fr:compose/oldstack/fmr.git")
                         (recursive? #t)))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DFMR_BUILD_TESTS=ON"
                          "-DFMR_USE_HDF5=ON")
                        ;; FIXME: trouble with STARPU /tmp dir.
                        #:tests? #f))

   (inputs `(("zlib" , zlib)
             ("bzip2" , bzip2)
             ("hdf5" , hdf5-1.10)
             ("lapack" ,mkl)))
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("gfortran" ,gfortran)))
   (synopsis "Fast and accurate Methods for Randomized numerical linear algebra")
   (description
    "This project provides routines for performing low-rank matrix
approximations based on randomized techniques.")
   (license license:cecill-c)))

;; FIXME: hdf5 delete in package inputs do not work
;;(define-public fmr+mpi
;;  (package
;;    (inherit fmr)
;;    (name "fmr-mpi")
;;    (arguments
;;     (substitute-keyword-arguments (package-arguments fmr)
;;                                   ((#:configure-flags flags '())
;;                                    `(cons "-DFMR_USE_CHAMELEON=ON" ,flags))))
;;    (inputs `(("chameleon" ,chameleon+mkl+mt)
;;              ("hdf5" ,hdf5-parallel-openmpi)
;;              ,@(delete `("hdf5" ,hdf5-1.10) (package-inputs fmr))))))

(define-public fmr+mpi
  (package
   (name "fmr-mpi")
   (version "0")
   (home-page "https://gitlab.inria.fr/compose/oldstack/fmr")
   (source (git-checkout (url "git@gitlab.inria.fr:compose/oldstack/fmr.git")
                         (recursive? #t)))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DFMR_BUILD_TESTS=ON"
                          "-DFMR_USE_HDF5=ON"
                          "-DFMR_USE_CHAMELEON=ON")
                        ;; FIXME: trouble with STARPU /tmp dir.
                        #:tests? #f))

   (inputs `(("zlib" , zlib)
             ("bzip2" , bzip2)
             ("hdf5" , hdf5-parallel-openmpi)
             ("lapack" ,mkl)
             ("chameleon" ,chameleon+mkl+mt)))
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("gfortran" ,gfortran)
                    ("ssh" ,openssh)))
   (synopsis "Fast and accurate Methods for Randomized numerical linear algebra")
   (description
    "This project provides routines for performing low-rank matrix
approximations based on randomized techniques.")
   (license license:cecill-c)))

(define-public cppdiodon
  (package
   (name "cppdiodon")
   (version "0")
   (home-page "https://gitlab.inria.fr/diodon/cppdiodon")
   (source (git-checkout (url "git@gitlab.inria.fr:diodon/cppdiodon.git")
                         (branch "master")))     ;or (commit "1234abc")
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DBUILD_SHARED_LIBS=ON"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DDIODON_USE_INTERNAL_FMR=OFF")
                        ;; FIXME: trouble with STARPU /tmp dir.
                        #:tests? #f))

   (inputs `(("zlib" ,zlib)
             ("bzip2" , bzip2)
             ("hdf5" , hdf5-1.10)
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

;; FIXME: fmr and hdf5 delete in package inputs do not work
;; (define-public cppdiodon+mpi
;;   (package
;;     (inherit cppdiodon)
;;     (name "cppdiodon-mpi")
;;     (arguments
;;      (substitute-keyword-arguments (package-arguments cppdiodon)
;;                                    ((#:configure-flags flags '())
;;                                     `(cons "-DDIODON_USE_CHAMELEON=ON" ,flags))))
;;     (inputs `(("chameleon" ,chameleon+mkl+mt)
;;               ("fmr" ,fmr+mpi)
;;               ("hdf5" ,hdf5-parallel-openmpi)
;;               ,@(delete `("fmr" ,fmr) (package-inputs cppdiodon))
;;               ,@(delete `("hdf5" ,hdf5-1.10) (package-inputs cppdiodon))))))

(define-public cppdiodon+mpi
  (package
   (name "cppdiodon-mpi")
   (version "0")
   (home-page "https://gitlab.inria.fr/diodon/cppdiodon")
   (source (git-checkout (url "git@gitlab.inria.fr:diodon/cppdiodon.git")
                         (branch "master")))     ;or (commit "1234abc")
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DBUILD_SHARED_LIBS=ON"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DDIODON_USE_INTERNAL_FMR=OFF"
                          "-DDIODON_USE_CHAMELEON=ON")
                        ;; FIXME: trouble with STARPU /tmp dir.
                        #:tests? #f))

   (inputs `(("zlib" ,zlib)
             ("bzip2" , bzip2)
             ("hdf5" , hdf5-parallel-openmpi)
             ("lapack" ,mkl)
             ("chameleon" ,chameleon+mkl+mt)
             ("fmr" ,fmr+mpi)))
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("gfortran" ,gfortran)
                    ("ssh" ,openssh)))

   (synopsis "Librairies for Multivariate Data Analysis and
Dimensionality Reduction for very large datasets")
   (description
    "Librairies for Multivariate Data Analysis and Dimensionality
Reduction for very large datasets.")
   (license license:cecill-c)))

(define-public disseq
  (package
   (name "disseq")
   (version "0")
   (home-page "https://gitlab.inria.fr/metabarcoding/pairwise_dis")
   (source (git-checkout (url "git@gitlab.inria.fr:metabarcoding/pairwise_dis.git")
                         (branch "master")))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DBUILD_SHARED_LIBS=OFF"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DDISSEQ=ON"
                          "-DMPI_DISSEQ=ON")
                        #:phases (modify-phases %standard-phases
                                                (add-after 'unpack 'chdir
                                                           (lambda _
                                                             (chdir "src"))))
                        #:tests? #f))

   (inputs `(("mpi" ,openmpi)))

   (synopsis "Compute pairwise distances between reads as edit distances")
   (description
    "This package has been developed for computing exact distances,
     without heuristics, between all pairs of reads of a NGS
     sample. This is a first step for supervised or unsupervised clustering
     of reads in an environmetal sample.")
   (license license:cecill-c)))
