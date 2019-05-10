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
  #:use-module (inria storm-pm2)
  #:use-module (inria hiepacs))

;; waiting for an hdf5 patch to enable threadsafe
(define-public hd5*
  (package
    (inherit hdf5)
    (name "hdf5-flo")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments hdf5)
       ((#:configure-flags flags '())
        `(append '("--with-pthread" "--enable-threadsafe" "--enable-unsupported")
                 ,flags))
       ((#:modules modules '((guix build gnu-build-system)
                             (guix build utils)))
        (cons `(srfi srfi-1) modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-configure
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "configure"
                 (("/bin/mv") "mv"))
               ;; The rest of the original 'patch-configure' phase is
               ;; concerned with Fortran things.
               #t))
           ;; Remove the 'split' phase, which is about moving Fortran files
           ;; to the "fortran" output.
           (delete 'split)))))))

(define-public fmr
  (package
    (name "fmr")
    (version "55938b987201c89f007eae3cc321597536096c06")
    (home-page "https://gitlab.inria.fr/piblanch/fmr")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)
                    ;; We need the submodule in 'CMakeModules/morse_cmake'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0bw71a8yqdy3ck8faz6vqryhz1ghwwsvbcgdd5r0wcchhkxxwd6k"))))
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
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)
                    ;; We need the submodule in 'cmake_modules/morse'.
                    (recursive? #t)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0bw71a8yqdy3ck8faz6vqryhz1ghwwsvbcgdd5r0wcchhkxxwd6k"))))
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
