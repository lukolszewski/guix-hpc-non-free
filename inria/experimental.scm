;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019 Inria

(define-module (inria experimental)
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
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (guix utils)
  )

(define-public qr_mumps
  (package
    (name "qr_mumps")
    (version "2.0")
    (home-page "http://buttari.perso.enseeiht.fr/qr_mumps/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))
                    ;; We do not need using submodule for the moment
                    ;; (recursive? #t)
                    ))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vsksr1n2wa1xabdfc0c7l5w78jxsy4bzf02s8d8sy3rjr2l9zy2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '(
                           "-Wno-dev"
                           ;; "-DBUILD_SHARED_LIBS=ON"
                           "-DQRM_WITH_STARPU=ON"
                           "-DQRM_ORDERING_SCOTCH=ON"
                           "-DQRM_ORDERING_METIS=ON"
                           ;; "-DQRM_WITH_CUDA=OFF"
                           ;; "-DARITH=d"
                           ;; "-DCMAKE_BUILD_TYPE=Release"
                           )

       #:phases (modify-phases %standard-phases
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      ;; StarPU expects $HOME to be writable.
                      (setenv "HOME" (getcwd))
                      )))

       ;; No make test rule yet
       #:tests? #f))

     ;; TODO: refine which package shall be native, propagated, or regular input
     (native-inputs `(("gforgran" ,gfortran)
                      ("pkg-config" ,pkg-config)))
     (inputs `(("openblas" ,openblas)
               ("ssh" ,openssh)
               ("perl" ,perl)
               ;; ("scotch" ,pt-scotch)
               ("scotch" ,scotch)
               ("hwloc" ,hwloc "lib")
               ("metis" ,metis)
               ("starpu" ,starpu)))
    (propagated-inputs `(("hwloc" ,hwloc "lib")
                         ;; ("scotch" ,scotch)
                         ))
    (synopsis "Sparse QR direct solver (experimental package)")
    (description
     "qr_mumps is a software package for the solution of sparse, linear systems
on multicore computers based on the QR factorization of the input matrix.
Therefore, it is suited to solving sparse least-squares problems and to
computing the minimum-norm solution of sparse, underdetermined problems. It can
obviously be used for solving square problems in which case the stability
provided by the use of orthogonal transformations comes at the cost of a higher
operation count with respect to solvers based on, e.g., the LU factorization.
qr_mumps supports real and complex, single or double precision arithmetic." )
    (license license:cecill)))
