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
  #:use-module (gnu packages wget)
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (non-free cuda)
  #:use-module (inria tainted storm)
  #:use-module (guix utils)
  )

(define-public qr_mumps
  (package
   (name "qr_mumps")
   (version "a3027b41545bd419dbbcda0ca86c66359e05f6b5")
   (home-page "http://buttari.perso.enseeiht.fr/qr_mumps/")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://gitlab.com/qr_mumps/qr_mumps/")
                  (commit version)
		              ;; (commit (string-append "v" version))
                  ;; We do not need using submodule for the moment
                  ;; (recursive? #t)
                  ))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1gzz2kzgwcc4ljdhlxnphwymhgb04nq5f125cirn651ad8lwk4l7"))))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags  (list
                          "-Wno-dev"
                          "-DBUILD_SHARED_LIBS=ON"
                          (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                         "-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib")
                          "-DQRM_WITH_STARPU=ON"
                          "-DQRM_ORDERING_SCOTCH=ON"
                          "-DQRM_ORDERING_METIS=ON"
			                    "-DQRM_WITH_MPI=ON"
                          "-DCMAKE_BUILD_TYPE=Release"
                          )
      #:phases (modify-phases %standard-phases
                              (add-before 'check 'prepare-test-environment
			                                    (lambda _
				                                    (setenv "HOME" (getcwd)) ;; StarPU expects $HOME to be writable.
	                                          (setenv "OMP_NUM_THREADS" "1")
				                                    (setenv "OMPI_MCA_rmaps_base_oversubscribe" "1") #t)))
      ;; No make test rule yet
      #:tests? #f))
   ;; TODO: refine which package shall be native, propagated, or regular input
   (native-inputs `(("gfortran" ,gfortran)
                    ("pkg-config" ,pkg-config)))
   (inputs `(("metis" ,metis)
             ("openblas" ,openblas)
             ("perl" ,perl)
             ("scotch32" ,scotch32)
	           ("ssh" ,openssh)
             ;; ("suitesparse" ,suitesparse) ;; for colamd; it would ideally be suitesparse:colamdonly
             ("wget" ,wget)
             ))
   (propagated-inputs `(("starpu" ,starpu)))
   (synopsis "Sparse QR direct solver (experimental package for distributed memroy version)")
   (description
    "qr_mumps is a software package for the solution of sparse, linear systems
on multicore computers based on the QR factorization of the input matrix.
Therefore, it is suited to solving sparse least-squares problems and to
computing the minimum-norm solution of sparse, underdetermined problems. It can
obviously be used for solving square problems in which case the stability
provided by the use of orthogonal transformations comes at the cost of a higher
operation count with respect to solvers based on, e.g., the LU factorization.
qr_mumps supports real and complex, single or double precision arithmetic. This
is an experimental version of the package for distributed memory." )
   (license license:cecill)))

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

