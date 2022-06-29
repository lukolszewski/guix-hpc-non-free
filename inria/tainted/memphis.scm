;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2019, 2022 Inria

(define-module (inria tainted memphis)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git)                         ;for 'git-checkout'
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  #:use-module (inria memphis)
  #:use-module (inria hiepacs)
  )

(define-public neos
  (package
   (name "neos")
   (version "0")
   (source (git-checkout (url "git@gitlab.inria.fr:memphis/neos.git")
                         (branch "master")
                         (recursive? #t)))
   (inputs
    (list libxml2 openblas bitpit))
   (propagated-inputs
    (list openmpi petsc-openmpi maphys++))
   (native-inputs
    (list pkg-config gfortran openssh))
   (build-system cmake-build-system)
   (arguments
    '(#:configure-flags `("-DBUILD_SHARED_LIBS=ON"
                          "-DCMAKE_NO_SYSTEM_FROM_IMPORTED=ON"
                          "-DGEN_PYTHON=OFF"
                          "-DUSE_EIGEN3=OFF"
                          "-DUSE_LAPACKE=ON"
                          "-DBUILD_CORE=ON"
                          "-DBUILD_GEOMETRY=ON"
                          "-DBUILD_LAPLACIAN=ON"
                          "-DENABLE_MPI=ON"
                          "-DBUILD_EXAMPLES=ON"
                          "-DBUILD_TESTS=ON"
                          ,(string-append "-DPETSC_DIR="
                                          (assoc-ref %build-inputs "petsc-openmpi")))
      #:tests? #f))

   (home-page "https://gitlab.inria.fr/memphis/neos")
   (synopsis "Numerical Enablers on OctreeS")
   (description "Numerical Enablers on OctreeS")
   (license license:lgpl3)))
