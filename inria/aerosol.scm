;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019, 2020 Inria

(define-module (inria aerosol)
  #:use-module (guix)
  #:use-module (guix channels)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (inria pampa)
  #:use-module (inria mpi))

(define openmpi-with-mpi1-compat-instead-of-openmpi
  ;; This is a procedure to replace OPENMPI by OPENMPI-WITH-MPI1-COMPAT,
  ;; recursively.
  (package-input-rewriting `((,openmpi . ,openmpi-with-mpi1-compat))))

(define petsc-openmpi-cxx98 ;; Deactivate C++11 default dialect, otherwise Aerosol does not compile
  (package
    (inherit petsc-openmpi)
    (name "petsc-openmpi-cxx98")

    (arguments
      (substitute-keyword-arguments (package-arguments petsc-openmpi)
                                    ((#:configure-flags cf)
                                    #~(cons "--with-cxx-dialect=0" 
                                            #$cf))))))

(define aerosol-std
  (let ((commit "f0430f10d9e59950553187af9740571ffde9419f")
        (revision "1"))
    (package
      (name "aerosol-std")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.inria.fr/aerosol/aerosol")

      ;; This will clone the repository over SSH on the client side, provided
      ;; you have permissions to do so.
      (source (git-checkout
               (url "git@gitlab.inria.fr:aerosol/aerosol.git")
               (commit commit)))

      (build-system cmake-build-system)
      (arguments
        `(#:configure-flags 
          (list 
            "-DWITH_TEST=ON"
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DWITH_BLOCKDIAGONALSOLVER=OFF"
            "-DWITH_DIAGONALSOLVER=ON"
            "-DWITH_PETSC=ON"
            "-DWITH_UMFPACK=OFF"
            "-DWITH_HDF5=ON"
            "-DWITH_SIMULATIONS=OFF"
            (string-append "-DCMAKE_INSTALL_PREFIX=" 
                           (assoc-ref %outputs "out")))
        #:phases (modify-phases %standard-phases
                                (add-after 'build 'mpi-setup
                                           ;; Set the test environment for Open MPI.
                                           ,%openmpi-setup))))
      (native-inputs
        (list pkg-config util-linux))           ;tests need the 'rev' command

      (propagated-inputs 
        (list openmpi 
              pampa
              openblas
              suitesparse
              hdf5-parallel-openmpi
              petsc-openmpi-cxx98
              pt-scotch32
              libxml2))

      (synopsis "Library of finite elements methods")
      (description
       "AeroSol library has been developed within CAGIRE and Cardamom (and
formerly Bacchus) teams.  The aim is to develop a library able to deal with
continuous and discontinuous finite elements methods on hybrid and possibly
curvilinear meshes, and able to run on heterogeneous architectures.  Fitting
with the first axis of the Bordeaux Sud-Ouest development strategy, which is
to build a coherent software suite scalable and efficient on new
architectures, AeroSol library relies on several tools developed in other
Inria teams, especially for the management of the parallel aspects.")
      (license #f))))                             ;private software so far


(define-public aerosol
  (package
    (inherit (openmpi-with-mpi1-compat-instead-of-openmpi aerosol-std))
    (name "aerosol")))



