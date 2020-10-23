;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2019, 2020 Inria

(define-module (inria aerosol)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (inria pampa))

(define-public aerosol
  (let ((commit "61248eaf8c42e541be1bbd2356ed8e06fc680a37")
        (revision "0"))
    (package
      (name "aerosol")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.inria.fr/aerosol/aerosol")

      ;; This will clone the repository over SSH on the client side, provided
      ;; you have permissions to do so.
      ;(source (git-checkout
      ;         (url "git@gitlab.inria.fr:aerosol/aerosol.git")
      ;         (commit commit)))
      (source (origin
              (method url-fetch)
              (uri "/local/home/mhaefele/dev/aerosol.tgz")))

      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DWITH_TEST=ON")    ;build the test suite
         #:tests? #f                          ;FIXME: there are test failures
         #:phases (modify-phases %standard-phases
                    (add-after 'build 'mpi-setup
                      ;; Set the test environment for Open MPI.
                      ,%openmpi-setup))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("util-linux" ,util-linux)))           ;tests need the 'rev' command
      (inputs
       `(,(assoc "openmpi" (package-inputs pampa)) ;use the same MPI as PaMPA
         ("pampa" ,pampa)
         ("hdf5-parallel-openmpi" ,hdf5-parallel-openmpi)
         ("openblas" ,openblas)
         ("suitesparse" ,suitesparse)
         ,(assoc "pt-scotch" (package-inputs pampa))
         ("libxml2" ,libxml2)))
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
