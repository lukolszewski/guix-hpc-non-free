;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (inria hawen)
               #:use-module (guix)
               #:use-module (guix git)                         ;for 'git-checkout'
               #:use-module (guix git-download)
               #:use-module (guix build-system cmake)
               #:use-module ((guix licenses) #:prefix license:)
               #:use-module (inria mpi)
               #:use-module (gnu packages algebra)
               #:use-module (gnu packages gcc)
               #:use-module (gnu packages maths)
               #:use-module (inria hiepacs)
               #:use-module (inria storm)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages ssh)
               #:use-module (gnu packages mpi)
               #:use-module (gnu packages fabric-management))
(define-public hawen
               (let ((commit "c58553648a3ce0b99d5dbd41129e3d4d6418d0ce")
                     (revision "0"))
                 (package
                   (name "hawen")
                   (version "0.5")
                   (home-page "git@gitlab.inria.fr:ffaucher/hawen_work.git")

                   ;; This will clone the repository over SSH on the client side, provided
                   ;; you have permissions to do so.license:bsd-3
                   (source (git-checkout
                             (url "git@gitlab.inria.fr:ffaucher/hawen_work.git")
                             (commit commit)))

                   (build-system cmake-build-system)
                   (arguments
                     `(#:phases (modify-phases %standard-phases
                                               (add-after 'build 'mpi-setup
                                                          ;; Set the test environment for Open MPI.
                                                          ,%openmpi-setup))

                       #:tests? #f
                       #:build-type "Release"))                            ;FIXME
                   (native-inputs
                     `(("gfortran" ,gfortran)))
                   (inputs
                     `(("mumps-openmpi" ,mumps-openmpi)
                       ("metis" ,metis)
                       ("openblas" ,openblas)
                       ("maphys", maphys)
                       ("pastix", pastix)
                       ("starpu", starpu)
                       ("scalapack" ,scalapack)
                       ("pt-scotch" ,pt-scotch)
                       ("scotch" ,scotch)
                       ("openmpi" ,openmpi)
                       ("openssh" ,openssh)
                       ("arb" ,arb)))
                   (synopsis "Hou10ni")
                   (description
                     "hou10ni is a DG wave propagation simulating library ")
                   (license #f))))
