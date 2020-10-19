;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (inria hawen)
               #:use-module (guix)
               #:use-module (guix git)                         ;for 'git-checkout'
               #:use-module (guix git-download)
               #:use-module (guix build-system gnu)
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







; here we define to select whichj
(define (make-hawen problem direction)
  (let ((commit "c58553648a3ce0b99d5dbd41129e3d4d6418d0ce")
        (revision "0"))
    (package
      (name (string-append "hawen " "-" problem "-" direction))
      (version "0.5")
      (home-page "git@gitlab.inria.fr:ffaucher/hawen_work.git")

      (source (git-checkout
              (url "git@gitlab.inria.fr:ffaucher/hawen_work.git")
              (commit commit)))

      (build-system gnu-build-system)
      (arguments
        `(#:tests? #f
          #:parallel-build? #f 
          #:phases 
           (modify-phases %standard-phases
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (bin (string-append out "/bin")))
                        (install-file "../bin/forward_helmholtz_acoustic-iso_hdg.out" bin))))
             (add-after 'build 'mpi-setup
                          ,%openmpi-setup)
                ;; go into code subdirectory,  yes I know it is verbose 
                ;; only for  a "cd" command 😀
                (add-after
                  'unpack 'chdir-to-src
                  (lambda _ (chdir "code") #t))
                ;; we replace the phase configure by writing
                ;; in the make.version_config file
                (replace
                  'configure 
                  (lambda* (#:key outputs inputs #:allow-other-keys)  
                    (let* ((in_metis (assoc-ref inputs "metis")) ; FIXME : factorize!!!
                           (in_scotch (assoc-ref inputs "scotch"))
                           (in_mumps (assoc-ref inputs "mumps-openmpi"))
                           (in_arb (assoc-ref inputs "arb"))
                           (in_arpack (assoc-ref inputs "arpack-ng-openmpi"))
                           )
                      ; write the make.version_config file
                      (call-with-output-file "config/make.version_config"
                        (lambda (port)
                          (format port "
F90  := mpif90
ARCH := GNU
include $(ROOT_DIR)/config/make.options_${ARCH}
LMETIS    = -L~a/lib -lmetis
IMETIS    = -I~a/include
LSCOTCH   = -L~a/lib
ISCOTCH   = -L~a/include
LMUMPS    = -L~a/lib -lcmumps -lzmumps -lmumps_common -lpord
LMUMPS   += -L~a/lib -lscalapack -lopenblas
IMUMPS    = -I~a/include
LARB      = -L~a/lib -larb
IARB      = -I~a/include
ARPACK    = -L~a/lib -larpack
LPARPACK  = -L~a/lib -lparpack
"         in_metis in_metis 
          in_scotch in_scotch 
          in_mumps in_mumps in_mumps
          in_arb in_arb
          in_arpack in_arpack
                          ) 
                        ) 
                      )
                      (call-with-output-file "config/make.version"
                        (lambda (port)
                          (format port "
ROOT_DIR         :=$(shell pwd)
MAKE_CONFIG_FILE :=$(ROOT_DIR)/config/make.version_config
BINARY_DIR       :=$(ROOT_DIR)/../bin
LIBRARY_DIR      :=$(ROOT_DIR)/../library
DEBUG            :=0
OPTIMIZATION     :=1
## parallelism can only be mpi for now
PARALLELISM    := mpi
PROBLEM        := ~a
DISCRETIZATION := hdg
PROPAGATOR     := ~a
# MUMPS_LATEST MUST BE SET TO 0 IF THE VERSION OF MUMPS IS < 5.3.3
MUMPS_LATEST=0
# ARB, ARPACK AND PARPACK DEPENDENCIES
DEPENDENCY_ARB=1
DEPENDENCY_ARPACK=1
DEPENDENCY_PARPACK=1
"          direction problem
                          ) 
                        ) 
                      ) 
                    ) 
                  ) 
                ) 
              )
            )
          )
      (native-inputs
        `(("gfortran" ,gfortran)))
      (inputs
        `(("mumps-openmpi" ,mumps-openmpi)
          ("arpack-ng-openmpi" , arpack-ng-openmpi)
          ("metis" ,metis)
          ("openblas" ,openblas)
          ("scalapack" ,scalapack)
          ("pt-scotch" ,pt-scotch)
          ("scotch" ,scotch)
          ("openmpi" ,openmpi)
          ("arb" ,arb)))
      (synopsis "Hawen")
      (description "Hawen is a DG wave propagation simulating library for solving inverse problems ")
      (license #f ))))
(define-public hawen-helmholtz_elastic-iso-inversion
 (make-hawen "helmholtz_elastic-iso" "inversion"))

(define-public hawen-elastic-forward
 (make-hawen "helmholtz_elastic-iso" "forward"))

(define-public hawen-acoustic-inversion
 (make-hawen "helmholtz_acoustic-iso" "inversion"))

(define-public hawen-acoustic-forward
 (make-hawen "helmholtz_acoustic-iso" "forward"))

