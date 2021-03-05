;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (bsc alya)
  #:use-module (guix)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (inria mpi)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (inria hiepacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fabric-management))

(define-public alya
  (package
   (name "alya")
   (version "2.9.0")
   (home-page "https://gitlab.bsc.es/alya/alya")
   (synopsis "Alya - High Performance Computational Mechanics")
   (description
    "Alya is a high performance computational mechanics code to solve complex
coupled multi-physics / multi-scale / multi-domain problems, which are mostly
coming from the engineering realm. Among the different physics solved by Alya we
can mention: incompressible/compressible flows, non-linear solid mechanics,
chemistry, particle transport, heat transfer, turbulence modeling, electrical
propagation, etc.")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://gitlab.bsc.es/alya/alya.git")
                   (commit "74acfff3a94621f8b44fb5ca01431eb8e5a8c6a0")
                   (recursive? #f)))
             (file-name (string-append name "-" version "-checkout"))
             (patches (search-patches "bsc/maphys.patch"))
             (sha256
              (base32
               "1p3a7sgmy5m2j28m1qnj70fzzmqn2qdrk1bsv4yzpk2a55hfrlqb"))))
    (arguments
     '(#:configure-flags '("-DWITH_MAPHYS=ON")))
    (build-system cmake-build-system)
    (inputs `(("openmpi" ,openmpi)
              ("ssh" ,openssh)
              ("maphys" ,maphys)
              ("openblas" ,openblas)))
    (native-inputs `(("gfortran" ,gfortran)
                     ("pkg-config" ,pkg-config)))
    (license #f)))
