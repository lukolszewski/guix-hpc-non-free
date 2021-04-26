;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020, 2021 Inria

(define-module (bsc alya)
  #:use-module (guix)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (inria hiepacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fabric-management))

(define gfortran-10
  ;; (custom-gcc gcc-10 "gfortran" '("fortran")
  ;;             (list (search-path-specification
  ;;                    (variable "CPATH")
  ;;                    (files '("include")))
  ;;                   (search-path-specification
  ;;                    (variable "LIBRARY_PATH")
  ;;                    (files '("lib" "lib64")))))
  (package
   (inherit gfortran)
   (version (package-version gcc-10))
   (source (package-source gcc-10))))

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
                   (url "https://gitlab.com/bsc-alya/projects/alya-maphys.git")
                   (commit "f0306db3a3b99e1d19f1165a7d2c7e6f71a88b23)")
                   (recursive? #f)))
             (file-name (string-append name "-" version "-checkout"))
             (patches (search-patches "bsc/maphys.patch"))
             (sha256
              (base32
               "0hwl1j171hmax0sfpk3hvn7rsw4z6qi03vsdkvmbgfw4bffjcjya"))))
    (arguments
     '(#:configure-flags '("-DWITH_MAPHYS=OFF")))
    (build-system cmake-build-system)
    (inputs `(("openmpi" ,openmpi)
              ("ssh" ,openssh)
              ;; ("maphys" ,maphys)
              ("openblas" ,openblas)))
    (native-inputs `(("gfortran" ,gfortran-10)
                     ("pkg-config" ,pkg-config)))
    (license #f)))
