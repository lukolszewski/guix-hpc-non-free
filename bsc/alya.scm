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
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages fabric-management))

(define-public alya
  (package
   (name "alya")
   (version "2.9.0")
   (home-page "https://gitlab.com/bsc-alya/alya")
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
                   (url "https://gitlab.com/bsc-alya/alya.git")
                   (commit "40b05f6aae6013f46fbbef696ae267ddc95fac2a")
                   (recursive? #f)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
		"0rz5li8z22f8hyvzhs391ca4ydbk7gmhqaw5mn9qmk46spsgs2h8"))))
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

