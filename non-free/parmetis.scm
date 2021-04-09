;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2021, Inria
(define-module (non-free parmetis)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages mpi))

(define-public parmetis
  (package
    (name "parmetis")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://glaros.dtc.umn.edu/gkhome/fetch/sw/parmetis/"
                           "parmetis-" version ".tar.gz"))
       (sha256
        (base32
         "0pvfpvb36djvqlcc3lq7si0c5xpb2cqndjg8wvzg35ygnwqs5ngj"))))
    (build-system cmake-build-system)
    (native-inputs `(("openmpi" ,openmpi)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:configure-flags `("-DSHARED=ON"
                           ,"-DCMAKE_C_COMPILER=mpicc"
                           ,"-DCMAKE_CXX_COMPILER=mpic++"
                           ,"-DCMAKE_VERBOSE_MAKEFILE=1"
                           ,(string-append "-DGKLIB_PATH=../parmetis-"
                                           ,(package-version parmetis) "/metis/GKlib")
                           ,(string-append "-DMETIS_PATH=../parmetis-"
                                           ,(package-version parmetis) "/metis"))))
    (home-page "http://glaros.dtc.umn.edu/gkhome/metis/parmetis/overview")
    (synopsis "Parallel Graph Partitioning and Fill-reducing Matrix Ordering")
    (description
     "ParMETIS is an MPI-based parallel library that implements a variety of algorithms
      for partitioning unstructured graphs, meshes, and for computing fill-reducing
      orderings of sparse matrices.  ParMETIS extends the functionality provided
      by METIS and includes routines that are especially
      suited for parallel AMR computations and large scale numerical simulations.
      The algorithms implemented in ParMETIS are based on the parallel multilevel k-way
      graph-partitioning, adaptive repartitioning, and parallel multi-constrained
      partitioning schemes developed in our lab.")
    ;; see license description at http://glaros.dtc.umn.edu/gkhome/metis/parmetis/download
    (license #f)))

