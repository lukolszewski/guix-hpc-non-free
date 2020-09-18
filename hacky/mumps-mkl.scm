;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (hacky mumps-mkl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-1)
  #:use-module (non-free mkl))

(define-public mumps-mkl
  (package
   (inherit mumps)
   (name "mumps-mkl")
   (inputs
    `(("blas" ,mkl)
      ("libomp" ,libomp)
      ,@(alist-delete "openblas" (package-inputs mumps))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments mumps)
     (
      (#:phases phases)
      `(modify-phases ,phases
                      (replace 'configure
                               (lambda* (#:key inputs #:allow-other-keys)
                                        (call-with-output-file "Makefile.inc"
                                          (lambda (port)
                                            (format port "
PLAT         =
LIBEXT       = .a
OUTC         = -o
OUTF         = -o
RM           = rm -f~:[
CC           = gcc
FC           = gfortran
FL           = gfortran
INCSEQ       = -I$(topdir)/libseq
LIBSEQ       = $(topdir)/libseq/libmpiseq.a
LIBSEQNEEDED = libseqneeded~;
CC           = mpicc
FC           = mpifort
FL           = mpifort~]
AR           = ar vr # rules require trailing space, ugh...
RANLIB       = ranlib
BLASDIR      = ~a
LIBBLAS      = -Wl,-rpath=$(BLASDIR) -Wl,-rpath='$$ORIGIN' -L$(BLASDIR)
LIBBLAS     += -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_lp64
LIBBLAS     += -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl~@[
SCALAPDIR    = ~a
SCALAP       = -Wl,-rpath=$(SCALAPDIR) -Wl,-rpath='$$ORIGIN' -L$(SCALAPDIR)
SCALAP      += -lscalapack~]
LIBOTHERS    = -pthread
CDEFS        = -DAdd_
PIC          = -fPIC
OPTF         = -O2 -DALLOW_NON_INIT -DBLR_MT -fopenmp $(PIC)
OPTL         = -O2 -fopenmp $(PIC)
OPTC         = -O2 $(PIC)
INCS         = $(INCSEQ)
OMPDIR       = ~a
LIBOMP       = -L$(OMPDIR)/lib -lgomp
LIBS         = $(SCALAP) $(LIBSEQ) $(LIBOMP)
LPORDDIR     = $(topdir)/PORD/lib
IPORD        = -I$(topdir)/PORD/include
LPORD        = $(LPORDDIR)/libpord.a
ORDERINGSF   = -Dpord~@[
METISDIR     = ~a
IMETIS       = -I$(METISDIR)/include
LMETIS       = -Wl,-rpath $(METISDIR)/lib -L$(METISDIR)/lib -lmetis
ORDERINGSF  += -Dmetis~]~@[~:{
SCOTCHDIR    = ~a
ISCOTCH      = -I$(SCOTCHDIR)/include
LSCOTCH      = -Wl,-rpath $(SCOTCHDIR)/lib -L$(SCOTCHDIR)/lib ~a-lesmumps
LSCOTCH     += -lscotch -lscotcherr
ORDERINGSF  += ~a~}~]
ORDERINGSC   = $(ORDERINGSF)
LORDERINGS   = $(LPORD) $(LMETIS) $(LSCOTCH) $(LIBSEQ) $(LIBOMP)
IORDERINGSF  = $(ISCOTCH) $(LIBOMP)
IORDERINGSC  = $(IPORD) $(IMETIS) $(ISCOTCH) $(LIBOMP)"
                        (assoc-ref inputs "mpi")
                        (assoc-ref inputs "blas")
                        (assoc-ref inputs "scalapack")
                        (assoc-ref inputs "libomp")
                        (assoc-ref inputs "metis")
                        (match (list (assoc-ref inputs "pt-scotch")
                                     (assoc-ref inputs "scotch"))
                          ((#f #f)
                           #f)
                          ((#f scotch)
                           `((,scotch "" "-Dscotch")))
                          ((ptscotch _)
                           `((,ptscotch
                              "-lptesmumps -lptscotch -lptscotcherr "
                              "-Dptscotch")))))))))))))
   (synopsis "Multifrontal sparse direct solver (compiled with OpenMP
directives-based multi-threading support and Intel® MKL instead of OpenBLAS)")))

(define-public mumps-mkl-metis
  (package
   (inherit mumps-mkl)
   (name "mumps-mkl-metis")
   (inputs
    (alist-delete "scotch" (package-inputs mumps-mkl)))))

(define-public mumps-mkl-openmpi
  (package
   (inherit mumps-mkl)
   (name "mumps-mkl-openmpi")
   (inputs
    `(("mpi" ,openmpi)
      ("scalapack" ,scalapack)
      ("pt-scotch" ,pt-scotch)
      ,@(alist-delete "scotch" (package-inputs mumps-mkl))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments mumps-mkl)
     ((#:phases phases)
      `(modify-phases ,phases
                      (add-before 'check 'mpi-setup
	                                ,%openmpi-setup)
                      (replace 'check
                               (lambda _
                                 ((assoc-ref ,phases 'check)
                                  #:exec-prefix '("mpirun" "-n" "2"))))))))
   (synopsis "Multifrontal sparse direct solver (compiled with combined MPI and
OpenMP multi-threading support and Intel® MKL instead of OpenBLAS)")))

(define-public mumps-mkl-metis-openmpi
  (package
   (inherit mumps-mkl-openmpi)
   (name "mumps-mkl-metis-openmpi")
   (inputs
    (alist-delete "pt-scotch" (package-inputs mumps-mkl-openmpi)))))
