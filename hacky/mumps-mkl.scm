;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020, 2022 Inria

(define-module (hacky mumps-mkl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (inria hiepacs)
  #:use-module (inria tadaam)
  #:use-module (srfi srfi-1)
  #:use-module (non-free mkl))

(define-public mumps-mkl
  (package
   (inherit mumps)
   (name "mumps-mkl")
   (inputs
    `(("blas" ,mkl)
      ,@(alist-delete "openblas" (package-inputs mumps))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments mumps)
     ((#:phases phases)
      `(modify-phases
        ,phases
        (replace 'configure
                 (lambda*
                  (#:key inputs outputs #:allow-other-keys)
                  (call-with-output-file "Makefile.inc"
                    (lambda (port)
                      (format port "
PLAT          =
LIBEXT        = .a
LIBEXT_SHARED = .so
OUTC          = -o
OUTF          = -o
BLASDIR       = ~a
LIBBLAS       = -Wl,-rpath=$(BLASDIR)/lib -Wl,-rpath='$$ORIGIN'
LIBBLAS      += -L${BLASDIR}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64
LIBBLAS      += -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl
OPTF          = -DGEMMT_AVAILABLE~@[
SCALAPDIR     = ~a
SCALAP        = -Wl,-rpath=$(SCALAPDIR)/lib -Wl,-rpath='$$ORIGIN'
SCALAP       += -L$(SCALAPDIR)/lib -lscalapack~]
RM            = rm -f~:[
CC            = gcc
FC            = gfortran
FL            = gfortran
INCSEQ        = -I$(topdir)/libseq
LIBSEQ        = $(LAPACK) -L$(topdir)/libseq -lmpiseq
LIBSEQNEEDED  = libseqneeded
INCS          = $(INCSEQ)
LIBS          = $(LIBSEQ)~;
CC            = mpicc
FC            = mpifort
FL            = mpifort
INCPAR        =
LIBPAR        = $(SCALAP) $(LAPACK)
LIBSEQNEEDED  = 
INCS          = $(INCPAR)
LIBS          = $(LIBPAR)~]
AR            = ar vr # rules require trailing space, ugh...
RANLIB        = ranlib
LIBOTHERS     = -pthread
CDEFS         = -DAdd_
PIC           = -fPIC
FPIC_OPT      = $(PIC)
RPATH_OPT     = -Wl,-rpath,~a/lib
OPTF         += -O2 -fopenmp -DALLOW_NON_INIT -DBLR_MT
OPTF         += -fallow-argument-mismatch $(PIC)
OPTL          = -O2 -fopenmp $(PIC)
OPTC          = -O2 -fopenmp $(PIC)
LPORDDIR      = $(topdir)/PORD/lib
IPORD         = -I$(topdir)/PORD/include
LPORD         = $(LPORDDIR)/libpord.a
ORDERINGSF    = -Dpord~@[
METISDIR      = ~a
IMETIS        = -I$(METISDIR)/include
LMETIS        = -Wl,-rpath $(METISDIR)/lib -L$(METISDIR)/lib -lmetis
ORDERINGSF   += -Dmetis~]~@[~:{
SCOTCHDIR     = ~a
ISCOTCH       = -I$(SCOTCHDIR)/include
LSCOTCH       = -Wl,-rpath $(SCOTCHDIR)/lib -L$(SCOTCHDIR)/lib ~a -lesmumps
LSCOTCH      += -lscotch -lscotcherr
ORDERINGSF   += ~a~}~]
ORDERINGSC    = $(ORDERINGSF)
LORDERINGS    = $(LPORD) $(LMETIS) $(LSCOTCH)
IORDERINGSF   = $(ISCOTCH)
IORDERINGSC   = $(IPORD) $(IMETIS) $(ISCOTCH)"
                              (assoc-ref inputs "mkl")
                              (assoc-ref inputs "scalapack")
                              (->bool (which "mpicc"))  ;; MPI support enabled?
                              (assoc-ref outputs "out")
                              (assoc-ref inputs "metis")
                              (match
                               (list
                                (assoc-ref inputs "pt-scotch")
                                (assoc-ref inputs "scotch"))
                               ((#f #f)
                                #f)
                               ((#f scotch)
                                `((,scotch "" "-Dscotch")))
                               ((ptscotch _)
                                `((,ptscotch
                                   "-lesmumps -lptscotch -lptscotcherr "
                                   "-Dptscotch")))))))))))))
   (synopsis "Multifrontal sparse direct solver (with Intel® MKL)")))

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
   (synopsis "Multifrontal sparse direct solver (with MPI and Intel® MKL)")))

(define-public mumps-mkl-metis-openmpi
  (package
   (inherit mumps-mkl-openmpi)
   (name "mumps-mkl-metis-openmpi")
   (inputs
    (alist-delete "pt-scotch" (package-inputs mumps-mkl-openmpi)))))


(define-public mumps-mkl-openmpi-with-pt-scotch-6
  (package
   (inherit mumps-mkl-openmpi)
   (name "mumps-mkl-openmpi-with-pt-scotch-6")
   (inputs
    (modify-inputs (package-inputs mumps-mkl-openmpi)
                   (delete "pt-scotch")
                   (prepend pt-scotch-6)))))
