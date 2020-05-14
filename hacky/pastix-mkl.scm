;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (hacky pastix-mkl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (inria hiepacs)
  #:use-module (non-free mkl))

(define-public pastix-5-mkl
  (package
   (inherit pastix-5)
   (name "pastix-5-mkl")
   (inputs
   `(("mkl" ,mkl)
     ,@(alist-delete "openblas" (package-inputs pastix-5))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments pastix-5)
     (
      (#:phases phases)
      `(modify-phases ,phases
                      (replace 'configure
                               (lambda _
                               (call-with-output-file "config.in"
                                 (lambda (port)
                                   (format port "
HOSTARCH    = i686_pc_linux
VERSIONBIT  = _32bit
EXEEXT      =
OBJEXT      = .o
LIBEXT      = .a
CCPROG      = gcc -Wall
CFPROG      = gfortran
CF90PROG    = gfortran -ffree-form
CXXPROG     = g++

MCFPROG     = mpif90
CF90CCPOPT  = -ffree-form -x f95-cpp-input
# Compilation options for optimization (make expor)
CCFOPT      = -O3
# Compilation options for debug (make | make debug)
CCFDEB      = -g3
CXXOPT      = -O3
NVCCOPT     = -O3

LKFOPT      =
MKPROG      = make
MPCCPROG    = mpicc -Wall
MPCXXPROG   = mpic++ -Wall
CPP         = cpp
ARFLAGS     = ruv
ARPROG      = ar
EXTRALIB    = -lgfortran -lm -lrt
CTAGSPROG   = ctags

VERSIONMPI  = _mpi
VERSIONSMP  = _smp
VERSIONSCH  = _static
VERSIONINT  = _int
VERSIONPRC  = _simple
VERSIONFLT  = _real
VERSIONORD  = _scotch

###################################################################
#                  SETTING INSTALL DIRECTORIES                    #
###################################################################
# ROOT          = /path/to/install/directory
# INCLUDEDIR    = ${ROOT}/include
# LIBDIR        = ${ROOT}/lib
# BINDIR        = ${ROOT}/bin
# PYTHON_PREFIX = ${ROOT}

###################################################################
#                  SHARED LIBRARY GENERATION                      #
###################################################################
SHARED=1
SOEXT=.so
SHARED_FLAGS =  -shared -Wl,-soname,__SO_NAME__
CCFDEB       := ${CCFDEB} -fPIC
CCFOPT       := ${CCFOPT} -fPIC
CFPROG       := ${CFPROG} -fPIC

###################################################################
#                          INTEGER TYPE                           #
###################################################################
# Uncomment the following lines for integer type support (Only 1)

#VERSIONINT  = _long
#CCTYPES     = -DFORCE_LONG -DINTSIZELONG
#---------------------------
VERSIONINT  = _int32
CCTYPES     = -DINTSIZE32
#---------------------------
#VERSIONINT  = _int64
#CCTYPES     = -DINTSSIZE64

###################################################################
#                           FLOAT TYPE                            #
###################################################################
CCTYPESFLT  =
# Uncomment the following lines for double precision support
VERSIONPRC  = _double
CCTYPESFLT := $(CCTYPESFLT) -DPREC_DOUBLE

# Uncomment the following lines for float=complex support
VERSIONFLT  = _complex
CCTYPESFLT := $(CCTYPESFLT) -DTYPE_COMPLEX

###################################################################
#                          FORTRAN MANGLING                       #
###################################################################
#CCTYPES    := $(CCTYPES) -DPASTIX_FM_NOCHANGE

###################################################################
#                          MPI/THREADS                            #
###################################################################

# Uncomment the following lines for sequential (NOMPI) version
#VERSIONMPI  = _nompi
#CCTYPES    := $(CCTYPES) -DFORCE_NOMPI
#MPCCPROG    = $(CCPROG)
#MCFPROG     = $(CFPROG)
#MPCXXPROG   = $(CXXPROG)

# Uncomment the following lines for non-threaded (NOSMP) version
#VERSIONSMP  = _nosmp
#CCTYPES    := $(CCTYPES) -DFORCE_NOSMP

# Uncomment the following line to enable a progression thread,
#  then use IPARM_THREAD_COMM_MODE
#CCPASTIX   := $(CCPASTIX) -DPASTIX_THREAD_COMM

# Uncomment the following line if your MPI doesn't support MPI_THREAD_MULTIPLE,
# level then use IPARM_THREAD_COMM_MODE
CCPASTIX   := $(CCPASTIX) -DPASTIX_FUNNELED

# Uncomment the following line if your MPI doesn't support MPI_Datatype
# correctly
#CCPASTIX   := $(CCPASTIX) -DNO_MPI_TYPE

# Uncomment the following line if you want to use semaphore barrier
# instead of MPI barrier (with IPARM_AUTOSPLIT_COMM)
#CCPASTIX    := $(CCPASTIX) -DWITH_SEM_BARRIER

# Uncomment the following lines to enable StarPU.
#CCPASTIX   := $(CCPASTIX) `pkg-config libstarpu --cflags` -DWITH_STARPU
#EXTRALIB   := $(EXTRALIB) `pkg-config libstarpu --libs`
# Uncomment the correct 2 lines
#CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=11
#NVCCOPT    := $(NVCCOPT) -maxrregcount 32 -arch sm_11
#CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=13
#NVCCOPT    := $(NVCCOPT) -maxrregcount 32 -arch sm_13
CCPASTIX   := $(CCPASTIX) -DCUDA_SM_VERSION=20
NVCCOPT    := $(NVCCOPT) -arch sm_20

# Uncomment the following line to enable StarPU profiling
# ( IPARM_VERBOSE > API_VERBOSE_NO ).
#CCPASTIX   := $(CCPASTIX) -DSTARPU_PROFILING

# Uncomment the following line to enable CUDA (StarPU)
#CCPASTIX   := $(CCPASTIX) -DWITH_CUDA

###################################################################
#                          Options                                #
###################################################################

# Show memory usage statistics
CCPASTIX   := $(CCPASTIX) -DMEMORY_USAGE

# Show memory usage statistics in solver
CCPASTIX   := $(CCPASTIX) -DSTATS_SOPALIN

# Uncomment following line for dynamic thread scheduling support
#CCPASTIX   := $(CCPASTIX) -DPASTIX_DYNSCHED

# Uncomment the following lines for Out-of-core
#CCPASTIX   := $(CCPASTIX) -DOOC -DOOC_NOCOEFINIT -DOOC_DETECT_DEADLOCKS

###################################################################
#                      GRAPH PARTITIONING                         #
###################################################################

# Uncomment the following lines for using metis ordering
#VERSIONORD  = _metis
#METIS_HOME  = ${HOME}/metis-4.0
#CCPASTIX   := $(CCPASTIX) -DMETIS -I$(METIS_HOME)/Lib
#EXTRALIB   := $(EXTRALIB) -L$(METIS_HOME) -lmetis

# Scotch always needed to compile
SCOTCH_HOME ?= ${HOME}/scotch_5.1/
SCOTCH_INC ?= $(SCOTCH_HOME)/include
SCOTCH_LIB ?= $(SCOTCH_HOME)/lib
# Uncomment on of this blocks
#scotch
CCPASTIX   := $(CCPASTIX) -I$(SCOTCH_INC) -DWITH_SCOTCH
EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lscotch -lscotcherrexit
#ptscotch
#CCPASTIX   := $(CCPASTIX) -I$(SCOTCH_INC) -DDISTRIBUTED -DWITH_SCOTCH
#if scotch >= 6.0
#EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lptscotch -lscotch -lptscotcherrexit
#else
#EXTRALIB   := $(EXTRALIB) -L$(SCOTCH_LIB) -lptscotch -lptscotcherrexit

###################################################################
#                Portable Hardware Locality                       #
###################################################################
# By default PaStiX uses hwloc to bind threads,
# comment this lines if you don't want it (not recommended)
HWLOC_HOME ?= /opt/hwloc/
HWLOC_INC  ?= $(HWLOC_HOME)/include
HWLOC_LIB  ?= $(HWLOC_HOME)/lib
CCPASTIX   := $(CCPASTIX) -I$(HWLOC_INC) -DWITH_HWLOC
EXTRALIB   := $(EXTRALIB) -L$(HWLOC_LIB) -lhwloc

###################################################################
#                             MARCEL                              #
###################################################################

# Uncomment following lines for marcel thread support
#VERSIONSMP := $(VERSIONSMP)_marcel
#CCPASTIX   := $(CCPASTIX) `pm2-config --cflags`
#CCPASTIX   += -I${PM2_ROOT}/marcel/include/pthread
#EXTRALIB   := $(EXTRALIB) `pm2-config --libs`
# ---- Thread Posix ------
EXTRALIB   := $(EXTRALIB) -lpthread

# Uncomment following line for bubblesched framework support
# (need marcel support)
#VERSIONSCH  = _dyn
#CCPASTIX   := $(CCPASTIX) -DPASTIX_BUBBLESCHED

###################################################################
#                              BLAS                               #
###################################################################

# Choose Blas library (Only 1)
# Do not forget to set BLAS_HOME if it is not in your environnement
# BLAS_HOME=/path/to/blas
#----  Blas    ----
#BLASLIB  = -lopenblas
#---- Gotoblas ----
#BLASLIB  = -L${BLAS_HOME} -lgoto
#----  MKL     ----
BLASLIB  = -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_lp64
BLASLIB += -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl
#----  Acml    ----
#BLASLIB  = -L$(BLAS_HOME) -lacml

###################################################################
#                             MURGE                               #
###################################################################
# Uncomment if you need MURGE interface to be thread safe
# CCPASTIX   := $(CCPASTIX) -DMURGE_THREADSAFE
# Uncomment this to have more timings inside MURGE
# CCPASTIX   := $(CCPASTIX) -DMURGE_TIME

###################################################################
#                          DO NOT TOUCH                           #
###################################################################

FOPT      := $(CCFOPT)
FDEB      := $(CCFDEB)
CCHEAD    := $(CCPROG) $(CCTYPES) $(CCFOPT)
CCFOPT    := $(CCFOPT) $(CCTYPES) $(CCPASTIX)
CCFDEB    := $(CCFDEB) $(CCTYPES) $(CCPASTIX)
NVCCOPT   := $(NVCCOPT) $(CCTYPES) $(CCPASTIX)

###################################################################
#                        MURGE COMPATIBILITY                      #
###################################################################

MAKE     = $(MKPROG)
CC       = $(MPCCPROG)
CFLAGS   = $(CCFOPT) $(CCTYPESFLT)
FC       = $(MCFPROG)
FFLAGS   = $(CCFOPT)
LDFLAGS  = $(EXTRALIB) $(BLASLIB)
CTAGS    = $(CTAGSPROG)
"
                                           ))) #t))))))
   (synopsis "Sparse matrix direct solver (using Intel® MKL instead of
OpenBLAS)")))
