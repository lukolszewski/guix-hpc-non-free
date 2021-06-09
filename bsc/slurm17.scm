(define-module (bsc slurm17)
  #:use-module (guix)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
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


(define-public slurm-17.11
  (package
   (name "slurm-17.11")
   (version "17.11.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://download.schedmd.com/slurm/slurm-"
                  version ".tar.bz2"))
            (sha256
             (base32
              "0mss0cblzk0an63pj9fc5bjp0hifndh6jyb56ixgcd8w1f3i1ax4"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; According to
                ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00534.html>
                ;; there are non-free bits under contribs/, though it's not
                ;; clear which ones.  libpmi is clearly free (it used to be
                ;; under src/api/), so remove all of contribs/ except
                ;; contribs/pmi/.
                (substitute* "configure.ac"
                  (("^[[:space:]]+contribs/(.*)$" all directory)
                   (if (and (string-prefix? "pmi" directory)
                            (not (string-prefix? "pmi2" directory)))
                       all
                       "")))
                ;;(rename-file "contribs/pmi" "tmp-pmi")
                (delete-file-recursively "contribs")
                (mkdir "contribs")
                ;;(rename-file "tmp-pmi" "contribs/pmi")
                #t))))
   ;; FIXME: More optional inputs could be added,
   ;; in particular mysql and gtk+.
   (inputs `(("freeipmi" ,freeipmi)
             ("hwloc" ,hwloc-1 "lib")
             ("json-c" ,json-c)
             ("linux-pam" , linux-pam)
             ("munge" ,munge)
             ("numactl" ,numactl)
             ("readline" ,readline)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("expect" ,expect)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list "--enable-pam" "--sysconfdir=/etc/slurm"
            "--disable-static"
            (string-append "--with-freeipmi=" (assoc-ref %build-inputs "freeipmi"))
            (string-append "--with-hwloc=" (assoc-ref %build-inputs "hwloc"))
            (string-append "--with-json=" (assoc-ref %build-inputs "json-c"))
            (string-append "--with-munge=" (assoc-ref %build-inputs "munge"))
            ;; 32-bit support is marked as deprecated and needs to be
            ;; explicitly enabled.
            ,@(if (target-64bit?) '() '("--enable-deprecated")))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'autoconf
          (lambda _ (invoke "autoconf")))         ;configure.ac was patched
        ;;(add-after 'install 'install-libpmi
        ;;  (lambda _
        ;;    ;; Open MPI expects libpmi to be provided by Slurm so install it.
        ;;    (invoke "make" "install" "-C" "contribs/pmi")))
        )))
   (home-page "https://slurm.schedmd.com/")
   (synopsis "Workload manager for cluster computing")
   (description
    "SLURM is a fault-tolerant and highly scalable cluster management and job
scheduling system for large and small clusters.  It allocates access to
resources (computer nodes) to users for some duration of time, provides a
framework for starting, executing, and monitoring work (typically a parallel
job) on a set of allocated nodes, and arbitrates contention for resources
by managing a queue of pending work.")
   (license (list license:bsd-2       ; src/common/log.[ch], src/common/uthash
                  license:expat       ; slurm/pmi.h
                  license:isc         ; src/common/strlcpy.c
                  license:lgpl2.1+    ; hilbert.[ch], src/common/slurm_time.h
                  license:zlib        ; src/common/strnatcmp.c
                  license:gpl2+))))   ; the rest, often with OpenSSL exception

