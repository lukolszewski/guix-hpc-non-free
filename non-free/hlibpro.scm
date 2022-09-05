;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2022 Inria

(define-module (non-free hlibpro)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public boost-1.74
  (package
    (inherit boost)
    (version "1.74.0")
    (source
     (origin
      (method url-fetch)
      (uri
       (string-append
        "https://boostorg.jfrog.io/artifactory/main/release/"
        "1.74.0/source/boost_1_74_0.tar.bz2"))
      (sha256
       (base32
        "1c8nw4jz17zy2y27h7p554a5jza1ymz8phkz71p9181ifx8c3gw3"))))))

(define-public hdf5-1.10.6
  (package
   (inherit hdf5-1.10)
   (version "1.10.6")
   (source
     (origin
       (method url-fetch)
       (uri
        (list
         (string-append "https://support.hdfgroup.org/ftp/HDF5/releases/"
                        "hdf5-" (version-major+minor version)
                        "/hdf5-" version "/src/hdf5-"
                        version ".tar.bz2")
         (string-append "https://support.hdfgroup.org/ftp/HDF5/current"
                        (apply string-append
                               (take (string-split version #\.) 2))
                        "/src/hdf5-" version ".tar.bz2")))
       (sha256
        (base32 "1gf38x51128hn00744358w27xgzjk0ff4wra4yxh2lk804ck1mh9"))
       (patches (search-patches "hdf5-config-date.patch"))))))

(define-public gsl-2.6
  (package
   (inherit gsl)
   (version "2.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://gnu/gsl/gsl-" version ".tar.gz"))
     (sha256
      (base32
       "1a460zj9xmbgvcymkdhqh313c4l29mn9cffbi5vf33x3qygk70mp"))))))

(define-public hlibpro
  (package
   (name "hlibpro")
   (version "3.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://www.hlibpro.com/archives/obaequeayeiru4irooz9eiwiaJeos3mo/"
       "hlibpro-" version "-Debian11.tgz"))
     (sha256
      (base32
       "1h6fick3xc0ga5cs7fkg5j4c6wax0grp3i573zlnpyivgirsn9k2"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules ((guix build utils)
                 (guix build gnu-build-system)
                 (ice-9 match))
      #:substitutable? #f ;; no substitutes for non-free software
      #:strip-binaries? #f ;; no need
      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (replace 'build
                (lambda* (#:key inputs outputs #:allow-other-keys)
                         (define out
                           (assoc-ref outputs "out"))
                         (define libc
                           (assoc-ref inputs "libc"))
                         (define gcc-lib
                           (assoc-ref inputs "gcc:lib"))
                         (define rpath
                           (string-join
                            (list "$ORIGIN"
                                  (string-append out "/lib")
                                  (string-append libc "/lib")
                                  (string-append gcc-lib "/lib")) ":"))
                         (define (patch-elf file)
                           (make-file-writable file)
                           (format
                            #t
                            "Setting RPATH for '~a'...~%"
                            file)
                           (invoke "patchelf" "--set-rpath" rpath
                                   "--force-rpath" file))
                         (for-each (lambda (file)
                                     (when (elf-file? file)
                                       (patch-elf file)))
                                   (find-files "./lib"
                                               (lambda (file stat)
                                                 (eq? 'regular
                                                      (stat:type
                                                       stat)))))
                         (substitute*
                          "SConstruct"
                          (("tbb_dir       = ''")
                           (string-append "tbb_dir       = '"
                                          (assoc-ref inputs "tbb") "'")))
                         (substitute*
                          "SConstruct"
                          (("boost_dir     = ''")
                           (string-append "boost_dir       = '"
                                          (assoc-ref inputs "boost") "'")))
                         (substitute*
                          "SConstruct"
                          (("metis_dir     = ''")
                           (string-append "metis_dir       = '"
                                          (assoc-ref inputs "metis") "'")))
                         (substitute*
                          "SConstruct"
                          (("hdf5_cflags   = '-I/usr/include/hdf5/serial'")
                           (string-append
                            "hdf5_cflags   = '-I"
                            (assoc-ref inputs "hdf5")  "/include'")))
                         (substitute*
                          "SConstruct"
                          (("-L/usr/lib/x86_64-linux-gnu/hdf5/serial")
                           (string-append
                            "-L" (assoc-ref inputs "hdf5")  "/lib")))
                         ;; Build examples for testing purposes.
                         (invoke "scons" "fullmsg=1") #t))
                (replace 'install
                         (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (inc (string-append out "/include"))
                                         (lib (string-append out "/lib")))
                                    (mkdir-p out)
                                    (mkdir-p inc)
                                    (mkdir-p lib)
                                    (copy-recursively "./include" inc)
                                    (copy-recursively "./lib" lib) #t)))
                (replace 'check
                         (lambda _
                           ;; Run one example as a test.
                           ;; FIXME: C++ examples build but C examples does not.
                           ;;        There are segmentation faults in HLibPRO.
                           ;;        They are hard to investigate without access
                           ;;        to sources.
                           (invoke "./examples/bem1d") #t)))))
   (native-inputs
    `(("patchelf" ,patchelf)))
   (inputs
    `(("gcc:lib" ,gcc "lib")
      ;; Required for building examples.
      ("lapack" ,lapack)
      ("boost" ,boost-1.74)
      ("hdf5" ,hdf5-1.10.6)
      ("tbb" ,tbb-2020)
      ("gsl" ,gsl-2.6)
      ("fftw3" ,fftw)
      ("metis" ,metis)
      ("scons" ,scons)
      ("zlib" ,zlib)))
   (synopsis
    "HLIBpro is a software library implementing algorithms for Hierarchical
matrices, or H-matrices.")
   (description
    "HLIBpro is a C++ library implementing H-matrix arithmetic and other
algorithms in the context of H-matrices, e.g. geometrical and algebraic
clustering, visualisation, BEM discretisation. The focus of the implemention in
HLIBpro is on robust algorithms on parallel computers. Here robustness is meant
to apply to the algorithms itself as well as on the implementation.")
   (home-page "https://www.hlibpro.com")
   (license #f)
   (supported-systems '("x86_64-linux"))))
                                        
                
