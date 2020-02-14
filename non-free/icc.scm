;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2019, 2020 Inria

(define-module (non-free icc)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages package-management))

(define-public intel-compilers
  (package
    (name "intel-compilers")
    (version "2020.0.166")
    (source (origin
              ;; This is a 2.8 GiB tarball.  See the list of URLs at
              ;; <https://github.com/jeffhammond/HPCInfo/blob/master/buildscripts/icc-release.sh>.
              (method url-fetch)
              (uri
               "http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/16225/parallel_studio_xe_2020_cluster_edition.tgz")
              (sha256
               (base32
                "0r1chjx1fxyxgp0n38i4870rrbdm2nnwyd09ny2wws3xf0h1sfsp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       ;; XXX: This is mostly copied from "mkl".
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (add-before 'install 'extract-rpms
           (lambda _
             ;; Instead of running their 'install.sh', a script-generating
             ;; script that attempts to run obscure pre-built binaries,
             ;; simply extract the RPMs to ./opt/intel.
             (mkdir "opt")
             (for-each (lambda (rpm)
                         (format #t "extracting ~a...~%" rpm)
                         (let* ((command (string-append  "rpm2cpio "
                                                         rpm " | cpio -i"))

                                (status  (system command)))

                           (unless (zero? status)
                             (error (format #f "command '~a' failed with ~a"
                                            command status)))))
                       (find-files "rpm"
                                   "^intel-(c-comp|comp-|icc-|openmp-).*\\.rpm$"))
             #t))
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source-prefix (string-append
                                    "opt/intel/compilers_and_libraries_"
                                    ,(package-version this-package) "/linux"))
                    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (includedir (string-append out "/include")))
               (copy-recursively (string-append source-prefix "/compiler/lib/intel64_lin")
                                 libdir)
               (copy-recursively (string-append source-prefix "/compiler/include")
                                 includedir)
               (copy-recursively (string-append source-prefix "/bin/intel64")
                                 bindir)

               (let ((ld.so (string-append (assoc-ref inputs "glibc")
                                           ,(glibc-dynamic-linker)))
                     (runpath (string-append (assoc-ref inputs "glibc")
                                             "/lib:"
                                             (assoc-ref inputs "gcc:lib")
                                             "/lib:" libdir)))
                 (define (candidate? file stat)
                   (and (not (string-suffix? ".o" file)) ;exclude crt*.o
                        (not (string-suffix? ".dbg" file))
                        (elf-file? file)))

	         (for-each (lambda (file)
                             ;; FILE does not necessarily have an interpreter
                             ;; (if it's not an executable), thus ignore
                             ;; 'patchelf --set-interpreter' errors.
			     (system* "patchelf" "--set-interpreter" ld.so file)

		             (invoke "patchelf" "--set-rpath" runpath file))
			   (find-files out candidate?)))
               #t))))

       ;; XXX: The offload bits depend on 'libcoi_device.so.0', which we
       ;; don't have (?).
       #:validate-runpath? #f

       ;; We don't need the tool chain, Coreutils, and all that.
       #:implicit-inputs? #f

       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f))
    (native-inputs
     `(("tar" ,tar)
       ("patchelf" ,patchelf)
       ("glibc" ,glibc)
       ("gzip" ,gzip)
       ("gcc:lib" ,gcc "lib")

       ("cpio" ,cpio)
       ("rpm" ,rpm)))

    ;; 32-bit libraries are not installed.
    (supported-systems '("x86_64-linux"))

    ;; ICC supports the same environment variables as GCC.
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (synopsis "Non-free Intel compilers")
    (description
     "This package provides Intel's non-free compilers, @command{icc} and
@command{ifort}.  You may need to set the @code{INTEL_LICENSE_FILE}
environment variable to use it.

Consider using GCC or Clang instead.")
    (home-page "https://software.intel.com/en-us/compilers")
    (license #f)))                                ;non-free

(define-public intel-compilers-2019/5
  (package
    (inherit intel-compilers)
    (version "2019.5.281")
    (source (origin
              (method url-fetch)
              (uri
               "http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/15809/parallel_studio_xe_2019_update5_cluster_edition.tgz")
              (sha256
               (base32
                "011rwddv95qypnsmq2lli9v8ap4wxh1l3ki5xm0fdm3bc7g22d60"))))))

(define-public intel-compilers-2019/4
  (package
    (inherit intel-compilers)
    (version "2019.4.243")
    (source (origin
              (method url-fetch)
              (uri
               "http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/15533/parallel_studio_xe_2019_update4_cluster_edition.tgz")
              (sha256
               (base32
                "17ix7gp9cmm4drhakx10fv39amr0dg0170spyz519jmmwcny3bij"))))))
