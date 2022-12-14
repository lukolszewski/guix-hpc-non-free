;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2019 Inria

(define-module (non-free mkl)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (guix git-download)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages tbb)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
;;  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-26))

(define-public libffi
  (package
    (name "libffi")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://sourceware.org/pub/libffi/"
                              name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dya49bnhianl0r65m65xndz6ls2jn1xngyn72gd28ls3n7bnvnh"))
              (patches (search-patches "libffi-3.2.1-complex-alpha.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Prevent the build system from passing -march and -mtune to the
       ;; compiler.  See "ax_cc_maxopt.m4" and "ax_gcc_archflag.m4".
       #:configure-flags '("--enable-portable-binary" "--without-gcc-arch")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (define out (assoc-ref outputs "out"))
             (symlink (string-append out "/lib/libffi-3.2.1/include")
                      (string-append out "/include"))
             #t)))))
    (outputs '("out" "debug"))
    (synopsis "Foreign function call interface library")
    (description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "http://sources.redhat.com/libffi/")

    ;; See <https://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license expat)))

(define-public level-zero
  (package
    (name "level-zero")
    (version "1.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oneapi-src/level-zero.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c8a0ijykv17db04d0djrvgj9vdi3ln2nlffh53qkg1np2qm3sjy"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (inputs (list gawk))
    (synopsis "oneAPI level-zero library")
    (description "oneAPI component level-zero")
    (home-page "https://github.com/oneapi-src/level-zero.git")
    (license expat)))


(define-public mkl
  (package
    (name "mkl")
    (version "2019.1.144")
    (source (origin
              (method url-fetch)
              (uri "http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/14895/l_mkl_2019.1.144.tgz")
              (sha256
               (base32
                "1fqgla8gzf656xmbqijk8phja31dkcw6g0w68ajgg1f6m5ha81aj"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
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
                       (find-files "rpm" "\\.rpm$"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source-prefix (string-append
                                    "opt/intel/compilers_and_libraries_"
                                    ,version "/linux/mkl"))
                    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (includedir (string-append out "/include")))

               ;; Install libraries.  Don't install .a files: that saves
               ;; ~1 GB and presumably nobody cares.
               (for-each (lambda (lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/lib/intel64_lin")
                                     "\\.so$"))

               (copy-recursively (string-append source-prefix "/include")
                                 includedir)
               (copy-recursively (string-append source-prefix "/bin")
                                 bindir)
               #t))))

       ;; We don't need the tool chain, Coreutils, and all that.
       #:implicit-inputs? #f

       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)

       ("cpio" ,cpio)
       ("rpm" ,rpm)))

    ;; 32-bit libraries are not installed.
    (supported-systems '("x86_64-linux"))

    (synopsis "Non-free library of optimized math routines")
    (description
     "Intel® Math Kernel Library (MKL) is a proprietary library of highly
optimized, extensively threaded routines for applications that require
maximum performance.  The library provides Fortran and C programming language
interfaces.  Intel MKL C language interfaces can be called from applications
written in either C or C++, as well as in any other language that can
reference a C interface.")
    (home-page "https://software.intel.com/en-us/mkl")
    (license #f)))                                ;non-free

(define-public mkl
  (package
    (name "mkl")
    (version "2022.1.0")
    (source (origin
              (method url-fetch)
              (uri "file:///media/Data/software/l_onemkl_p_2022.1.0.223_offline.sh")
              (sha256
               (base32
                "0gjnljgg5h340j3gq2h04mjc3bc4frfvbyy8wr62zran9hy5lcjb"))
	      (file-name (string-append name "-" version ".sh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
       #~(modify-phases %standard-phases
	 (replace 'unpack
		  (lambda* (#:key source #:allow-other-keys)
;;;		    (chmod source #o755)
		    (invoke "bash" source "--extract-only")))
         (delete 'configure)
         (delete 'check)
         (delete 'build)
	 (add-before 'install 'extract-cups
	   (lambda _
	     (for-each (lambda (cupfile)
			 (format #t "extracting ~a...~%" cupfile)
			 (let* ((command (string-append "7z x -bd -y " cupfile))
				(status (system command)))
			   (unless (zero? status)
			     (error (format #f "command '~a' failed with ~a"
					    command status)))))
		       (find-files "l_onemkl_p_2022.1.0.223_offline" ".cup$"))
	     
	     #t))
	 (replace 'install
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (source-prefix (string-append
				    "_installdir/mkl/"
				    #$version))
		    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (includedir (string-append out "/include")))
	       (for-each (lambda (lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/lib/intel64")
                                     "\\.so$|\\.so[/.0-9]+$"))

               (copy-recursively (string-append source-prefix "/include")
                                 includedir)
               (copy-recursively (string-append source-prefix "/bin/intel64")
                                 bindir)
	       (copy-recursively (string-append source-prefix "/lib/pkgconfig")
                                 (string-append libdir "/pkgconfig"))
	       (copy-recursively (string-append source-prefix "/lib/cmake")
                                 (string-append libdir "/cmake"))
               ;;; Now compiler libs too
	       (for-each (lambda (lib)
			   (display lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/../../compiler/" #$version "/linux/lib")
                                     "\\.so$|\\.so[/.0-9]+$"))
	       (for-each (lambda (lib)
         			   (display lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/../../compiler/" #$version "/linux/compiler/lib/intel64_lin/")
                                     "\\.so$|\\.so[/.0-9]+$"))
	       (copy-recursively (string-append source-prefix "/../../compiler/" #$version "/lib/pkgconfig")
                                 (string-append libdir "/pkgconfig"))
	       )))
         (add-after 'install 'install-copy
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
 	     (display "test1")
             (chdir "..")
             (use-modules (ice-9 ftw)
                          (ice-9 regex)
                          (ice-9 textual-ports))
             (let* ((libdir (string-append #$output "/lib"))
                    (bindir (string-append #$output "/bin"))
                    (etcdir (string-append #$output "/etc")))
               
               ;; ------------------------------
               ;; patchelf
	       (display "test2")
               (let* ((ld.so (string-append #$glibc #$(glibc-dynamic-linker)))

                      (rpath (string-join
                              (list "$ORIGIN"
                                    (string-append #$output "/lib")
                                    (string-append #$glibc "/lib")
                                    (string-append #$glib "/lib")
				    (string-append #$tbb "/lib")
                                    (string-append #$zlib "/lib")
                                    (string-append #$level-zero "/lib")
				    (string-append #$elfutils "/lib")
                                    (string-append #$libffi "/lib")
                                    (string-append #$gcc:lib "/lib"))
                              ":")))
                 (define (patch-elf file)
                   (format #t "Patching ~a ...~%" file)
                   (unless (string-contains file ".so")
		     (invoke "patchelf" "--set-interpreter" ld.so file))
                   (invoke "patchelf" "--set-rpath" rpath file))
                 (for-each (lambda (file)
                             (when (elf-file? file)
                               (patch-elf file)))
                           (find-files #$output  ".*\\.so"))
		 (patch-elf (string-append bindir "/" "mkl_link_tool")))))))
       ;; We don't need the tool chain, Coreutils, and all that.
       #:implicit-inputs? #f
       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f))
    (inputs (list zlib glib tbb glibc `(,gcc "lib") elfutils level-zero libffi))
    (native-inputs (list patchelf tar bash gzip gawk coreutils p7zip))

    ;; 32-bit libraries are not installed.
    (supported-systems '("x86_64-linux"))

    (synopsis "Non-free library of optimized math routines")
    (description
     "Intel® Math Kernel Library (MKL) is a proprietary library of highly
optimized, extensively threaded routines for applications that require
maximum performance.  The library provides Fortran and C programming language
interfaces.  Intel MKL C language interfaces can be called from applications
written in either C or C++, as well as in any other language that can
reference a C interface.")
    (home-page "https://software.intel.com/en-us/mkl")
    (license #f)))
