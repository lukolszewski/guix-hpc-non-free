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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-26))

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
     `(#:phases
       (modify-phases %standard-phases
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
				    ,version))
		    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (includedir (string-append out "/include")))
	       (for-each (lambda (lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/lib/intel64")
                                     "\\.so$"))

               (copy-recursively (string-append source-prefix "/include")
                                 includedir)
               (copy-recursively (string-append source-prefix "/bin/intel64")
                                 bindir)
	       (copy-recursively (string-append source-prefix "/lib/pkgconfig")
                                 (string-append libdir "/pkgconfig"))
	       (copy-recursively (string-append source-prefix "/lib/cmake")
                                 (string-append libdir "/cmake"))))))
       ;; We don't need the tool chain, Coreutils, and all that.
       #:implicit-inputs? #f
       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f))
    (native-inputs
     `(("tar" ,tar)
       ("bash" ,bash)
       ("gzip" ,gzip)
       ("gawk" ,gawk)
       ("coreutils" ,coreutils)
       ("p7zip", p7zip)))

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
