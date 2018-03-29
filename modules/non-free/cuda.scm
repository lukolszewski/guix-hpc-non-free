;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2018 Inria

(define-module (non-free cuda)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (ice-9 match))

(define (make-cuda version origin)
  (package
    (name "cuda-toolkit")
    (version version)
    (source origin)
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;196 MiB
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gremlin)
                  (guix build gnu-build-system)
                  (guix elf)
                  (ice-9 binary-ports)
                  (ice-9 match))

       #:strip-binaries? #f                       ;no need

       ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
       ;; not DT_RUNPATH.
       #:validate-runpath? #f

       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((source (assoc-ref inputs "source")))
                        (invoke "sh" source "--keep" "--noexec")
                        (chdir "pkg/run_files")
                        (match (find-files "." "^cuda-linux64-rel.*\\.run$")
                          ((run)
                           (invoke "sh" run "--keep" "--noexec")))
                        (chdir "pkg"))))
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out
                        (assoc-ref outputs "out"))
                      (define libc
                        (assoc-ref inputs "libc"))
                      (define gcc-lib
                        (assoc-ref inputs "gcc:lib"))
                      (define ld.so
                        (string-append libc ,(glibc-dynamic-linker)))
                      (define rpath
                        (string-join (list "$ORIGIN"
                                           (string-append out "/lib")
                                           (string-append libc "/lib")
                                           (string-append gcc-lib "/lib"))
                                     ":"))

                      (define (patch-elf file)
                        (unless (string-contains file ".so")
                          (format #t "Setting interpreter on '~a'...~%" file)
                          (invoke "patchelf" "--set-interpreter" ld.so
                                  file))
                        (format #t "Setting RPATH on '~a'...~%" file)
                        (invoke "patchelf" "--set-rpath" rpath
                                "--force-rpath" file))

                      (for-each (lambda (file)
                                  (when (elf-file? file)
                                    (patch-elf file)))
                                (find-files "."
                                            (lambda (file stat)
                                              (eq? 'regular
                                                   (stat:type stat)))))
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out   (assoc-ref outputs "out"))
                             (lib   (string-append out "/lib"))
                             (lib64 (string-append out "/lib64")))
                        (mkdir-p out)
                        (setenv "PERL5LIB" (getcwd)) ;for InstallUtils.pm
                        (invoke "perl" "install-linux.pl"
                                (string-append "--prefix=" out))
                        (rename-file lib64 lib)
                        #t)))
                  (add-after 'install 'create-libcuda-symlink
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The package installs 'libcuda.so' with SONAME
                      ;; 'libcuda.so.1', but it fails to install a file with
                      ;; that name.  Fix that for all these files.
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib/stubs")))
                        (define (symlink-soname so)
                          (let* ((elf     (call-with-input-file so
                                            (compose parse-elf
                                                     get-bytevector-all)))
                                 (dyninfo (elf-dynamic-info elf))
                                 (soname  ((@@ (guix build gremlin)
                                               elf-dynamic-info-soname)
                                           dyninfo)))
                            (format #t "'~a' has SONAME '~a'~%"
                                    so soname)
                            (symlink (basename so)
                                     (string-append lib "/" soname))))

                        (for-each symlink-soname
                                  (find-files lib "\\.so$"))
                        #t)))
                  (add-after 'install 'move-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out    (assoc-ref outputs "out"))
                             (doc    (assoc-ref outputs "doc"))
                             (docdir (string-append doc "/share/doc/cuda")))
                        (mkdir-p (dirname docdir))
                        (rename-file (string-append out "/doc") docdir)
                        #t))))))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (synopsis
     "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (license #f)
    (supported-systems '("x86_64-linux"))))

(define-syntax-rule (cuda-source url hash)
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define-public cuda-8.0
  (make-cuda "8.0.61"
             (cuda-source
              "https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run"
              "1i4xrsqbad283qffvysn88w2pmxzxbbby41lw0j1113z771akv4w")))

(define-public cuda
  ;; Default version.
  cuda-8.0)
