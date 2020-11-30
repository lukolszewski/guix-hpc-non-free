;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2018, 2019, 2020 Inria

(define-module (non-free cudnn)
               #:use-module (guix)
               #:use-module (guix build-system gnu)
               #:use-module ((guix licenses) #:prefix license:)
               #:use-module (gnu packages base)
               #:use-module (gnu packages bootstrap)
               #:use-module (gnu packages elf)
               #:use-module (gnu packages gcc)
               #:use-module (ice-9 match))


(define-public patchelf1G
               (package 
                 (inherit patchelf)
                 (name "patchelf1G")
                 (arguments
                   ;; we want to patch the hardsize of 32M to 1.5G
                   '(#:phases (modify-phases %standard-phases
                                             (add-after 'unpack 'patch-to-1G
                                                        (lambda _ 
                                                          (substitute* "src/patchelf.cc"
                                                                       (("32 \\* 1024") "1536 * 1024"))
                                                          #true)
                                                        ))
                     #:tests? #f)
                   ))
               )
(define (make-cudnn version origin)
  (package
    (name "cudnn")
    (version version)
    (source origin)
    (build-system gnu-build-system)
    (outputs '("out"))                           
    (arguments
      `(#:modules ((guix build utils)
                   (srfi srfi-1) 
                   (srfi srfi-26) 
                   (guix build gnu-build-system)
                   (ice-9 match))

        ;; Let's not publish or obtain substitutes for that.
        #:substitutable? #f

        #:strip-binaries? #f                       ;no need

        ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
        ;; not DT_RUNPATH.
        #:validate-runpath? #f

        #:phases (modify-phases %standard-phases
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
                                                    (make-file-writable file)
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
                                                  (let* ((out (assoc-ref outputs "out"))
                                                         (libdir (string-append out "/lib"))
                                                         (includedir (string-append out "/include")))

                                                    ;; Install libraries  Don't install .a files to save disk space
                                                    (for-each (lambda (lib)
                                                                (install-file lib libdir))
                                                     (remove (cut string-suffix? <> ".a") (find-files "lib64")))

                                                    ;; install include files
                                                    (copy-recursively (string-append "." "/include")
                                                                      includedir)
                                                    #t)))
                                )))
    (native-inputs
      `(("patchelf1G" ,patchelf1G))) 
    (inputs
      `(("gcc:lib" ,gcc "lib")))
    (synopsis
      "NVIDIA CUDA Deep Neural Network library, used by pytorch")
    (description
      "This package provides a GPU-accelerated library of primitives for deep neural networks
      for NVIDIA GPUs, which is proprietary.")
                                (home-page "https://developer.nvidia.com/cudnn")
                                (license #f)
                                (supported-systems '("x86_64-linux"))))

(define-syntax-rule (cudnn-source url hash)
                    ;; stole url from arch linux : https://github.com/archlinux/svntogit-community/blob/packages/cudnn/trunk/PKGBUILD
                    (origin
                      (uri url)
                      (sha256 (base32 hash))
                      (method url-fetch)))

(define-public cudnn-8.0.5
               (make-cudnn "8.0.5.39"
                           (cudnn-source
                             "https://developer.download.nvidia.com/compute/redist/cudnn/v8.0.5/cudnn-11.1-linux-x64-v8.0.5.39.tgz"
                             "1khcn3wldm6dpq7rxjm05r23ji3m31wm1cbcdz6ap79rg7x6n10x")))

(define-public cudnn cudnn-8.0.5)
