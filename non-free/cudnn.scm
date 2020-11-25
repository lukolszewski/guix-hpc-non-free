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
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gcc)
  #:use-module (ice-9 match))

(define (make-cudnn version origin)
  (package
    (name "cudnn")
    (version version)
    (source origin)
    (build-system gnu-build-system)
    (outputs '("out"))                           
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))

       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f

       #:strip-binaries? #f                       ;no need

       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  (delete 'build) ;; for now, delete maybe later we need to patchelf
                  (replace 'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (libdir (string-append out "/lib"))
                              (includedir (string-append out "/include")))

                      ;; Install libraries  Don't install .a files to save disk space
                      (for-each (lambda (lib)
                         (install-file lib libdir))
                             (find-files (string-append "." "/lib64")
                                     "\\.so$"))
                      ;; install include files
                      (copy-recursively (string-append "." "/include")
                                 includedir)
                  #t)))
                  )))
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
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define-public cudnn-8.0.5
  (make-cudnn "8.0.5.39"
             (cudnn-source
  ;; stole url from arch linux : https://github.com/archlinux/svntogit-community/blob/packages/cudnn/trunk/PKGBUILD
              "https://developer.download.nvidia.com/compute/redist/cudnn/v8.0.5/cudnn-11.1-linux-x64-v8.0.5.39.tgz"
              "1khcn3wldm6dpq7rxjm05r23ji3m31wm1cbcdz6ap79rg7x6n10x")))

(define-public cudnn cudnn-8.0.5)
