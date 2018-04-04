;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2018 Inria

(define-module (inria tainted storm)
  #:use-module (guix)
  #:use-module (inria storm)
  #:use-module (non-free cuda))

(define-public starpu+cuda
  (package
    (inherit starpu)
    (name "starpu-cuda")
    (native-inputs
     `(("no-float128" ,no-float128)
       ,@(package-native-inputs starpu)))
    (inputs
     `(("cuda" ,cuda)
       ,@(package-inputs starpu)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu)
       ((#:configure-flags flags '())
        `(append (list "--enable-cuda"
                       (string-append "--with-cuda-dir="
                                      (assoc-ref %build-inputs "cuda"))
                       (string-append "--with-cuda-lib-dir="
                                      (assoc-ref %build-inputs "cuda")
                                      "/lib/stubs"))
                 ,flags))
       ((#:tests? #f #f)
        ;; We don't have actual CUDA support (drivers) and GPUs so we cannot
        ;; run the test suite.
        #f)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'configure 'assume-cuda-works
             (lambda _
               ;; The code uses 'AC_RUN_IFELSE' and assumes libcuda.so will
               ;; be found.  This is not true when we are linking against the
               ;; stubs provided by 'cuda-toolkit': the stubs provide
               ;; 'libcuda.so' but the SONAME is 'libcuda.so.1.2.3', and that
               ;; file is *not* provided (it ships with the CUDA driver), so
               ;; running code does not work.
               (substitute* "configure"
                 (("have_valid_cuda=\"no\"")
                  "have_valid_cuda=yes"))
               #t))))
       ((#:validate-runpath? #f #f)
        ;; Likewise, RUNPATH validation would fail since libcuda.so.1 is
        ;; nowhere to be found.
        #f)))))
