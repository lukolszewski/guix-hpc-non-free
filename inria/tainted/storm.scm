;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2018, 2019, 2020 Inria

(define-module (inria tainted storm)
  #:use-module (guix)
  #:use-module (gnu packages gcc)
  #:use-module (inria storm)
  #:use-module (non-free cuda)

  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define (gnu-build-system-with-compiler compiler)
  "Return a variant of GNU-BUILD-SYSTEM that uses COMPILER instead of the
implicit GCC."
  (define lower
    (build-system-lower gnu-build-system))

  (define (lower* . args)
    (let ((lowered (apply lower args)))
      (bag
        (inherit lowered)
        (build-inputs (map (match-lambda
                             (("gcc" _ rest ...)
                              `("compiler" ,compiler ,@rest))
                             (input input))
                           (bag-build-inputs lowered))))))

  (build-system
    (inherit gnu-build-system)
    (lower lower*)))

(define gfortran-sans-libstdc++
  ;; XXX: Currently gfortran includes a copy of libstdc++ and its headers.
  ;; For some reason, nvcc chokes on those:
  ;;
  ;;   /gnu/store/…-gfortran-7.5.0/include/c++/bits/exception.h(64): error: expected a ";"
  ;;   /gnu/store/…-gfortran-7.5.0/include/c++/bits/exception.h(69): error: expected a ";"
  ;;
  ;; Work around it by removing those headers from CPATH and CPLUS_INCLUDE_PATH.
  (package
    (inherit gfortran)
    (outputs '("out"))
    (build-system trivial-build-system)
    (arguments
     '(#:builder (let ((out (assoc-ref %outputs "out"))
                       (gfortran (assoc-ref %build-inputs "gfortran")))
                   (mkdir out)
                   (symlink (string-append gfortran "/bin")
                            (string-append out "/bin")))))
    (native-inputs `(("gfortran" ,gfortran)))
    (inputs '())
    (propagated-inputs '())
    (native-search-paths '())
    (search-paths '())
    (synopsis "GFortran without libstdc++")))

(define-public starpu+cuda
  (package
    (inherit starpu)
    (name "starpu-cuda")
    ;; "host_config.h" in 'cuda-toolkit' says "gcc versions later than 5 are
    ;; not supported".  Thus, provide GCC 5.x.
    (build-system (gnu-build-system-with-compiler gcc-5))
    (native-inputs
     `(("no-float128" ,no-float128)
       ("gfortran" ,gfortran-sans-libstdc++)
       ,@(alist-delete "gfortran" (package-native-inputs starpu))))
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
