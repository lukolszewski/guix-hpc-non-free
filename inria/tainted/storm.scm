;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2018, 2019, 2020, 2022 Inria

(define-module (inria tainted storm)
  #:use-module (guix)
  #:use-module (gnu packages gcc)
  #:use-module (inria storm)
  #:use-module (inria eztrace)
  #:use-module (gnu packages python)
  #:use-module (non-free cuda)

  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

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
    (native-inputs
     `(("gcc" ,gcc-8)                             ;CUDA requires GCC <= 8
       ("gfortran" ,gfortran-sans-libstdc++)
       ,@(alist-delete "gfortran" (package-native-inputs starpu))))
    (inputs
     `(("cuda" ,cuda)
       ,@(package-inputs starpu)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu)
       ((#:configure-flags flags '())
	`(append (list "--enable-cuda"
		       "--disable-opencl"
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

(define-public starpu+cuda+fxt
  ;; When FxT support is enabled, performance is degraded, hence the separate
  ;; package.
  (package
    (inherit starpu+cuda)
    (name "starpu-cuda-fxt")
    (inputs `(("fxt" ,fxt)
	      ,@(package-inputs starpu+cuda)))
    (arguments
     (substitute-keyword-arguments (package-arguments starpu+cuda)
       ((#:configure-flags flags '())
	`(cons "--with-fxt" ,flags))))
    ;; some tests require python.
    (native-inputs
     `(("python-wrapper" ,python-wrapper)
       ,@(package-native-inputs starpu+cuda)))))
