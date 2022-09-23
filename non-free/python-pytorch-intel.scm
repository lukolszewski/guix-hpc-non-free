(define-module (non-free python-pytorch-intel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages commencement)
  #:use-module (non-free python-numpy-intel)
  #:use-module (non-free mkl)
  #:use-module (non-free cuda)
  #:use-module (non-free cudnn)
 ;; #:use-module (nongnu packages nvidia)
  #:use-module (ice-9 match))

(define-public magma-cuda
  (package
    (name "magma-cuda")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://icl.utk.edu/projectsfiles/magma/downloads/magma-" version ".tar.gz"))
              (sha256
               (base32
                "0w8av977wv08b8ml7bpkcay8ry77a0z92b4p1g8y40q9n3d59dbm"))))
    (build-system cmake-build-system)
    (arguments 
     '(#:phases (modify-phases %standard-phases
		  (add-before 'configure 'set-environment-vars
		    (lambda* (#:key inputs #:allow-other-keys)
		      (let ((mkl (assoc-ref inputs "mkl"))
			    (cuda (assoc-ref inputs "cuda-11.6")))
			(setenv "MKLROOT" mkl)
			(setenv "CUDADIR" cuda)
			#t))))
       #:configure-flags (list "-DMAGMA_ENABLE_CUDA=ON"
                               "-DBLA_VENDOR=Intel10_64lp"
			       "-DMAGMA_WITH_MKL=ON"
			       "-DGPU_TARGET='Turing Ampere'"
			       "-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))
    (inputs (list bash cuda-11.6 mkl))
    (native-inputs (list bash gfortran-toolchain))
    (synopsis "Magma CUDA library")
    (description "Magma CUDA library provides LAPACK like functionality")
    (home-page "https://bitbucket.org/icl/magma.githttps://bitbucket.org/icl/magma.git")
    (license expat)))

(define-public gloo-cuda
  (let ((version "0.0.0") ; no proper version tag
        (commit "950c0e2")
        (revision "1"))
    (package
      (name "gloo-cuda")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "033p8ja70qi7p79f00c43galm60wr7kq9s7v4lrdbwaji2h2rkvf"))
	 (patches (search-patches "gloo-cuda11-6-arch.patch"))))
      (build-system cmake-build-system)
      (native-inputs
       (list googletest))
      (inputs
       (list openssl cuda-11.6))
      (arguments
       `(#:configure-flags '("-DUSE_CUDA=ON"
			     "-DBUILD_TEST=1")
         #:phases
         (modify-phases %standard-phases
	   (add-before 'configure 'set-environment-vars
		    (lambda* (#:key inputs #:allow-other-keys)
		      (let (  ;;(mkl (assoc-ref inputs "mkl"))
			    (cuda (assoc-ref inputs "cuda-11.6")))
			;;(setenv "MKLROOT" mkl)
			(setenv "CUDADIR" cuda)
			#t)))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "make" "gloo_test")
		 (invoke "make" "gloo_cuda" )))))))
      (synopsis "Collective communications library")
      (description
       "Gloo is a collective communications library.  It comes with a
number of collective algorithms useful for machine learning applications.
These include a barrier, broadcast, and allreduce.")
      (home-page "https://github.com/facebookincubator/gloo")
      (license license:bsd-3))))

(define-public nccl
  (package
    (name "nccl")
    (version "2.12.10-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nccl.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00g77idm26wa12qqrrabrikahza23ggj2azzz03f83r0dp7938s2"))))
    (build-system gnu-build-system)
    (arguments `(#:phases
		 (modify-phases %standard-phases
		   (delete 'configure)
		   (add-before 'build 'set-environment-vars
		    (lambda* (#:key inputs #:allow-other-keys)
		      (let ((cuda (assoc-ref inputs "cuda-11.6")))
			(setenv "CUDA_HOME" cuda)
			#t))))
		 #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
		 #:tests? #f))
    (inputs (list bash which openssl cuda-11.6))
    (synopsis "NVidia nccl library")
    (description "NVIDIA's nccl")
    (home-page "https://developer.nvidia.com/nccl")
    (license expat)))

(define-public python-pytorch-cuda
  (package
    (name "python-pytorch-cuda")
    (version "1.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/pytorch")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pdqi91qzgyx947zv4pw2fdj9vpqvdhfzw1ydjd4mpqm8g5njgnz"))
              (patches (search-patches
			"python-pytorch-system-libraries.patch"
			"python-pytorch-dependency.patch"
			"python-pytorch-cuda-gtest.patch"
                        "python-pytorch-runpath.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; XXX: Let's be clear: this package is a bundling fest.  We
                  ;; delete as much as we can, but there's still a lot left.
                  (for-each (lambda (directory)
                              (delete-file-recursively
                               (string-append "third_party/" directory)))
                            '("benchmark" "cpuinfo" "eigen"
                              ;; FIXME: QNNPACK (of which XNNPACK is a fork)
                              ;; needs these.
                              ;; "FP16" "FXdiv" "gemmlowp" "psimd"
                              "gloo" "googletest" "ios-cmake" "NNPACK"
                              "onnx" "protobuf" "pthreadpool"
                              "pybind11" "python-enum" "python-peachpy"
                              "python-six" "tbb" "XNNPACK" "zstd"))))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
			(add-before 'build 'set-environment-vars
			  (lambda* (#:key inputs #:allow-other-keys)
			    (let (
				  (cudnn (assoc-ref inputs "cudnn")))
			      ;;(setenv "USE_DISTRIBUTED" "OFF")
			      ;;(setenv "USE_NCCL" "OFF")
			      (setenv "GPU_TARGET" "Turing Ampere")
			      (setenv "USE_SYSTEM_NCCL" "ON")
			      (setenv "USE_CUDNN" "1")
			      (setenv "CUDNN_INCLUDE_PATH" (string-append cudnn "/include"))
			      (setenv "CUDNN_LIBRARY_PATH" (string-append cudnn "/lib"))
			      #t)))
			(add-before 'build 'use-system-libraries
			  (lambda* (#:key outputs #:allow-other-keys)
			    ;; Tell 'setup.py' to let 'CMakeLists.txt' know that we
			    ;; want to use "system libraries" instead of the bundled
			    ;; ones.
			    (setenv "USE_SYSTEM_LIBS" "1")
			    
			    (substitute* "cmake/Dependencies.cmake"
                              (("if\\(USE_SYSTEM_BIND11\\)")
                               "if(TRUE)"))
			    
			    ;; XXX: Disable that for simplicity for now.
			    (setenv "USE_FBGEMM" "0")))
			(add-before 'build 'make-things-writable
			  (lambda _
			    ;; The 'build_caffe2' function in
			    ;; 'tools/build_pytorch_libs.py', called from the
			    ;; top-level 'setup.py', needs write access to this
			    ;; directory.
			    (for-each make-file-writable
                                      (find-files "caffe2/proto" "."
						  #:directories? #t))))
			(replace 'check
			  (lambda* (#:key inputs outputs tests? #:allow-other-keys)
			    ;; Run the test suite following the instructions in
			    ;; 'CONTRIBUTING.md'.  XXX: Unfortunately this doesn't
			    ;; work, unless you set GUIX_PYTHONPATH presumably.
			    (when tests?
                              (add-installed-pythonpath inputs outputs)
                              (invoke "python" "test/run_test.py"))))
			
			(add-after 'install 'remove-test-executables
			  (lambda* (#:key inputs outputs #:allow-other-keys)
			    ;; Remove test executables, but keep other executables
			    ;; such as 'torch_shm_manager' and and .so files such as
			    ;; 'libtorch_global_deps.so'.
			    (let ((python-site (site-packages inputs outputs)))
                              (for-each delete-file
					(find-files python-site
						    "(^test_cpp_rpc|_test)$")))))
			(add-after 'install 'remove-caffe2-onnx-scripts
			  (lambda* (#:key outputs #:allow-other-keys)
			    (let* ((out (assoc-ref outputs "out"))
				   (bin (string-append out "/bin")))
                              ;; Remove 'convert-caffe2-to-onnx' and
                              ;; 'convert-onnx-to-caffe2': they seem to be
                              ;; deprecated and they cause a failure of the
                              ;; 'sanity-check' phase:
                              ;;
                              ;; ImportError: cannot import name 'metanet_pb2' from partially initialized module 'caffe2.proto' (most likely due to a circular import)
                              (for-each delete-file
					(find-files bin "^convert-.*caffe2"))
			      
                              (substitute* (find-files out "^entry_points\\.txt$")
				(("^convert-.*" all)
				 (string-append "# " all "\n")))))))
	   
	   ;; XXX: Tests attempt to download data such as
	   ;; <https://raw.githubusercontent.com/pytorch/test-infra/master/stats/slow-tests.json>.
	   ;; We're also missing some Python modules, such as expecttest.
	   #:tests? #f))
    (native-inputs
     (list bash cmake ninja glog))
    (inputs
     (list eigen
           ;; ("fmt" ,fmt)
           fp16
           gemmlowp
	   glog
           googletest
           googlebenchmark
           gloo-cuda
           nnpack
	   cuda-11.6
	   cudnn
	   mkl
   	   nvidia-libs ;; this is from the nonguix channel (nongnu packages nvidia)
	   magma-cuda
	   nccl
           pthreadpool
           protobuf
           pybind11
           sleef
           xnnpack
           zstd))
    (propagated-inputs
     (list python-astunparse
           python-click
           python-numpy-intel
           python-pyyaml
           python-cffi
           python-typing-extensions
           python-future
           python-six
           python-requests
           onnx                             ;propagated for its Python modules
           onnx-optimizer
           cpuinfo))
    (home-page "https://pytorch.org/")
    (synopsis "Python library for tensor computation and deep neural networks")
    (description
     "PyTorch is a Python package that provides two high-level features:

@itemize
@item tensor computation (like NumPy) with strong GPU acceleration;
@item deep neural networks (DNNs) built on a tape-based autograd system.
@end itemize

You can reuse Python packages such as NumPy, SciPy, and Cython to extend
PyTorch when needed.

Note: currently this package does not provide GPU support.")
    (license license:bsd-3)))
