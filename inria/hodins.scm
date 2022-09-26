(define-module (inria hodins)
    #:use-module (guix)
    #:use-module (guix build-system cmake)
    #:use-module (guix utils)
    #:use-module (guix git)
    #:use-module (guix git-download)
    #:use-module (guix packages)
    #:use-module (inria storm)
    #:use-module (inria tainted storm)
    #:use-module (non-free cuda)
    #:use-module (gnu packages pkg-config)
    #:use-module (gnu packages python)
    #:use-module (gnu packages python-xyz)
    #:use-module (gnu packages check)
    #:use-module (gnu packages maths)
    #:use-module (gnu packages serialization)
    #:use-module (gnu packages commencement))

;; Should be the canonic way of doing toolchain switch, but does not work for obscure reason
;(define-public yaml-cpp-gcc8
;               (let ((toolchain (specification->package "gcc-toolchain@8")))
;                 (package-with-c-toolchain (package (inherit yaml-cpp) (name "yaml-cpp-gcc8")) `(("toolchain" ,toolchain)))))

(define yaml-cpp-gcc8
(package
  (inherit yaml-cpp)
  (name "yaml-cpp-gcc8")
  (native-inputs
    `(("gcc-toolchain" ,gcc-toolchain-8)))
  (synopsis "yaml-cpp package compiled with gcc-toolchain 8")))


(define sundials-hodins
(package/inherit sundials
  (name "sundials-hodins")
  (properties '((tunable? . #t)))
  (arguments
    (substitute-keyword-arguments (package-arguments sundials)
    ((#:configure-flags cf)
     #~`("-DCMAKE_C_FLAGS=-O3 -g -fcommon" 
       ,@(delete "-DCMAKE_C_FLAGS=-O2 -g -fcommon" #$cf)))))))

(define sundials-hodins-cuda
(package/inherit sundials-hodins
  (name "sundials-hodins-cuda")
  (native-inputs
    `(("gcc-toolchain" ,gcc-toolchain-8)))
   (inputs
     (modify-inputs (package-inputs sundials-hodins)
       (append (lookup-package-input starpu+cuda "cuda"))))
  (arguments
    (substitute-keyword-arguments (package-arguments sundials)
                                  ((#:configure-flags cf)
                                   #~`("-DENABLE_CUDA=ON"
                                       "-DCMAKE_CUDA_ARCHITECTURES=60;61;62" ,@#$cf))
                                  ((#:tests? runtests '()) #f)))))  ; tests on GPU fail when run on non GPU nodes

(define-public hodins
  (let ((commit "1824710cd71bd542da602a3cc5c0a48bccf8b6a0")
        (revision "0"))
    (package
      (name "hodins")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.inria.fr/cagire/hodins")
      (properties '((tunable? . #t)))
      ;; This will clone the repository over SSH on the client side, provided
      ;; you have permissions to do so.
      (source (git-checkout (url "git@gitlab.inria.fr:cagire/hodins.git") ;
                            (commit commit)))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release" "-DWITH_CUDA=OFF")
         #:phases (modify-phases %standard-phases
                    (add-before 'check 'prepare-test-environment
                      (lambda _
                        (setenv "HOME"
                                (getcwd)) ;StarPU expects $HOME to be writable.
                        #t)))))
      (native-inputs (list pkg-config))
      (inputs (list python python-pytest python-pyaml))
      (propagated-inputs (list starpu yaml-cpp sundials-hodins lapack))
      (synopsis "Task-based solver for the resolution of the Euler system")
      (description
       "Resolution of the Euler system through a finite volume approach 
        on structured grids and making use of the StarPU runtime system. 
        Work in progress : (i) resolution through a discontinuous Galerkin approach
        , and (ii) consideration of a source term in a multi-species reactive framework.")
      (license #f))))                             ;private software so far

(define-public hodins-cuda
(package/inherit hodins
   (name "hodins-cuda")
   (native-inputs
     `(("gcc-toolchain" ,gcc-toolchain-8) ,@(package-native-inputs hodins)))
   (inputs
     (modify-inputs (package-inputs hodins)
       (append (lookup-package-input starpu+cuda "cuda"))))
   (propagated-inputs
     (modify-inputs (package-propagated-inputs hodins)
       (replace "starpu" starpu+cuda)
       (replace "sundials-hodins" sundials-hodins-cuda)
       (replace "yaml-cpp" yaml-cpp-gcc8)))
  (arguments  
    (substitute-keyword-arguments (package-arguments hodins)
                                  ((#:configure-flags cf)
                                   `(cons "-DWITH_CUDA=ON" ,cf))
                                  ((#:tests? runtests '()) #f)))  ; tests on GPU fail when run on non GPU nodes

  (synopsis "hodins package with Cuda support")))  

