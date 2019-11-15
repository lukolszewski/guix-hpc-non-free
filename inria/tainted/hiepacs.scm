;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Note that this module provides packages that depend on "non-free"
;;; software, which denies users the ability to study and modify it.
;;;
;;; Copyright © 2019 Inria

(define-module (inria tainted hiepacs)
  #:use-module (guix)
  #:use-module (inria hiepacs)
  #:use-module (inria storm)
  #:use-module (inria tainted storm)
  #:use-module (non-free cuda))

(define-public chameleon+cuda
  (package
    (inherit chameleon)
    (name "chameleon-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments chameleon)
                                   ((#:configure-flags flags '())
                                    `(cons "-DCHAMELEON_USE_CUDA=ON" ,flags))))
    (inputs
     `(("cuda" ,cuda)
       ,@(package-inputs chameleon)))
    (propagated-inputs `(("starpu" ,starpu+cuda)
                         ,@(delete `("starpu" ,starpu) (package-inputs chameleon))))))
