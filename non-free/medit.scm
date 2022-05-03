;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2022 Inria

(define-module (non-free medit)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gl))

(define-public medit
  (let ((commit "19da32235587ad3f12706b10404f46d2e132891e")
        (revision "0"))
    (package
      (name "medit")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/Algiane/medit")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1b4wdfaz0rm5spfh09rzr9qpcfgdg033vc7922r4hnbpcdif4him"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f                              ;no 'test' target
         #:phases (modify-phases %standard-phases
                    (add-before 'configure 'set-install-prefix
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; 'CMakeLists.txt' is set to install things in
                        ;; $HOME, so set it.
                        (let ((out (assoc-ref outputs "out")))
                          (setenv "HOME" out)))))))
      (inputs
       `(("mesa" ,mesa)
         ("freeglut" ,freeglut)))
      (synopsis "Mesh visualization tool")
      (description
       "Medit was developped to visualize numerical simulation results on
unstructured meshes in two and three dimensions.  Scalar, vector and tensor
fields can be easily associated and displayed with meshes.")
      (license #f))))                             ;no license
