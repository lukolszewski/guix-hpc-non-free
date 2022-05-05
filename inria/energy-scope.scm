;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2022 Inria

(define-module (inria energy-scope)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python))

(define-public energy-scope-record
  (package
    (name "energy-scope-record")
    (version "0.0.0")
    (source (origin
              (method url-fetch)
              (uri "http://example.org")          ;XXX: placeholder
              (file-name (string-append "energy-scope-" version ".tar.bz2"))
              (sha256
               (base32
                "034zzrw6877l5k8a56pyldmqh1wipgbrl9ddzx7ywd0f6jq7lix6"))))
    (build-system copy-build-system)     ;no setup.py, so copy files manually
    (arguments
     `(#:install-plan '(("./" "bin"
                         #:include-regexp ("\\.py$" "\\.sh$"
                                           "\\.json")) ;XXX
                        ("eula.html" "share/doc/energy-scope"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'set-source-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Set the 'ENERGY_SCOPE_SRC_DIR' environment variable
                      ;; in executable scripts.
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (substitute* (find-files "." "\\.sh$")
                          (("^# author.*" all)
                           (string-append all "\n"
                                          "ENERGY_SCOPE_SRC_DIR=\"" bin
                                          "\"\n"
                                          "export ENERGY_SCOPE_SRC_DIR\n"))))))
                  (add-after 'unpack 'make-files-executable
                    (lambda _
                      ;; Make the *.py and *.sh scripts executable.
                      (for-each (lambda (file)
                                  (chmod file #o555))
                                (find-files "." "\\.(py|sh)$")))))))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("python-wrapper" ,python-wrapper)))
    (synopsis "Recording module of Energy Scope")
    (description
     "Record the energy consumption of programs.")
    (home-page "http://energy-scope.bordeaux.inria.fr/")
    (license #f)))                                ;non-free—see eula.html
