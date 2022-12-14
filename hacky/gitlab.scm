;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>

(define-module (hacky gitlab)
  #:use-module (guix)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages version-control))

(define-public gitlab-runner
  (package
    (name "gitlab-runner")
    (version "11.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://" name
                                  "-downloads.s3.amazonaws.com/v"
                                  version "/binaries/" name "-linux-amd64"))
              (file-name (string-append name "-linux-amd64"))
              (sha256
               (base32
                "1mg5azzwkm7issv6nbpk5vlzyibx2d7hgxwdp7ivknzwg80z48pv"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (out    (assoc-ref %outputs "out"))
                          (install-dir (string-append out "/bin"))
                          (executable  (string-append install-dir
                                                      "/gitlab-runner"))
                          (bash    (string-append
                                    (assoc-ref %build-inputs "bash")
                                    "/bin/"))
                          (git-dir (string-append
                                    (assoc-ref %build-inputs "git")
                                    "/bin/")))
                     (setenv "PATH" (string-append
                                     (getenv "PATH") ":"
                                     bash))
                     (mkdir-p install-dir)
                     (copy-file source executable)
                     (chmod executable #o555)
                     (wrap-program executable
                       `("PATH" ":" prefix (,git-dir)))))))
    (inputs
     `(("git" ,git)
       ("bash" ,bash)))
    (synopsis "Gitlab Runner")
    (description
     "The official GitLab Runner written in Go.  It runs tests and sends the
results to GitLab.  GitLab CI is the open-source continuous integration
service included with GitLab that coordinates the testing.")
    (home-page "https://gitlab.com/gitlab-org/gitlab-runner")
    (license license:expat)))
