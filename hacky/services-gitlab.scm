;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>

(define-module (hacky services-gitlab)
  #:use-module (hacky gitlab)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (gitlab-runner-configuration
            gitlab-runner-configuration?
            gitlab-runner-service-type))

;;;
;;; Gitlab-runner daemon
;;;

(define-record-type* <gitlab-runner-configuration>
  gitlab-runner-configuration
  make-gitlab-runner-configuration
  gitlab-runner-configuration?
  (package gitlab-runner-configuration-package
           (default gitlab-runner))
  (name gitlab-runner-configuration-name
        (default "guix-runner"))
  (url gitlab-runner-configuration-url
       (default "gitlab.com"))
  (token gitlab-runner-configuration-token
         (default ""))
  (executor gitlab-runner-configuration-executor
            (default "shell"))
  (output-limit gitlab-runner-configuration-output-limit
                (default 16384)))

(define %gitlab-runner-accounts
  (list (user-account
         (name "gitlab-runner")
         (group "gitlab-runner")
         (system? #t)
         (comment "gitlab-runner daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "gitlab-runner")
         (system? #t))))

(define %gitlab-runner-activation
  (match-lambda
    (($ <gitlab-runner-configuration> package name url token
                                      executor output-limit)
     #~(begin
         (use-modules (guix build utils))

         (define (mkdir-p/perms directory owner perms)
           (mkdir-p directory)
           (chown   directory
                    (passwd:uid owner)
                    (passwd:gid owner))
           (chmod   directory perms))
         
         (let ((user (getpwnam "gitlab-runner"))
               (config-dir "/var/cache/guix/gitlab-runner"))
           (mkdir-p/perms config-dir user #o755))))))

(define gitlab-runner-shepherd-service
  (match-lambda
    (($ <gitlab-runner-configuration> package name url token
                                      executor output-limit)
     (with-imported-modules (source-module-closure
                             '((gnu build shepherd)))
       (shepherd-service
        (documentation "Gitlab-runner daemon")
        (provision '(gitlab-runner))
        (requirement '(networking))
        (modules '((gnu build shepherd)
                   (shepherd service)))
        (start
         #~(lambda ()
             (let ((config-file "/var/cache/guix/gitlab-runner/gitlab.toml")
                   (certs-dir #$(file-append nss-certs "/etc/ssl/certs/")))
               (define (register-runner)
                 (if (not (file-exists? config-file))
                     (zero?
                      (system* #$(file-append package "/bin/gitlab-runner")
                               "register"
                               "--non-interactive"
                               "--config" config-file
                               "--name" #$name
                               "--url" #$url
                               "--executor" #$executor
                               "--output-limit"
                               (number->string #$output-limit)
                               "--registration-token" #$token))
                     #t))
               (let ((command (list #$(file-append package "/bin/gitlab-runner")
                                    "run" "--config" config-file))
                     (environment (list "HOME=/builds" ;for 'guix pull'
                                        "PATH=/run/current-system/profile/bin"
                                        (string-append "SSL_CERT_DIR="
                                                       certs-dir)
                                        (string-append "GUIX_LOCPATH="
                                                       #$glibc-utf8-locale
                                                       "/lib/locale"))))
                 (if (register-runner)
                     (fork+exec-command command
                                        #:user "gitlab-runner"
                                        #:group "gitlab-runner"
                                        #:log-file
                                        "/var/log/gitlab-runner.log"
                                        #:environment environment)
                     #f)))))
        (stop #~(make-kill-destructor)))))))

(define gitlab-runner-service-type
  (service-type
   (name 'gitlab-runner)
   (description
    "Run gitlab-runner daemon @command{gitlab-runner run}.")
   (extensions
    (list (service-extension account-service-type
                             (const %gitlab-runner-accounts))
          (service-extension shepherd-root-service-type
                             (compose list gitlab-runner-shepherd-service))
          (service-extension activation-service-type
                             %gitlab-runner-activation)

          ;; 'gitlab-runner' wants to run Git, but since it runs its scripts
          ;; with 'bash --login', PATH is essentially limited to
          ;; /run/current-system/profile/bin.  So put Git in there.
          (service-extension profile-service-type
                             (const (list git-minimal)))))
   (default-value (gitlab-runner-configuration))))
