;;; Guix Home counterpart of nix/hm-module.nix: the client runs permanently in
;;; a detachable dtach session supervised by Shepherd, and `mugge' attaches a
;;; terminal to it.
;;;
;;; Usage in ~/.config/guix/home/config.scm:
;;;
;;;   (use-modules (mugge home))
;;;   (home-environment
;;;     (services (list (service home-mugge-chat-service-type))))

(define-module (mugge home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages screen)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (mugge packages)
  #:export (home-mugge-chat-configuration
            home-mugge-chat-configuration?
            home-mugge-chat-configuration-package
            home-mugge-chat-configuration-attach
            home-mugge-chat-configuration-command
            home-mugge-chat-configuration-socket-path
            home-mugge-chat-configuration-log-file
            home-mugge-chat-configuration-auto-start?
            home-mugge-chat-service-type))

(define-record-type* <home-mugge-chat-configuration>
  home-mugge-chat-configuration make-home-mugge-chat-configuration
  home-mugge-chat-configuration?
  (package     home-mugge-chat-configuration-package     (default mugge-client))
  (attach      home-mugge-chat-configuration-attach      (default mugge-attach))
  ;; Binary of PACKAGE to supervise.  The default bakes in the production
  ;; server host/port, so nothing else needs configuring.
  (command     home-mugge-chat-configuration-command     (default "mugge-azure"))
  ;; #f means $XDG_RUNTIME_DIR/mugge.sock, resolved when the service starts.
  (socket-path home-mugge-chat-configuration-socket-path (default #f))
  (log-file    home-mugge-chat-configuration-log-file    (default #f))
  (auto-start? home-mugge-chat-configuration-auto-start? (default #t)))

(define (home-mugge-chat-shepherd-services config)
  (let ((client (home-mugge-chat-configuration-package config))
        (command (home-mugge-chat-configuration-command config))
        (socket-path (home-mugge-chat-configuration-socket-path config))
        (log-file (home-mugge-chat-configuration-log-file config))
        (auto-start? (home-mugge-chat-configuration-auto-start? config)))
    (list
     (shepherd-service
      (provision '(mugge-chat))
      (documentation "Mugge chat background client, detachable via dtach.")
      (auto-start? auto-start?)
      ;; Shepherd's counterpart of Restart=on-failure: the client's watchdog
      ;; exits nonzero when the connection dies, and gets respawned.
      (respawn? #t)
      (start
       #~(lambda _
           (let* ((runtime (or (getenv "XDG_RUNTIME_DIR")
                               (string-append "/run/user/"
                                              (number->string (getuid)))))
                  (socket (or #$socket-path
                              (string-append runtime "/mugge.sock")))
                  (log (or #$log-file
                           (string-append runtime "/mugge-chat.log"))))
             ;; dtach refuses to bind a socket left behind by a crashed run.
             (false-if-exception (delete-file socket))
             (fork+exec-command
              (list #$(file-append dtach "/bin/dtach")
                    "-N" socket "-e" "^\\"
                    #$(file-append client "/bin/" command))
              ;; -N keeps dtach in the foreground so Shepherd supervises it.
              ;; Inheriting the session environment is what lets notify-send
              ;; reach the user D-Bus bus.
              #:environment-variables (cons "MUGGE_SERVICE=1" (environ))
              #:log-file log))))
      (stop #~(make-kill-destructor))))))

(define (home-mugge-chat-profile-packages config)
  (list (home-mugge-chat-configuration-attach config)
        dtach))

(define home-mugge-chat-service-type
  (service-type
   (name 'home-mugge-chat)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-mugge-chat-shepherd-services)
          (service-extension home-profile-service-type
                             home-mugge-chat-profile-packages)))
   (default-value (home-mugge-chat-configuration))
   (description
    "Run the Mugge chat client as an always-on Shepherd user service inside a
@code{dtach} session, and install the @command{mugge} command that attaches a
terminal to it.")))
