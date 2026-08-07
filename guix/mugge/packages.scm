;;; Guix package definitions for the Mugge chat client.
;;;
;;; Guix cannot build the Scala sources: sbt/coursier are not packaged and
;;; builds have no network access, so there is no way to resolve the Maven
;;; dependency tree in a derivation.  Instead this repacks the jars from the
;;; GitHub release tarball (built by .github/workflows/release.yml) and runs
;;; them on Guix's own openjdk.  The tarball's bundled Temurin runtime and its
;;; jpackage ELF launcher are dropped: they are generic-glibc binaries that do
;;; not run on Guix System (no FHS dynamic linker) — the bytecode is portable,
;;; the launcher is not.
;;;
;;; Bumping to a new release:
;;;   guix download https://github.com/HNIKT-Tjenesteutvikling-Systemutvikling/mugge/releases/download/vX.Y.Z/mugge-linux-x64.tar.gz
;;; then update %mugge-version and the sha256 below.

(define-module (mugge packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define %mugge-version "1.0.0")

(define %mugge-home-page
  "https://github.com/HNIKT-Tjenesteutvikling-Systemutvikling/mugge")

;; Commands the client spawns at runtime: sh + stty (terminal size and raw
;; mode), ssh-keygen/openssl (auth), git (identity), notify-send (mentions,
;; pings, file offers), script (--assist bridge), gdbus + swayidle (idle
;; detection), pw-record/pw-play (voice).  All of them soft-fail, so a Guix
;; user who does not want voice simply never triggers pipewire.
(define %client-runtime-inputs
  (list bash-minimal coreutils openssh openssl git-minimal libnotify
        util-linux pipewire swayidle))

(define %azure-defaults
  (string-append
   "CHAT_SERVER_HOST=\"${CHAT_SERVER_HOST:-mugge-chat-server.norwayeast.azurecontainer.io}\"\n"
   "CHAT_SERVER_PORT=\"${CHAT_SERVER_PORT:-20222}\"\n"
   "export CHAT_SERVER_HOST CHAT_SERVER_PORT\n"))

(define-public mugge-client
  (package
    (name "mugge-client")
    (version %mugge-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append %mugge-home-page "/releases/download/v" version
                           "/mugge-linux-x64.tar.gz"))
       (sha256
        (base32 "1pvsiwg2p9pfam8rv181r8xhkdcf0763qakbbn29cz2vhnql352n"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("lib/app/" "share/mugge/lib" #:include-regexp ("\\.jar$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-launchers
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (lib (string-append #$output "/share/mugge/lib"))
                     (sh #$(file-append bash-minimal "/bin/sh"))
                     (java #$(file-append openjdk21 "/bin/java"))
                     (path (string-join
                            (append (list #$@(map (lambda (pkg)
                                                    (file-append pkg "/bin"))
                                                  %client-runtime-inputs))
                                    (list (string-append #$glib:bin "/bin")))
                            ":")))
                (define (install-script name text)
                  (let ((file (string-append bin "/" name)))
                    (call-with-output-file file
                      (lambda (port) (display text port)))
                    (chmod file #o555)))

                (mkdir-p bin)
                ;; The JVM expands the "/*" classpath entry itself, so new
                ;; jars need no launcher change.
                (install-script
                 "mugge-client"
                 (string-append
                  "#!" sh "\n"
                  "PATH=\"" path "${PATH:+:$PATH}\"\n"
                  "export PATH\n"
                  "exec " java " -Xmx256m -cp \"" lib "/*\" chat.ChatClient \"$@\"\n"))
                (install-script
                 "mugge-azure"
                 (string-append
                  "#!" sh "\n"
                  #$%azure-defaults
                  "exec " bin "/mugge-client \"$@\"\n"))
                ;; stdio TRAMP bridge: nothing may be written to stdout.
                (install-script
                 "mugge-bridge"
                 (string-append
                  "#!" sh "\n"
                  #$%azure-defaults
                  "exec " bin "/mugge-client --assist \"$@\"\n"))))))))
    (supported-systems '("x86_64-linux"))
    (inputs (append (list glib openjdk21) %client-runtime-inputs))
    (home-page %mugge-home-page)
    (synopsis "Mugge chat client")
    (description
     "Terminal chat client for the Mugge server: TLS with a pinned server key,
SSH-key based authentication, desktop notifications, file transfer and voice.
This package runs the jars from the upstream release tarball on Guix's
@code{openjdk}; the tarball's bundled runtime and launcher are discarded.")
    (license license:asl2.0)))

;; Separate package so that installing the client does not shadow the attach
;; script (and vice versa): the client provides mugge-client/mugge-azure/
;; mugge-bridge, only this one provides "mugge".  The Guix Home service
;; installs this one alone, mirroring nix/hm-module.nix.
(define-public mugge-attach
  (package
    (name "mugge-attach")
    (version %mugge-version)
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (file (string-append bin "/mugge")))
            (mkdir-p bin)
            (call-with-output-file file
              (lambda (port)
                (display
                 (string-append
                  "#!" #$(file-append bash-minimal "/bin/sh") "\n"
                  "set -eu\n"
                  "sock=\"${XDG_RUNTIME_DIR:-/run/user/$("
                  #$(file-append coreutils "/bin/id") " -u)}/mugge.sock\"\n"
                  "if [ ! -e \"$sock\" ]; then\n"
                  "  echo \"The mugge-chat background service is not running.\"\n"
                  "  echo \"Start it with: herd start mugge-chat\"\n"
                  "  exit 1\n"
                  "fi\n"
                  "echo \"Attaching to mugge chat. Detach with Ctrl-\\\\ (the connection stays up).\"\n"
                  "echo \"To stop the background service entirely: herd stop mugge-chat\"\n"
                  "exec " #$(file-append dtach "/bin/dtach")
                  " -a \"$sock\" -e '^\\' -r ctrl_l\n")
                 port)))
            (chmod file #o555)))))
    (inputs (list bash-minimal coreutils dtach))
    (home-page %mugge-home-page)
    (synopsis "Attach to the always-on Mugge chat session")
    (description
     "Provides the @command{mugge} command, which attaches the terminal to the
@code{dtach} session held by the @code{mugge-chat} Shepherd service.  Detaching
with @kbd{Ctrl-\\} leaves the chat connected.")
    (license license:asl2.0)))
