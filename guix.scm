;; Entry point for `guix build -f guix.scm' from a checkout, so the package can
;; be built without adding this repository as a channel.  It builds the pinned
;; release tarball, not the working tree — see guix/mugge/packages.scm for why.
(add-to-load-path (string-append (dirname (current-filename)) "/guix"))

(use-modules (mugge packages))

mugge-client
