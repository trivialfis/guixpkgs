(define-module (prolog)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public gprolog
  (package
    (name "gprolog")
    (version "1.4.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
	    "http://gprolog.org/gprolog-" version ".tar.gz"))
      (sha256
       (base32
	"0z4cc42n3k6i35b8mr816iwsvrpxshw6d7dgz6s2h1hy0l7g1p5z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append
              "--with-install-dir=" %output "/share/gprolog"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-dir-n-fix-shells
           (lambda _
             (chdir "src")
             (substitute* "configure"
               (("-/bin/sh")  (string-append "-"  (which "sh")))
               (("= /bin/sh") (string-append "= " (which "sh"))))
             #t)))))
    (home-page "https://www.gnu.org/software/gprolog/")
    (synopsis "Prolog compiler")
    (description
     "GNU Prolog is a standards-compliant Prolog compiler with constraint
solving over finite domains.  It accepts Prolog+ constraint programs and
produces a compiled, native binary which can function in a stand-alone
manner.  It also features an interactive interpreter.")
    (license (list gpl2+ lgpl3+))

    ;; See 'configure' for the list of supported architectures.
    (supported-systems (fold delete
                             %supported-systems
                             '("armhf-linux" "mips64el-linux")))))
