(define-module (vcs)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python))

(define-public mercurial
  (package
    (name "mercurial")
    (version "4.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.mercurial-scm.org/"
                                  "release/mercurial-" version ".tar.gz"))
              (sha256
               (base32
                "1bv6wgcdx8glihjjfg22khhc52mclsn4kwfqvzbzlg0b42h4xl0w"))))
    (build-system python-build-system)
    (arguments
     `(#:python
       ,python-2
       ;; Test after install, haven't figured out the how-to yet.
       #:tests? #f))
    (home-page "https://www.mercurial-scm.org/")
    (synopsis "Decentralized version control system")
    (description
     "Mercurial is a free, distributed source control management tool.
It efficiently handles projects of any size
and offers an easy and intuitive interface.")
    (license license:gpl2+)))
