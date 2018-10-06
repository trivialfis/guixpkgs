(define-module (build)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages terminals)
  #:use-module (python))

(define-public ronin
  (let* ((commit "588643cc9d18ef951778c53e96b116483ca5c9c0")
	 (revision "0")
	 (version (string-append "1.1.1" revision commit)))
    (package
      (name "ronin")
      (version version)
      (home-page "https://github.com/tliron/ronin")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url home-page)
		      (commit commit)))
		(sha256
		 (base32
		  "1n2d0xsqlsj2z2spc5q651syjgsixc455g41x8vavv1ls58y0fkf"))
		(file-name (git-file-name name version))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-glob2" ,python-glob2)
	 ("python-blessings" ,python-blessings)
	 ("python-colorma" ,python-colorama)))
      (synopsis "Straightforward but powerful build system based on Ninja and
Python.")
      (description "A straightforward but powerful build system based on Ninja
and Python.")
      (license license:asl2.0))))
