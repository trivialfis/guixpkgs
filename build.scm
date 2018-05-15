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
      (native-inputs
       `(("python-glob2" ,python-glob2)
	 ("python-blessings" ,python-blessings)
	 ("python-colorma" ,python-colorama)))
      (synopsis "Straightforward but powerful build system based on Ninja and
Python.")
      (description "A straightforward but powerful build system based on Ninja
and Python.")
      (license license:asl2.0))))

(define-public bear
  (package
    (name "bear")
    (version "2.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rizsotto/Bear/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m0w0wqnz983l7fpp5p9pdsqr7n3ybrzp8ywjcvn0rihsrzj65j6"))))
    (build-system cmake-build-system)
    (inputs
     `(("python" ,python-wrapper)))
    ;; (arguments
    ;;  `(#:modules ((guix build utils)
    ;; 		  (guix build cmake-build-system)
    ;; 		  ((guix build gnu-build-system) #:prefix gnu:))
    ;;    #:phases
    ;;    (modify-phases %standard-phases
    ;; 	 (add-before 'configure 'patch-python-scripts
    ;; 		     (assoc-ref gnu:%standard-phases 'patch-source-shebangs)))
    ;;    ))
    (home-page "https://github.com/rizsotto/Bear")
    (synopsis "Tool for generating a compilation database")
    (description "A JSON compilation database is used in the Clang project to
provide information on how a given compilation unit is processed.  With this,
it is easy to re-run the compilation with alternate programs.  Bear is used to
generate such a compilation database.")
    (license license:gpl3+)))
