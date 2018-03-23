(define-module (bear)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public bear
  (package
   (name "bear")
   (version "2.3.11")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://github.com/rizsotto/Bear/archive/"
		  version ".tar.gz"))
	    (sha256
	     (base32
	      "1m0w0wqnz983l7fpp5p9pdsqr7n3ybrzp8ywjcvn0rihsrzj65j6"))))
   (build-system cmake-build-system)
   (home-page "https://github.com/rizsotto/Bear")
   (synopsis "Bear is a tool that generates a compilation database for clang 
tooling")
   (description "The JSON compilation database is used in the clang project to
provide information on how a single compilation unit is processed. With this,
it is easy to re-run the compilation with alternate programs. Bear is used to
generate such compilation database.")
   (license license:gpl3+)))
