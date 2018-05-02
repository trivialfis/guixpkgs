(define-module (cv)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages image))

(define-public vigra-c
  (let* ((commit "a2ff675f42079e2623318d8ff8b4288dbe7a7f06")
	 (revision "0")
	 (version (string-append "0.0.0" revision commit)))
    (package
      (name "vigra-c")
      (version version)
      (home-page "https://github.com/BSeppke/vigra_c")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url home-page)
		      (commit commit)))
		(sha256
		 (base32
		  "1f1phmfbbz3dsq9330rd6bjmdg29hxskxi9l17cyx1f4mdqpgdgl"))
		(file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))			; No test target.
      (native-inputs
       `(("doxygen" ,doxygen)))
      (inputs
       `(("vigra" ,vigra)
	 ("fftw" ,fftw)
	 ("fftwf" ,fftwf)))
      (synopsis "wraps (parts of) VIGRA's functionality into a C shared
library")
      (description "An easy understandable C-Wrapper to re-use functionality of
the VIGRA computer vision library in other programming environments.")
      (license license:expat))))
