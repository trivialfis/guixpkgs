(define-module (regex)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost))

(define-public hyperscan
  (package
   (name "hyperscan")
   (version "5.1.0")
   (home-page "https://www.hyperscan.io")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/intel/hyperscan/")
		  (commit (string-append "v" version))))
	    (sha256
	     (base32
	      "0r2c7s7alnq14yhbfhpkq6m28a3pyfqd427115k0754afxi82vbq"))
	    (file-name (git-file-name name version))))
   (arguments
    `(#:configure-flags
      '("-DBUILD_SHARED_LIBS=ON"
	"-DFAT_RUNTIME=ON")
      #:test-target "unit"))
   (native-inputs
    `(("boost" ,boost)
      ("python" ,python-2)))
   (inputs
    `(("ragel" ,ragel)))
   (build-system cmake-build-system)
   (synopsis "High-performance regular expression matching library")
   (description "Hyperscan is a high-performance multiple regex matching
library. It follows the regular expression syntax of the commonly-used libpcre
library, but is a standalone library with its own C API.

Hyperscan uses hybrid automata techniques to allow simultaneous matching of
large numbers (up to tens of thousands) of regular expressions and for the
matching of regular expressions across streams of data.

Hyperscan is typically used in a DPI library stack.")
   (license license:bsd-3)))
