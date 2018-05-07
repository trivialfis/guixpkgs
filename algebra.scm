(define-module (algebra)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (cpp)
  #:use-module (gnu packages check))

(define-public xtensor
  (package
    (name "xtensor")
    (version "0.15.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://github.com/QuantStack/xtensor/archive/"
		    version ".tar.gz"))
              (sha256
               (base32
		"0mlsw4p1w5mh7pscddfdamz27zq3wml5qla3vbzgvif34vsqc8ra"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest" ,googletest)
       ("xtl" ,xtl)))
    (arguments
     `(#:configure-flags
       '("-DBUILD_TESTS=ON")
       #:test-target "xtest"))
    (home-page "https://github.com/nlohmann/json")
    (synopsis "JSON for Modern C++")
    (description "JSON for Modern C++")
    (license license:bsd-3)))
