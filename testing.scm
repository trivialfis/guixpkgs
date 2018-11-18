(define-module (testing)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public googletest
  (package
   (name "googletest")
   (version "1.8.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/google/googletest/archive/"
                         "release-" version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "17147961i01fl099ygxjx4asvjanwdd446nwbq9v8156h98zxwcv"))
     (patches
      (search-patches "google-test-Remove-record_property_env.patch"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags
      '("-DBUILD_SHARED_LIBS=ON"
	"-Dgmock_build_tests=ON"
	"-Dgtest_build_tests=ON")))
   (native-inputs
    `(("python-2" ,python-2)))		; 1.8.1 still requrie python2
   (home-page "https://github.com/google/googletest/")
   (synopsis "Test discovery and XUnit test framework")
   (description "Google Test features an XUnit test framework, automated test
discovery, death tests, assertions, parameterized tests and XML test report
generation.")
   (license license:bsd-3)))
