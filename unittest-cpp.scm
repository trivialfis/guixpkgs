(define-module (unittest-cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake))

(define-public unittest-cpp
  (package
   (name "unittest-cpp")
   (version "2.0.0")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://github.com/unittest-cpp/unittest-cpp/archive/v"
		  version ".tar.gz"))
	    (sha256
	     (base32 "1fgmna2la7z4pwwy2gd10gpgi2q1fk89npjfvkmzvhkxhyc231bl"))))
   (arguments
    `(#:tests? #f))			; It's run after build automatically.
   (build-system cmake-build-system)
   (home-page "https://github.com/unittest-cpp/unittest-cpp")
   (synopsis "Lightweight unit testing framework for C++.")
   (description "UnitTest++ is a lightweight unit testing framework for C++.
It was designed to do test-driven development on a wide variety of platforms.
Simplicity, portability, speed, and small footprint are all very important
aspects of UnitTest++. UnitTest++ is mostly standard C++ and makes minimal use
of advanced library and language features, which means it should be easily
portable to just about any platform.")
   (license license:non-copyleft)))
