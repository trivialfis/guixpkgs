(define-module (ffi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python))

(define-public pybind11
  (package
    (name "pybind11")
    (version "2.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://github.com/pybind/pybind11/archive/v"
		    version ".tar.gz"))
              (sha256
               (base32
		"1sj0x4fwsbnwdai5sxpw1l1vh8m5hpbkfk3zanxcbcgs39jpnfrs"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python)
       ("python-pytest" ,python-pytest)))
    (arguments
     `(#:test-target "check"))
    (home-page "https://github.com/pybind/pybind11/")
    (synopsis "Seamless operability between C++11 and Python")
    (description "pybind11 is a lightweight header-only library that exposes
C++ types in Python and vice versa, mainly to create Python bindings of
existing C++ code. Its goals and syntax are similar to the excellent
Boost.Python library by David Abrahams: to minimize boilerplate code in
traditional extension modules by inferring type information using compile-time
introspection.")
    (license license:expat)))

