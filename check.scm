(define-module (check)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public catch2
  (package
    (name "catch2")
    (version "1.12.2")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0g2ysxc6adqca5wh7nsicnxb9wkxg75cd5izjsl39rcj0v903gr7"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))
