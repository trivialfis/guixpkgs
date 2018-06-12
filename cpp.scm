;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This file is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This file is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This file.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (code)
  #:use-module (check))

(define-public json-modern-cxx
  (package
    (name "json-modern-cxx")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nlohmann/json/archive/v" version ".tar.gz"))
       (sha256
        (base32
         "0m5fhdpx2qll933db2nsi30nns3cifavzvijzz6mxhdkpmngmzz8"))
       (file-name (string-append name "-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "./third_party")
           (delete-file-recursively "./test/thirdparty")
           (delete-file-recursively "./benchmarks/thirdparty")
           ;; Splits catch and fifo_map
           (with-directory-excursion "test/src"
             (let ((files (find-files "." ".*\\.cpp")))
               (substitute* files
                 (("#include ?\"(catch.hpp)\"" all catch-hpp)
                  (string-append "#include <catch/" catch-hpp ">")))
               (substitute* files
                 (("#include ?\"(fifo_map.hpp)\"" all fifo-map-hpp)
                  (string-append
                   "#include <fifo_map/" fifo-map-hpp ">")))))))))
    (native-inputs
     `(("amalgamate" ,amalgamate)
       ("catch2" ,catch2)
       ("clang-runtime" ,clang-runtime)
       ("fifo-map" ,fifo-map)))
    (home-page "https://github.com/nlohmann/json")
    (build-system cmake-build-system)
    (synopsis "JSON parser and printer library for C++")
    (description "JSON for Modern C++ is a C++ json library that provides
intutive syntax and trivial integration.")
    (license license:expat)))

(define-public xtl
  (package
    (name "xtl")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/QuantStack/xtl/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "05bcz9y590b77bxcip0k31rgsapmkwqi1smvsvc84zz7m87d4jvy"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (native-inputs
     `(("googletest" ,googletest)
       ("json-modern-cxx" ,json-modern-cxx)))
    (arguments
     `(#:configure-flags
       '("-DBUILD_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* _
             (with-directory-excursion "test"
               (invoke "./test_xtl")
               #t))))))
    (home-page "https://github.com/QuantStack/xtl")
    (build-system cmake-build-system)
    (synopsis "C++ template library providing some basic tools")
    (description "xtl is a C++ header-only template library providing basic
tools (containers, algorithms) used by other QuantStack packages.")
    (license license:bsd-3)))

(define-public xsimd
  (package
    (name "xsimd")
    (version "4.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/QuantStack/xsimd/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "0x05l4xpqr9b66sm6lkf48n6x7999ks921x6k2hzkkg6mh3gqd46"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (home-page "https://github.com/QuantStack/xsimd")
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "xtest"))
    (native-inputs
     `(("googletest" ,googletest)))
    (synopsis "C++ wrappers for SIMD intrinsics and parallelized, optimized
math implementations")
    (description "xsimd provides a unified means for using these features for
library authors.  Namely, it enables manipulation of batches of numbers with
the same arithmetic operators as for single values.  It also provides
accelerated implementation of common mathematical functions operating on
batches.")
    (license license:bsd-3)))

(define-public fifo-map
  (let* ((commit "0dfbf5dacbb15a32c43f912a7e66a54aae39d0f9")
         (revision "0")
         (version (string-append "1.1.1" revision commit)))
    (package
      (name "fifo-map")
      (version version)
      (home-page "https://github.com/nlohmann/fifo_map")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0pi77b75kp0l7z454ihcd14nzpi3nc5m4nyjbsgy5f9bw3676196"))
                (patches (search-patches "fifo-map-remove-catch.hpp.patch"
                                         "fifo-map-fix-flags-for-gcc.patch"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet '(delete-file-recursively "./test/thirdparty"))))
      (native-inputs
       `(("catch2" ,catch2)))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (invoke "./unit")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (inc (string-append out "/include/fifo_map")))
                 (mkdir-p inc)
                 (with-directory-excursion
                     (string-append "../" ,name "-" ,version "-checkout")
                   (copy-file "src/fifo_map.hpp"
                              (string-append inc "/fifo_map.hpp")))))))))
      (synopsis "FIFO-ordered associative container for C++")
      (description "Fifo_map is a C++ header only library for  associative
container which uses the order in which keys were inserted to the container
as ordering relation.")
      (license license:expat))))

(define-public rct-next
  (let* ((commit "60d28748efadabaec03ceca18565caa4bb5ef7b3")
         (revision "2")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "rct-next")
      (version version)
      (home-page "https://github.com/Andersbakken/rct")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "08yy979dw0qnc854ikfwc284z8jl6l41y1x4dd75p8fm8kify27p"))
                (patches (search-patches "rct-cmake-Add-missing-headers.patch"
                                         "rct-build-test.patch"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         '("-DWITH_TESTS=ON"            ; To run the test suite
           "-DRCT_RTTI_ENABLED=ON")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (with-directory-excursion "./tests"
                 (invoke "./rct_tests")))))))
      (native-inputs
       `(("cppunit" ,cppunit)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("openssl" ,openssl)
         ("zlib" ,zlib)))
      (synopsis "C++ library providing Qt-like APIs on top of the STL")
      (description "Rct is a set of C++ tools that provide nicer (more Qt-like)
 APIs on top of Standard Template Library (@dfn{STL}) classes.")
      (license (list license:expat        ; cJSON
                     license:bsd-4)))))
