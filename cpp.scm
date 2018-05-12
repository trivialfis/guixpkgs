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
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check))

(define-public json-modern-cxx
  (package
    (name "json-modern-cxx")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/nlohmann/json/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0m5fhdpx2qll933db2nsi30nns3cifavzvijzz6mxhdkpmngmzz8"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (home-page "https://github.com/nlohmann/json")
    (build-system cmake-build-system)
    (synopsis "JSON for Modern C++")
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
             (invoke "make" "test_xtl")
             (chdir "test")
             (invoke "./test_xtl")
             (chdir ".."))))))
    (home-page "https://github.com/QuantStack/xtl")
    (build-system cmake-build-system)
    (synopsis "C++ template library providing some basic tools")
    (description "xtl is a C++ header-only template library providing basic
tools (containers, algorithms) used by other quantstack packages")
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
