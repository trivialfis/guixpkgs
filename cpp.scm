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

(define-public cxx-json
  (package
    (name "cxx-json")
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
    (description "JSON for Modern C++")
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
       ("cxx-json" ,cxx-json)))
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
    (description "xtl is a C++ header-only librar template providing basic
tools (containers, algorithms) used by other quantstack packages")
    (license license:expat)))
