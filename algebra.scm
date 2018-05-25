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
    (home-page "http://quantstack.net/xtensor")
    (synopsis "C++ tensors with broadcasting and lazy computing.")
    (description "xtensor is a C++ library meant for numerical analysis with
multi-dimensional array expressions.

xtensor provides
@itemize
@item an extensible expression system enabling lazy broadcasting.
@item an API following the idioms of the C++ standard library.
@item tools to manipulate array expressions and build upon xtensor.
@end itemize")
    (license license:bsd-3)))
