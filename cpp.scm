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
  #:use-module (guix build-system cmake))

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
