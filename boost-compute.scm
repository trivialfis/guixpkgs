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

(define-module (boost-compute)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages opencl)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public boost-compute
  (package
    (name "boost-compute")
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/boostorg/compute/archive/boost-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1a573qf7inph3xfb2cmsanylw40fpkb7rwdjvniyb1m37k9mz178"))))
    (arguments
     `(#:tests? #f))
    ;; (native-inputs `(("boost" ,boost)))
    (inputs `(("opencl-headers" ,opencl-headers)
              ("boost" ,boost)
              ("ocl-icd" ,ocl-icd)))
    (build-system cmake-build-system)
    (home-page "https://github.com/boostorg/compute")
    (synopsis "A C++ GPU Computing Library for OpenCL")
    (description "Boost.Compute is a GPU/parallel-computing library for C++
 based on OpenCL.")
    (license license:bsd-2)))
