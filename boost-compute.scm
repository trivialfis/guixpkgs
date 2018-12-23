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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (opencl))

(define-public boost-compute
  (package
    (name "boost-compute")
    (version "1.69.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/boostorg/compute/archive/boost-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
		"1aks58zkkfi4rbc8w35jca9ndvs75sl9pj93nm7347w63z6g2hr0"))))
    ;; Header-only library, native-inputs for running tests.
    (native-inputs `(("boost" ,boost)
		     ("opencl-headers" ,opencl-headers-1.2)
		     ("pocl" ,pocl)))
    ;; If ocl-icd is placed in native-inputs, boost-compute throws
    ;; `CL_DEVICE_NOT_FOUND' during test.
    (inputs
     `(("ocl-icd" ,ocl-icd)))
    (arguments
     `(#:configure-flags
       '("-DBOOST_COMPUTE_BUILD_TESTS=ON"
	 ;; As long as we don't use OpenCL-2.2, there will be deprecated
	 ;; messages
	 "-DCMAKE_CXX_FLAGS=-std=gnu++11 -Wno-deprecated")
       #:phases
       (modify-phases %standard-phases
	 (add-before 'check 'set-envs
	   (lambda _
	     (setenv "HOME" "/tmp")
	     (setenv "BOOST_TEST_LOG_LEVEL" "all")
	     #t)))))
    (build-system cmake-build-system)
    (home-page "https://github.com/boostorg/compute")
    (synopsis "A C++ GPU Computing Library for OpenCL")
    (description "Boost.Compute is a GPU/parallel-computing library for C++
 based on OpenCL.")
    (license license:bsd-2)))
