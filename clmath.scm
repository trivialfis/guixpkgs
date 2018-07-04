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

(define-module (clmath)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages check)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages opencl))

(define-public clblas
  (let ((version "2.12"))
    (package
      (name "clblas")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/clMathLibraries/clBLAS/archive/v"
                      version ".tar.gz"))
                (file-name (string-append name "-release-" version ".tar.gz"))
                (sha256
                 (base32
                  "1acp8995wnkndpql3zxbldr4xmg66ahfn410fyb5wg540v5wfsbj"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
	 (list
	  (string-append "../clBLAS-" ,version "/src")
	  "-DBUILD_SHARED_LIBS=ON"
	  "-DUSE_SYSTEM_GTEST=ON"
          "-DBUILD_TEST=OFF")
	 ;; Many tests regarding to local variables failed. The kernel code is
	 ;; generated, which is quite hard to patch.
	 #:tests? #f))
      (native-inputs `(("boost" ,boost)
		       ("gfortran" ,gfortran)
		       ("opencl-headers" ,opencl-headers-1.2)
                       ("pkg-config" ,pkg-config)
                       ("python" ,python-wrapper)))
      (inputs `(("ocl-icd" ,ocl-icd)))
      (home-page "http://www.glfw.org/")
      (synopsis "Software library containing BLAS functions written in OpenCL")
      (description "The primary goal of clBLAS is to make it easier for
developers to utilize the inherent performance and power efficiency benefits of
heterogeneous computing.  clBLAS interfaces do not hide nor wrap OpenCL
interfaces, but rather leaves OpenCL state management to the control of the
user to allow for maximum performance and flexibility.  The clBLAS library does
generate and enqueue optimized OpenCL kernels, relieving the user from the task
of writing, optimizing and maintaining kernel code themselves.")
      (license (list license:asl2.0)))))

(define-public clfft
  (package
    (name "clfft")
    (version "2.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/clMathLibraries/"
                                  name "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "04kvi4gc5b5v03pzgvcrvysy6gmd08i71dwpd4z6m36ld8a8qd77"))
              (file-name (string-append name "-release-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags
                 (list
		  (string-append "../clFFT-" ,version "/src")
                  "-DBUILD_SHARED_LIBS=ON"
                  "-DBUILD_TEST=ON"
                  "-DUSE_SYSTEM_GTEST=ON"
		  (string-append "-DBOOST_INCLUDEDIR="
				 (assoc-ref %build-inputs "boost")
				 "/include/")
		  (string-append "-DBOOST_LIBRARYDIR="
				 (assoc-ref %build-inputs "boost")
				 "/lib/")
		  (string-append "-DBoost_LIBRARIES="
                            "-lboost_iostreams "
                            "-lboost_filesystem "
                            "-lboost_system "
                            "-lboost_thread "
                            "-lboost_timer "
                            "-lboost_chrono "
                            "-lboost_program_options")
		  "-DBoost_FOUND=TRUE"
		  "-DFIND_LIBRARY_USE_LIB64_PATHS=FALSE")
		 #:phases
		 (modify-phases %standard-phases
		   (add-before 'check 'set-HOME
		     (lambda _
		       (setenv "HOME" "/tmp")
		       #t))
		   (replace 'check
		     (lambda _
		       (with-directory-excursion "staging"
			 (invoke "test-short")))))))
    (native-inputs
     `(("boost" ,boost)
       ("googletest" ,googletest)
       ("fftw" ,fftw)
       ("opencl-headers" ,opencl-headers)
       ("pocl" ,pocl)))
    (inputs
     `(("ocl-icd" ,ocl-icd)))
    (home-page "https://github.com/clMathLibraries/clFFT")
    (synopsis "Software library containing FFT functions written in OpenCL")
    (description "clFFT is a software library containing FFT functions written
in OpenCL.  In addition to GPU devices, the library also supports running on
CPU devices to facilitate debugging and heterogeneous programming.")
    (license license:asl2.0)))
