;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
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

(define-module (arrayfire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (boost-compute)
  #:use-module (clmath)
  #:use-module (cuda)
  #:use-module (opencl)
  ;; #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control))

(define* (make-arrayfire cl? cuda? graphics? name #:optional (cpu? #t))
  (let* ((cpu-native `(,cpu? (("googletest" ,googletest)
			      ("pkg-config" ,pkg-config))))
	 (cpu-inputs `(,cpu? (("freeimage" ,freeimage)
			      ("openblas" ,openblas)
			      ("fftw" ,fftw)
			      ("fftwf" ,fftwf)
			      ("fftw-avx" ,fftw-avx)
			      ("lapack" ,lapack)
			      ("boost" ,boost))))
	 (cl-native `(,cl?
		      (("opencl-headers" ,opencl-headers)
		       ("cl2hpp-header" ,cl2hpp-header)
		       ("boost-compute" ,boost-compute))))
	 (cl-inputs `(,cl?
		      (("clBLAS" ,clBLAS)
		       ("clFFT" ,clFFT)
		       ("ocl-icd" ,ocl-icd))))
	 (cuda-native `(,cuda? ()))
	 (cuda-inputs `(,cuda?
			(("cuda" ,cuda))))
	 (native-inputs `(,cpu-native ,cl-native ,cuda-native))
	 (inputs `(,cpu-inputs ,cl-inputs ,cuda-inputs))

	 ;; (switchs (lambda (s) (if s "ON" "OFF")))
	 )
    (define (make-inputs inputs-list)
      (if (equal? inputs-list '())
	  '()
	  (let ((first (car inputs-list))
		(rest (cdr inputs-list)))
	    (if (equal? (car first) #t)
		(append (cadr first) (make-inputs rest))
		(make-inputs rest)))))
    (define (switchs s)
      (if s "ON" "OFF"))
    (define (make-flags)
      `((string-append "-DBUILD_CUDA=" ,(switchs cuda?))
	(string-append "-DBUILD_CL=" ,(switchs cl?))
	(if ,cl? "-DUSE_SYSTEM_CLBLAS=ON" "")
	(if ,cl? "-DUSE_SYSTEM_CLFFT=OFF" "")
	(if ,cl? "-DUSE_SYSTEM_CL2HPP=ON" "")
	(string-append "-DBUILD_GRAPHICS" ,(switchs graphics?))
	"-DUSE_SYSTEM_BOOST_COMPUTE=ON"
	"-DBUILD_TEST=ON"))
    (package
     (name name)
     (version "3.5.1")
     (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "http://arrayfire.com/arrayfire_source/arrayfire-full-"
		    version ".tar.bz2"))
	      (file-name (string-append name "-full-" version ".tar.bz2"))
	      ;; (patches (search-patches
	      ;; 	      "Restore-USE_SYSTEM_CLBLAS-and-USE_SYSTEM_CLFFT.patch"))
	      (sha256
	       (base32
		"1w11kfw20nqhvw8fnrab6n4cs8a7az3fq7xygrnq4kcx4zy2zzxn"))))
     (native-inputs (make-inputs native-inputs))
     (inputs (make-inputs inputs))
     (build-system cmake-build-system)
     (arguments
      ;; For master branch, use: AF_BUILD_CL, AF_WITH_GRAPHICS
      ;; USE_SYSTEM_* need to be patched back.
      ;; Actually, arrayfire will disable the corresponding backends if
      ;; dependency is missing.
      `(#:configure-flags `(,(if ,cuda? "-DBUILD_CUDA=ON" "-DBUILD_CUDA=OFF")
			    ,(if ,cl? "-DBUILD_CL=ON" "-DBUILD_CL=OFF")
			    "-DUSE_SYSTEM_CLBLAS=ON"
			    "-DUSE_SYSTEM_CLFFT=ON"
			    "-DUSE_SYSTEM_CL2HPP=ON"
			    "-DBUILD_GRAPHICS=OFF"
			    "-DUSE_SYSTEM_BOOST_COMPUTE=ON"
			    "-DBUILD_TEST=ON")))
     (home-page "http://arrayfire.com/")
     (synopsis "ArrayFire: a general purpose GPU library.")
     (description "ArrayFire is a general-purpose library that simplifies the
process of developing software that targets parallel and massively-parallel
architectures including CPUs, GPUs, and other hardware acceleration devices.")
     (license (list license:bsd-3)))))

(define-public arrayfire-cpu
  (make-arrayfire #f #f #f "arrayfire-cpu"))
(define-public arrayfire-cl
  (make-arrayfire #t #f #f "arrayfire-cl"))
(define-public arrayfire-cuda
  (make-arrayfire #f #t #f "arrayfire-cuda"))
(define-public arrayfire-full
  (make-arrayfire #t #t #f "arrayfire-full"))

(define-public glbinding
  (package
   (name "glbinding")
   (version "2.1.4")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://github.com/cginternals/"
				name "/archive/v" version ".tar.gz"))
	    (sha256
	     (base32 "1qlbixc26z0dsj0c4h8dxq98qazx0f43ccad62r1gln0hsq72nfb"))))
   (build-system cmake-build-system)
   (inputs `(("glfw" ,glfw)
	     ("python" ,python)))
   (home-page "http://www.glbinding.org")
   (synopsis "A C++ binding for the OpenGL API, generated using the gl.xml specification")
   (description "Glbinding leverages modern C++11 features like enum classes, lambdas, and variadic templates, instead of relying on macros.")
   (license license:non-copyleft)))

;; FIXME: Dead code, to be remove. 
(define-public arrayfire
  (package
   (name "arrayfire")
   (version "3.5.1")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "http://arrayfire.com/arrayfire_source/arrayfire-full-"
		  version ".tar.bz2"))
	    (file-name (string-append name "-full-" version ".tar.bz2"))
	    ;; (patches (search-patches
	    ;; 	      "Restore-USE_SYSTEM_CLBLAS-and-USE_SYSTEM_CLFFT.patch"))
	    (sha256
	     (base32
	      "1w11kfw20nqhvw8fnrab6n4cs8a7az3fq7xygrnq4kcx4zy2zzxn"))))
   (native-inputs `(("boost-compute" ,boost-compute)
		    ("cl2hpp-header" ,cl2hpp-header)
		    ("googletest" ,googletest)
		    ("ocl-icd" ,ocl-icd)
		    ("git" ,git)
		    ("opencl-headers" ,opencl-headers)
		    ("pkg-config" ,pkg-config)))
   (inputs `(("clBLAS" ,clBLAS)
	     ("clFFT" ,clFFT)
	     ;; ("cuda" ,cuda)
	     ;; ("glew" ,glew)
	     ;; ("glfw" ,glfw)
	     ;; ("glu" ,glu)
	     ;; ("glbinding" ,glbinding)
	     ("freeimage" ,freeimage)
	     ("lapack" ,lapack)
	     ("openblas" ,openblas)
	     ("fftw" ,fftw)
	     ("fftwf" ,fftwf)
	     ("fftw-avx" ,fftw-avx)
	     ("boost" ,boost)))
   (build-system cmake-build-system)
   (arguments
    ;; For master branch, use: AF_BUILD_CL, AF_WITH_GRAPHICS
    ;; USE_SYSTEM_* need to be patched back.
    `(#:configure-flags '("-DBUILD_CUDA=OFF"
			  "-DBUILD_CL=ON"
			  "-DBUILD_GRAPHICS=OFF" ; FIXME
			  "-DUSE_SYSTEM_BOOST_COMPUTE=ON"
			  "-DUSE_SYSTEM_CL2HPP=ON"
			  "-DUSE_SYSTEM_CLBLAS=ON"
			  "-DUSE_SYSTEM_CLFFT=ON"
			  "-DBUILD_TEST=OFF")
			;; #:phases (modify-phases %standard-phases
			;; 		 (replace 'check
			;; 		   (lambda _
			;; 		     (invoke "ctest" "-R" "Test_.*_cpu")
			;; 		     (invoke "ctest" "-R" "Test_.*_opencl")
			;; 		     ;; (invoke "ctest" "-R" "Test_.*_unified")
			;; 		     )))
			#:tests? #f))
   (home-page "http://arrayfire.com/")
   (synopsis "ArrayFire: a general purpose GPU library.")
   (description "ArrayFire is a general-purpose library that simplifies the
process of developing software that targets parallel and massively-parallel
architectures including CPUs, GPUs, and other hardware acceleration devices.")
   (license (list license:bsd-3))))
