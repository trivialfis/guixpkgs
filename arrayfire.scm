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
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control))

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


(define-public forge
  (package
    (name "forge")
    (version "1.0.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://github.com/arrayfire/forge/archive/v"
		    version "-ft.tar.gz"))
	      (sha256
	       (base32 "0m0bc75a6gdq84gs3sh069f7q0idmgh1ikc6znjjm2hxc35pz7wd"))))
    (native-inputs `(("cl2hpp-header" ,cl2hpp-header)
		     ("glm" ,glm)))
    (inputs `(("freeimage" ,freeimage)
	      ("freetype" ,freetype)
	      ("fontconfig" ,fontconfig)
	      ("glfw" ,glfw)
	      ("glu" ,glu)
	      ("boost" ,boost)
	      ("glbinding" ,glbinding)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DUSE_SYSTEM_CL2HPP=ON"
			   "-DUSE_SYSTEM_GLBINDING=ON"
			   "-DUSE_SYSTEM_FREETYPE=ON"
			   "-DUSE_SYSTEM_GLM=ON"
			   "-DBUILD_DOCUMENTATION=OFF"
			   "-DBUILD_EXAMPLES=OFF")
			 #:tests? #f))
    (home-page "https://github.com/arrayfire/forge")
    (synopsis "High Performance Visualization.")
    (description "An OpenGL interop library that can be used with ArrayFire or
any other application using CUDA or OpenCL compute backend. The goal of Forge
is to provide high performance OpenGL visualizations for C/C++ applications
that use CUDA/OpenCL. ")
    (license license:bsd-3)))


(define* (make-arrayfire cl? cuda? name #:optional (cpu? #t))
  (let* ((cpu-native `(,cpu? (("googletest" ,googletest)
			      ("pkg-config" ,pkg-config))))
	 (cpu-inputs `(,cpu? (("freeimage" ,freeimage)
			      ("openblas" ,openblas)
			      ("glbinding" ,glbinding)
			      ("forge" ,forge)
			      ("glew" ,glew)
			      ("fftw" ,fftw)
			      ("fftwf" ,fftwf)
			      ("fftw-avx" ,fftw-avx)
			      ("lapack" ,lapack)
			      ("boost" ,boost))))
	 (cl-native `(,cl?
		      (("beignet" ,beignet) ; Only used for testing.
		       ("boost-compute" ,boost-compute)
		       ("cl2hpp-header" ,cl2hpp-header)
		       ("opencl-headers@2.2" ,opencl-headers-2.2))))
	 (cl-inputs `(,cl?
		      (("clBLAS" ,clBLAS)
		       ("clFFT" ,clFFT)
		       ("ocl-icd" ,ocl-icd))))
	 (cuda-native `(,cuda? ()))
	 (cuda-inputs `(,cuda?
			(("cuda" ,cuda))))
	 (native-inputs `(,cpu-native ,cl-native ,cuda-native))
	 (inputs `(,cpu-inputs ,cl-inputs ,cuda-inputs))
	 (switchs (lambda (s) (if s "ON" "OFF")))
	 (flags `(list
		  (string-append "-DBUILD_CUDA=" (if ,cuda? "ON" "OFF"))
		  (string-append "-DBUILD_OPENCL=" (if ,cl? "ON" "OFF"))
		  "-DUSE_SYSTEM_CLBLAS=ON"
		  "-DUSE_SYSTEM_CLFFT=ON"
		  "-DUSE_SYSTEM_CL2HPP=ON"
		  "-DBUILD_GRAPHICS=OFF"
		  "-DUSE_SYSTEM_FORGE=ON"
		  "-DUSE_SYSTEM_GLBINDING=ON"
		  "-DUSE_SYSTEM_BOOST_COMPUTE=ON"
		  "-DBUILD_TEST=ON")))
    (define (make-inputs inputs-list)
      (if (equal? inputs-list '())
	  '()
	  (let ((first (car inputs-list))
		(rest (cdr inputs-list)))
	    (if (equal? (car first) #t)
		(append (cadr first) (make-inputs rest))
		(make-inputs rest)))))
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
       `(#:configure-flags ,flags))
      (home-page "http://arrayfire.com/")
      (synopsis "ArrayFire: a general purpose GPU library.")
      (description "ArrayFire is a general-purpose library that simplifies the
process of developing software that targets parallel and massively-parallel
architectures including CPUs, GPUs, and other hardware acceleration devices.")
      (license (list license:bsd-3)))))

(define-public arrayfire-cpu
  (make-arrayfire #f #f "arrayfire-cpu"))
(define-public arrayfire-cl
  (make-arrayfire #t #f "arrayfire-cl"))
(define-public arrayfire-cuda
  (make-arrayfire #f #t "arrayfire-cuda"))
(define-public arrayfire-full
  (make-arrayfire #t #t "arrayfire-full"))

(define-public glm
  (package
   (name "glm")
   (version "0.9.8.5")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://github.com/g-truc/glm/archive/"
				version
				".tar.gz"))
	    (sha256
	     (base32
	      "08691x1xmh0n18ff62wk080jf4blx3yi9sj5vzw08mbfy1c9kkw0"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags '("-DGLM_TEST_ENABLE=ON"
			  "-DGLM_TEST_ENABLE_CXX_11=ON"
			  "-DGLM_TEST_FORCE_PURE=ON")))
   (home-page "http://glm.g-truc.net")
   (synopsis "OpenGL Mathematics library")
   (description "OpenGL Mathematics (GLM) is a header-only C++ mathematics
library for graphics software based on the OpenGL Shading Language (GLSL)
specifications.")
   (license license:expat)))

(define-public arrayfire-minimum
  (package
    (name "arrayfire-minimum")
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://arrayfire.com/arrayfire_source/arrayfire-full-"
                    version ".tar.bz2"))
              (file-name (string-append name "-full-" version ".tar.bz2"))
              (sha256
               (base32
                "1w11kfw20nqhvw8fnrab6n4cs8a7az3fq7xygrnq4kcx4zy2zzxn"))))
    ;; (native-inputs `(("googletest" ,googletest)))
    (inputs `(("openblas" ,openblas)
              ("fftw" ,fftw)
              ("fftwf" ,fftwf)
              ("fftw-avx" ,fftw-avx)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_CUDA=OFF"
         "-DBUILD_CL=OFF"
         "-DBUILD_GRAPHICS=OFF"
         "-DBUILD_TEST=OFF")
       #:tests? #f))
    (home-page "http://arrayfire.com/")
    (synopsis "ArrayFire: a general purpose GPU library.")
    (description "ArrayFire is a general-purpose library that simplifies the
process of developing software that targets parallel and massively-parallel
architectures including CPUs, GPUs, and other hardware acceleration devices.")
    (license (list license:bsd-3))))
