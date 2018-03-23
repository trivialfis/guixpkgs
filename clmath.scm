(define-module (clmath)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  ;; #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  ;; #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  ;; #:use-module (gnu packages glib)
  ;; #:use-module (gnu packages image)
  ;; #:use-module (gnu packages video)
  ;; #:use-module (gnu packages textutils)
  ;; #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (opencl))

(define-public clBLAS
  (let ((version "2.12"))
    (package
     (name "clBLAS")
     (version version)
     (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://github.com/clMathLibraries/clBLAS/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
		"1acp8995wnkndpql3zxbldr4xmg66ahfn410fyb5wg540v5wfsbj"))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f
		 #:configure-flags
		 '(,(string-append "../clBLAS-" version "/src")
		   "-DBUILD_SHARED_LIBS=ON"
		   "-DCMAKE_BUILD_TYPE=Release"
		   "-DBUILD_TEST=OFF")))
     (native-inputs `(
		      ;; ("cmake" ,cmake)
		      ("gfortran" ,gfortran)
		      ("pkg-config" ,pkg-config)))
     (inputs `(("curl" ,curl)
	       ("boost" ,boost)
	       ("ocl-icd" ,ocl-icd)
	       ("opencl-headers" ,opencl-headers)
	       ("python" ,python)))
     (home-page "http://www.glfw.org/")
     (synopsis "glfw is an Open Source, multi-platform library for creating
 windows with OpenGL contexts and receiving input and events.")
     (description "glfw is an Open Source, multi-platform library for creating
 windows with OpenGL contexts and receiving input and events.")
     (license (list license:asl2.0)))))

(define-public clFFT
  (package
   (name "clFFT")
   (version "2.12.2")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://github.com/clMathLibraries/"
				name "/archive/v" version ".tar.gz"))
	    (sha256 (base32
		     "04kvi4gc5b5v03pzgvcrvysy6gmd08i71dwpd4z6m36ld8a8qd77"))))
   (build-system cmake-build-system)
   (arguments `(#:tests? #f		; FIXME: Can't find boost.
		#:configure-flags
		'(,(string-append "../clFFT-" version "/src")
		  "-DBUILD_SHARED_LIBS=ON"
		  "-DBUILD_TEST=ON"
		  "-DUSE_SYSTEM_GTEST=ON")))
   (native-inputs
    `(("googletest" ,googletest)
      ("fftw-avx" ,fftw-avx)))
   (inputs
    `(("opencl-headers" ,opencl-headers)
      ("boost" ,boost)
      ("ocl-icd" ,ocl-icd)))
   (home-page "https://github.com/clMathLibraries/clFFT")
   (synopsis "Software library containing FFT functions written in OpenCL ")
   (description "clFFT is a software library containing FFT functions written
in OpenCL. In addition to GPU devices, the library also supports running on
CPU devices to facilitate debugging and heterogeneous programming.")
   (license license:asl2.0)))
