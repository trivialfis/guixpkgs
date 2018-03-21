(define-module (clmath)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages python))

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
     (native-inputs `(("cmake" ,cmake)
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

(define-public ocl-icd
  (package
   (name "ocl-icd")
   (version "2.2.9")
   (source (origin
            (method url-fetch)
            (uri (string-append
		  "https://forge.imag.fr/frs/download.php/716/ocl-icd-"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
	      "1rgaixwnxmrq2aq4kcdvs0yx7i6krakarya9vqs7qwsv5hzc32hc"))))
   (inputs `(("zip" ,zip)
             ("autoconf" ,autoconf)
             ("automake" ,automake)
             ("ruby" ,ruby)
             ("libtool" ,libtool)
             ("opencl-headers" ,opencl-headers)
             ("libgcrypt" ,libgcrypt)))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
		     (add-after 'unpack `bootstrap
				(lambda _
				  (zero? (system* "autoreconf" "-vfi")))))))
   (home-page "https://forge.imag.fr/projects/ocl-icd/")
   (synopsis "OpenCL implementations are provided as ICD (Installable Client
 Driver).")
   (description "OpenCL implementations are provided as ICD (Installable Client
 Driver). An OpenCL program can use several ICD thanks to the use of an ICD
Loader as provided by this project. This free ICD Loader can load any (free or
non free) ICD")
   (license (list license:gpl2 license:ruby))))

(define-public opencl-headers
  (let ((commit "c1770dc"))
    (package
     (name "opencl-headers")
     (version (string-append "2.1-" commit ))
     (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/KhronosGroup/OpenCL-Headers.git")
		    (commit commit)))
	      (file-name (string-append name "-" commit))
	      (sha256
	       (base32
                "0m9fkblqja0686i2jjqiszvq3df95gp01a2674xknlmkd6525rck"))))
     (propagated-inputs '())
     (inputs '())
     (native-inputs '())
     (build-system gnu-build-system)
     (arguments
      '(#:phases
	(modify-phases
	 %standard-phases
	 (delete 'configure)
	 (delete 'build)
	 (delete 'check)
	 (replace 'install
		  (lambda* (#:key outputs #:allow-other-keys)
		    (copy-recursively "." (string-append
					   (assoc-ref outputs "out")
					   "/include/CL")))))))
     (synopsis "The Khronos OpenCL headers")
     (description "This package provides the Khronos OpenCL headers")
     (home-page "https://www.khronos.org/registry/cl/")
     (license (list license:gpl2)))))
