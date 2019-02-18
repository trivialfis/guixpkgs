(define-module (llvm)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages llvm))

(define-public clang-runtime-7.0.1
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "http://releases.llvm.org/"
		    version "/compiler-rt-" version ".src.tar.xz"))
	      (sha256
	       (base32
		"065ybd8fsc4h2hikbdyricj6pyv4r7r7kpcikhb2y5zf370xybkq"))
	      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs
     (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f))
    (home-page "https://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license license:ncsa)
    ;; <https://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public clang-7.0.1
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin (method url-fetch)
	     (uri (string-append "http://releases.llvm.org/"
				 version "/cfe-" version ".src.tar.xz"))
	     (sha256
	      (base32
	       "067lwggnbg0w1dfrps790r5l6k8n5zwhlsw7zb6zvmfpwpfn4nx4"))
	     (patches (search-patches "clang-add-CUDA-path-params.patch"))
	     (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ("gcc" ,gcc)
       ("clang-extra-tools"
	,(origin
           (method url-fetch)
           (uri
	    (string-append "http://releases.llvm.org/"
			   version "/clang-tools-extra-"
			   version ".src.tar.xz"))
           (file-name (string-append "clang-extra-tools-" version ".tar.xz"))
           (sha256
            (base32
	     "1v9vc7id1761qm7mywlknsp810232iwyz8rd4y5km4h7pg9cg4sc"))))
       ("linux-libre-headers" ,linux-libre-headers)
       ,@(package-inputs llvm)))
    (propagated-inputs
     `(("llvm" ,llvm)
       ("clang-runtime" ,clang-runtime-7.0.1)))
    (arguments
     `(#:configure-flags
       (list
	"-DCLANG_INCLUDE_TESTS=True"
	"-DCLANG_DEFAULT_CXX_STDLIB=libstdc++"
	"-DCLANG_DEFAULT_RTLIB=libgcc"
        ;; Find libgcc_s, crtbegin.o, and crtend.o.
        (string-append
	 "-DGCC_INSTALL_PREFIX="
         (assoc-ref %build-inputs "gcc-lib"))
        ;; Use a sane default include directory.
        (string-append
	 "-DC_INCLUDE_DIRS="
         (assoc-ref %build-inputs "libc")
         "/include" ":"
	 (assoc-ref %build-inputs "gcc")
	 "/include/c++" ":"
	 (assoc-ref %build-inputs "gcc")
	 "/include/c++/x86_64-unknown-linux-gnu/" ":"
	 (assoc-ref %build-inputs "linux-libre-headers")
	 "/include" ":"
	 (assoc-ref %build-inputs "gcc-lib") ; openmp
	 "/lib/gcc/x86_64-unknown-linux-gnu/" ,(package-version gcc) "/include/"))
       ;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'unpack-extra-tools
	   (lambda* (#:key inputs #:allow-other-keys)
	     (let ((untar
		    (lambda (tarball output)
		      (with-directory-excursion output
			(invoke "tar" "-xvf" (assoc-ref inputs tarball))))))
	       (untar "clang-extra-tools" "tools/")
	       (with-directory-excursion "tools/"
		 (rename-file
		  (string-append "clang-tools-extra-" ,version ".src")
		  "extra"))
	       #t)))
         (add-after
	     'unpack 'set-glibc-file-names
           (lambda* (#:key inputs #:allow-other-keys)
	     (let ((compiler-rt (assoc-ref inputs "clang-runtime")))
	       (substitute* "lib/Driver/ToolChain.cpp"
                 (("getDriver\\(\\)\\.ResourceDir")
		  (string-append "\"" compiler-rt "\"")))
	       #t))))))
    ;; Clang supports the same environment variables as GCC.
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))
    (home-page "https://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (license license:ncsa)))
