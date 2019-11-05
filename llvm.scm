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
  #:use-module (gnu packages commencement) ; gcc-toolchain
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages llvm))

(define-public llvm-8.0.1
  (package
    (name "llvm-t")
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
	     version "/llvm-" version ".src.tar.xz"))
       (sha256
	(base32
	 "1rvm5gqp5v8hfn17kqws3zhk94w4kxndal12bqa0y57p09nply24"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("perl"   ,perl)
       ("python", python-wrapper)
       ("python-psutil" ,python-psutil))) ; for llvm-lit
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs
     `(("zlib" ,zlib)))                 ;to use output from llvm-config
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
	     "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
	     "-DBUILD_SHARED_LIBS:BOOL=TRUE"
	     "-DLLVM_ENABLE_FFI:BOOL=TRUE"
	     "-DLLVM_REQUIRES_RTTI=1"  ; Needed for rustc.
	     "-DLLVM_INSTALL_UTILS=ON" ; For some third-party utilities
	     )
       ;; Don't use '-g' during the build, to save space.
       #:build-type "Release"
       #:test-target "check-all"
       #:phases
       (modify-phases %standard-phases
	 (add-before 'check 'set-HOME
	   (lambda _
	     (setenv "HOME" "/tmp")
	     #t)))))
    (home-page "https://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.")
    (license license:ncsa)))

(define-public clang-runtime-8.0.1
  (package
    (name "clang-runtime-t")
    (version (package-version llvm-8.0.1))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
	     version "/compiler-rt-" version ".src.tar.xz"))
       (sha256
	(base32
	 "0dqqf8f930l8gag4d9qjgn1n0pj0nbv2anviqqhdi1rkhas8z0hi"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(,@(package-native-inputs llvm-8.0.1)))
    (inputs
     `(("llvm-t" ,llvm-8.0.1)))
    (arguments
     `(#:build-type "Release" ;; Don't use '-g' during the build to save space.
       #:tests? #f)) ; FIXME: testingsupport library not installed, some tests will be skipped
    (home-page "https://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license license:ncsa)))

(define-public clang-8.0.1
  (package
    (name "clang-t")
    (version (package-version llvm-8.0.1))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
	     version "/cfe-"version".src.tar.xz"))
       (sha256
	(base32
	 "0ihnbdl058gvl2wdy45p5am55bq8ifx8m9mhcsgj9ax8yxlzvvvh"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(,@(package-native-inputs llvm-8.0.1)))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ("clang-extra-tools"
	,(origin
           (method url-fetch)
           (uri
	    (string-append
	     "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
	     version "/clang-tools-extra-" version ".src.tar.xz"))
           (file-name (string-append "clang-extra-tools-" version ".tar.xz"))
           (sha256
            (base32
	     "1qf3097bc5ia8p6cpmbx985rjr3yaah5s8fc0nv7pw742yv7jw8q"))))
       ("linux-libre-headers" ,linux-libre-headers)
       ,@(package-inputs llvm-8.0.1)))
    (propagated-inputs
     `(("llvm-t" ,llvm-8.0.1)
       ("clang-runtime-t" ,clang-runtime-8.0.1)))
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
	     (let ((compiler-rt (assoc-ref inputs "clang-runtime-t")))
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

(define-public lld-8.0.1
  (package
   (name "lld-t")
   (version (package-version llvm-8.0.1))
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
       version "/lld-" version ".src.tar.xz"))
     (sha256
      (base32
       "121xhxrlvwy3k5nf6p1wv31whxlb635ssfkci8z93mwv4ja1xflz"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (arguments
     `(#:tests? #f))
   (inputs `(("llvm-t" ,llvm-8.0.1)))
   (build-system cmake-build-system)
   (home-page "https://lld.llvm.org/")
   (synopsis "The LLVM Linker.")
   (description "LLD is a linker from the LLVM project that is a
drop-in replacement for system linkers and runs much faster than
them. It also provides features that are useful for toolchain
developers.")
   (license license:ncsa)))

(define-public libcxx-8.0.1
  (package
   (name "libcxx-t")
   (version (package-version llvm-8.0.1))
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
       version "/libcxx-" version ".src.tar.xz"))
     (sha256
      (base32
       "0y4vc9z36c1zlq15cnibdzxnc1xi5glbc6klnm8a41q3db4541kz"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (native-inputs
    `(("clang-t" ,clang-8.0.1)
      ("llvm-t" ,llvm-8.0.1)))
   (build-system cmake-build-system)
   (home-page "https://lld.llvm.org/")
   (synopsis "Implementation of the C++ standard library.")
   (description "Implementation of the C++ standard library.")
   (license license:ncsa)))

(define-public openmp
  (package
   (name "openmp-t")
   (version (package-version llvm-8.0.1))
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append "https://github.com/llvm/llvm-project/releases/download/llvmorg-"
		     version "/openmp-" version ".src.tar.xz"))
     (sha256
      (base32
       "0b3jlxhqbpyd1nqkpxjfggm5d9va5qpyf7d4i5y7n4a1mlydv19y"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (native-inputs
    `(("llvm-t" ,llvm-8.0.1)
      ("perl"   ,perl)))
   (arguments
     `(#:tests? #f))
   (build-system cmake-build-system)
   (home-page "https://openmp.llvm.org/")
   (synopsis "")
   (description "")
   (license license:ncsa)))

(define-public llvm-project
  (package
   (name "llvm-project")
   (version "9.0.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/llvm/llvm-project/archive/llvmorg-"  version ".tar.gz"))
     (sha256
      (base32
       "1win216na1k26qi87fpxqjfbqjykbn2wsinaanclxqihag1gl1vq"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (build-system cmake-build-system)
   (arguments
     `(#:configure-flags
       (list "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
	     "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
	     "-DBUILD_SHARED_LIBS:BOOL=TRUE"
	     "-DLLVM_ENABLE_FFI:BOOL=TRUE"
	     "-DLLVM_REQUIRES_RTTI=1"  ; Needed for rustc.
	     "-DLLVM_INSTALL_UTILS=ON" ; For some third-party utilities
	     "-DLLVM_ENABLE_PROJECTS=clang;lldb;compiler-rt;libcxx;libcxxabi;openmp;polly"
	     )
       ;; Don't use '-g' during the build, to save space.
       #:test-target "check-all"
       #:phases
       (modify-phases %standard-phases
	 (add-before 'configure 'chwdir
	     (lambda -
	       (chdir "llvm")))
	 (add-before 'check 'set-HOME
	   (lambda _
	     (setenv "HOME" "/tmp")
	     #t)))))
   (native-inputs
     `(("perl"   ,perl)
       ("python", python-wrapper)
       ("python-psutil" ,python-psutil))) ; for llvm-lit
    (inputs
     `(("libffi" ,libffi)
       ("swig" ,swig)
       ("linux-libre-headers" ,linux-libre-headers) ; clang
       ("libxml2" ,libxml2)			    ; clang
       ("ncurses" ,ncurses)			    ; lldb
       ("libedit" ,libedit)))			    ; lldb
    (propagated-inputs
     `(("zlib" ,zlib)))
   (home-page "http://llvm.org/")
   (synopsis "Collection of modular and reusable compiler and toolchain technologies")
   (description "Collection of modular and reusable compiler and toolchain technologies")
   (license license:ncsa)))
