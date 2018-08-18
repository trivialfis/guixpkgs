(define-module (llvm)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (guix utils))

;; Next version of LLVM/Clang, Clang is not building yet. Source is simply zip
;; from git repo.

(define-public llvm
  (let* ((commit "2c9adfa4a6145410f512350f39eb4d15c6e87240")
         (revision "0")
         (version (git-version "7.0" revision commit)))
    (package
      (name "llvm")
      (version version)
      (source "/home/fis/Others/git-repos/compilers/llvm.tar.gz")
      (build-system cmake-build-system)
      (native-inputs
       `(("python" ,python-2) ;bytes->str conversion in clang>=3.7 needs python-2
         ("perl"   ,perl)))
      (inputs
       `(("libffi" ,libffi)))
      (propagated-inputs
       `(("zlib" ,zlib)))                 ;to use output from llvm-config
      (arguments
       `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                             "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
                             "-DBUILD_SHARED_LIBS:BOOL=TRUE"
                             "-DLLVM_ENABLE_FFI:BOOL=TRUE"
                             "-DLLVM_INSTALL_UTILS=ON") ; Needed for rustc.

                           ;; Don't use '-g' during the build, to save space.
                           #:build-type "Release"
                           #:phases (modify-phases %standard-phases
                                      (add-before 'build 'shared-lib-workaround
                                        ;; Even with CMAKE_SKIP_BUILD_RPATH=FALSE, llvm-tblgen
                                        ;; doesn't seem to get the correct rpath to be able to run
                                        ;; from the build directory.  Set LD_LIBRARY_PATH as a
                                        ;; workaround.
                                        (lambda _
                                          (setenv "LD_LIBRARY_PATH"
                                                  (string-append (getcwd) "/lib"))
                                          #t)))))
      (home-page "https://www.llvm.org")
      (synopsis "Optimizing compiler infrastructure")
      (description
       "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
      (license license:ncsa))))

(define-public clang-runtime
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source "/home/fis/Others/git-repos/compilers/compiler-rt.tar.gz")
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f))                    ; Tests require gtest
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

(define-public clang
  ;; Not building yet.
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     "/home/fis/Others/git-repos/compilers/clang.tar.gz")
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ,@(package-inputs llvm)))
    (propagated-inputs
     `(("llvm" ,llvm)
       ("clang-runtime" ,clang-runtime)))
    (arguments
     `(#:configure-flags
       (list "-DCLANG_INCLUDE_TESTS=True"

             ;; Find libgcc_s, crtbegin.o, and crtend.o.
             (string-append "-DGCC_INSTALL_PREFIX="
                            (assoc-ref %build-inputs "gcc-lib"))

             ;; Use a sane default include directory.
             (string-append "-DC_INCLUDE_DIRS="
                            (assoc-ref %build-inputs "libc")
                            "/include"))

       ;; Don't use '-g' during the build to save space.
       #:build-type "Release"

       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'set-glibc-file-names
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((libc (assoc-ref inputs "libc"))
                           (compiler-rt (assoc-ref inputs "clang-runtime")))
                       (case (string->number ,(version-major
                                               (package-version clang-runtime)))
                         ((7)
                          (substitute* "lib/Driver/ToolChain.cpp"
                            (("getDriver\\(\\)\\.ResourceDir")
                             (string-append "\"" compiler-rt "\""))))
                         ((6)
                          ;; Link to libclang_rt files from clang-runtime.
                          (substitute* "lib/Driver/ToolChain.cpp"
                            (("getDriver\\(\\)\\.ResourceDir")
                             (string-append "\"" compiler-rt "\"")))

                          ;; Make "LibDir" refer to <glibc>/lib so that it
                          ;; uses the right dynamic linker file name.
                          (substitute* "lib/Driver/ToolChains/Linux.cpp"
                            (("(^[[:blank:]]+LibDir = ).*" _ declaration)
                             (string-append declaration "\"" libc "/lib\";\n"))

                            ;; Make sure libc's libdir is on the search path, to
                            ;; allow crt1.o & co. to be found.
                            (("@GLIBC_LIBDIR@")
                             (string-append libc "/lib"))))
                         ((3)
                          (substitute* "lib/Driver/Tools.cpp"
                            ;; Patch the 'getLinuxDynamicLinker' function so that
                            ;; it uses the right dynamic linker file name.
                            (("/lib64/ld-linux-x86-64.so.2")
                             (string-append libc
                                            ,(glibc-dynamic-linker))))

                          ;; Link to libclang_rt files from clang-runtime.
                          ;; This substitution needed slight adjustment in 3.8.
                          (if (< 3.8 (string->number ,(version-major+minor
                                                       (package-version
                                                        clang-runtime))))
                              (substitute* "lib/Driver/Tools.cpp"
                                (("TC\\.getDriver\\(\\)\\.ResourceDir")
                                 (string-append "\"" compiler-rt "\"")))
                              (substitute* "lib/Driver/ToolChain.cpp"
                                (("getDriver\\(\\)\\.ResourceDir")
                                 (string-append "\"" compiler-rt "\""))))

                          ;; Make sure libc's libdir is on the search path, to
                          ;; allow crt1.o & co. to be found.
                          (substitute* "lib/Driver/ToolChains.cpp"
                            (("@GLIBC_LIBDIR@")
                             (string-append libc "/lib")))))
                       #t)))
                  (add-before 'build 'set-env
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((llvm-lib (string-append (assoc-ref inputs "llvm") "/lib")))
                        (setenv "LD_LIBRARY_PATH" llvm-lib)
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
