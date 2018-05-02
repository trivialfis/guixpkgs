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

(define-module (opencl)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages video)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg))


(define (make-opencl-headers major-version subversion)
  (let ((commit "e986688daf750633898dfd3994e14a9e618f2aa5")
        (revision "0"))
    (package
      (name "opencl-headers")
      (version (git-version
                (string-append major-version "." subversion ".0")
                revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/KhronosGroup/OpenCL-Headers.git")
                      (commit commit)))
                (file-name (string-append name "-" commit))
                (sha256
                 (base32
                  "176ydpbyws5nr4av6hf8p41pkhc0rc4m4vrah9w6gp2fw2i32838"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (delete 'check)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively (string-append "./opencl" (string-append
                                                            ,major-version
                                                            ,subversion) "/CL")
                                 (string-append
                                  (assoc-ref outputs "out")
                                  "/include/CL")))))))
      (synopsis "The Khronos OpenCL headers")
      (description "This package provides the Khronos OpenCL c headers.")
      (home-page "https://www.khronos.org/registry/cl/")
      (license license:expat))))

(define-public opencl-headers-2.2
  (make-opencl-headers "2" "2"))
(define-public opencl-headers-2.1
  (make-opencl-headers "2" "1"))
(define-public opencl-headers-2.0
  (make-opencl-headers "2" "0"))
(define-public opencl-headers-1.2
  (make-opencl-headers "1" "2"))
(define-public opencl-headers-1.1
  (make-opencl-headers "1" "1"))
(define-public opencl-headers-1.0
  (make-opencl-headers "1" "0"))

(define-public opencl-headers opencl-headers-2.2)

(define-public opencl-clhpp
  (package
    (name "opencl-clhpp")
    (version "2.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/KhronosGroup/OpenCL-CLHPP/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0awg6yznbz3h285kmnd47fykx2qa34a07sr4x1657yn3jmi4a9zs"))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     `(("opencl-headers" ,opencl-headers)))
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          "-DBUILD_EXAMPLES=OFF"
          "-DBUILD_TESTS=OFF"
          (string-append "-DCMAKE_INSTALL_PREFIX="
                         (assoc-ref %outputs "out")
                         "/include")))
       ;; regression tests requires a lot more dependencies
       #:tests? #f))
    (build-system cmake-build-system)
    (home-page "http://github.khronos.org/OpenCL-CLHPP/")
    (synopsis "Khronos OpenCL-CLHPP")
    (description "OpenCL Host API C++ bindings cl2.hpp.")
    (license license:expat)))

(define-public clinfo
  (package
    (name "clinfo")
    (version "2.2.18.04.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Oblomov/clinfo/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7cy01irwdgns6lzaprkmm0502pp5a24zhhffydxz1sgfjj2w7p"))))
    (build-system gnu-build-system)
    (native-inputs `(("opencl-headers" ,opencl-headers)))
    (inputs
     `(("ocl-icd" ,ocl-icd)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((cores (number->string (parallel-job-count))))
               (setenv "CC" "gcc")
               (invoke "make" "-j" cores))))
         (delete 'install)
         (add-after 'build 'make-install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install" (string-append
                                       "PREFIX="
                                       (assoc-ref outputs "out")))))
         (add-after 'make-install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/clinfo")))
               (wrap-program bin
                 `("OPENCL_VENDOR_PATH" ":" prefix
                   ("$GUIX_PROFILE$GUIX_ENVIRONMENT/etc/OpenCL/vendors")))))))
       ;; Cannot be run in store environment.
       #:tests? #f))
    (home-page "https://github.com/Oblomov/clinfo")
    (synopsis "Print all known information about all available OpenCL platforms
and devices in the system")
    (description "clinfo is a simple command-line application that enumerates
all possible (known) properties of the OpenCL platform and devices available on
the system.")
    (license license:non-copyleft)))

(define-public ocl-icd
  (package
    (name "ocl-icd")
    (version "2.2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://forge.imag.fr/frs/download.php/836/ocl-icd-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x2dr8p4dkfds56r38av360i3nv1y3326jmshxvjngaf6mlg6rbn"))
              ;; The patch is used to get all debug message, need to disable
              ;; tests.
              ;; (patches (search-patches "ocl-icd-Full-debug.patch"))
              ))
    (inputs `(("ruby" ,ruby)
              ("opencl-headers" ,opencl-headers)
              ("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    ;; FIXME: enable database
    (arguments
     '(#:configure-flags
       '(
         ;; "--enable-update-database"
         "DEBUG_OCL_ICD=1")))
    (native-search-paths
     (list (search-path-specification
            (variable "OPENCL_VENDOR_PATH")
            (files '("etc/OpenCL/vendors")))))
    (search-paths native-search-paths)
    (home-page "https://forge.imag.fr/projects/ocl-icd/")
    (synopsis "OpenCL implementations are provided as ICD (Installable Client
 Driver).")
    (description "OpenCL implementations are provided as ICD (Installable Client
 Driver). An OpenCL program can use several ICD thanks to the use of an ICD
Loader as provided by this project. This free ICD Loader can load any (free or
non free) ICD")
    (license (list license:gpl2 license:ruby))))


(define-public beignet
  ;; Beignet failed to recognize device at tests, which means all tests
  ;; failed.
  (package
    (name "beignet")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/intel/beignet/archive/Release_v"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18r0lq3dkd4yn6bxa45s2lrr9cjbg70nr2nn6xablvgqwzw0jb0r"))
              (patches (search-patches "beignet-correct-paths.patch"))))
    (native-inputs `(("pkg-config" ,pkg-config)
		     ("python" ,python)))
    (inputs `(("clang@3.7" ,clang-3.7)
	      ("clang-runtime@3.7" ,clang-runtime-3.7)
	      ("glu" ,glu)
	      ("llvm@3.7" ,llvm-3.7)
              ("libdrm" ,libdrm)
	      ("libedit" ,libedit)
	      ("libpthread-stubs", libpthread-stubs)
              ("libsm" ,libsm)
	      ("libva" ,libva)
              ("libxfixes" ,libxfixes)
              ("libxext" ,libxext)
              ("mesa-utils" ,mesa-utils)
              ("ncurses" ,ncurses)
              ("ocl-icd" ,ocl-icd)
	      ("opencl-headers" ,opencl-headers)
	      ("xextproto" ,xextproto)
	      ("zlib" ,zlib)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLANG_LIBRARY_DIR="
                            (assoc-ref %build-inputs "clang@3.7") "/lib")
             "-DENABLE_GL_SHARING=ON"
             "-DEXPERIMENTAL_DOUBLE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively
                (string-append out "/include")))))
	 (add-after 'remove-headers 'install-kernels
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (builddir (getcwd))
		    (source-dir (string-append
				 builddir
				 "/../beignet-Release_v1.3.2/kernels")))
	       (copy-recursively source-dir (string-append
					     out
					     "/lib/beignet/kernels"))))))
       #:tests? #f))
    (home-page "https://wiki.freedesktop.org/www/Software/Beignet/")
    (synopsis "Intel's OpenCL framework")
    (description "Intel's OpenCL framework that works with Intel IvyBridge GPUs
and above.")
    (license license:gpl2)))

(define-public pocl
  ;; pocl tests failed at beginning.
  ;; ocl-icd loads libpocl.so correctly, I don't know why the tests fail.
  (package
    (name "pocl")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pocl/pocl/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lrw3hlb0w53xzmrf2hvbda406l70ar4gyadflvlkj4879lx138y"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libltdl" ,libltdl)))
    (inputs
     `(("llvm" ,llvm)
       ("hwloc" ,hwloc "lib")
       ("clang" ,clang)
       ("ocl-icd" ,ocl-icd)))
    (arguments
     `(#:configure-flags
       '("-DENABLE_ICD=ON"
         "-DENABLE_TESTSUITES=OFF"
         "-DENABLE_CONFORMANCE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively
                (string-append out "/include"))))))
       #:tests? #f))                    ; failed
    (home-page "http://portablecl.org/")
    (synopsis "Portable Computing Language (pocl)")
    (description "pocl is being developed towards an efficient implementation
of OpenCL standard which can be easily adapted for new targets.")
    (license license:non-copyleft)))

(define (make-opencl-cts spec-version revision commit impl-name impl)
  ;; Doesn't work yet, might never work.
  (let* ((commit commit)
	 (revision "0")
	 (version (git-version spec-version revision commit)))
    (package
      (name (string-append "opencl-cts-" impl-name))
      (version version)
      (source (origin
		(method git-fetch)
		(uri (git-reference
                      (url "https://github.com/KhronosGroup/OpenCL-CTS.git")
                      (commit commit)))
		(sha256
		 (base32
		  "0fcc2g9vp9nfsm712b4gbkk0hhr96lk5yqvm0a5bmvz25qwzyf86"))
		(file-name (string-append name "-" commit))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
	 (list
	  (string-append "-DCL_OFFLINE_COMPILER="
			 (assoc-ref %build-inputs ,impl-name)
			 "/bin/poclcc"))))
      (native-inputs
       `((,impl-name ,impl)
	 ("opencl-headers" ,opencl-headers)))
      (home-page "https://github.com/KhronosGroup/OpenCL-CTS/")
      (synopsis "The OpenCL Conformance Tests")
      (description "The OpenCL Conformance Tests.")
      (license license:asl2.0))))

(define-public pocl-cts-2.2
  (make-opencl-cts "2.2.0" "0" "4a6af23ff362cd95477abada53d85a948d394069"
		   "pocl" pocl))
(define-public pocl-cts-2.1
  (make-opencl-cts "2.1.0" "0" "71a5c8251e1210bc9afda116353f212d33841910"
		   "pocl" pocl))
(define-public pocl-cts-2.0
  (make-opencl-cts "2.0.0" "0" "5b19ef73d98e98b62f0afdc009fcdf5ea9482ea7"
		   "pocl" pocl))
(define-public pocl-cts-1.2
  (make-opencl-cts "1.2.0" "0" "5413bcf52e3c8f5d51da657ce9169e754a2414ba"
		   "pocl" pocl))
(define-public beignet-cts-1.2
  (make-opencl-cts "1.2.0" "0" "5413bcf52e3c8f5d51da657ce9169e754a2414ba"
		   "beignet" beignet))


(define-public gmmlib
  (let* ((commit "b32d2124aa5187b20b64df24d2e83bcbe7a57d7d")
         (revision "1")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "gmmlib")
      (version version)
      (home-page "https://github.com/intel/gmmlib")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "0d6w7bfp1my3jb8m5wa8ighjr8msq993m0flhfb0d34sackyn7s6"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'install))           ; No such a phase
         #:tests? #f))                  ; Run automatically.
      (native-inputs `(("googletest" ,googletest)))
      (synopsis "Device specific buffer management for Intel(R) Graphics
Compute Runtime")
      (description "The Intel(R) Graphics Memory Management Library provides
device specific and buffer management for the Intel(R) Graphics Compute Runtime
for OpenCL(TM) and the Intel(R) Media Driver for VAAPI.")
      (license license:non-copyleft))))

(define-public beignet-tests
  ;; Just extracted from beignet, complete garbage, not working.

  ;; Some how the unit tests compiled in beignet dependents on extra inputs
  ;; listed here by having them in rpath, but none of them is supplied during
  ;; build. I don't know why does it happen, so I just make the tests a
  ;; seperate package.
  (package/inherit
   beignet
   (name "beignet-tests")
   (native-inputs `(("patchelf" ,patchelf)
		    ,@(package-native-inputs beignet)))
   (inputs `(("beignet" ,beignet)
	     ("gcc:lib" ,gcc "lib")
	     ("libice" ,libice)
	     ("libx11" ,libx11)
	     ("mesa" ,mesa)
	     ,@(package-inputs beignet)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'install 'install-tests
          (lambda* (#:key inputs outputs #:allow-other-keys)

            (let* ((builddir (getcwd))
                   (testdir (string-append builddir "/utests"))
                   (beignet-src (string-append builddir "/.."))

                   (cores (number->string (parallel-job-count)))

                   (out (assoc-ref outputs "out"))
                   (utests (assoc-ref outputs "tests"))
		   ;; (beignet-out (assoc-ref (package-outputs beignet) "out"))

                   (outlib (string-append out "/lib/beignet"))
                   (testlib (string-append utests "/lib"))

                   (libc (assoc-ref inputs "libc"))
                   (gcc:lib (assoc-ref inputs "gcc:lib"))
                   (zlib (assoc-ref inputs "zlib"))
                   (libxfixes (assoc-ref inputs "libxfixes"))
                   (libsm (assoc-ref inputs "libsm"))
                   (libxext (assoc-ref inputs "libxext"))
                   (xextproto (assoc-ref inputs "xextproto"))
                   (mesa-utils (assoc-ref inputs "mesa-utils"))
                   (mesa (assoc-ref inputs "mesa"))
                   (glu (assoc-ref inputs "glu"))
                   (libdrm (assoc-ref inputs "libdrm"))
                   (libx11 (assoc-ref inputs "libx11"))
                   (libice (assoc-ref inputs "libice"))
                   (ld-so (string-append libc ,(glibc-dynamic-linker)))

                   (libocl-path "/backend/src/libocl")
                   (setenv-with-tests (lambda (name value)
                                        (format #t "Name: ~s\nValue: ~s\n"
                                                name value)
                                        (if (not (file-exists? value))
                                            (error name)
                                            (setenv name value)))))

              (invoke "make" (string-append "-j" cores) "utest")

              (format #t "builddir: ~s\n" builddir)
              (format #t "testdir: ~s\n" testdir)
              (invoke "ls" "-l")

              (let ((source-dir (string-append
                                 builddir
                                 "/../beignet-Release_v1.3.2/kernels")))
                (copy-recursively source-dir (string-append
                                              out
                                              "/lib/beignet/kernels")))

              (install-file (string-append testdir "/libutests.so")
                            (string-append utests "/lib"))

              (mkdir (string-append utests "/bin"))
              (copy-file (string-append testdir "/utest_run")
                         (string-append utests "/bin/utest_run"))
              (copy-file (string-append testdir "/flat_address_space")
                         (string-append utests "/bin/flat_address_space"))

              (let ((rpath
                     (string-append outlib ":"
                                    testlib ":"
                                    (string-append libc "/lib:")
                                    (string-append gcc:lib "/lib:")
                                    (string-append libxfixes "/lib:")
                                    (string-append libdrm "/lib:")
                                    (string-append libxext "/lib:")
                                    (string-append libsm "/lib:")
                                    (string-append xextproto "/lib:")
                                    (string-append glu "/lib:")
                                    (string-append mesa-utils "/lib:")
                                    (string-append mesa "/lib:")
                                    (string-append zlib "/lib:")
                                    (string-append libice "/lib:")
                                    (string-append libx11 "/lib"))))
                (invoke "patchelf" "--set-rpath"
                        rpath
                        (string-append utests "/lib/libutests.so"))
                (invoke "patchelf" "--set-rpath"
                        rpath
                        (string-append utests "/bin/utest_run"))
                (invoke "patchelf" "--set-rpath"
                        rpath
                        (string-append utests "/bin/flat_address_space")))

              (invoke "patchelf" "--set-interpreter" ld-so
                      (string-append utests "/bin/utest_run"))
              (invoke "patchelf" "--set-interpreter" ld-so
                      (string-append utests "/bin/flat_address_space")))))

        (add-after 'remove-headers 'wrap
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((utests (assoc-ref outputs "tests"))
                   (utest-run (string-append utests "/bin/utest_run"))
                   (fas (string-append utests "/bin/flat_address_space"))
                   (out (assoc-ref outputs "out"))
                   (beignet-lib (string-append
                                 out "/lib/beignet"))
                   (beignet-bit (string-append beignet-lib "/beignet.bc"))
                   (beignet-kernel (string-append beignet-lib "/kernels"))
                   (beignet-gbe (string-append beignet-lib "/libgbe.so"))
                   (beignet-pch (string-append beignet-lib "/beignet.pch"))
                   (beignet-inter (string-append beignet-lib "/libgbeinterp.so"))
                   (beignet-inc (string-append beignet-lib "/include"))

                   (wrap-test
                    (lambda (prog)
                      (wrap-program prog
                        `("OCL_BITCODE_LIB_PATH" = (,beignet-lib))
                        `("OCL_IGNORE_SELF_TEST" = ("1"))
                        `("OCL_HEADER_FILE_DIR" = (,beignet-inc))
                        `("OCL_PCH_PATH" = (,beignet-pch))
                        `("OCL_KERNEL_PATH" = (,beignet-kernel))
                        `("OCL_GBE_PATH" = (,beignet-gbe))
                        `("OCL_INTERP_PATH" = (,beignet-inter))
                        `("OCL_PCH_20_PATH" = (""))))))
              (wrap-test utest-run)
              (wrap-test fas)))))))))
