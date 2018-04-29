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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
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


(define (opencl-headers major-version subversion)
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
  (opencl-headers "2" "2"))
(define-public opencl-headers-2.1
  (opencl-headers "2" "1"))
(define-public opencl-headers-2.0
  (opencl-headers "2" "0"))
(define-public opencl-headers-1.2
  (opencl-headers "1" "2"))
(define-public opencl-headers-1.1
  (opencl-headers "1" "1"))
(define-public opencl-headers-1.0
  (opencl-headers "1" "0"))

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
     `(("opencl-headers@2.2" ,opencl-headers-2.2)))
    (arguments
     '(#:configure-flags
       '("-DBUILD_EXAMPLES=OFF"
         "-DBUILD_TESTS=OFF")
       #:tests? #f))
    (build-system cmake-build-system)
    (home-page "http://github.khronos.org/OpenCL-CLHPP/")
    (synopsis "Khronos OpenCL-CLHPP")
    (description "OpenCL Host API C++ bindings cl2.hpp.")
    (license license:expat)))

(define-public clinfo
  ;; Not working yet, ld can't find OpenCL. Should I give it a cmake build?
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
    (native-inputs `(("opencl-headers@2.2" ,opencl-headers-2.2)
                     ("pocl" ,pocl)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda _
                      (let ((cores (number->string (parallel-job-count))))
                        (setenv "CC" "gcc")
                        (invoke "make" "-j" cores)))))))
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
                "1x2dr8p4dkfds56r38av360i3nv1y3326jmshxvjngaf6mlg6rbn"))))
    (inputs `(("ruby" ,ruby)
              ("opencl-headers@2.2" ,opencl-headers-2.2)
              ("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    ;; FIXME:
    ;; (arguments
    ;;  '(#:configure-flags
    ;;    '("--enable-update-database")))
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
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libpthread-stubs", libpthread-stubs)
              ("clang@3.7" ,clang-3.7)
              ("libdrm" ,libdrm)
              ("libsm" ,libsm)
              ("libxfixes" ,libxfixes)
              ("libxext" ,libxext)
              ("libedit" ,libedit)
              ("xextproto" ,xextproto)
              ("python" ,python)
              ("opencl-headers@2.2" ,opencl-headers-2.2)
              ("glu" ,glu)
              ("zlib" ,zlib)
              ("libva" ,libva)
              ("llvm@3.7" ,llvm-3.7)
              ("clang-runtime@3.7" ,clang-runtime-3.7)
              ("mesa-utils" ,mesa-utils)
              ("ncurses" ,ncurses)
              ("ocl-icd" ,ocl-icd)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLANG_LIBRARY_DIR="
                            (assoc-ref %build-inputs "clang@3.7") "/lib")
             "-DENABLE_GL_SHARING=ON"
             "-DEXPERIMENTAL_DOUBLE=ON")
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Don't be mislead by this function, tests don't work.
             (let* ((builddir (getcwd))
                    (testdir (string-append builddir "/utests"))
                    (beignet-src (string-append builddir "/.."))
                    (cores (number->string (parallel-job-count)))
                    ;; (source (assoc-ref inputs "source"))
                    (out (assoc-ref outputs "out"))
                    (libocl-path "/backend/src/libocl")
                    (setenv-with-tests (lambda (name value)
                                         (format #t "Name: ~s\nValue: ~s\n" name value)
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

               ;; (format #t "src: ~s target: ~s\n"
               ;;              (string-append testdir "/libutests.so")
               ;;              (string-append out "/lib/libutests.so"))
               ;; (install-file (string-append testdir "/libutests.so")
               ;;                 (string-append out "/lib/libutests.so"))

               ;; (mkdir (string-append out "/bin"))
               ;; (format #t "src: ~s target: ~s\n"
               ;;              (string-append testdir "/utest_run")
               ;;              (string-append out "/bin/utest_run"))
               ;; (install-file (string-append testdir "/utest_run")
               ;;                 (string-append out "/bin/utest_run"))

               (setenv-with-tests "OCL_BITCODE_LIB_PATH"
                                  (string-append builddir
                                                 libocl-path
                                                 out
                                                 "/lib/beignet/beignet.bc"))
               (setenv-with-tests "OCL_HEADER_FILE_DIR"
                                  (string-append builddir
                                                 libocl-path
                                                 out
                                                 "/lib/beignet/include"))
               (setenv "OCL_BITCODE_LIB_20_PATH" "")
               (setenv-with-tests "OCL_PCH_PATH"
                                  (string-append builddir
                                                 libocl-path
                                                 out
                                                 "/lib/beignet/beignet.local.pch"))
               (setenv "OCL_PCH_20_PATH" "")
               (setenv-with-tests "OCL_KERNEL_PATH"
                                  (string-append
                                   out "/lib/beignet/kernels"))
               (setenv-with-tests "OCL_GBE_PATH"
                                  (string-append
                                   builddir "/backend/src/libgbe.so"))
               (setenv-with-tests "OCL_INTERP_PATH"
                                  (string-append
                                   builddir
                                   "/backend/src/libgbeinterp.so"))
               (setenv "OCL_IGNORE_SELF_TEST" "1")

               (chdir testdir)
               (invoke "ls" "-l")
               ;; Tests don't pass, failed to recognize device
               (let ((status (system "./utest_run")))
                 (unless (zero? status)
                   (error (format #f "Tests exit with non-zero code" status)))
                 (zero? status))))))
       ))
    (home-page "https://wiki.freedesktop.org/www/Software/Beignet/")
    (synopsis "Intel's OpenCL framework")
    (description "Intel's OpenCL framework that works with Intel IvyBridge GPUs
and above.")
    (license license:gpl2)))

(define-public pocl
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
         "-DENABLE_TESTSUITES=all"
         "-DENABLE_CONFORMANCE=ON")
       #:tests? #f))
    (home-page "http://portablecl.org/")
    (synopsis "Portable Computing Language (pocl)")
    (description "pocl is being developed towards an efficient implementation
of OpenCL standard which can be easily adapted for new targets.")
    (license license:non-copyleft)))

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
       `(#:phases (modify-phases %standard-phases
                    (delete 'install))  ; No such a phase
                  #:tests? #f))                 ; Run automatically.
      (native-inputs `(("googletest" ,googletest)))
      (synopsis "Device specific buffer management for Intel(R) Graphics
Compute Runtime")
      (description "The Intel(R) Graphics Memory Management Library provides
device specific and buffer management for the Intel(R) Graphics Compute Runtime
for OpenCL(TM) and the Intel(R) Media Driver for VAAPI.")
      (license license:non-copyleft))))
