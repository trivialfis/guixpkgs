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

(define-module  (machine-learning)
  #:use-module (ice-9 match)		; openmpi
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)	; openmpi
  #:use-module (gnu packages linux)	; openmpi
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)	; openmpi
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages pkg-config) ; openmpi
  #:use-module (gnu packages valgrind)	 ; openmpi
  #:use-module (guix utils))		 ; openmpi

(define-public python-autograd
  (let* ((commit "442205dfefe407beffb33550846434baa90c4de7")
         (revision "0")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "python-autograd")
      (home-page "https://github.com/HIPS/autograd")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "189sv2xb0mwnjawa9z7mrgdglc1miaq93pnck26r28fi1jdwg0z4"))
                (file-name (git-file-name name version))))
      (version version)
      (build-system python-build-system)
      (native-inputs
       `(("python-nose" ,python-nose)
         ("python-pytest" ,python-pytest)))
      (propagated-inputs
       `(("python-future" ,python-future)
         ("python-numpy" ,python-numpy)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "py.test" "-v"))))))
      (synopsis "Efficiently computes derivatives of numpy code")
      (description "Autograd can automatically differentiate native Python and
Numpy code. It can handle a large subset of Python's features, including loops,
ifs, recursion and closures, and it can even take derivatives of derivatives of
derivatives. It supports reverse-mode differentiation (a.k.a. backpropagation),
which means it can efficiently take gradients of scalar-valued functions with
respect to array-valued arguments, as well as forward-mode differentiation, and
the two can be composed arbitrarily. The main intended application of Autograd
is gradient-based optimization.")
      (license license:non-copyleft))))

(define-public python2-autograd
  (package-with-python2 python-autograd))

(define-public dmlc-core
  (let* ((commit "d26d9e7982b233d4aa105ae084fbecc500d254ff")
         (revision "0")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "dmlc-core")
      (version version)
      (home-page "https://github.com/dmlc/dmlc-core")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0lf8my5b9p458q5y45q2hav07i8q7qnlvqi6145zzb9nrzkjdkgp"))
                (file-name (git-file-name name version))))
      (native-inputs
       `(("googletest" ,googletest)))
      (arguments
       `(#:configure-flags
	 '("-DDBUILD_SHARED_LIBS=ON")))
      (build-system cmake-build-system)
      (synopsis "Distributed machine learning common codebase")
      (description "DMLC-Core is the backbone library to support all DMLC
projects,  offers the bricks to build efficient and scalable distributed
xmachine learning libraries.")
      (license license:asl2.0))))

(define-public openmpi-1.10.7
  (package
    (name "openmpi-1.10.7")
    (version "1.10.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                          (version-major+minor version)
                          "/downloads/openmpi-" version ".tar.bz2"))
      (sha256
       (base32
        "142s1vny9gllkq336yafxayjgcirj2jv0ddabj879jgya7hyr2d0"))))
    (build-system gnu-build-system)
    (inputs
     `(("hwloc" ,hwloc "lib")
       ("gfortran" ,gfortran)
       ("libfabric" ,libfabric)
       ,@(match (%current-system)
                ((member (package-supported-systems psm))
                 `(("psm" ,psm)))
                (_ `()))
       ("rdma-core" ,rdma-core)
       ("valgrind" ,valgrind)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags `("--enable-mpi-ext=affinity" ;cr doesn't work
                           "--enable-memchecker"
                           "--with-sge"

                           ;; VampirTrace is obsoleted by scorep and disabling
                           ;; it reduces the closure size considerably.
                           "--disable-vt"

                           ,(string-append "--with-valgrind="
                                           (assoc-ref %build-inputs "valgrind"))
                           ,(string-append "--with-hwloc="
                                           (assoc-ref %build-inputs "hwloc")))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-absolute
                    (lambda _
                      ;; Remove compiler absolute file names (OPAL_FC_ABSOLUTE
                      ;; etc.) to reduce the closure size.  See
                      ;; <https://lists.gnu.org/archive/html/guix-devel/2017-07/msg00388.html>
                      ;; and
                      ;; <https://www.mail-archive.com/users@lists.open-mpi.org//msg31397.html>.
                      (substitute* '("orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c"
                                     "ompi/tools/ompi_info/param.c")
                        (("_ABSOLUTE") ""))
                      ;; Avoid valgrind (which pulls in gdb etc.).
                      (substitute*
                          '("./ompi/mca/io/romio/src/io_romio_component.c")
                        (("MCA_io_romio_COMPLETE_CONFIGURE_FLAGS")
                         "\"[elided to reduce closure]\""))
                      #t))
                  (add-before 'build 'scrub-timestamps ;reproducibility
                    (lambda _
                      (substitute* '("ompi/tools/ompi_info/param.c"
                                     "orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c")
                        ((".*(Built|Configured) on.*") ""))
                      #t))
                  (add-after 'install 'remove-logs ;reproducibility
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each delete-file (find-files out "config.log"))
                        #t))))))
    (home-page "http://www.open-mpi.org")
    (synopsis "MPI-3 implementation")
    (description
     "The Open MPI Project is an MPI-3 implementation that is developed and
maintained by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and resources
from all across the High Performance Computing community in order to build the
best MPI library available.  Open MPI offers advantages for system and
software vendors, application developers and computer science researchers.")
    ;; See file://LICENSE
    (license license:bsd-2)))

(define-public rabit
  (let* ((commit "7bc46b8c75a6d530b2ad4efcf407b6aeab71e44f")
         (revision "0")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "rabit")
      (home-page "https://github.com/dmlc/rabit")
      (version version)
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url home-page)
                            (commit commit)))
                      (sha256
                       (base32
                        "1zybls07a7kwafn0m97cvwcrvnmch95y0mw0ir1485mdlix7qwac"))
		      (patches (search-patches "rabit-fix-building-shared-library.patch"))
                      (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (inputs
       `(("openmpi-1.10.7" ,openmpi-1.10.7)))
      (arguments
       `(#:configure-flags
         '("-DRABIT_BUILD_TESTS=ON"
	   "-DBUILD_SHARED_LIBS=ON"
	   "-DRABIT_BUILD_MPI=ON"
	   "-DCMAKE_CXX_FLAGS=-std=gnu++11")
	 #:tests? #f))			; Not available for cmake yet.
      (synopsis "Reliable Allreduce and Broadcast Interface")
      (description "Rabit is a light weight library that provides a fault
tolerant interface of Allreduce and Broadcast. It is designed to support easy
implementations of distributed machine learning programs, many of which fall
naturally under the Allreduce abstraction.")
      (license license:asl2.0))))

(define-public xgboost
  (package
    (name "xgboost")
    (version "0.71")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dmlc/xgboost/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0csvwmanqfqm1cy0gmz3yjpk9088iyk0770qc02zwxm0wazkkb8q"))
              (patches (search-patches "xgboost-don-t-use-submodules.patch"
                                       "xgboost-fix-test_param.patch"
				       "xgboost-add-install.patch"))
              (file-name (string-append name "-" version ".tar.gz"))
	      (modules '((guix build utils)))
	      (snippet
	       '(begin
		  (delete-file-recursively "cub")
		  (delete-file-recursively "dmlc-core")
		  (delete-file-recursively "nccl")
		  (delete-file-recursively "rabit")))))
    (native-inputs
     `(("googletest" ,googletest)))
    (inputs
     `(("dmlc-core" ,dmlc-core)
       ("rabit" ,rabit)))
    (arguments
     `(#:configure-flags
       '("-DGOOGLE_TEST=ON")
       #:phases
       (modify-phases %standard-phases
	 (add-before 'configure 'remove-find-gtest
	   (lambda* (#:key inputs #:allow-other-keys)
	     (delete-file "cmake/modules/FindGTest.cmake")))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
	     (with-directory-excursion ,(string-append "../" name "-" version)
	       (invoke "./testxgboost")))))))
    (build-system cmake-build-system)
    (home-page "https://xgboost.readthedocs.io/en/latest/")
    (synopsis "Scalable and flexible gradient boosting")
    (description "XGBoost is an optimized distributed gradient boosting library
designed to be highly efficient, flexible and portable. It implements machine
learning algorithms under the Gradient Boosting framework.")
    (license license:asl2.0)))

(define-public python-xgboost
  (package
    (name "python-xgboost")
    (version "0.71")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dmlc/xgboost/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0csvwmanqfqm1cy0gmz3yjpk9088iyk0770qc02zwxm0wazkkb8q"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "python-xgboost-find-library-in-sys-path.patch"))
              (snippet
               '(begin
		  (delete-file "./python-package/xgboost/build-python.sh")
		  (delete-file "./python-package/setup_pip.py")
		  (delete-file "./python-package/prep_pip.sh")
		  ;; Following are symlinks to main source tree
                  (delete-file "./python-package/xgboost/src")
                  (delete-file "./python-package/xgboost/rabit")
                  (delete-file "./python-package/xgboost/make")
		  (delete-file "./python-package/xgboost/lib")
                  (delete-file "./python-package/xgboost/include")
                  (delete-file "./python-package/xgboost/dmlc-core")))))
    (inputs
     `(("xgboost" ,xgboost)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-python
           (lambda _
             (chdir "python-package")
	     #t)))))
    (home-page "https://xgboost.readthedocs.io/en/latest/")
    (synopsis "Python binding for xgboost")
    (description "XGBoost is an optimized distributed gradient boosting library
designed to be highly efficient, flexible and portable. It implements machine
learning algorithms under the Gradient Boosting framework.")
    (license license:asl2.0)))

(define-public libffm
  (package
    (name "libffm")
    (version "123")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://github.com/guestwalk/libffm/archive/v"
		    version ".tar.gz"))
	      (sha256
               (base32
		"1m8k9icg9k7dimfz81vnp8i5m08591hgqri49cb07plad8n50jy7"))
	      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (delete 'configure)
	 (replace 'install
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (builddir (getcwd))
		    (bin (string-append out "/bin"))
		    (doc (string-append out "/doc")))
	       (mkdir out)
	       (mkdir bin)
	       (mkdir doc)
	       (copy-file (string-append builddir "/ffm-predict")
			  (string-append bin "/ffm-predict"))
	       (copy-file (string-append builddir "/ffm-train")
			  (string-append bin "/ffm-train"))
	       (copy-file (string-append builddir "/README")
			  (string-append doc "/README"))
	       (copy-file (string-append builddir "/COPYRIGHT")
			  (string-append doc "/COPYRIGHT"))))))
       #:tests? #f))
    (home-page "https://github.com/guestwalk/libffm")
    (synopsis "Library for Field-aware Factorization Machines")
    (description "LIBFFM is a library for field-aware factorization machine
(FFM).")
    (license license:bsd-3)))

(define-public python-twython
  (let* ((commit "c9e8a462000898dcd91b9babf130b907986591ea")
	 (revision "0")
	 (version (git-version "3.4.0" revision commit)))
    (package
     (name "python-twython")
     (version version)
     (home-page "https://github.com/ryanmcgrath/twython")
     (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url home-page)
		    (commit commit)))
	      (sha256
	       (base32
		"1fpi5nn9chgiljapqwv577w3rwl3k5r381s4hagw91gixdyy3xjd"))
	      (file-name (git-file-name name version))))
     (build-system python-build-system)
     (propagated-inputs
      `(("python-requests" ,python-requests)
	("python-requests-oauthlib" ,python-requests-oauthlib)))
     (native-inputs
      `(("python-responses" ,python-responses)))
     (synopsis "Python wrapper for the Twitter API")
     (description "Twython is the premier Python library providing an easy
(and up-to-date) way to access Twitter data. Actively maintained and featuring
support for Python 2.6+ and Python 3.")
     (license license:expat))))
