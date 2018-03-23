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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby))

(define-public cl2-hpp
  ;; Not working yet, requires cmock.
  (package
    (name "cl2-hpp")
    (version "2.0.10")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://github.com/KhronosGroup/OpenCL-CLHPP/archive/v"
		    version ".tar.gz"))
	      (sha256
	       (base32
		"0awg6yznbz3h285kmnd47fykx2qa34a07sr4x1657yn3jmi4a9zs"))))
    (build-system cmake-build-system)
    (native-inputs `(()))
    (home-page "https://github.com/KhronosGroup/OpenCL-CLHPP")
    (synopsis "Khronos OpenCL-CLHPP")
    (description "Sources for the OpenCL Host API C++ bindings
 (cl.hpp and cl2.hpp).")
    (license license:non-copyleft)))

(define-public cl2hpp-header
  (package
    (name "cl2hpp-header")
    (version "2.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/KhronosGroup/OpenCL-CLHPP/releases/download/v"
	     version "/cl2.hpp"))
       (sha256
	(base32
	 "1v4q0g6b6mwwsi0kn7kbjn749j3qafb9r4ld3zdq1163ln9cwnvw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
		  ;; (delete 'unpack)
		  (replace 'unpack
		    (lambda* (#:key inputs outputs #:allow-other-keys)
		      (let* ((source (assoc-ref inputs "source")))
			(copy-file source "cl2.hpp"))))
		  (delete 'configure)
		  (delete 'build)
		  (delete 'check)
		  (replace 'install
		    (lambda* (#:key outputs #:allow-other-keys)
		      (delete-file "environment-variables")
		      (copy-recursively "." (string-append
					     (assoc-ref outputs "out")
					     "/include/CL")))))))
    (home-page "https://github.com/KhronosGroup/OpenCL-CLHPP")
    (synopsis "Khronos OpenCL-CLHPP")
    (description "OpenCL Host API C++ bindings cl2.hpp.")
    (license license:non-copyleft)))

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
