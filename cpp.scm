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

(define-module (cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (code))

(define-public reproc
  (package
    (name "reproc")
    (version "1.0.0")
    (home-page "https://github.com/DaanDeMeyer/reproc")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/" version ".tar.gz"))
       (sha256
        (base32
         "0gk4g9rcmbimn2w916gclm2j9qbzdhp4ls7dd9avk92dvknpz46v"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (arguments
     `(#:configure-flags
       '("-DREPROC_BUILD_TESTS=ON"
         "-DREPROC_INSTALL=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./test/tests"))))))
    (build-system cmake-build-system)
    (license license:expat)
    (synopsis "Cross-platform library that simplifies working with external CLI
applications from C and C++.")
    (description "Reproc (Redirected Process) is cross-platform library that
 starts external processes from within a C or C++ application, reads/writes to
 their stdin/stdout/stderr streams and waits for them to exit or forcefully
stops them.")))

(define-public visit-struct
  (package
   (name "visit-struct")
   (version "1.0")
   (home-page "https://github.com/cbeck88/visit_struct/")
   (source (origin
            (method url-fetch)
            (uri (string-append home-page "archive/v"version".tar.gz"))
            (sha256
             (base32
              "1760ncj4fz1dgarflfkgy1kwzwi1xgsja1l4hb8l9r7vv5xp4pby"))
            (file-name (string-append name "-" version ".tar.gz"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("python" ,python-wrapper)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'build)
        (delete 'configure)
        (delete 'check)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (inc (string-append out "/include")))
              (copy-recursively "include/visit_struct" inc)))))))
   (synopsis "Miniature library for struct-field reflection in C++.")
   (description "A header-only library providing structure visitors for C++11
and C++14.")
   (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"))))
