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
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (code))

(define-public rct-next
  (let* ((commit "60d28748efadabaec03ceca18565caa4bb5ef7b3")
         (revision "2")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "rct-next")
      (version version)
      (home-page "https://github.com/Andersbakken/rct")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "08yy979dw0qnc854ikfwc284z8jl6l41y1x4dd75p8fm8kify27p"))
                (patches
                 (search-patches "rct-cmake-Add-missing-headers.patch"
                                 "rct-build-test.patch"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         '("-DWITH_TESTS=ON"            ; To run the test suite
           "-DRCT_RTTI_ENABLED=ON")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (with-directory-excursion "./tests"
                 (invoke "./rct_tests")))))))
      (native-inputs
       `(("cppunit" ,cppunit)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("openssl" ,openssl)
         ("zlib" ,zlib)))
      (synopsis "C++ library providing Qt-like APIs on top of the STL")
      (description "Rct is a set of C++ tools that provide nicer (more Qt-like)
 APIs on top of Standard Template Library (@dfn{STL}) classes.")
      (license (list license:expat        ; cJSON
                     license:bsd-4)))))

(define-public reproc
  (package
    (name "reproc")
    (version "1.0.0")
    (home-page "https://github.com/DaanDeMeyer/reproc")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/"version".tar.gz"))
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
