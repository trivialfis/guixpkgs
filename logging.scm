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

(define-module (logging)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages))

(define-public loguru
  (package
    (name "loguru")
    (version "1.8.0")
    (home-page "https://github.com/emilk/loguru")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "10pnjb5zlwihk3sxhrpzlg3311fycz9k435z9rf7c3fkpf4ldds5"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "loguru-Fix-unused-ptr.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir-to-tests
           (lambda _
             (chdir "./test")
	     #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (inc (string-append out "/include")))
               (install-file "loguru.hpp" inc))))
	 (add-before 'install 'chdir-to-root
	   (lambda _
	     (chdir "..")
	     #t)))))
    (synopsis "Lightweight C++ logging library")
    (description "Loguru provides a set of functions and macros to aid logging to
one or several files and/or to stderr. It prefixes each log line with useful
information such as time.")
    (license license:non-copyleft)))
