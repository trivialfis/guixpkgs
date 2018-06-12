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

(define-module (code)
  #:use-module (srfi srfi-1)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages code)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (cpp))

(define-public amalgamate
  (let* ((commit "c91f07eea1133aa184f652b8f1398eaf03586208")
         (revision "0")
         (version (string-append "1.1.1" revision commit)))
    (package
      (name "amalgamate")
      (version version)
      (home-page "https://github.com/edlund/amalgamate")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (sha256
          (base32
           "0cllaraw8mxs8q2nr28nhgzkb417gj2wcklqg59w84f4lc78k3yb"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(substitute* "test.sh"
             (("test_command \"cc -Wall -Wextra -o source.out source.c\"" all)
              "test_command \"gcc -Wall -Wextra -o source.out source.c\"")))))
      (build-system gnu-build-system)
      (inputs
       `(("python" ,python-wrapper)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir out)
                 (mkdir bin)
                 (copy-file "amalgamate.py"
                            (string-append bin "/amalgamate.py")))))
           (replace 'check
             (lambda _
               (invoke "./test.sh"))))))
      (synopsis "Tool for amalgamating C source and header files.")
      (description "amalgamate.py aims to make it easy to use SQLite-style C
source and header amalgamation in projects.")
      (license license:bsd-3))))

(define-public universal-ctags
  (let* ((commit "6f7654b98be0dd9a15c539882ab7ea3914ab7bf8")
         (revision "0")
         (version (string-append "1.1.1" revision commit)))
    (package
      (name "universal-ctags")
      (version version)
      (home-page "https://github.com/universal-ctags/ctags")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0xghdvjadcwm9agzxzv9rvlmkyn2gjf860ffdp8s6y7m2frlsl3y"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
	   (delete 'bootstrap)
           (add-before 'configure 'auto-gen
             (lambda _
               (invoke "./autogen.sh")))
           (add-before 'auto-gen 'make-files-writable
             (lambda _
               (with-directory-excursion "./optlib"
                 (for-each (lambda (file) (chmod file #o644))
                           (find-files "." "\\.c"))))))
         #:tests? #f))                  ; FIXME: Disable known bug tests.
      (synopsis "A maintained ctags implementation")
      (description "universal-ctags has the objective of continuing the
development from what existed in the Sourceforge area.  The goal of the
project is preparing and maintaining common/unified working space where people
interested in making ctags better can work together.")
      (license license:gpl2+))))

(define-public rtags-next
  (let* ((commit "8bf22578d0e28d0534411fb681eb0cd1e392b6ee")
         (revision "0")
         (version (git-version (package-version rtags) revision commit)))
    (package
      (inherit rtags)
      (name "rtags-next")
      (version version)
      (home-page "https://github.com/Andersbakken/rtags")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "12mn2lkhzj0dphjq0a07ghh0780h1ffsrw939d2akdqjbm8vc5i7"))
                (snippet (origin-snippet (package-source rtags)))
                (modules (origin-modules (package-source rtags)))
                (patches (origin-patches (package-source rtags)))))
      (inputs
       `(,@(alist-delete "rct" (package-inputs rtags))
         ("rct-next" ,rct-next))))))
