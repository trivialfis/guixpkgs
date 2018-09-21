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
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages code)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (cpp)
  #:use-module (llvm))

(define-public universal-ctags
  (let* ((commit "44a0e9791e57d243668005f524a7014f0ebfc2bd")
         (revision "1")
         (version (git-version "1.1.1" revision commit)))
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
		  "0iw4q437wwcjsb4pbvh1908h3ldl6v0hx6cpika616pf1sm0q0lf"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:configure-flags
	 '("--enable-tmpdir=/tmp")
	 #:modules ((guix build gnu-build-system)
                    (guix build utils))
	 #:phases
         (modify-phases %standard-phases
	   (add-before 'bootstrap 'patch-source-shebangs
	     (assoc-ref %standard-phases 'patch-source-shebangs))
           (add-before 'bootstrap 'make-files-writable
             (lambda _
               (with-directory-excursion "./optlib"
                 (for-each (lambda (file)
			     (format (current-output-port) "Changing ~s~%" file)
			     (chmod file #o666))
                           (find-files "." "\\.c"))))))
         #:tests? #f))                  ; FIXME: Disable known bug tests.
      (synopsis "A maintained ctags implementation")
      (description "universal-ctags has the objective of continuing the
development from what existed in the Sourceforge area.  The goal of the
project is preparing and maintaining common/unified working space where people
interested in making ctags better can work together.")
      (license license:gpl2+))))

(define-public global-ctags
  (package
    (inherit global)
    (name "global-ctags")
    (inputs `(,@(package-inputs global)
	      ("universal-ctags" ,universal-ctags)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses"))
             (string-append "--with-sqlite3="
                            (assoc-ref %build-inputs "sqlite"))
	     (string-append "--with-exuberant-ctags="
			    (assoc-ref %build-inputs "universal-ctags")
			    "/bin/ctags"))

       #:phases
       (modify-phases %standard-phases
        (add-after 'install 'post-install
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Install the Emacs Lisp file in the right place.
            (let* ((out  (assoc-ref outputs "out"))
                   (data (string-append out "/share/gtags"))
                   (lisp (string-append out "/share/emacs/site-lisp")))
              (install-file (string-append data "/gtags.el") lisp)
              (delete-file (string-append data "/gtags.el"))
              #t))))))))

;; It's a patched cquery, official cquery should just work.
(define-public cquery
  (let* ((commit "cd6f2f3c077aa88bca23557588da6ff3f43035a4")
	 (revision "0")
	 (version (git-version "2018.07.18" revision commit)))
    (package
      (name "cquery")
      (version version)
      (home-page "https://github.com/cquery-project/cquery")
      (source "/home/fis/Workspace/cquery.tar.gz")
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
	 '("-DSYSTEM_CLANG=ON")
	 #:tests? #f))
      (inputs
       `(("clang" ,clang)))
      (synopsis "C/C++ language server supporting multi-million line code base,
powered by libclang.")
      (description "cquery is a highly-scalable, low-latency language server for
C/C++/Objective-C. It is tested and designed for large code bases like
Chromium. cquery provides accurate and fast semantic analysis without
interrupting workflow.")
      (license license:expat))))
