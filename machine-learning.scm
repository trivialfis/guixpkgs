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
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web))

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
