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
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python))

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
      (build-system cmake-build-system)
      (synopsis "Distributed machine learning common codebase")
      (description "DMLC-Core is the backbone library to support all DMLC
projects,  offers the bricks to build efficient and scalable distributed
xmachine learning libraries.")
      (license license:asl2.0))))

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
                     (file-name (git-file-name name version))))
     (build-system cmake-build-system)
     (arguments
      `(#:configure-flags
	'("-DCMAKE_CXX_FLAGS=-std=gnu++11")
	#:tests? #f))			; use test.mk, not cmake
     (synopsis "Reliable Allreduce and Broadcast Interface")
     (description "Rabit is a light weight library that provides a fault
tolerant interface of Allreduce and Broadcast. It is designed to support easy
implementations of distributed machine learning programs, many of which fall
naturally under the Allreduce abstraction.")
     (license license:asl2.0))))

(define-public xgboost
  ;; Not working yet.
  (package
    (name "xgboost")
    (version "0.71")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://github.com/dmlc/xgboost/archive/v"
				  version ".tar.gz"))
	      (sha256
	       (base32
		"0csvwmanqfqm1cy0gmz3yjpk9088iyk0770qc02zwxm0wazkkb8q"))
	      (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (home-page "https://xgboost.readthedocs.io/en/latest/")
    (synopsis "Scalable and flexible gradient boosting")
    (description "XGBoost is an optimized distributed gradient boosting library
designed to be highly efficient, flexible and portable. It implements machine
learning algorithms under the Gradient Boosting framework.")
    (license license:asl2.0)))
