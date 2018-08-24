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

(define-module (python)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages databases))

(define-public python-nose-warnings-filters
  (package
   (name "python-nose-warnings-filters")
   (version "0.1.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "nose_warnings_filters" version))
     (sha256
      (base32
       "17dvfqfy2fm7a5cmiffw2dc3064kpx72fn5mlw01skm2rhn5nv25"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-nose" ,python-nose)))
   (home-page "https://github.com/Carreau/nose_warnings_filters")
   (synopsis
    "Allow to inject warning filters during ``nosetest``.")
   (description
    "Allow to inject warning filters during ``nosetest``.")
   (license license:expat)))

(define-public python-backcall
  (package
    (name "python-backcall")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backcall" version))
       (sha256
        (base32
         "1r01dqch3f8fdj3n6fviw8hxqrs6w5v0qw4izmvqzry1w9dxiv1q"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/takluyver/backcall")
    (synopsis
     "Specifications for callback functions passed in to an API")
    (description
     "Specifications for callback functions passed in to an API")
    (license license:bsd-3)))

(define-public python-ipython
  (package
    (name "python-ipython")
    (version "6.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipython" version))
       (sha256
        (base32
         "196m8y4wjll0bwk29zahbh1l1j1m9zg6s2z17vv8x9m4mngfzwmh"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-parso" ,python-parso)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-pexpect", python-pexpect)
       ("python-jedi", python-jedi)
       ("python-prompt-toolkit", python-prompt-toolkit)
       ("python-decorator", python-decorator)
       ("python-simplegeneric", python-simplegeneric)
       ("python-pygments" ,python-pygments)
       ("python-backcall" ,python-backcall)
       ("python-pickleshare" ,python-pickleshare)
       ("python-traitlets" ,python-traitlets)))
    (arguments
     `(#:tests? #f))
    ;; pytest --cov ipykernel --durations 10 -v ipykernel
    ;; matplotlib
    (home-page "https://ipython.org")
    (synopsis
     "IPython: Productive Interactive Computing")
    (description
     "IPython: Productive Interactive Computing")
    (license license:bsd-3)))

(define-public python-ipykernel
  (package
    (name "python-ipykernel")
    (version "4.8.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
	    "https://github.com/ipython/ipykernel/archive/"
	    version ".tar.gz"))
      (sha256
       (base32
	"1cyd7629whjy74yszi0y8nag0xx4f1bwlib4ddza80y3c42kwq4d"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-nose-warnings-filters" ,python-nose-warnings-filters)))
    (propagated-inputs
     `(("python-ipython" ,python-ipython)))
    ;; The tests load a submodule of IPython.  However, IPython itself depends
    ;; on ipykernel.
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     ;; imported at runtime during connect
     `(("python-jupyter-client" ,python-jupyter-client)))
    (home-page "https://ipython.org")
    (synopsis "IPython Kernel for Jupyter")
    (description
     "This package provides the IPython kernel for Jupyter.")
    (license license:bsd-3)))
