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
  #:use-module (gnu packages python))

(define-public python-yapf
  (package
    (name "python-yapf")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yapf" version))
       (sha256
        (base32
         "144gc7d6b1415vh02409rnb8qd5kxi6mxlr7y64d5cizgxbf72kx"))))
    (build-system python-build-system)
    (home-page "https://github.com/google/yapf")
    (synopsis "Formatter for Python code")
    (description "YAPF is a formatter for Python code.  It's based off of
@dfn{clang-format}, developed by Daniel Jasper.  In essence, the algorithm
takes the code and reformats it to the best formatting that conforms to the
style guide, even if the original code didn't violate the style guide.")
    (license license:asl2.0)))

(define-public python2-yapf
  (package-with-python2 python-yapf))

(define-public python-parso
  (package
    (name "python-parso")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parso" version))
       (sha256
        (base32
         "0lamywk6dm5xshlkdvxxf5j6fa2k2zpi7xagf0bwidaay3vnpgb2"))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (build-system python-build-system)
    (home-page
     "https://github.com/davidhalter/parso")
    (synopsis "A Python Parser")
    (description "A Python Parser")
    (license license:expat)))

(define-public python2-parso
  (package-with-python2 python-parso))


(define-public python-jedi
  (package
    (name "python-jedi")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jedi" version))
       (sha256
        (base32
         "1bcr7csx4xil1iwmk03d79jis0bkmgi9k0kir3xa4rmwqsagcwhr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _
                           (invoke "py.test" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-parso" ,python-parso)
       ("python-docopt" ,python-docopt)))
    (home-page "https://github.com/davidhalter/jedi")
    (synopsis
     "Autocompletion for Python that can be used for text editors")
    (description
     "Jedi is an autocompletion tool for Python that can be used for text
 editors.")
    (license license:expat)))

(define-public python2-jedi
  (package-with-python2 python-jedi))

(define-public python-autopep8
  (package
    (name "python-autopep8")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "autopep8" version))
       (sha256
	(base32
	 "192bvhzi4d0claqxgzymvv7k3qnj627742bc8sgxpzjj42pd9112"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pycodestyle" ,python-pycodestyle)))
    (home-page "https://github.com/hhatto/autopep8")
    (synopsis "Format Python code according to the PEP 8 style guide")
    (description
     "@code{autopep8} automatically formats Python code to conform to
the PEP 8 style guide.  It uses the pycodestyle utility to determine
what parts of the code needs to be formatted.  @code{autopep8} is
capable of fixing most of the formatting issues that can be reported
by pycodestyle.")
    (license (license:non-copyleft
              "https://github.com/hhatto/autopep8/blob/master/LICENSE"))))

(define-public python-glob2
  (package
    (name "python-glob2")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/miracle2k/python-glob2/archive/"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "0ja168f0dz4kbz4m06dm0rd3acaypk6hjx2km541pw22y9s40mag"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (home-page "https://github.com/miracle2k/python-glob2/")
    (synopsis "Version of the glob module that supports recursion via **, and
can capture patterns.")
    (description "This is an extended version of Python's builtin glob module
@url{http://docs.python.org/library/glob.html} which adds:

@itemize
@item The ability to capture the text matched by glob patterns, and return
those matches alongside the filenames.
@item A recursive '**' globbing syntax, akin for example to the globstar
option of the bash shell.
@item The ability to replace the filesystem functions used, in order to glob
on virtual filesystems.
@item Compatible with Python 2 and Python 3 (tested with 3.3).
@end itemize\n
It's currently based on the glob code from Python 3.3.1.")
    (license license:bsd-2)))
