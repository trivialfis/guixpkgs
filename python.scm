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
