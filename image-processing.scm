;;; Copyright @ 2018 Björn Höfling
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

(define-module (image-processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image))

(define-public guile-cv
  ;; Doesn't work yet. Don't wanna pull in texlive for this packages.
  (package
   (name "guile-cv")
   (version "0.1.9")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "http://ftp.gnu.org/gnu/guile-cv/guile-cv-"
				version ".tar.gz"))
	    (sha256
	     (base32
	      "1brvf4qq98nckjhs7wg4nwpi6qfpv4ngbj4f8m7xsflm5xm0vgrp"))
	    (file-name (string-append name "-" version ".tar.gz"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("texlive" ,texlive)))
   (inputs
    `(("guile" ,guile-2.2)
      ("vigra-c" ,vigra-c)))
   (home-page "https://www.gnu.org/software/guile-cv/index.html")
   (synopsis "Computer Vision functional programming library for the Guile
Scheme language")
   (description "Based on @code{Vigra}, Guile-CV comprises a direct binding to
@code{Vigra C}, enriched with pure Guile scheme algorithms, all accessible
through a nice, clean and easy to use high level API.")
   (license license:gpl3+)))
