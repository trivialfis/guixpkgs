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

(define-module (emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages webkit))

(define-public emacs-trivialfis
  (package (inherit emacs)
    (name "emacs-trivialfis")
    (version "26.1")
    (source (origin
	      (inherit (package-source emacs))
	      (patches (cons
			(search-patch "emacs-suppress-message-for-autoloads.patch")
			(cons
			 (search-patch "emacs-add-atom-dark-theme.el.patch")
			 (origin-patches (package-source emacs)))))))
    (inputs
     `(,@(package-inputs emacs)
       ;; ("wxwidgets" ,wxwidgets-3.1)
       ("webkitgtk" ,webkitgtk)
       ;; FIXME: configure script doesn't catch libxcomposite.
       ("libxcomposite" ,libxcomposite)))
    (arguments
     `(#:configure-flags
       '("--with-mailutils"
    	 "--with-xaw3d"			; FIXME: This one doesn't do anything.
    	 ;; "--with-xwidgets"
    	 "--with-x-toolkit=gtk3"
    	 "CFLAGS=-O3 -march=native")
       ,@(package-arguments emacs)))))
