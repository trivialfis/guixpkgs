(define-module (emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages webkit)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-trivialfis
  (package (inherit emacs)
    (name "emacs-trivialfis")
    (version "26.1")
    (source (origin
	      (inherit (package-source emacs))
	      (patches (cons
			(search-patch "emacs-add-atom-dark-theme.el.patch")
			(origin-patches (package-source emacs))))))
    (inputs
     `(,@(package-inputs emacs)
       ("wxwidgets" ,wxwidgets-3.1)
       ("webkitgtk" ,webkitgtk)
       ;; FIXME: configure script doesn't catch libxcomposite.
       ("libxcomposite" ,libxcomposite)))
    (arguments
     `(#:configure-flags
       '("--with-mailutils"
    	 "--with-xaw3d"			; FIXME: This one doesn't do anything.
    	 "--with-xwidgets"
    	 "--with-x-toolkit=gtk3"
    	 "CFLAGS=-O3 -march=native")
       ,@(package-arguments emacs)))))
