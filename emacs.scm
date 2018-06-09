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
