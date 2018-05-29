(define-module (emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
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
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-trivialfis
  (package
    (inherit emacs)
    (name "emacs-trivialfis")
    (version "26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emacs/emacs-"
                                  version ".tar.xz"))
              (sha256
               (base32
		"0b6k1wq44rc8gkvxhi1bbjxbz3cwg29qbq8mklq2az6p1hjgrx0w"))
              (patches (search-patches "emacs-exec-path.patch"
                                       "emacs-fix-scheme-indent-function.patch"
                                       "emacs-source-date-epoch.patch"
				       "emacs-add-atom-dark-theme.el.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete the bundled byte-compiled elisp files and
               ;; generated autoloads.
               '(with-directory-excursion "lisp"
                  (for-each delete-file
                            (append (find-files "." "\\.elc$")
                                    (find-files "." "loaddefs\\.el$")
                                    ;; This is the only "autoloads" file that
                                    ;; does not have "*loaddefs.el" name.
                                    '("eshell/esh-groups.el")))

                  ;; Make sure Tramp looks for binaries in the right places on
                  ;; remote GuixSD machines, where 'getconf PATH' returns
                  ;; something bogus.
                  (substitute* "net/tramp-sh.el"
                    ;; Patch the line after "(defcustom tramp-remote-path".
                    (("\\(tramp-default-remote-path")
                     (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                             "~/.guix-profile/bin" "~/.guix-profile/sbin"
                             "/run/current-system/profile/bin"
                             "/run/current-system/profile/sbin")))

                  ;; Make sure Man looks for C header files in the right
                  ;; places.
                  (substitute* "man.el"
                    (("\"/usr/local/include\"" line)
                     (string-join
                      (list line
                            "\"~/.guix-profile/include\""
                            "\"/var/guix/profiles/system/profile/include\"")
                      " ")))))))
    (arguments
     `(#:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/pwd
           (lambda _
             ;; Use `pwd', not `/bin/pwd'.
             (substitute* (find-files "." "^Makefile\\.in$")
               (("/bin/pwd")
                "pwd"))))
         (add-after 'install 'install-site-start
           ;; Use 'guix-emacs' in "site-start.el".  This way, Emacs packages
           ;; provided by Guix and installed in
           ;; ~/.guix-profile/share/emacs/site-lisp/guix.d/PACKAGE-VERSION are
           ;; automatically found.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/site-lisp")))
               (copy-file (assoc-ref inputs "guix-emacs.el")
                          (string-append lisp-dir "/guix-emacs.el"))
               (with-output-to-file (string-append lisp-dir "/site-start.el")
                 (lambda ()
                   (display
                    (string-append "(when (require 'guix-emacs nil t)\n"
                                   "  (guix-emacs-autoload-packages))\n"))))
               #t))))
       #:configure-flags
       '("--with-x-toolkit=gtk3"
	 "--with-mailutils"
	 "--with-xaw3d"
	 "CFLAGS=-O3 -march=native")))))
