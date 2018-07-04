(define-module (gnome)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnome))

(define-public nautilus-3.28
  (package
    (inherit nautilus)
    (version "3.28.1")
    (name (string-append "nautilus-" version))
    (source (origin
              (method url-fetch)
	      (uri (string-append "https://download.gnome.org/sources/nautilus/"
				  (version-major+minor version)
				  "/" name ".tar.xz"))
              (sha256
               (base32
		"19dhpa2ylrg8d5274lahy7xqr2p9z3jnq1h4qmsh95czkpy7is4w"))))
    (arguments
     '(#:glib-or-gtk?
       #t
       ;; XXX: FAIL: check-nautilus
       ;;   Settings schema 'org.gnome.nautilus.preferences' is not installed
       #:tests? #f))
    (inputs
     `(,@(package-inputs nautilus)
       ("gexiv2" ,gexiv2)
       ("tracker-2.0.3" ,tracker-2.0.3)))))

(define-public tracker-2.0.3
  (package (inherit tracker)
    (name "tracker-2.0.3")
    (version "2.0.3")
    (source (origin
	      (method url-fetch)
	      (uri "https://download.gnome.org/sources/tracker/2.0/tracker-2.0.3.tar.xz")
	      (sha256
	       (base32
		"1005w90vhk1cl8g6kxpy2vdzbskw2jskfjcl42lngv18q5sb4bss"))
	      (patches (search-patches "tracker-freeze-hash.patch"))))
    ;; (build-system meson-build-system)
    (native-inputs
     `(,@(package-native-inputs tracker)
       ("python" ,python-wrapper)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-before 'check 'set-lang
	   (lambda _
	     (setenv "LANG" "en_US.UTF-8"))))
       #:tests? #t))))
