(define-module (c)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl))

(define-public libcork
  (let* ((commit "17bf6a28abbd90c5f895c748ecf546e48b00f0b7")
         (revision "0")
         (version (git-version "0.15.0" revision commit)))
    (package
      (name "libcork")
      (version version)
      (home-page "https://github.com/dcreager/libcork")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
		  "17qyqi0y88vg21yv6rg4dy73yiz74vgsnkan580ib3pim23nhp8k"))
                (file-name (git-file-name name version))
		(patches (search-patches "cork-remove-execute_process.patch"))))
      (arguments
       `(#:configure-flags
	 (list
	  "-DVERSION=0.15.0"
	  (string-append "-DGIT_SHA1=" ,commit))
	 #:phases
	 (modify-phases %standard-phases
	   (add-after 'unpack 'replace-sh
	     (lambda* (#:key inputs outputs #:allow-other-keys)
	       (let* ((out (assoc-ref outputs "out"))
		      (sh (assoc-ref inputs "bash"))
		      (cram.py "tests/cram.py"))
		 (substitute* cram.py
		   (("p.add_option\\('--shell', action='store', default='/bin/sh', metavar='PATH',")
		    (string-append
		     "p.add_option('--shell', action='store', default='"
		     sh "', metavar='PATH',"))
		   ))
	       #t)))
	 #:tests? #f))			; permission denied for bash
      (native-inputs
       `(("pkg-config", pkg-config)
	 ("python" ,python-wrapper)
	 ("check" ,check)))
      (build-system cmake-build-system)
      (synopsis "Simple, easily embeddable cross-platform C library.")
      (description "Simple, easily embeddable cross-platform C library.")
      (license license:bsd-3))))

(define-public libbloom
  (package
   (name "libbloom")
   (version "1.6")
   (home-page "https://github.com/jvirkki/libbloom")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/jvirkki/" name
	   "/archive/v" version ".tar.gz"))
     (sha256
      (base32 "0yhrznx5755l90r2dnlxljad8kid6cxzqvsnniys1y3k00y8pdla"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (native-inputs
    `(("perl" ,perl)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
	(delete 'configure)
	(add-after 'unpack 'change-makefile
	  (lambda* (#:key inputs #:allow-other-keys)
	    (let* ((makefile "Makefile")
		   (top (getcwd)))
	      (substitute* makefile
		(("TOP := \\$\\(shell /bin/pwd\\)")
		 (string-append "TOP := " top))))))
	(add-before 'build 'set-cc
	    (lambda _
	      (setenv "CC" "gcc")))
	(replace 'install
	  (lambda* (#:key outputs #:allow-other-keys)
	    (let* ((out (assoc-ref outputs "out"))
                   (out-inc (string-append out "/include"))
		   (out-lib (string-append out "/lib")))
	       (install-file "bloom.h" out-inc)
	       (install-file "build/libbloom.a" out-lib)
	       (install-file "build/libbloom.so" out-lib)))))
      #:tests? #f))
   (build-system gnu-build-system)
   (synopsis "Simple and small bloom filter implementation in plain C")
   (description "Simple and small bloom filter implementation in plain C")
   (license '(license:bsd-2 license:expat)))) ; murmur2 is licensed under MIT
