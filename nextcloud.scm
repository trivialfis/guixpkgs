(define-module (nextcloud)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression))

(define-public breakpad
  ;; Doesn't work
  (let* ((commit "54fa71efbe50fb2b58096d871575b59e12edba6d")
	 (revision "0")
	 (version (git-version "0.0.0" revision commit)))
    (package
     (name "breakpad")
     (version version)
     (home-page "https://chromium.googlesource.com/breakpad/breakpad")
     (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url home-page)
		    (commit commit)))
	      (sha256
	       (base32
		"0895aczbpj8gach2x28809v6gz2qjmjwzfynjpvdm81xnvlr2b21"))
	      (file-name (git-file-name name version))))
     (build-system gnu-build-system)
     (synopsis "")
     (description "")
     (license license:bsd-3))))

(define-public nextcloud-desktop
  (package
    (name "nextcloud-desktop")
    (version "2.4.0")
    (home-page "https://nextcloud.com/")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/nextcloud/desktop.git")
		    (commit "9a1f736e306f135628e77bca5c1edeed8b1f3736")
		    (recursive? #t)))
	      (sha256
	       (base32
		"1mmi7gxpn1chdpbzis0avdp7wwm7ig9micm5dp1vmnz5bkf5fvba"))
	      (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DUNIT_TESTING=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("qtkeychain" ,qtkeychain)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (synopsis "")
    (description "")
    (license license:gpl2)))
