(define-module (qbittorrent)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bittorrent))

(define-public qBittorrent
  (package
   (name "qBittorrent")
   (version "4.0.4")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://github.com/qbittorrent/qBittorrent/archive/release-"
		  version
		  ".tar.gz"))
	    (file-name (string-append
			name "-release-" version ".tar.gz"))
	    (sha256
	     (base32
	      "18wlcsgappiy6xk700w0g8pspj5hd1d31z5hl37lhkzdxaazmcwv"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list (string-append "--with-boost-libdir="
                           (assoc-ref %build-inputs "boost")
                           "/lib")
     	    ;; (string-append "ac_cv_path_LRELEASE="
            ;;                (assoc-ref %build-inputs "qttools")
            ;;                "/bin/lrelease")
	    "QMAKE_LRELEASE=lrelease")
      ))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("qttools" ,qttools)))
   (inputs
    `(("qtbase" ,qtbase)
      ("zlib" ,zlib)
      ("qtsvg" ,qtsvg)
      ("boost" ,boost)			;libtorrent-rasterbar
      ("libtorrent-rasterbar" ,libtorrent-rasterbar)))
   (home-page "https://www.qbittorrent.org/")
   (synopsis "qBittorrent BitTorrent client")
   (description
    "qBittorrent is a bittorrent client programmed in C++ / Qt that uses
 libtorrent (sometimes called libtorrent-rasterbar) by Arvid Norberg.

It aims to be a good alternative to all other bittorrent clients out there.
 qBittorrent is fast, stable and provides unicode support as well as many
 features.")
   ;; FIXME, additional OpenSSL license
   (license license:gpl2+))
  )
