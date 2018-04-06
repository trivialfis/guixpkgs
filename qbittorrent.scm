(define-module (qbittorrent)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages tls))

;; For qBittorrent
;; see  https://github.com/qbittorrent/qBittorrent/issues/8402
;; and  https://github.com/qbittorrent/qBittorrent/issues/5265
(define-public libtorrent-rasterbar-c++11
  (package
    (inherit libtorrent-rasterbar)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
			    (assoc-ref %build-inputs "boost")
			    "/lib")
	     "--enable-python-binding"
	     "--enable-tests"
	     "CXXFLAGS=-std=c++11")	; std::chrono
       #:make-flags (list
                     (string-append "LDFLAGS=-Wl,-rpath="
                                    (assoc-ref %outputs "out") "/lib"))))))

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
	      "145r4lv7rqdhrm5znn3ndxsfdf579n46zvj7c53c422am8ir5xhp"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list
       (string-append "--with-boost-libdir="
                      (assoc-ref %build-inputs "boost")
                      "/lib")
       "--enable-debug"
       ;; "CXXFLAGS=-std=c++11"
       "QMAKE_LRELEASE=lrelease")))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("qttools" ,qttools)))
   (inputs
    `(("boost" ,boost)
      ("libtorrent-rasterbar" ,libtorrent-rasterbar-c++11)
      ("openssl" ,openssl)
      ("python" ,python)
      ("qtbase" ,qtbase)
      ("qtsvg" ,qtsvg)
      ("zlib" ,zlib)))
   (home-page "https://www.qbittorrent.org/")
   (synopsis "qBittorrent BitTorrent client.")
   (description
    "qBittorrent is a bittorrent client programmed in C++ / Qt that uses
 libtorrent (sometimes called libtorrent-rasterbar) by Arvid Norberg.

It aims to be a good alternative to all other bittorrent clients out there.
 qBittorrent is fast, stable and provides unicode support as well as many
 features.")
   ;; FIXME, additional OpenSSL license
   (license license:gpl2+)))
