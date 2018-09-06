(define-module (networking)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix l:))

(define-public qbittorrent
  (package
    (name "qbittorrent")
    (version "4.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://github.com/qbittorrent/qBittorrent/archive/release-"
		    version ".tar.gz"))
              (file-name (string-append name "-release-" version ".tar.gz"))
              (sha256
               (base32
		"00vq6ik8vwh3xsz20fgnxmk4as2pza72pz7r3132n49d7l7j3igz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--enable-debug"
             "QMAKE_LRELEASE=lrelease")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("libtorrent-rasterbar" ,libtorrent-rasterbar)
       ("openssl" ,openssl)
       ("python" ,python)		; for the search engine
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (home-page "https://www.qbittorrent.org/")
    (synopsis "Graphical BitTorrent client")
    (description
     "qBittorrent is a BitTorrent client programmed in C++/Qt that uses
libtorrent (sometimes called libtorrent-rasterbar) by Arvid Norberg.

It aims to be a good alternative to all other BitTorrent clients out there.
qBittorrent is fast, stable and provides unicode support as well as many
features.")
    (license l:gpl2+)))
