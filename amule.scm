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

(define-module (amule)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages image)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl))

(define-public amule
  (package
    (name "amule")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/amule-project/amule/archive/"
                    version
                    ".tar.gz"))
              (file-name (string-append "amule-" version ".tar.gz"))
              (sha256
               (base32
                "1wvcj0n9xz03xz5c2xwp6dwfp7sqjhhwbki3m0lwikskpn9lkzk2"))
	      ;; Patch for adopting crypto++ >= 6.0
	      (patches (search-patches "Amule_Crypto-6.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'autogen
           (lambda _
	     (invoke "sh" "autogen.sh"))))
       #:configure-flags
       '("--disable-rpath"
         "--enable-wxcas"
         "--enable-cas"
         "--enable-alc"
         "--enable-alcc"
         "--enable-xas"
         "--enable-amulecmd"
         "--enable-geoip"
         "--enable-ccache"
         "--enable-nls"
         "--enable-optimize"
         "--enable-amule-gui"
         "--enable-amule-daemon"
         "--enable-webserver"
         "--with-denoise-level=0")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("perl" ,perl)))
    (inputs
     `(("zlib" ,zlib)
       ("crypto++" ,crypto++)
       ("libpng" ,libpng)
       ("wxwidgets-gtk2", wxwidgets-gtk2)))
    (home-page "http://amule.org/")
    (synopsis "Peer-to-peer client for the eD2K and Kademlia networks")
    (description
     "aMule is an eMule-like client for the eD2k and Kademlia
networks, supporting multiple platforms.  Currently aMule
(officially) supports a wide variety of platforms and operating
systems, being compatible with more than 60 different
hardware+OS configurations.  aMule is entirely free, its
sourcecode released under the GPL just like eMule, and includes
no adware or spyware as is often found in proprietary P2P
applications.")
    (license license:gpl2+)))
