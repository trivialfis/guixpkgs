(define-module (network)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (c)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages libevent)	;libev
  #:use-module (gnu packages crypto))	;libsodium

(define-public shadowsocks
  (let* ((commit "5ff694b2c2978b432918dea6ac104706b25cbf48")
         (revision "0")
         (version (git-version "2.9.1" revision commit)))
    (package
      (name "shadowsocks")
      (version version)
      (home-page "https://github.com/shadowsocks/shadowsocks")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
		  "07g4qdqk93ij0ddhmas2ngpqa354gbnlk45ygy1j7kwsfsc8fimx"))
                (file-name (git-file-name name version))))
      (arguments
       `(#:phases
	 (modify-phases %standard-phases
	   ;; Load openssl from guix.  Currently we just replace every
	   ;; call to `load_openssl' with abs path.
	   (add-after 'unpack 'use-opensslpath
	     (lambda* (#:key inputs outputs #:allow-other-keys)
	       (let* ((out (assoc-ref outputs "out"))
		      (ssldir (assoc-ref inputs "openssl"))
		      (ssl (string-append ssldir "/lib/libcrypto.so"))
		      (files (find-files "." ".*\\.py")))
		 (substitute* files
		   (("load_openssl\\(.*\\)[^:]")
		    (string-append
		     "load_openssl(crypto_path={'openssl': '" ssl "'})\n"))
		   ))
	       #t)))))
      (build-system python-build-system)
      (inputs
       `(("openssl" ,openssl)))
      (synopsis "Fast tunnel proxy that helps you bypass firewalls")
      (description
       "This package is a fast tunnel proxy that helps you bypass firewalls.

Features:
@itemize
@item TCP & UDP support
@item User management API
@item TCP Fast Open
@item Workers and graceful restart
@item Destination IP blacklist
@end itemize")
      (license license:asl2.0))))

(define-public libevent
  (package
    (name "libevent")
    (version "2.1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append
		   "https://github.com/" name "/" name
		   "/archive/release-" version "-beta.tar.gz"))
             (sha256
              (base32
	       "0sg3wc269laqzz7lq069cpdq3ykyhb70agp240g3vvqxmjmm1cha"))))
    (build-system cmake-build-system)
    (arguments
     `(#:parallel-tests? #t))
    (inputs
     `(("python" ,python)
       ("openssl", openssl)))           ; for 'event_rpcgen.py'
    (native-inputs
     `(("which" ,which)))
    (home-page "http://libevent.org/")
    (synopsis "Event notification library")
    (description
     "The libevent API provides a mechanism to execute a callback
function when a specific event occurs on a file descriptor or after a
timeout has been reached.  Furthermore, libevent also support callbacks
due to signals or regular timeouts.

libevent is meant to replace the event loop found in event driven
network servers.  An application just needs to call event_dispatch() and
then add or remove events dynamically without having to change the event
loop.")
    (license license:bsd-3)))

(define-public redsocks
  (package
    (name "redsocks")
    (version "0.5")
    (home-page "http://darkk.net.ru/redsocks/")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://github.com/darkk/"name"/archive/release-"
		    version
		    ".tar.gz"))
	      (sha256
               (base32
		"0qpfl85kxik1mcd4yq3xpbrx6hffmhyp7mhv5w8dg1hrgwfm7sxv"))
	      (file-name (string-append name "-" version ".tar.gz"))
	      (patches (search-patches "redsocks-use-cc.patch"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (delete 'configure)
	 (replace 'install
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (bin (string-append out "/bin"))
		    (etc (string-append out "/etc")))
	       (install-file "redsocks" bin)
	       (install-file "redsocks.service" etc)))))
       #:tests? #f))			; requires docker to build
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)))
    (synopsis "Redirect any TCP connection to SOCKS or HTTPS proxy using your
firewall")
    (description "This tool allows you to redirect any TCP connection to SOCKS
or HTTPS proxy using your firewall, so redirection may be system-wide or
network-wide.  When is redsocks useful?
@itemize

@item You want to route part of TCP traffic via OpenSSH DynamicForward Socks5 port
 using firewall policies. That was original redsocks development goal;

@item You use DVB ISP and this ISP provides internet connectivity with some
special daemon that may be also called \"Internet accelerator\" and the
accelerator acts as a proxy and has no \"transparent proxy\" feature and you
need it. Globax was an example of alike accelerator, but Globax 5 has
transparent proxy feature. That was the second redsocks` development goal;

@item You have to pass traffic through proxy due to corporate network
limitation. That was never a goal for redsocks, but users have reported success
with some proxy configurations.

@end itemize")
    (license license:expat)))

(define-public libudns
  (package
   (name "libudns")
   (version "0.4")
   (home-page "http://www.corpit.ru/mjt/udns.html")
   (source
    (origin
     (method url-fetch)
     (uri "http://www.corpit.ru/mjt/udns/udns-0.4.tar.gz")
     (sha256
      (base32
       "0447fv1hmb44nnchdn6p5pd9b44x8p5jn0ahw6crwbqsg7f0hl8i"))
     (file-name (string-append name "-" version ".tar.gz"))))
   (outputs '("out" "doc"))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
	(replace 'configure
	  (lambda _
	    (invoke "./configure" "--enable-ipv6")))
	(replace 'install
	  (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-inc (string-append out "/include"))
		    (out-bin (string-append out "/bin"))
		    (out-lib (string-append out "/lib"))
		    (doc (assoc-ref outputs "doc"))
		    (doc-man (string-append doc "/man")))
	       (install-file "udns.h" out-inc)
	       (install-file "dnsget" out-bin)
	       (install-file "rblcheck" out-bin)
	       (install-file "libudns.a" out-lib)
	       (install-file "libudns.so.0" out-lib)
	       (symlink (string-append out-lib "/libudns.so.0")
			(string-append out-lib "/libudns.so"))
	       (install-file "dnsget.1" (string-append doc-man "/man1"))
	       (install-file "rblcheck.1" (string-append doc-man "/man1"))
	       (install-file "udns.3" (string-append doc-man "/man3"))))))
      #:make-flags (list "staticlib" "sharedlib" "rblcheck" "dnsget")
      ;; no test target
      #:tests? #f))
   (build-system gnu-build-system)
   (synopsis "")
   (description "")
   (license license:lgpl2.0+)))

(define-public libipset
  (let* ((commit "7f9c44ffe405b015d6ab9ca8359a62c09ba17f4d")
	 (revision "0")
	 (version (git-version "1.1.1" revision commit)))
    (package
      (name "libipset")
      (version version)
      (home-page "https://github.com/shadowsocks/ipset")
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url home-page)
	       (commit commit)))
	 (sha256
	  (base32
	   "00hlfpl8j6i4fc5j0q84rd46qabamwml17p0sva5dnrbzlj0ciri"))))
      (native-inputs `(("pkg-config" ,pkg-config)
		       ("check" ,check)))
      (inputs `(("libcork" ,libcork)))
      (build-system cmake-build-system)
      (synopsis "small C helper library for storing sets of IPv4 and IPv6 addresses")
      (description "")
      (license license:bsd-3))))

;; Not working yet.
(define-public shadowsocks-libev
  (let* ((commit "c9159fc927e643f38bf60c2ded443fb1b6c70c51")
	 (revision "0")
	 (version (git-version "3.2.4" revision commit)))
    (package
     (name "shadowsocks-libev")
     (version version)
     (home-page "https://github.com/shadowsocks/shadowsocks-libev/")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)
	     (recursive? #f)))
       (sha256
	(base32
	 "1l7p7hks0m01nyg8k42dy7bazl6jih7h64rwp9gw3dpq23kkmwwy"))
       (patches (search-patches "shadowsocks-libev-add-find-correct-ipset.patch"
				"shadowsocks-libev-add-library-paths.patch"
				"shadowsocks-libev-remove-not-relevant-code.patch"
				"shadowsocks-libev-remove-more-non-relevant-code.patch"
				"shadowsocks-libev-install-appropriate-target.patch"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
	      (snippet
	       '(begin
		  (delete-file-recursively "libcork")
		  (delete-file-recursively "libbloom")
		  (delete-file-recursively "libipset")))))
     (build-system cmake-build-system)
     (synopsis "Fast tunnel proxy that helps you bypass firewalls")
     (arguments
      `(#:configure-flags
	(list "-DWITH_STATIC=OFF"
	      "-DWITH_EMBEDDED_SRC=OFF")
	#:tests? #f))
     (native-inputs
      `(("pkg-config", pkg-config)))
     (inputs
      `(("libev" ,libev)
	("c-ares" ,c-ares)
	("mbedtls-apache" ,mbedtls-apache)
	("libsodium" ,libsodium)
	("libipset" ,libipset)
	("libbloom" ,libbloom)
	("libcork" ,libcork)
	("pcre" ,pcre)))
     (description
      "This package is a fast tunnel proxy that helps you bypass firewalls.

Features:
@itemize
@item TCP & UDP support
@item User management API
@item TCP Fast Open
@item Workers and graceful restart
@item Destination IP blacklist
@end itemize")
     (license license:gpl3+))))
