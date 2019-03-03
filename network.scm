(define-module (network)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages python))

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
       `(("openssl" ,openssl-next)))
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
       ("openssl", openssl-next)))           ; for 'event_rpcgen.py'
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
