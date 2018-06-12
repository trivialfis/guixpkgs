(define-module (network)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python))

(define-public shadowsocks
  (let* ((commit "e332ec93e9c90f1cbee676b022bf2c5d5b7b1239")
         (revision "0")
         (version (git-version "2.8.2" revision commit)))
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
		  "1idd9b4f2pnhcpk1bh030hqg5zq25gkwxd53xi3c0cj242w7sp2j"))
                (file-name (git-file-name name version))))
      (build-system python-build-system)
      (synopsis "Fast tunnel proxy that helps you bypass firewalls")
      (description "
This package is a fast tunnel proxy that helps you bypass firewalls.

Features:
@itemize
@item TCP & UDP support
@item User management API
@item TCP Fast Open
@item Workers and graceful restart
@item Destination IP blacklist
@end itemize")
      (license license:asl2.0))))
