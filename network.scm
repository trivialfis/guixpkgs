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
