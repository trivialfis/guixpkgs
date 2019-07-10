(define-module (font)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system font)
  #:use-module (gnu packages fontutils))

(define-public NotoSansCJK-Regular
  (package
   (name "NotoSansCJK-Regular")
   (version "2017-10-24")
   (home-page "https://www.google.com/get/noto/help/cjk/")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://noto-website-2.storage.googleapis.com/pkgs/NotoSerifCJK-Regular.ttc.zip"))
	    (sha256
	     (base32
	      "0n4naxidqjw0pda1sx2k71lq0d4084bk15nx5gby5iwlg12y00sc"))
	    (file-name (string-append name "-" version ".zip"))))
   (build-system font-build-system)
   (synopsis "")
   (description"")
   (license license:silofl1.1)))
