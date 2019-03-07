(define-module (nih)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check))

(define-public nih
  (let* ((commit "c5db5a8c3ba3b817af33f6be92d6a6e6ef6e3c2a")
	 (revision "0")
	 (version (git-version "0.0.0" revision commit)))
    (package
      (name "nih")
      (version version)
      (home-page "https://github.com/trivialfis/nih")
      (source (origin
		(method git-fetch)
		(uri (git-reference
                      (url home-page)
                      (commit commit)))
		(sha256
		 (base32
		  "0vs06d8mr4xz5q9d9lkw670kq4hvgzrcsrb65gd18kfqrn1w42la"))
		(file-name (git-file-name name version))))
      (native-inputs
       `(("googletest" ,googletest)))
      (build-system cmake-build-system)
      (synopsis "")
      (description "")
      (license license:lgpl3+))))
