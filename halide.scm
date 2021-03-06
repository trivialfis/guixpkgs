(define-module (halide)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (llvm)
  #:use-module ((guix licenses) #:prefix license:))

(define-public halide-wip
  (let* ((commit "406cd214115d6be939ecc1ef6f9f10729493189c")
	 (revision "0")
	 (version (string-append "2018.02.15" revision commit)))
    (package
      (name "halide-wip")
      (version version)
      (home-page "http://halide-lang.org/")
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/halide/Halide.git")
		      (commit commit)))
		(sha256
		 (base32
		  "0xcavm152w7yr7mdf8wizz3nclmfa15pw82mi3wby3cq9sj79aya"))
		(file-name (git-file-name name version))
		(patches (search-patches "halide-Use-shared-library.patch"))))
      (build-system cmake-build-system)
      (inputs
       `(("llvm" ,llvm)
	 ("clang" ,clang-8.0.1)))
      (arguments
       `(#:configure-flags
	 (list (string-append
		"-DCLANG_BINARY_DIR="
		(assoc-ref %build-inputs "clang")
		"/bin/clang"))))
      (synopsis "Embedded language for fast, portable data-parallel computation")
      (description "Halide is a programming language designed to make it easier
to write high-performance image processing code on modern machines.")
      (license license:asl2.0))))
