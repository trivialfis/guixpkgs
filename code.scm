(define-module (code)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public amalgamate
  (let* ((commit "c91f07eea1133aa184f652b8f1398eaf03586208")
         (revision "0")
         (version (string-append "1.1.1" revision commit)))
    (package
      (name "amalgamate")
      (version version)
      (home-page "https://github.com/edlund/amalgamate")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (sha256
          (base32
           "0cllaraw8mxs8q2nr28nhgzkb417gj2wcklqg59w84f4lc78k3yb"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(substitute* "test.sh"
             (("test_command \"cc -Wall -Wextra -o source.out source.c\"" all)
              "test_command \"gcc -Wall -Wextra -o source.out source.c\"")))))
      (build-system gnu-build-system)
      (inputs
       `(("python" ,python-wrapper)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir out)
                 (mkdir bin)
                 (copy-file "amalgamate.py"
                            (string-append bin "/amalgamate.py")))))
           (replace 'check
             (lambda _
               (invoke "./test.sh"))))))
      (synopsis "Tool for amalgamating C source and header files.")
      (description "amalgamate.py aims to make it easy to use SQLite-style C
source and header amalgamation in projects.")
      (license license:bsd-3))))

(define-public universal-ctags
  (let* ((commit "6f7654b98be0dd9a15c539882ab7ea3914ab7bf8")
         (revision "0")
         (version (string-append "1.1.1" revision commit)))
    (package
      (name "universal-ctags")
      (version version)
      (home-page "https://github.com/universal-ctags/ctags")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0xghdvjadcwm9agzxzv9rvlmkyn2gjf860ffdp8s6y7m2frlsl3y"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("perl" ,perl)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
	   (delete 'bootstrap)
           (add-before 'configure 'auto-gen
             (lambda _
               (invoke "./autogen.sh")))
           (add-before 'auto-gen 'make-files-writable
             (lambda _
               (with-directory-excursion "./optlib"
                 (for-each (lambda (file) (chmod file #o644))
                           (find-files "." "\\.c"))))))
         #:tests? #f))                  ; FIXME: Disable known bug tests.
      (synopsis "A maintained ctags implementation")
      (description "universal-ctags has the objective of continuing the
development from what existed in the Sourceforge area.  The goal of the
project is preparing and maintaining common/unified working space where people
interested in making ctags better can work together.")
      (license license:gpl2+))))
