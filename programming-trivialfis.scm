(define-module (programming-trivialfis)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages code)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python)
  #:use-module (emacs))

(define-public trivialfis/basic
  (package
    (name "basic-programming")
    (version "0.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules
       ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (let ((out (assoc-ref %outputs "out")))
                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))))))
    (inputs
     `(("coreutils" ,coreutils)
       ("emacs-trivialfis" ,emacs-trivialfis)
       ("findutils" ,findutils)
       ("glibc-locales" ,glibc-locales)
       ("grep" ,grep)
       ("nautilus" ,nautilus)
       ("procps" ,procps)
       ("sed" ,sed)
       ("which" ,which)))
    (native-search-paths (list (search-path-specification
                                (variable "GUIX_LOCPATH")
                                (files '("lib/locale")))))
    (home-page "None")
    (synopsis "None")
    (description "None")
    (license license:gpl3+)))

(define-public trivialfis/python
  (package
    (name "python-programming")
    (version "0.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules
       ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (let ((out (assoc-ref %outputs "out")))
                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))))))
    (inputs
     `(("python" ,python)
       ("python-autopep8" ,python-autopep8)
       ("python-flake8" ,python-flake8)
       ("python-jedi" ,python-jedi)
       ("python-yapf" ,python-yapf)

       ("basic-programming" ,trivialfis/basic)))
    (native-search-paths (append (package-native-search-paths python)
				 (package-native-search-paths trivialfis/basic)))
    (home-page "None")
    (synopsis "Basic programming tools for python")
    (description "Basic programming tools for python.")
    (license license:gpl3+)))

(define-public trivialfis/c++
  (package
    (name "cxx-programming")
    (version "0.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules
       ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (let ((out (assoc-ref %outputs "out")))
                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))))))
    (inputs
     `(("gcc-toolchain" ,gcc-toolchain)
       ("cmake" ,cmake)
       ("make" ,gnu-make)
       ("global" ,global)
       ("meson" ,meson)
       ("ninja" ,ninja)
       ("rtags" ,rtags)
       ("gdb" ,gdb)

       ("basic-programming" ,trivialfis/basic)))
    (native-search-paths (append (package-native-search-paths gcc-toolchain)
                                 (package-native-search-paths python)
				 (package-native-search-paths trivialfis/basic)))
    (home-page "None")
    (synopsis "Basic programming tools for c/c++")
    (description "Basic programming tools for c/c++.")
    (license license:gpl3+)))
