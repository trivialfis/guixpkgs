(define-module (python)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python))

(define-public python-yapf
  (package
    (name "python-yapf")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yapf" version))
       (sha256
        (base32
         "144gc7d6b1415vh02409rnb8qd5kxi6mxlr7y64d5cizgxbf72kx"))))
    (build-system python-build-system)
    (home-page "https://github.com/google/yapf")
    (synopsis "Formatter for Python code")
    (description "YAPF is a formatter for Python code.  It's based off of
@dfn{clang-format}, developed by Daniel Jasper.  In essence, the algorithm
takes the code and reformats it to the best formatting that conforms to the
style guide, even if the original code didn't violate the style guide.")
    (license license:asl2.0)))

(define-public python2-yapf
  (package-with-python2 python-yapf))

(define-public python-parso
  (package
    (name "python-parso")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parso" version))
       (sha256
        (base32
         "0lamywk6dm5xshlkdvxxf5j6fa2k2zpi7xagf0bwidaay3vnpgb2"))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (build-system python-build-system)
    (home-page
     "https://github.com/davidhalter/parso")
    (synopsis "A Python Parser")
    (description "A Python Parser")
    (license license:expat)))

(define-public python2-parso
  (package-with-python2 python-parso))


(define-public python-jedi
  (package
    (name "python-jedi")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jedi" version))
       (sha256
        (base32
         "1bcr7csx4xil1iwmk03d79jis0bkmgi9k0kir3xa4rmwqsagcwhr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _
                           (invoke "py.test" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-parso" ,python-parso)
       ("python-docopt" ,python-docopt)))
    (home-page "https://github.com/davidhalter/jedi")
    (synopsis
     "Autocompletion for Python that can be used for text editors")
    (description
     "Jedi is an autocompletion tool for Python that can be used for text
 editors.")
    (license license:expat)))

(define-public python2-jedi
  (package-with-python2 python-jedi))

(define-public python-autopep8
  (package
  (name "python-autopep8")
  (version "1.3.5")
  (source
   (origin
     (method url-fetch)
     (uri (pypi-uri "autopep8" version))
     (sha256
      (base32
       "192bvhzi4d0claqxgzymvv7k3qnj627742bc8sgxpzjj42pd9112"))))
  (build-system python-build-system)
  (propagated-inputs
    `(("python-pycodestyle" ,python-pycodestyle)))
  (home-page "https://github.com/hhatto/autopep8")
  (synopsis "Format Python code according to the PEP 8 style guide")
  (description
    "@code{autopep8} automatically formats Python code to conform to
the PEP 8 style guide.  It uses the pycodestyle utility to determine
what parts of the code needs to be formatted.  @code{autopep8} is
capable of fixing most of the formatting issues that can be reported
by pycodestyle.")
  (license (license:non-copyleft
            "https://github.com/hhatto/autopep8/blob/master/LICENSE"))))