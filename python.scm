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

(define-module (python)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages databases))

(define-public python-nose-warnings-filters
  (package
   (name "python-nose-warnings-filters")
   (version "0.1.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "nose_warnings_filters" version))
     (sha256
      (base32
       "17dvfqfy2fm7a5cmiffw2dc3064kpx72fn5mlw01skm2rhn5nv25"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-nose" ,python-nose)))
   (home-page "https://github.com/Carreau/nose_warnings_filters")
   (synopsis
    "Allow to inject warning filters during ``nosetest``.")
   (description
    "Allow to inject warning filters during ``nosetest``.")
   (license license:expat)))

(define-public python-backcall
  (package
    (name "python-backcall")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backcall" version))
       (sha256
        (base32
         "1r01dqch3f8fdj3n6fviw8hxqrs6w5v0qw4izmvqzry1w9dxiv1q"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (replace 'check
	   (lambda _
	     (invoke "py.test"))))))
    (home-page
     "https://github.com/takluyver/backcall")
    (synopsis
     "Specifications for callback functions passed in to an API")
    (description
     "Specifications for callback functions passed in to an API")
    (license license:bsd-3)))

(use-modules (gnu packages image)
	     (gnu packages imagemagick)
	     (gnu packages fontutils)
	     (gnu packages gtk)
	     (gnu packages glib)
	     (gnu packages tcl)
	     (gnu packages maths)
	     (gnu packages time)
	     (gnu packages pkg-config))

(define-public python-matplotlib
  (package
    (name "python-matplotlib")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matplotlib" version))
       (sha256
        (base32
	 "1rcc7x9ig3hpchkc4cwdvym3y451w74275fxr455zkfagrsvymbk"))))
    (build-system python-build-system)
    (propagated-inputs ; the following packages are all needed at run time
     `(("python-cycler" ,python-cycler)
       ("python-kiwisolver" ,python-kiwisolver)
       ("python-pyparsing" ,python-pyparsing)
       ("python-pygobject" ,python-pygobject)
       ("gobject-introspection" ,gobject-introspection)
       ("python-tkinter" ,python "tk")
       ("python-dateutil" ,python-dateutil)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pytz" ,python-pytz)
       ("python-six" ,python-six)
       ;; The 'gtk+' package (and 'gdk-pixbuf', 'atk' and 'pango' propagated
       ;; from 'gtk+') provides the required 'typelib' files used by
       ;; 'gobject-introspection'. The location of these files is set with the
       ;; help of the environment variable GI_TYPELIB_PATH. At build time this
       ;; is done automatically by a 'native-search-path' procedure. However,
       ;; at run-time the user must set this variable as follows:
       ;;
       ;; export GI_TYPELIB_PATH=~/.guix-profile/lib/girepository-1.0
       ("gtk+" ,gtk+)
       ;; From version 1.4.0 'matplotlib' makes use of 'cairocffi' instead of
       ;; 'pycairo'. However, 'pygobject' makes use of a 'pycairo' 'context'
       ;; object. For this reason we need to import both libraries.
       ;; https://pythonhosted.org/cairocffi/cffi_api.html#converting-pycairo
       ("python-pycairo" ,python-pycairo)
       ("python-cairocffi" ,python-cairocffi)))
    (inputs
     `(("libpng" ,libpng)
       ("imagemagick" ,imagemagick)
       ("freetype" ,freetype)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ;; FIXME: Add backends when available.
       ;("python-wxpython" ,python-wxpython)
       ("tcl" ,tcl)
       ("tk" ,tk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure-environment
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((cairo (assoc-ref inputs "cairo"))
                   (gtk+ (assoc-ref inputs "gtk+")))
               ;; Setting these directories in the 'basedirlist' of 'setup.cfg'
               ;; has not effect.
               (setenv "LD_LIBRARY_PATH"
                       (string-append cairo "/lib:" gtk+ "/lib"))
               (setenv "HOME" (getcwd))
               (call-with-output-file "setup.cfg"
                 (lambda (port)
                   (format port "[directories]~%
basedirlist = ~a,~a~%
 [rc_options]~%
backend = TkAgg~%"
                        (assoc-ref inputs "tcl")
                        (assoc-ref inputs "tk")))))
             #t)))))
    (home-page "http://matplotlib.org")
    (synopsis "2D plotting library for Python")
    (description
     "Matplotlib is a Python 2D plotting library which produces publication
quality figures in a variety of hardcopy formats and interactive environments
across platforms.  Matplotlib can be used in Python scripts, the python and
ipython shell, web application servers, and six graphical user interface
toolkits.")
    (license license:psfl)
    (properties `((python2-variant . ,(delay python2-matplotlib))))))

;; Enabled tests.
(define-public python-decorator
  (package
    (name "python-decorator")
    (version "4.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "decorator" version))
       (sha256
	(base32 "03iaf116rm3w8b4agb8hzf6z9331mrvi4khfxq35zkx17sgxsikx"))))
    (build-system python-build-system)
    (home-page "https://github.com/micheles/decorator")
    (synopsis "Python module to simplify usage of decorators")
    (description
     "The aim of the decorator module is to simplify the usage of decorators
for the average programmer, and to popularize decorators usage giving examples
of useful decorators, such as memoize, tracing, redirecting_stdout, locked,
etc.  The core of this module is a decorator factory.")
    (license license:expat)))

(define-public python-ipython-genutils
  ;; TODO: This package is retired, check if can be removed, see description.
  (package
    (name "python-ipython-genutils")
    (version "0.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/i/"
                          "ipython_genutils/ipython_genutils-"
                          version ".tar.gz"))
      (sha256
       (base32
	"1a4bc9y8hnvq6cp08qs4mckgm6i6ajpndp4g496rvvzcfmp12bpb"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://ipython.org")
    (synopsis "Vestigial utilities from IPython")
    (description
     "This package provides retired utilities from IPython.  No packages
outside IPython/Jupyter should depend on it.

This package shouldn't exist.  It contains some common utilities shared by
Jupyter and IPython projects during The Big Split.  As soon as possible, those
packages will remove their dependency on this, and this package will go
away.")
    (license license:bsd-3)))

(define-public python-traitlets
  (package
    (name "python-traitlets")
    (version "4.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "traitlets" version))
       (sha256
        (base32
	 "0dbq7sx26xqz5ixs711k5nc88p8a0nqyz6162pwks5dpcz9d4jww"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-decorator" ,python-decorator)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://ipython.org")
    (synopsis "Configuration system for Python applications")
    (description
     "Traitlets is a framework that lets Python classes have attributes with
type checking, dynamically calculated default values, and ‘on change’
callbacks.  The package also includes a mechanism to use traitlets for
configuration, loading values from files or from command line arguments.  This
is a distinct layer on top of traitlets, so you can use traitlets in your code
without using the configuration machinery.")
    (license license:bsd-3)))

(use-modules (gnu packages networking))
(define-public python-pyzmq
  (package
    (name "python-pyzmq")
    (version "17.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyzmq" version))
       (sha256
        (base32 "1pyxxrz60f88ffm0y6vpbx3q8jcr9ybz8fcilihwzwhh36n84ax7"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--zmq=" (assoc-ref %build-inputs "zeromq")))
       ;; FIXME: You must build pyzmq with 'python setup.py build_ext
       ;; --inplace' for 'python setup.py test' to work.
       #:tests? #f))
    (inputs
     `(("zeromq" ,zeromq)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/zeromq/pyzmq")
    (synopsis "Python bindings for 0MQ")
    (description
     "PyZMQ is the official Python binding for the ZeroMQ messaging library.")
    (license license:bsd-4)))

(use-modules (gnu packages python-crypto))
(define-public python-tornado
  (package
    (name "python-tornado")
    (version "5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32
	 "10hmh387ycfrpgfp1r4bz9q1q7qk34z2qk6ajdqkhixr5hbs4rjg"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: Two tests error out with:
       ;; AssertionError: b'Error in atexit._run_exitfuncs:\nFileNotF[44 chars]ry\n' != b''
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       ;; 'setup.py test' hits an AssertionError on BSD-specific
       ;;       ;; "tornado/platform/kqueue.py". This is the supported method:
       ;;       (invoke- "python" "-m" "tornado.test")
       ;;       #t)))
       ;; Building with tests failed.
       #:tests? #f))
    (native-inputs
     `(("python-certifi" ,python-certifi)))
    (home-page "http://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))

(define-private python-ipython-bootstrap
  (package
    (name "python-ipython-bootstrap")
    (version "6.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipython" version))
       (sha256
        (base32
         "196m8y4wjll0bwk29zahbh1l1j1m9zg6s2z17vv8x9m4mngfzwmh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pexpect", python-pexpect)
       ("python-jedi", python-jedi)
       ("python-prompt-toolkit", python-prompt-toolkit)
       ("python-decorator", python-decorator)
       ("python-simplegeneric", python-simplegeneric)
       ("python-pygments" ,python-pygments)
       ("python-backcall" ,python-backcall)
       ("python-pickleshare" ,python-pickleshare)
       ("python-traitlets" ,python-traitlets)))
    (arguments
     `(#:tests? #f))
    (home-page "https://ipython.org")
    (synopsis
     "IPython: Productive Interactive Computing")
    (description
     "IPython: Productive Interactive Computing")
    (license license:bsd-3)))

(define-public python-ipykernel-bootstrap
  ;; build_ext runs tests automatically
  (package
    (name "python-ipykernel-bootstrap")
    (version "4.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/ipython/ipykernel/archive/"
	     version ".tar.gz"))
       (sha256
	(base32
	 "1cyd7629whjy74yszi0y8nag0xx4f1bwlib4ddza80y3c42kwq4d"))))
    (build-system python-build-system)
    ;; (native-inputs
    ;;  `(("python-nose" ,python-nose)
    ;;    ("python-nose-warnings-filters" ,python-nose-warnings-filters)))
    ;; (propagated-inputs
    ;;  `(("python-ipython-bootstrap" ,python-ipython-bootstrap)))
    ;; The tests load a submodule of IPython.  However, IPython itself depends
    ;; on ipykernel.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (delete 'check))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     `(("python-ipython-bootstrap" ,python-ipython-bootstrap)
       ("python-jupyter-client" ,python-jupyter-client)))
    (home-page "https://ipython.org")
    (synopsis "IPython Kernel for Jupyter")
    (description
     "This package provides the IPython kernel for Jupyter.")
    (license license:bsd-3)))

(define-public python-jupyter-client
  (package
    (name "python-jupyter-client")
    (version "5.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_client" version))
       (sha256
        (base32
	 "0qdpcqs7r8fw38mjbqllgxyss0fi5n86lk81afcf40bb8kqgrgi7"))))
    (build-system python-build-system)
    ;; Tests fail because of missing native python kernel which I assume is
    ;; provided by the ipython package, which we cannot use because it would
    ;; cause a dependency cycle.
    ;; (arguments `(#:tests? #f))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-msgpack" ,python-msgpack)
       ("python-ipython-bootstrap" ,python-ipython-bootstrap)
       ))
    (propagated-inputs
     `(("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)
       ("python-tornado" ,python-tornado)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-dateutil" ,python-dateutil)))
    (home-page "http://jupyter.org/")
    (synopsis "Jupyter protocol implementation and client libraries")
    (description
     "The @code{jupyter_client} package contains the reference implementation
of the Jupyter protocol.  It also provides client and kernel management APIs
for working with kernels, and the @code{jupyter kernelspec} entrypoint for
installing @code{kernelspec}s for use with Jupyter frontends.")
    (license license:bsd-3)))

(define-public python-ipython
  (package
    (inherit python-ipython-bootstrap)
    (name "python-ipython")
    (build-system python-build-system)
    (native-inputs
     `(("python-parso" ,python-parso)
       ("python-pytest" ,python-pytest)
       ("python-nose" ,python-nose)
       ("python-testpath" ,python-testpath)
       ("python-matplotlib" ,python-matplotlib)
       ("python-nbformat" ,python-nbformat)))
    (propagated-inputs
     `(,@(package-propagated-inputs python-ipython-bootstrap)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (with-directory-excursion "/tmp"
               (let* ((out (assoc-ref outputs "out"))
                      (iptest (string-append out "/bin/iptest")))
                 ;; Make installed package available for running the tests
                 (add-installed-pythonpath inputs outputs)
                 (setenv "HOME" "/tmp/") ;; required by a test
                 (invoke iptest "core"))))))))
    (home-page "https://ipython.org")
    (synopsis
     "IPython: Productive Interactive Computing")
    (description
     "IPython: Productive Interactive Computing")
    (license license:bsd-3)))
