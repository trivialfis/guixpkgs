(define-module (video)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages swig))

(define-public libopenshot-audio
  (package
    (name "libopenshot-audio")
    (version "0.1.4")
    (source
     (origin (method url-fetch)
             (uri
              (string-append
               "https://github.com/OpenShot/libopenshot-audio/archive/v"
               version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (modules '((guix build utils)))
             (snippet
              '(with-directory-excursion "JuceLibraryCode/modules/juce_graphics/"
                 (let ((files (find-files "." ".*\\.cpp|.*\\.h")))
                   (display "find-files\n")
                   (display files)
                   (substitute* files
                     (("#include ?<(ft2build\\.h)>" all header)
                      (string-append "#include <freetype2/" header ">")))
                   #t)))
             (sha256
              (base32
               "1hd1h6r2z7brrjr41kllpvyha70209hyfnbfk04i17s8cn5g6qcy"))))
    (arguments
     `(#:configure-flags
       (list (string-append "-DCMAKE_CXX_FLAGS=-I"
                            (string-append
                             (assoc-ref %build-inputs "freetype")
                             "/include/freetype2")))
       #:tests? #f))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxcursor" ,libxcursor)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)))
    (build-system cmake-build-system)
    (home-page "http://www.openshot.org/")
    (synopsis "High-quality editing and playback of audio")
    (description "program that allows the  high-quality editing and playback of
audio, and is based on the amazing JUCE library.")
    (license license:gpl3+)))
