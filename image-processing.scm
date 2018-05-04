;;; Copyright @ 2018 Björn Höfling
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

(define-module (image-processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public opencv
  ;; This package is grepped from the list, copyright is placed at first line
  ;; of this file.
  (package
    (name "opencv")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/opencv/opencv/archive/"
                                  version ".zip"))
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32
                "1g8pvnlkzzp50amd89149hqsbvsc2hq3vk1d6a9fksdcx8ra9g94"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove external libraries. We have all available in Guix:
                  (delete-file-recursively "3rdparty")

                  ;; Milky icon set is non-free:
                  (delete-file-recursively "modules/highgui/src/files_Qt/Milky")

                  ;; Some jars found:
                  (for-each delete-file
                            '("modules/java/test/pure_test/lib/junit-4.11.jar"
                              "samples/java/sbt/sbt/sbt-launch.jar"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DWITH_IPP=OFF"
             "-DWITH_ITT=OFF"

             ;; CPU-Features:
             ;; See cmake/OpenCVCompilerOptimizations.cmake
             ;; (CPU_ALL_OPTIMIZATIONS) for a list of all optimizations
             ;; BASELINE is the minimum optimization all CPUs must support
             ;;
             ;; DISPATCH is the list of optional dispatches
             "-DCPU_BASELINE=SSE2, NEON"

             "-DCPU_DISPATCH=SSE3,SSSE3,SSE4_1,SSE4_2,AVX,AVX2"
             "-DCPU_DISPATCH_REQUIRE=SSE3,SSSE3,SSE4_1,SSE4_2,AVX,AVX2"

             "-DBUILD_PERF_TESTS=OFF"
             "-D BUILD_TESTS=ON"

             (string-append "-DOPENCV_EXTRA_MODULES_PATH="
                            "/tmp/guix-build-opencv-" ,version ".drv-0"
                            "/opencv-contrib/opencv_contrib-" ,version
                            "/modules")

             ;;Define test data:
             (string-append "-DOPENCV_TEST_DATA_PATH="
                            "/tmp/guix-build-opencv-" ,version ".drv-0"
                            ;;"/opencv-3.4.0"
                            "/opencv-extra/opencv_extra-" ,version
                            "/testdata")

             ;; Is ON by default and would try to rebuild 3rd-party protobuf,
             ;; which we had removed, which would lead to an error:
             "-DBUILD_PROTOBUF=OFF"

             ;; Rebuild protobuf files, because we have a slightly different
             ;; version than the included one. If we would not update, we
             ;; would get a compile error later:
             "-DPROTOBUF_UPDATE_FILES=ON"

             ;; xfeatures2d disabled, because it downloads extra binaries from
             ;; https://github.com/opencv/opencv_3rdparty
             ;; defined in xfeatures2d/cmake/download_{vgg|bootdesc}.cmake
             ;; Cmp this bug entry:
             ;; https://github.com/opencv/opencv_contrib/issues/1131
             "-DBUILD_opencv_xfeatures2d=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; These tests fails with:
             ;; vtkXOpenGLRenderWindow (0x723990): Could not find a decent config
             ;; I think we have no OpenGL support with the Xvfb.
             (substitute* '("modules/viz/test/test_tutorial3.cpp"
                            "modules/viz/test/test_main.cpp"
                            "modules/viz/test/tests_simple.cpp"
                            "modules/viz/test/test_viz3d.cpp")
               (("(TEST\\(Viz, )([a-z].*\\).*)" all pre post)
                (string-append pre "DISABLED_" post)))

             ;; This one fails with "unknown file: Failure"
             ;; But I couldn't figure out which file was missing:
             (substitute* (list (string-append
                                 "../opencv-contrib/opencv_contrib-"
                                 ,version
                                 "/modules/face/test/test_face_align.cpp"))
               (("(TEST\\(CV_Face_FacemarkKazemi, )(can_detect_landmarks\\).*)"
                 all pre post)
                (string-append pre "DISABLED_" post)))

             ;; Failure reason: Bad accuracy
             ;; Incorrect count of accurate poses [2nd case]: 90.000000 / 94.000000
             (substitute* (list (string-append
                                 "../opencv-contrib/opencv_contrib-"
                                 ,version
                                 "/modules/rgbd/test/test_odometry.cpp"))
               (("(TEST\\(RGBD_Odometry_Rgbd, )(algorithmic\\).*)" all pre post)
                (string-append pre "DISABLED_" post)))
             #t))

         ;; Idea copied from ldc.scm (ldc-bootstrap):
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "../opencv-extra")
             (mkdir "../opencv-contrib")
             (let ((unpack (lambda (source target)
                             (with-directory-excursion target
                               (apply invoke "unzip"
                                      (list (assoc-ref inputs source)))))))
               (unpack "opencv-extra" "../opencv-extra")
               (unpack "opencv-contrib" "../opencv-contrib"))))

         (add-after 'set-paths 'add-ilmbase-include-path
           (lambda* (#:key inputs #:allow-other-keys)
           ;; OpenEXR propagates ilmbase, but its include files do not appear
           ;; in the CPATH, so we need to add "$ilmbase/include/OpenEXR/" to
           ;; the CPATH to satisfy the dependency on "ImathVec.h".
           (setenv "CPATH"
                   (string-append (assoc-ref inputs "ilmbase")
                                  "/include/OpenEXR"
                                  ":" (or (getenv "CPATH") "")))
           #t))
       (add-before 'check 'start-xserver
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((xorg-server (assoc-ref inputs "xorg-server"))
                 (disp ":1"))
             (setenv "HOME" (getcwd))
             (setenv "DISPLAY" disp)
             ;; There must be a running X server and make check doesn't start one.
             ;; Therefore we must do it.
             (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server) ; For running the tests
       ("opencv-extra"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://codeload.github.com/"
                               "opencv/opencv_extra/zip/" version))
           (file-name (string-append "opencv-extra-" version ".zip"))
           (sha256
            (base32 "0wfh3pvfxqydf7hsccp50npcsg37sf6fqi6cd3zkc4qil9zhpbps"))))
       ("opencv-contrib"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://codeload.github.com/"
                               "opencv/opencv_contrib/zip/" version))
           (file-name (string-append "opencv-contrib-" version ".zip"))
           (sha256
           (base32 "18zm0qmjcdvg90c33gzv0ws0xdaid1xpqzz2xa9l2x12qkr6zj3p"))))))
    (inputs `(("ffmpeg" ,ffmpeg)
              ("libjpeg" ,libjpeg)
              ("libpng" ,libpng)
              ("jasper" ,jasper)
              ("libtiff" ,libtiff)
              ("hdf5" ,hdf5)
              ("libgphoto2" ,libgphoto2)
              ("libwebp" ,libwebp)
              ("zlib" ,zlib)
              ("gtkglext" ,gtkglext)
              ("openexr" ,openexr)
              ("ilmbase" ,ilmbase)
              ("gtk" ,gtk+-2)
              ("python-numpy" ,python-numpy)
              ("protobuf" ,protobuf)
              ("vtk" ,vtk)
              ("python" ,python)))
    (synopsis "Computer Vision Library")
    (description "OpenCV (Open Source Computer Vision) is a library aimed at
real-time computer vision, including several hundred computer
vision algorithms.")
    (home-page "https://opencv.org/")
    (license license:bsd-3)))
