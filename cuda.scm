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

(define-module (cuda)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  ;; #:use-module (gnu packages commencement)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl))

;; (define EULA
;;   (license "EULA"
;; 	   "http://www.nvidia.com/object/nv_sw_license.html"
;; 	   "http://www.nvidia.com/object/nv_sw_license.html"))

(define-private cuda-patch-85-1
  (package
    (name "cuda-patch-85-1")
    (version "9.1.85.1")
    (source
     (origin
       (method url-fetch)
       (uri
	(string-append
	 "https://developer.nvidia.com/compute/cuda/9.1/Prod/patches/1/cuda_"
	 version "_linux"))
       (sha256
	(base32 "1f53ij5nb7g0vb5pcpaqvkaj1x4mfq3l0mhkfnqbk8sfrvby775g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build utils))
	     (let* ([source (assoc-ref inputs "source")]
		    [file-name "cuda-patch.run"])
	       (copy-file source file-name))))
	 (delete 'build)
	 (delete 'configure)
	 (replace 'install
	   (lambda (#:key inputs outputs #:allow-other-keys)
	     (let* ([out (assoc-ref outputs "out")])
	       (copy-file "cuda-patch.run" (string-append out "cuda-patch.run")))))
	 (delete 'validate-runpath)
	 (delete 'check)
	 )
       #:strip-binaries? #f))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "Cuda binary patches.")
    (description "Cuda binary patches.")
    (license gpl3+)))


(define-public cuda
  ;; This DOESN'T work yet, due to runpath, I'm kind of running out of idea now.
  (package
    (name "cuda")
    (version "9.1.85")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://developer.nvidia.com/compute/cuda/9.1/Prod/local_installers/cuda_"
		    version
		    "_387.26_linux"))
	      (sha256
	       (base32
		"0lz9bwhck1ax4xf1fyb5nicb7l1kssslj518z64iirpy2qmwg5l4"))))
    (build-system gnu-build-system)
    (native-inputs `(
		     ;; ("perl" ,perl)
		     ;; ("bash" ,bash)
		     ;; ("sed" ,sed)
		     ("patchelf" ,patchelf)))
    (inputs `(
	      ;; ("gcc:lib" ,gcc-6 "lib")
	      ))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build utils))
	     (let* ((source (assoc-ref inputs "source"))
		    (file-name (string-append ,name "-" ,version ".run"))
		    ;; (sh (string-append (assoc-ref inputs "bash") "/bin/bash"))
		    ;; (sed (string-append (assoc-ref inputs "sed") "/bin/sed"))
		    ;; (perl (string-append (assoc-ref inputs "perl") "/bin/perl"))
		    (build-root (getcwd)))
	       (copy-file source (string-append ,name "-" ,version ".run"))

	       (mkdir-p "tmp")
	       ;; (invoke sed "-i"
	       ;; 	       (string-append " 1s|.*|#!" sh "|") file-name)
	       ;; (invoke sed "-i" (string-append " 518s|.*|rm -rf $tmpdir|")
	       ;; 	       file-name)
	       ;; (display (string-append "--tmpdir=" build-root "/tmp"))
	       (invoke (string-append "./" ,name "-" ,version ".run")
		       "--keep" "--noexec"
		       "--tmpdir=" (string-append build-root "/tmp")
		       "--verbose")
	       (chdir "pkg/run_files")
	       ;; (invoke sed "-i" (string-append " 137s|> /dev/tty|>& 1|")
	       ;; 	       "cuda-linux.9.1.85-23083092.run")
	       (invoke "./cuda-linux.9.1.85-23083092.run" "--keep" "--noexec"
		       "--tmpdir" (string-append build-root "/tmp"))
	       (invoke "./cuda-samples.9.1.85-23083092-linux.run" "--keep" "--noexec"
		       "--tempdir" (string-append build-root "/tmp"))

	       (invoke "mv" "pkg" (string-append "../../cuda"))
	       (chdir "../../")
	       (invoke "ls" "-l")

	       (invoke "rm" "-rf" "./pkg"))))

	 (delete 'configure)
	 (delete 'build)
	 (replace 'install	; patchelf
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (ice-9 ftw)
	 		  (ice-9 regex)
	 		  (ice-9 popen)
	 		  (ice-9 rdelim)) ; for read-line
	     (let* ((ld-so (string-append
	 		    (assoc-ref
	 		     inputs "libc") ,(glibc-dynamic-linker)))
	 	    (gcclib (string-append (assoc-ref
	 				    inputs "gcc:lib") "/lib"))
	 	    (out (assoc-ref outputs "out"))
	 	    (rpath (string-append
	 		    gcclib ":"
	 		    (string-append out "/lib64" ":")
	 		    (string-append out "/lib" ":")
	 		    (string-append out "/nvvm/lib64" ":")
	 		    (string-append out "/nvvm/lib"))))
	       (invoke "pwd")
	       (invoke "ls" "-l")
	       (let* ((out (assoc-ref outputs "out")))
		 (chdir "cuda")
		 (invoke "./install-linux.pl" (string-append "--prefix=" out)
	 		 "--noprompt")
		 (chdir out))

	       (invoke "rm" "-rf" (string-append out "lib"))
	       (invoke "rm" "-rf" (string-append out "bin/uninstall*"))

	       (let ((hd (string-append out "include/host_defines.h"))
		     (sedcommand (string-append "'1 i#define _BITS_FLOATN_H " hd "'")))
		 (invoke "sed" "-i" sedcommand))
	       ;; (invoke "sed" "-i" "'1 i#define _BITS_FLOATN_H" "$out/include/host_defines.h'")
	       ;; (setenv "lib" lib)
	       (display (string-append "out: " out))
	       (nftw "./cuda"
	 	     (lambda (filename statinfo flag base level)
	 	       ;; (display (string-append "filename" filename "\n"))
	 	       (let* ((port (open-input-pipe
	 			     (string-append "file " filename)))
	 		      (elf (read-line port)))
	 		 (close-pipe port)
	 		 (if (and
	 		      ;; (not (equal? flag 'symlink))
	 		      (string-match "ELF" elf))
	 		     (begin
	 		       (display (string-append "Patching: " filename ".\n"))
	 		       ;; (display (string-append rpath "\n"))
	 		       (system* "patchelf" "--set-interpreter"
	 		     		ld-so filename)
	 		       (invoke "patchelf" "--set-rpath"
	 			       rpath "--force-rpath" filename))))
	 	       #t)))))
	 ;; (add-after 'install 'fixup
	 ;;   (lambda (#:key inputs outputs #:allow-other-keys)
	 
	 ;;     ))
	 ;; (add-after 'install 'warp-program
	 ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
	 ;;     (let* ((out (assoc-ref outputs "out"))
	 ;; 	    (nvcc (string-append out "/bin/nvcc")))
	 ;;       ;; The environment variable DTK_PROGRAM tells emacspeak what
	 ;;       ;; program to use for speech.
         ;;       (wrap-program nvcc
         ;;         `("--prefix" ))
         ;;       #t)))

	 ;; (replace 'install
	 ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
	 ;;     (use-modules (guix build utils))

	 ;;       (invoke "rm" "-rf" (string-append out "lib"))
	 ;;       (system*
	 ;; 	"while IFS= read -r -d ''$'\0' i; do
	 ;; 	   if ! isELF \"$i\"; then continue; fi
	 ;; 	   echo \"patching $i...\"
	 ;; 	   if [[ ! $i =~ \\.so ]]; then
	 ;; 	     patchelf \\
	 ;; 	       --set-interpreter \"''$(cat $NIX_CC/nix-support/dynamic-linker)\" $i
	 ;; 	   fi
	 ;; 	   if [[ $i =~ libcudart ]]; then
	 ;; 	     rpath2=
	 ;; 	   else
	 ;; 	     rpath2=$rpath:$lib/lib:$out/jre/lib/amd64/jli:$out/lib:$out/lib64:$out/nvvm/lib:$out/nvvm/lib64
	 ;; 	   fi
	 ;; 	     patchelf --set-rpath $rpath2 --force-rpath $i
	 ;; 	   done < <(find $out -type f -print0)")
	 ;;       ;; (invoke "sed" "-i" (string-append out "/include/host_config -e 's/#error\(.*unsupported GNU version\)/#warning\1"))
	 ;;       ;; (display (string-append out "/lib64/stubs/libcuda.so"))
	 ;;       ;; (symlink (string-append out "/lib64/stubs/libcuda.so")
	 ;;       ;; 		(string-append out "/lib64/libcuda.so"))
	 ;;       )))

	 (delete 'check)
	 ;; (delete 'validate-runpath)
	 )
       #:strip-binaries? #f))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (supported-systems '("x86_64-linux"))
    (synopsis "The NVIDIA® CUDA® Toolkit provides a development environment for
creating high performance GPU-accelerated applications.")
    (description "With the CUDA Toolkit, you can develop, optimize and deploy
your applications on GPU-accelerated embedded systems, desktop workstations,
enterprise data centers, cloud-based platforms and HPC supercomputers. The
toolkit includes GPU-accelerated libraries, debugging and optimization tools,
a C/C++ compiler and a runtime library to deploy your application.")
    (license (list asl2.0
		   gpl3+
		   bsd-3
		   bsd-2
		   unlicense)))) ; Please check out EULA.txt along with distribution.
