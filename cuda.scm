(define-module (cuda)
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

(define-public cuda
  ;; This DOESN'T work yet, due to runpath, I'm kind of running out of idea now.
  (package
    (name "cuda")
    (version "9.1.85")
    (source "/home/fis/Workspace/pkgs/cuda/cuda_9.1.85_387.26_linux.run")
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
		     ("bash" ,bash)
		     ("sed" ,sed)
		     ("patchelf" ,patchelf)))
    (inputs `(("gcc:lib" ,gcc-6 "lib")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build utils))
	     (let* ((source (assoc-ref inputs "source"))
		    (file-name (string-append ,name "-" ,version ".run"))
		    (sh (string-append (assoc-ref inputs "bash") "/bin/bash"))
		    (sed (string-append (assoc-ref inputs "sed") "/bin/sed"))
		    (perl (string-append (assoc-ref inputs "perl") "/bin/perl"))
		    (cwd (getcwd)))
	       (copy-file source (string-append ,name "-" ,version ".run"))
	       (mkdir-p "tmp")
	       (invoke sed "-i"
		       (string-append " 1s|.*|#!" sh "|") file-name)
	       (invoke sed "-i" (string-append " 518s|.*|rm -rf $tmpdir|")
		       file-name)
	       (display (string-append "--tmpdir=" cwd "/tmp"))
	       (invoke sh (string-append ,name "-" ,version ".run")
		       "--keep" "--noexec"
		       "--tmpdir=" (string-append cwd "/tmp")
		       "--verbose")
	       (chdir "pkg/run_files")
	       (invoke sed "-i" (string-append
				 " 137s|> /dev/tty|>& 1|")
		       "cuda-linux.9.1.85-23083092.run")
	       (invoke sh "cuda-linux.9.1.85-23083092.run" "--keep" "--noexec"
		       "--tmpdir" (string-append cwd "/tmp"))
	       (chdir cwd))))

	 (delete 'configure)
	 (replace 'build	; patchelf
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
	       (nftw "./pkg/run_files/pkg"
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

	 (replace 'install
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build utils))
	     (let* ((out (assoc-ref outputs "out")))
	       (chdir "pkg/run_files/pkg")
	       (invoke "./install-linux.pl" (string-append "--prefix=" out)
		       "--noprompt")
	       (chdir out)
	       (display (string-append out "/lib64/stubs/libcuda.so"))
	       (symlink (string-append out "/lib64/stubs/libcuda.so")
			(string-append out "/lib64/libcuda.so")))))

	 (delete 'check)
	 (delete 'validate-runpath))
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
