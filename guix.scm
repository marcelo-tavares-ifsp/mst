(use-modules (guix licenses)
             (guix gexp)
             (gnu packages)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages xorg)
             (gnu packages wm)
             (gnu packages python)
             (gnu packages docker)
             (gnu packages xdisorg)
             (gnu packages display-managers)
             (gnu packages bash)
             (gnu packages gl)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages texinfo)
             (gnu packages qt)
             (gnu packages gettext)
             (gnu packages linux)
             (guix utils)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system qt)
             (guix build-system gnu)
             ((guix build utils) #:select (alist-replace))
             (ice-9 match)
             ((srfi srfi-1) #:select (alist-delete)))


(define %source-dir (dirname (current-filename)))


(package
  (name    "mst")
  (version "git")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (arguments
   `(#:tests? #t
     #:imported-modules
     (,@%qt-build-system-modules)
     #:modules
     ((guix build gnu-build-system)
      ((guix build qt-build-system)
       #:prefix qt:)
      (guix build utils))
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'qm-chmod
         (lambda _
           ;; Make sure 'lrelease' can modify the qm files.
           (for-each (lambda (po)
                       (chmod po #o666))
                     (find-files "i18n" "\\.qm$"))
           #t))
       (add-after 'unpack 'patch
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (substitute* "mst.pro"
             (("\\$\\$\\[QT_INSTALL_BINS\\]/lrelease")
              (string-append (assoc-ref inputs "qttools")
                             "/bin/lrelease")))
           (substitute* "mst/core/components/awesome.cpp"
             (("awesome --version")
              (format #f
                      "~a/bin/awesome --version"
                      (assoc-ref inputs "awesome"))))
           (substitute* "mst/core/components/display_manager.cpp"
             (("platform::popen_read\\(\"lightdm\",")
              (format #f
                      "platform::popen_read(\"~a/sbin/lightdm\","
                      (assoc-ref inputs "lightdm"))))
           (substitute* "mst/core/components/xorg.cpp"
             (("xdpyinfo")
              (format #f
                      "~a/bin/xdpyinfo"
                      (assoc-ref inputs "xdpyinfo"))))
           (substitute* "mst/core/components/vgl.cpp"
             (("vglclient")
              (format #f
                      "~a/bin/vglclient"
                      (assoc-ref inputs "virtualgl")))
             (("vglserver_config")
              (format #f
                      "~a/bin/vglserver_config"
                      (assoc-ref inputs "virtualgl"))))
           (substitute* "mst/main.cpp"
             (("set_template_dir\\(\"/var/lib/mst/\"\\)")
              (format #f
                      "set_template_dir(\"~a/var/lib/mst/\")"
                      (assoc-ref outputs "out")))
             (("xset")
              (format #f "~a/usr/bin/xset" (assoc-ref inputs "xset"))))
           (substitute* "templates/vgl.sh.template"
             (("/var/lib/vgl/vgl_xauth_key")
              (format #f
                      "~a/var/lib/vgl/vgl_xauth_key"
                      (assoc-ref inputs "virtualgl")))
             (("/usr/lib/vglrun.vars")
              (format #f
                      "~a/usr/lib/vglrun.vars"
                      (assoc-ref inputs "virtualgl"))))
           (substitute* "templates/xinitrc.template"
             (("awesome")
              (format #f "~a/bin/awesome" (assoc-ref inputs "awesome"))))
           (substitute* (list "templates/rc.lua.template"
                              "templates/rc.lua.4.template")
             (("/usr/share/awesome")
              (format #f
                      "~a/usr/share/awesome"
                      (assoc-ref inputs "awesome")))
             (("/usr/bin/xset")
              (format #f "~a/usr/bin/xset" (assoc-ref inputs "xset"))))
           (substitute* "templates/sudoers.template"
             (("/usr/bin/bash")
              (format #f "~a/usr/bin/bash" (assoc-ref inputs "bash")))
             ;; TODO:
             ;; (("/bin/su")
             ;;  (format #f "~a/usr/bin/su" (assoc-ref inputs "")))
             (("/usr/sbin/lightdm")
              (format #f "~a/usr/sbin/lightdm" (assoc-ref inputs "lightdm")))
             (("/usr/bin/dm-tool")
              (format #f "~a/usr/bin/dm-tool" (assoc-ref inputs "lightdm")))
             (("/usr/bin/Xephyr")
              (format #f "~a/usr/bin/Xephyr" (assoc-ref inputs "xorg-server")))
             (("/usr/bin/xset")
              (format #f "~a/usr/bin/xset" (assoc-ref inputs "xorg"))))
           #t))
       (add-after 'configure 'generate-version-file
         (lambda* (#:key outputs #:allow-other-keys)
           (with-output-to-file "mst/version.h"
             (lambda ()
               (format #t
                       "const string VERSION = \"~a\";~%"
                       version)))
           #t))
       (replace 'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (invoke-qmake
                   (lambda ()
                     (invoke "qmake"
                             (string-append "PREFIX=" out)
                             (string-append "BINDIR=" out "/bin")
                             (string-append "DATADIR=" out "/share")
                             (string-append "PLUGINDIR="
                                            out
                                            "/lib/qt5/plugins")))))
             (invoke-qmake)
             (let ((cwd (getcwd)))
               (chdir "mst")
               (invoke-qmake)
               (chdir cwd)))
           #t))
       (add-after 'install 'qt-wrap
         (assoc-ref qt:%standard-phases 'qt-wrap)))))
  (native-inputs
   (list automake
         autoconf
         gnu-make
         bash-minimal
         texinfo
         gettext-minimal))
  (inputs
   (list awesome
         docker
         unclutter
         lightdm
         bash-minimal
         guile-udev
         virtualgl
         eudev
         guile-3.0
         qtbase-5
         xorg-server
         xdpyinfo
         qttools))
  (home-page "https://gitlab.com/gkaz/mst")
  (synopsis
   "Multi-seat configurator.")
  (description
   "MST (Multi-Seat Toolkit) is a graphical multi-seat configurator and a s
set of tools that enables easy configuration of multi-seat setups.")
  (license gpl3))

;;; guix.scm ends here.
