;;; unmouser.scm -- Hide/show X11 mouse cursor.

;; Copyright (C) 2022 "AZ Company Group" LLC <https://gkaz.ru/>
;; Copyright (C) 2022 Artyom V. Poptsov <a@gkaz.ru>
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains procedures to hide/show the mouse cursor in X11
;; system.


;;; Code:

(define-module (mst unmouser)
  #:use-module (oop goops)
  ;; #:use-module (system foreign-library)
  #:use-module (system foreign)
  #:export (<unmouser>
            unmouser-toggle
            unmouser-free))


;; XXX: ALT Linux 8, 9, 10 does not have (system foreign-library) so we have
;; to use this dirty hack to provide 'foreign-library-function'.

(define *libs* (make-hash-table))

(define* (foreign-library-function lib
                                   name
                                   #:key
                                   (return-type void)
                                   (arg-types '())
                                   (return-errno? #f))
  (let* ((handle (if (hash-ref *libs* lib)
                     (hash-ref *libs* lib)
                     (let ((handle (dynamic-link lib)))
                       (hash-set! *libs* lib handle)
                       handle)))
         (pointer (dynamic-func name handle)))
    (pointer->procedure return-type
                        pointer
                        arg-types
                        #:return-errno? return-errno?)))


;; Low-level procedures.
(define x-open-display
  (foreign-library-function "libX11"
                            "XOpenDisplay"
                            #:return-type '*
                            #:arg-types (list '*)))

(define x-default-screen
  (foreign-library-function "libX11"
                            "XDefaultScreen"
                            #:return-type int
                            #:arg-types (list '*)))

(define x-root-window
  (foreign-library-function "libX11"
                            "XRootWindow"
                            #:return-type int
                            #:arg-types (list '* int)))

(define x-fixes-hide-cursor
  (foreign-library-function "libXfixes"
                            "XFixesHideCursor"
                            #:return-type void
                            #:arg-types (list '* int)))

(define x-flush
  (foreign-library-function "libX11"
                            "XFlush"
                            #:return-type void
                            #:arg-types (list '*)))

(define x-destroy-window
  (foreign-library-function "libX11"
                            "XDestroyWindow"
                            #:return-type void
                            #:arg-types (list '* int)))

(define x-close-display
  (foreign-library-function "libX11"
                            "XCloseDisplay"
                            #:return-type void
                            #:arg-types (list '*)))


(define-class <unmouser> ()
  ;; <number>
  (display-number
   #:init-value   0
   #:init-keyword #:display-number
   #:getter       unmouser-display-number)

  ;; <pointer>
  (display
   #:init-value   #f
   #:getter       unmouser-display
   #:setter       unmouser-display-set!)

  ;; <number>
  (window
   #:init-value  #f
   #:getter      unmouser-window
   #:setter      unmouser-window-set!))


(define-method (initialize (unmouser <unmouser>) initargs)
  "The class constructor."
  (next-method)
  (setenv "DISPLAY" (format #f ":~a" (unmouser-display-number unmouser)))
  (let* ((display (x-open-display %null-pointer))
         (screen  (x-default-screen display)))
    (unmouser-display-set! unmouser display)
    (unmouser-window-set! unmouser (x-root-window display screen))))


(define-method (unmouser-toggle (unmouser <unmouser>))
  "Hide/show the mouse cursor.  Return value is undefined."

  (unless (and (unmouser-display unmouser) (unmouser-window unmouser))
    (error "Unmouser is not initialized properly" unmouser))

  (x-fixes-hide-cursor (unmouser-display unmouser)
                       (unmouser-window unmouser))
  (x-flush (unmouser-display unmouser)))

(define-method (unmouser-free (unmouser <unmouser>))
  "Free all the resources acquired by a UNMOUSER instance.  After calling this
procedure, the instance cannot be used.  Return value is undefined."
  (x-destroy-window (unmouser-window unmouser))
  (unmouser-window-set! unmouser #f)
  (x-close-display (unmouser-display unmouser))
  (unmouser-display-set! unmouser #f))

;;; unmouser.scm ends here.
