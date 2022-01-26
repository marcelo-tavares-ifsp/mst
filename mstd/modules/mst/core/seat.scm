;;; seat.scm -- MST seat.

;; Copyright (C) 2021 "AZ Company Group" LLC <https://gkaz.ru/>
;; Copyright (C) 2021 Artyom V. Poptsov <a@gkaz.ru>
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

;; This module contains a <seat> class and its methods that describes
;; an MST seat.


;;; Code:

(define-module (mst core seat)
  #:use-module (oop goops)
  #:export (<seat>
            seat-display
            seat-interface
            seat-resolution
            seat-keyboard
            seat-mouse
            seat-usb
            seat-configured?

            list->seat
            seat->list))


;;; Seat list accessors.

(define (%seat:display seat)
  (and (> (length seat) 0)
       (list-ref seat 0)))

(define (%seat:interface seat)
  (and (> (length seat) 1)
       (list-ref seat 1)))

(define (%seat:resolution seat)
  (and (> (length seat) 2)
       (list-ref seat 2)))

(define (%seat:keyboard seat)
  (and (> (length seat) 3)
       (list-ref seat 3)))

(define (%seat:mouse seat)
  (and (> (length seat) 4)
       (list-ref seat 4)))

(define (%seat:usb seat)
  (and (> (length seat) 5)
       (list-ref seat 5)))



;; This class describes an MST seat.
(define-class <seat> ()

  ;; <number>
  (display
   #:init-value   0
   #:init-keyword #:display
   #:getter       seat-display)

  ;; <string>
  (interface
   #:init-value   #f
   #:init-keyword #:interface
   #:getter       seat-interface)

  ;; <string>
  (resolution
   #:init-value   #f
   #:init-keyword #:resolution
   #:getter       seat-resolution)

  ;; <string>
  (keyboard
   #:init-value   #f
   #:init-keyword #:keyboard
   #:getter       seat-keyboard)

  ;; <string>
  (mouse
   #:init-value   #f
   #:init-keyword #:mouse
   #:getter       seat-mouse)

  ;; <string>
  (usb
   #:init-value   #f
   #:init-keyword #:usb
   #:getter       seat-usb))


(define-method (%display (seat <seat>) (port <port>))
  (format port "#<seat ~a ~a ~a ~a ~a ~a ~a>"
          (seat-display    seat)
          (seat-interface  seat)
          (seat-resolution seat)
          (seat-keyboard   seat)
          (seat-mouse      seat)
          (seat-usb        seat)
          (number->string (object-address seat) 16)))

(define-method (display (seat <seat>) (port <port>))
  (%display seat port))

(define-method (write (seat <seat>) (port <port>))
  (%display seat port))


;; Convert a list @var{lst} into a <seat> instance.
(define-method (list->seat (lst <list>))
  (make <seat>
    #:display    (%seat:display    lst)
    #:interface  (%seat:interface  lst)
    #:resolution (%seat:resolution lst)
    #:keyboard   (%seat:keyboard   lst)
    #:mouse      (%seat:mouse      lst)
    #:usb        (%seat:usb        lst)))

;; Convert a @var{seat} to a list.
(define-method (seat->list (seat <seat>))
  (list (seat-display    seat)
        (seat-interface  seat)
        (seat-resolution seat)
        (seat-keyboard   seat)
        (seat-mouse      seat)
        (seat-usb        seat)))


;; Check if a @var{seat} has properly configured parameters.
(define-method (seat-configured? seat)
  (and (seat-display    seat)
       (seat-interface  seat)
       (seat-resolution seat)
       (seat-keyboard   seat)
       (seat-mouse      seat)
       (seat-mouse      seat)
       (seat-usb        seat)))

;;; seat.scm ends here.
