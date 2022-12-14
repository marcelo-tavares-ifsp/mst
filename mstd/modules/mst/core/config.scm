;;; config.scm -- MST configuration procedures.

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

;; This module contains the procedures reading the MST configuration
;; file.


;;; Code:

(define-module (mst core config)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (mst core seat)
  #:use-module (mst core log)
  #:use-module (mst system)
  #:export (read-seats-configuration
            config-get-seat
            device-path->display-number
            config-seat-lookup-by-device))

(define (read-seats-configuration config-file)
  "Read seats configuration from a CONFIG-FILE.  Return the
configuration as an alist."
  (let ((port (open-input-file config-file)))
    (let read ((line (read-line port))
               (data '()))
      (if (eof-object? line)
          (reverse data)
          (read (read-line port)
                (cons (list->seat (string-split line #\space))
                      data))))))

(define (config-get-seat config display)
  (find (lambda (seat) (equal? (seat-display seat) display))
        config))

(define (config-seat-lookup-by-device seats device-id-path)
  "Lookup a seat from a SEATS list by a DEVICE-ID-PATH.  Return a seat
instance that uses the device or #f if no owning seat found."
  (log-info "config-seat-lookup-by-device: ~a" device-id-path)
  (if (null? seats)
      #f
      (let* ((seat     (car seats))
             (keyboard (seat-keyboard seat))
             (mouse    (seat-mouse seat)))
        (log-info "config-seat-lookup-by-device: keyboard: ~a" keyboard)
        (log-info "config-seat-lookup-by-device: mouse:    ~a" mouse)
        (if (or (string-contains keyboard device-id-path)
                (string-contains mouse device-id-path))
            seat
            (config-seat-lookup-by-device (cdr seats)
                                          device-id-path)))))
        

(define (device-path->display-number config device-path)
  "Try to determine a display number that device specified by its
DEVICE-PATH belongs to, using a CONFIG."
  (let* ((record       (car config))
         (base-devpath (seat-usb record)))
    (if (string-contains device-path base-devpath)
	(seat-display record)
        (if (null? (cdr config))
            #f
            (device-path->display-number (cdr config) device-path)))))

;;; config.scm ends here.

