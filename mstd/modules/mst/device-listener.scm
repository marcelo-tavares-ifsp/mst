;;; device-listener.scm -- Listen to and handle device events.

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

;; This module contains procedures to handle connecting/disconnecting
;; USB devices.


;;; Code:

(define-module (mst device-listener)
  #:use-module (udev udev)
  #:use-module (udev monitor)
  #:use-module (udev device)
  #:use-module (mst system)
  #:use-module (mst core config)
  #:use-module (mst core log)
  #:export (device-listener-start))


(define (notify-broadcast config message)
  "Broadcast a notify with a MESSAGE across all seats that are listed
in a CONFIG."
  (for-each (lambda (rec)
              (notify-send (seat-display rec) message))
            config))

(define (error-handler monitor error-message)
  (log-error "ERROR: In ~a: ~a~%" monitor error-message))


(define (device-listener-start config)
  "Start udev listener."
  (let* ((udev         (make-udev))
         (udev-monitor (make-udev-monitor udev
                                          #:timeout-sec 1
                                          #:error-callback error-handler
                                          #:filter '("block" "partition")))
         (udev-input-monitor (make-udev-monitor udev
                                                #:timeout-sec 1
                                                #:error-callback error-handler
                                                #:filter '("usb" "usb_device"))))
    (log-info "Config: ~a~%" config)

    (let ((callback
           (lambda (device)
             (when (string=? (udev-device-get-action device) "add")
                   (let* ((devpath        (udev-device-get-devpath device))
                          (display-number (device-path->display-number config
                                                                       devpath))
                          (user           (display-number->user display-number))
                          (devname (udev-device-get-property-value device
                                                                   "DEVNAME")))
                     (if user
                         (begin
                           (log-info "Mounting ~a for seat ~a (user ~a)~%"
                                     devpath display-number user)
                           (mount devname user))
                         (display "Could not determine user.")))))))

      (udev-monitor-set-callback! udev-monitor callback))

    (let ((callback
           (lambda (device)
             (let ((action  (udev-device-get-property-value device "ACTION"))
                   (devname (udev-device-get-property-value device
                                                            "DEVNAME"))
                   (devpath (udev-device-get-devpath device)))
               (cond
                ((string=? action "remove")
                 (log-info "Device ~a was removed.~%" devname)
                 (unless (device-path->display-number config devpath)
                         (notify-broadcast config
                                           (format #f
                                                   (string-append
                                                    "Seat input device ~a was removed.~%"
                                                    "Please re-connect the device and"
                                                    " reboot the computer.")
                                                   devname))))
                ((string=? action "add")
                 (log-info "Device ~a was added.~%" devname)))))))

      (udev-monitor-set-callback! udev-input-monitor callback))

    (udev-monitor-start-scanning! udev-monitor)
    (udev-monitor-start-scanning! udev-input-monitor)
    (while #t
           (sleep 1))))

;;; device-listener.scm ends here.
