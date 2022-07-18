;;; xephyr.scm -- Xephyr procedures.

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

;; This module contains Xephyr procedures.


;;; Code:

(define-module (mst component xephyr)
  #:export (make-xephyr-command
            mouse->xephyr-device
            keyboard->xephyr-device))

(define %xephyr-binary "/usr/bin/Xephyr")

(define (mouse->xephyr-device mouse-device)
  "Convert a MOUSE-DEVICE to a format suitable for passing to '-mouse'
Xephyr option."
  (format #f "evdev,5,device=~a" mouse-device))

(define (keyboard->xephyr-device keyboard-device)
  "Convert a KEYBOARD-DEVICE to a format suitable for passing to
'-keybd' Xephyr option."
  (format #f "evdev,,device=~a" keyboard-device))


(define* (make-xephyr-command #:key
                              mouse-dev
                              keyboard-dev
                              resolution
                              display-number)
  (list %xephyr-binary
        "-softCursor"
        "-ac"
        "-br"
        "-resizeable"
        "-mouse" (mouse->xephyr-device mouse-dev)
        "-keybd" (keyboard->xephyr-device keyboard-dev)
        "-screen" (format #f "~a" resolution)
        (format #f ":~a" display-number)))

;;; xephyr.scm ends here.
