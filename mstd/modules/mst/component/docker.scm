;;; docker.scm -- MST docker procedures.

;; Copyright (C) 2021-2022 "AZ Company Group" LLC <https://gkaz.ru/>
;; Copyright (C) 2021-2022 Artyom V. Poptsov <a@gkaz.ru>
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

;; This module contains the procedures for working with Docker.


;;; Code:

(define-module (mst component docker)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (mst core log)
  #:use-module (mst core seat)
  #:use-module (mst core docker-container)
  #:use-module (mst system)
  #:use-module (mst component xephyr)
  #:export (docker-start-xephyr))


;;; Constants.

(define %xephyr-docker-image "gkaz/xephyr")


;;;
(define-generic docker-start-xephyr)

(define-method (docker-start-xephyr display-number resolution mouse keyboard)
  "Start a new Xephyr instance inside a Docker container.  Return the new
<docker-container> instance."
  (log-info "Starting Xephyr (~a) for display ~a; resolution: ~a; mouse: ~a; keyboard: ~a"
            %xephyr-docker-image
            display-number resolution mouse keyboard)
  (let ((mouse-dev    (device-name->path mouse))
        (keyboard-dev (device-name->path keyboard)))
    (make-docker-container %xephyr-docker-image
                           (make-xephyr-command #:mouse-dev mouse-dev
                                                #:keyboard-dev keyboard-dev
                                                #:resolution resolution
                                                #:display-number display-number)
                           #:devices (list mouse-dev keyboard-dev)
                           #:environ (list "DISPLAY=:0")
                           #:volumes (list "/tmp/.X11-unix:/tmp/.X11-unix:rw"))))

(define-method (docker-start-xephyr (seat <seat>))
  (docker-start-xephyr (seat-display    seat)
                       (seat-resolution seat)
                       (seat-mouse      seat)
                       (seat-keyboard   seat)))

;;; docker.scm ends here.
