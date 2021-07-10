;;; lightdm.scm -- LightDM procedures.

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

;; This module contains LightDM procedures.


;;; Code:

(define-module (mst component lightdm)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (mst core log)
  #:export (lightdm-start
            lightdm-add-seat
            is-seat-running?
            get-running-seats

            %make-command:add-seat))


;;; Constants.

(define %lightdm-binary "/usr/sbin/lightdm")
(define %lightdm-config "/etc/lightdm/lightdm-mst.conf")


;;;

(define (%make-command:add-seat number)
  (format #f "/usr/bin/dm-tool add-local-x-seat ~a" number))

(define (lightdm-add-seat number)
  (log-info "Adding seat number ~a" number)
  (system (%make-command:add-seat number)))


(define (lightdm-start config-file)
  (log-info "Starting lightdm with the config: ~a" config-file)
  (let ((pid (primitive-fork)))
    (log-info "LightDM PID: ~a" pid)
    (cond
     ((zero? pid)
      (execle %lightdm-binary (environ)
              %lightdm-binary "--config" config-file))
     ((> pid 0)
      (log-info "Lightdm started.  PID: ~a" pid)
      pid)
     (else
      (log-error "Could not start the display manager")
      (error "Could not start the display manager")))))


(define (is-seat-running? id)
  "Check if a seat with ID is running."
  (let* ((port (open-input-pipe
                (format #f "/usr/bin/dm-tool list-seats | grep 'Seat~a'"
                        (- id 1))))
         (result (read-line port)))
    (waitpid -1 WNOHANG)
    (not (eof-object? result))))


(define (get-running-seats)
  "Get the number of running seats."
  (let ((data (read-line
               (open-input-pipe "/usr/bin/dm-tool list-seats | grep -c 'Seat'"))))
    (waitpid -1 WNOHANG)
    (if (eof-object? data)
        0
        (string->number data))))

;;; lightdm.scm ends here.

