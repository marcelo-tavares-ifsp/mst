;;; lightdm.scm -- LightDM procedures.

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

;; This module contains LightDM procedures.


;;; Code:

(define-module (mst component lightdm)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (mst core log)
  #:use-module (mst config)
  #:export (lightdm-start
            lightdm-started?
            lightdm-add-seat
            lightdm-seat-running?
            lightdm-running-seats
            lightdm-running-greeters
	    lightdm-delete-pid-file!
            lightdm-pid

            %make-command:add-seat))


;;; Constants.

(define %lightdm-pid-file "/var/run/mstd-lightdm.pid")
(define %lightdm-config "/etc/lightdm/lightdm-mst.conf")


(define (lightdm-pid)
  "Get the LigthDM PID.  Return #f on error."
  (let ((port (open-input-file %lightdm-pid-file)))
    (if port
        (let ((pid (read-line port)))
          (close port)
          pid)
        (begin
          (log-error "Could not open LightDM PID file: ~a"
                     %lightdm-pid-file)
          #f))))


;;;

(define (%make-command:add-seat number)
  (format #f "~a add-local-x-seat ~a" %dm-tool-binary number))

(define (lightdm-add-seat number)
  (log-info "lightdm-add-seat: Adding seat number ~a" number)
  (system (%make-command:add-seat number)))


(define (lightdm-start config-file)
  (log-info "lightdm-start: Starting lightdm with the config: ~a" config-file)
  (let ((pid (primitive-fork)))
    (log-info "lightdm-start: LightDM PID: ~a" pid)
    (cond
     ((zero? pid)
      (execle %lightdm-binary
	      (environ)
              %lightdm-binary
	      "--config" config-file
	      "--pid-file" %lightdm-pid-file))
     ((> pid 0)
      (log-info "lightdm-start: Lightdm started.  PID: ~a" pid)
      pid)
     (else
      (log-error "lightdm-start: Could not start the display manager")
      (error "Could not start the display manager")))))


(define (lightdm-seat-running? id)
  "Check if a seat with ID is running."
  (let* ((port (open-input-pipe
                (format #f "/usr/bin/dm-tool list-seats | grep 'Seat~a'"
                        (- id 1))))
         (result (read-line port)))
    (close port)
    (waitpid -1 WNOHANG)
    (not (eof-object? result))))


(define (lightdm-running-seats)
  "Get the number of running seats."
  (let* ((port (open-input-pipe "/usr/bin/dm-tool list-seats | grep -c 'Seat'"))
	 (data (read-line port)))
    (close port)
    (waitpid -1 WNOHANG)
    (if (eof-object? data)
        0
        (string->number data))))

(define (lightdm-running-greeters)
  (let* ((port (open-input-pipe "ps aux | grep 'lightdm-.*-greeter' | grep -c -v grep"))
	 (data (read-line port)))
    (close port)
    (waitpid -1 WNOHANG)
    (if (eof-object? data)
        0
        (string->number data))))


(define (lightdm-started?)
  "Check if the LigthDM process is started.  Return #t if it is, #f
otherwise."
  (and (file-exists? %lightdm-pid-file)
       (let* ((port   (open-input-file %lightdm-pid-file))
	      (result (read-line port 'trim)))
	 (close port)
	 (and (not (eof-object? result))
	      (file-exists? (format #f "/proc/~a" result))))))

(define (lightdm-delete-pid-file!)
  "Delete the LightDM PID file if it exists."
  (when (file-exists? %lightdm-pid-file)
    (delete-file %lightdm-pid-file)))

;;; lightdm.scm ends here.

