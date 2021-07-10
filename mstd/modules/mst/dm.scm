;;; dm.scm -- Display manager procedures.

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

;; This module contains procedures interaction with the display
;; manager.


;;; Code:

(define-module (mst dm)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (mst config)
  #:use-module (mst docker)
  #:use-module (mst system)
  #:use-module (mst component lightdm)
  #:use-module (mst core log)
  #:use-module (mst core seat)
  #:export (dm-start
            dm-stop-xephyrs
            is-seat-used?

            %make-command:xephyr/docker))

(define *debug?* #f)

(define (set-dm-debug! value)
  (set! *debug?* value))

(define %lightdm-config "/etc/lightdm/lightdm-mst.conf")
(define %xephyr-binary "/usr/bin/Xephyr")


(define *xephyrs* (make-hash-table 2))

(define (start-xephyr display-number resolution mouse keyboard)
  (log-info "Starting Xephyr for display ~a; resolution: ~a; mouse: ~a; keyboard: ~a"
            display-number resolution mouse keyboard)
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (execle %xephyr-binary (cons "DISPLAY=:0" (environ))
              %xephyr-binary
              "-softCursor"
              "-ac"
              "-br"
              "-resizeable"
              "-mouse" (format #f "evdev,5,device=/dev/input/by-path/~a" mouse)
              "-keybd" (format #f "evdev,,device=/dev/input/by-path/~a" keyboard)
              "-screen" (format #f "~a" resolution)
              (format #f ":~a" display-number)))
     ((> pid 0)
      (log-info "Xephyr is started.  PID: ~a" pid)
      pid)
     (else
      (log-error "Could not start a Xephyr instance")
      (error "Could not start a Xephyr instance")))))

(define (start-seats seat-number)
  (log-info "Starting seats: ~a" seat-number)
  (let loop ((idx 1))
    (add-seat idx)
    (when (< idx seat-number)
      (loop (+ idx 1)))))

(define (main-loop config)
  (let ((seat-count (length config)))
    (if (graphics-available?)
        (begin
          (log-info "Graphics available")
          (unless (lightdm-started?)
            (log-info "  starting lightdm ...")
            (start-lightdm %lightdm-config)
            (log-info "  starting lightdm ... done"))

          (log-info "  starting Xephyrs ... ")
          (for-each (lambda (seat-config)
                      (let ((seat-display    (seat-display seat-config))
                            (seat-interface  (seat-interface seat-config))
                            (seat-resolution (seat-resolution seat-config))
                            (seat-keyboard   (seat-keyboard seat-config))
                            (seat-mouse      (seat-mouse seat-config)))
                        (when (and seat-display
                                   seat-resolution
                                   seat-mouse
                                   seat-keyboard)
                          (let ((id (start-xephyr/docker seat-display
                                                         seat-resolution
                                                         seat-mouse
                                                         seat-keyboard)))
                            (log-info "    Docker ID: ~a" id)
                            (when id
                              (hash-set! *xephyrs* seat-display id))))))

                    config)
          (log-info "  starting Xephyrs ... done")

          (sleep 2)

          (log-info "Starting seats: ~a ..." seat-count)
          (start-seats seat-count)
          (log-info "Starting seats: ~a ... done" seat-count)

          (log-info "Starting main loop")
          (while #t
                 (let ((running-seats-number (get-running-seats)))
                   (if (< running-seats-number seat-count)
		       (begin
			 (hash-for-each
			  (lambda (key value)
			    (unless (docker-container-running? value)
			      (let* ((seat (config-get-seat config key))
				     (seat-display    (seat-display seat))
				     (seat-resolution (seat-resolution seat))
				     (seat-mouse      (seat-mouse seat))
				     (seat-keyboard   (seat-keyboard seat))
				     (id   (start-xephyr/docker
					    seat-display
					    seat-resolution
					    seat-mouse
					    seat-keyboard)))
				(if id
				    (hash-set! *xephyrs* seat-display id)
				    (log-error
				     "Could not start a Docker container for seat: ~a"
				     seat-display)))))
			  
			  *xephyrs*)
			 (start-seats seat-count))))
                 (sleep 1)))
        (begin
          (log-info "Graphics is not available.  Waiting...")
          (sleep 1)
          (main-loop config)))))

(define (dm-start config)
  "Returns a new thread."
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (let ((sighandler (lambda (arg)
                          (dm-stop-xephyrs)
                          (exit))))
        (sigaction SIGINT sighandler)
        (sigaction SIGTERM sighandler))
      (main-loop config))
     ((> pid 0)
      (log-info "Display manager started; PID: ~a" pid)
      pid)
     (else
      (error "Could not start the DM main loop")))))

(define (dm-stop-xephyrs)
  (hash-for-each
   (lambda (key value)
     (log-info "Stopping container ~a for seat: ~a ..."
               value key)
     (docker-stop value)
     (log-info "Stopping container ~a for seat: ~a ... done"
               key value)

     (log-info "Removing container ~a for seat: ~a ..."
               value key)
     (docker-container-rm value)
     (log-info "Removing container ~a for seat: ~a ... done"
               value key))
   *xephyrs*))
