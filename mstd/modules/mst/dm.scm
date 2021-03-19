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
  #:use-module (mst system)
  #:use-module (mst log)
  #:export (add-seat
            dm-start
	    is-seat-used?
	    get-running-seats
	    start-lightdm))

(define *debug?* #f)

(define (set-dm-debug! value)
  (set! *debug?* value))


(define (add-seat number)
  (log-info "Adding seat number ~a" number)
  (system (format #f "/usr/bin/dm-tool add-local-x-seat ~a" number)))

(define (start-lightdm config-file)
  (log-info "Staring lightdm with the config: ~a" config-file)
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (system (format #f "/usr/sbin/lightdm --config ~a" config-file)))
     ((> pid 0)
      (log-info "Lightdm started.  PID: ~a" pid)
      pid)
     (else
      (log-error "Could not start the display manager")
      (error "Could not start the display manager")))))


;; Check if a seat with ID is used.
(define (is-seat-used? id)
  (define regexp (format #f ".*(:~a).*" id))
  (let loop ((p (open-input-pipe "who")))
    (let ((line (read-line p)))
      (if (eof-object? line)
	  #f
	  (if (string-match regexp line)
	      #t
	      (loop p))))))

(define (get-running-seats)
  "Get the number of running seats."
  (string->number
   (read-line
    (open-input-pipe "/usr/bin/dm-tool list-seats | grep -c 'Seat'"))))

(define (start-seats seat-number)
  (let loop ((idx 1))
    (unless (is-seat-used? idx)
      (for-each
       (lambda (pid)
         (let ((env (proc-environ pid)))
           (unless env
             (log-error "Process is not available: ~a" pid)
             (error "Process is not available" pid))
           (let ((disp (memq "DISPLAY" env)))
	   (when (and disp (= (string->number (cdr disp)) idx))
		 (kill pid SIGTERM)))))
       (proc-get-pids))
      (add-seat idx))
    (when (< idx seat-number)
      (loop (+ idx 1)))))

(define (main-loop seat-count)
  (if (graphics-available?)
      (begin
        (log-info "Graphics available; starting seats: ~a" seat-count)
        (start-lightdm "/etc/lightdm/lightdm-mst.conf")

        (let ((result (system "xset -dpms")))
          (unless (zero? result)
            (log-error "Could not execute 'xset -dpms': ~a" result)
            (error "Could not execute 'xset -dpms'" result)))

        (let ((result (system "xset s off")))
          (unless (zero? result)
            (log-error "Could not execute 'xset s off': ~a" result)
            (error "Could not execute 'xset s off'" result)))

        (let loop ((idx 1))
          (add-seat idx)
          (if (< idx seat-count)
              (loop (+ idx 1))))

        (while #t
               (let ((running-seats-number (get-running-seats)))
                 (if (< running-seats-number seat-count)
                     (start-seats seat-count)))
               (sleep 1)))
      (begin
        (sleep 1)
        (main-loop seat-count))))
  
(define (dm-start seat-count)
  "Returns a new thread."
  (start-lightdm "/etc/lightdm/lightdm-mst.conf")
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (main-loop seat-count))
     ((> pid 0)
      (log-info "Display manager started; PID: ~a" pid)
      pid)
     (else
      (error "Could not start the DM main loop")))))
