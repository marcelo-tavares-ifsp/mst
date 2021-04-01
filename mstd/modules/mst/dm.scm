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
            start-lightdm

            %make-command:add-seat))

(define *debug?* #f)

(define (set-dm-debug! value)
  (set! *debug?* value))

(define %lightdm-binary "/usr/sbin/lightdm")
(define %lightdm-config "/etc/lightdm/lightdm-mst.conf")


(define (%make-command:add-seat number)
  (format #f "/usr/bin/dm-tool add-local-x-seat ~a" number))

(define (add-seat number)
  (log-info "Adding seat number ~a" number)
  (system (%make-command:add-seat number)))

(define (start-lightdm config-file)
  (log-info "Starting lightdm with the config: ~a" config-file)
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (execlp %lightdm-binary %lightdm-binary "--config" config-file))
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
      (catch #t
             (lambda ()
               (waitpid -1 WNOHANG))
             (lambda args
               #t))
      (if (eof-object? line)
          #f
          (if (string-match regexp line)
              #t
              (loop p))))))

(define (get-running-seats)
  "Get the number of running seats."
  (let ((result
         (string->number
          (read-line
           (open-input-pipe "/usr/bin/dm-tool list-seats | grep -c 'Seat'")))))
    (waitpid -1 WNOHANG)
    result))

(define (start-seats seat-number)
  (log-info "Starting seats: ~a" seat-number)
  (let loop ((idx 1))
    (log-info "  Checking seat: ~a ..." idx)
    (unless (or (is-seat-used? idx) (not (xephyr-started? idx)))
      (for-each
       (lambda (pid)
         (when *debug?*
               (log-info "  Checking PID: ~a ..." pid))
         (let ((env (proc-environ pid)))
           (if env
               (let ((disp (memq "DISPLAY" env)))
                 (when (and disp (= (string->number (cdr disp)) idx))
                       (kill pid SIGTERM))))))
       (proc-get-pids))
      (add-seat idx))
    (when (< idx seat-number)
      (loop (+ idx 1)))))

(define (main-loop seat-count)
  (if (xephyr-started?)
      (begin
        (log-info "Graphics available")
        (unless (lightdm-started?)
          (log-info "  starting lightdm ...")
          (start-lightdm %lightdm-config)
          (log-info "  starting lightdm ... done"))
        (sleep 1)
        (log-info "Starting seats: ~a ..." seat-count)
        (let loop ((idx 1))
          (add-seat idx)
          (log-info "    starting seat: ~a ..." idx)
          (if (< idx seat-count)
              (loop (+ idx 1))))
        (log-info "Starting seats: ~a ... done" seat-count)

        (sleep 5)

        (log-info "Starting main loop")

        (while #t
               (let ((running-seats-number (get-running-seats)))
                 (if (< running-seats-number seat-count)
                     (start-seats seat-count)))
               (sleep 1)))
      (begin
        (log-info "Graphics is not available.  Waiting...")
        (sleep 1)
        (main-loop seat-count))))

(define (dm-start seat-count)
  "Returns a new thread."
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (main-loop seat-count))
     ((> pid 0)
      (log-info "Display manager started; PID: ~a" pid)
      pid)
     (else
      (error "Could not start the DM main loop")))))
