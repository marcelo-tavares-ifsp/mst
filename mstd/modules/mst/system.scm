;;; system.scm -- MST system procedures.

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

;; This module contains the procedures for interaction with the
;; underlying system.


;;; Code:

(define-module (mst system)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (mst core log)
  #:use-module (mst config)
  #:export (notify-send
            display-number->user
            graphics-available?
            xephyr-started?
            proc-get-pids
            proc-environ
            proc-running?
            mount
            set-system-debug!
	    device-name->path
	    is-seat-used?

            %make-command:mount
            %make-command:notify-send
            %make-command:display-number->user))


(define *debug?* #f)


(define (set-system-debug! value)
  (set! *debug?* value))


(define (%make-command:notify-send display-number message)
  (string-append
   (format #f "DISPLAY=:~a" display-number)
   " " %notify-send-binary
   " --urgency=critical"
   " --icon=error"
   (string-append " '" message "'")))

(define (notify-send display-number message)
  "Show a notify with a MESSAGE on the given DISPLAY-NUMBER."
  (system (%make-command:notify-send display-number message)))

;; Make a 'mount' command to mount a DEVICE for a specified USER.
(define (%make-command:mount device user)
  (string-append
   (format #f "sudo --user='~a' -- " user)
   "udisksctl mount --no-user-interaction --block-device "
   device))

(define (mount device user)
  "Mount a DEVICE for a USER by means of udisksctl command."
  (let ((command (%make-command:mount device user)))
    (when *debug?*
          (format #t "mount command: ~a~%" command))
    (system command)))

(define (%make-command:display-number->user display-number)
  (string-append
   "who | "
   (format #f "grep '(:~a)'" display-number)
   " | sed -re 's/^([^ ]+) +.* \\(:[0-9]+\\)/\\1/g' | head -1"))

(define (display-number->user display-number)
  "Find out the user by its display number.  Return user name or #f if the
user is not found."
  (let ((port (open-input-pipe
               (%make-command:display-number->user display-number))))
    (let ((result (read-line port)))
      (waitpid -1 WNOHANG)
      (if (not (eof-object? result))
          result
          #f))))

(define (graphics-available?)
  (let* ((port   (open-input-pipe "ps aux | grep xinit | grep -v grep"))
         (result (read-line port)))
    (waitpid -1 WNOHANG)
    (not (eof-object? result))))

(define xephyr-started?
  (case-lambda
   ((number)
    (let* ((port   (open-input-pipe
                    (format #f "ps aux | grep \"Xephyr.*:~a$\" | grep -v grep"
                            number)))
           (result (read-line port)))
      (waitpid -1 WNOHANG)
      (not (eof-object? result))))
   (()
    (let* ((port (open-input-pipe
                  "ps aux | grep \"Xephyr.*:.*$\" | grep -v grep"))
           (result (read-line port)))
      (waitpid -1 WNOHANG)
      (not (eof-object? result))))))


(define (proc-get-pids)
  (map string->number
       (scandir "/proc" (lambda (entry)
                          (string-match "[0-9]+" entry)))))

(define (proc-environ pid)
  "Get the environment of a process with the PID.  Returns #f when the
process is not available."
  (catch #t
         (lambda ()
           (let* ((port (open-input-file (format #f "/proc/~a/environ" pid)))
                  (env  (read-line port)))
             (if (eof-object? env)
                 #f
                 (map (lambda (env)
                        (string-split env #\=))
                      (string-split (string-drop-right env 1) #\nul)))))
         (lambda args
           #f)))

(define (proc-running? pid)
  "Check if a process with the given @var{pid} is running."
  (file-exists? (format #f "/proc/~a" pid)))


(define (device-name->path name)
  (catch
   #t
   (lambda ()
     (let ((event (readlink (string-append "/dev/input/by-path/"
                                           name))))
       (string-append "/dev/input/"
                      (basename event))))
   (lambda (key . args)
     (log-error "Could not find a device with specified name: '~a'" name)
     #f)))


(define (is-seat-used? id)
  "Check if a seat with @var{id} is used."
  (define regexp (format #f ".* (:~a)$" id))
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
