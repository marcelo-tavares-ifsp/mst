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
  #:use-module (mst log)
  #:export (notify-send
            display-number->user
            graphics-available?
            proc-get-pids
            proc-environ
            mount
            set-system-debug!

            %make-mount-command))


(define *debug?* #f)


(define (set-system-debug! value)
  (set! *debug?* value))


(define (notify-send display-number message)
  "Show a notify with a MESSAGE on the given DISPLAY-NUMBER."
  (system (string-append
           (format #f "DISPLAY=:~a" display-number)
           " /usr/bin/notify-send"
           " --urgency=critical"
           " --icon=error"
           (string-append " '" message "'"))))

;; Make a 'mount' command to mount a DEVICE for a specified USER.
(define (%make-mount-command device user)
  (string-append
   (format #f "sudo --user='~a' -- " user)
   "udisksctl mount --no-user-interaction --block-device "
   device))

(define (mount device user)
  "Mount a DEVICE for a USER by means of udisksctl command."
  (let ((command (%make-mount-command device user)))
    (when *debug?*
          (format #t "mount command: ~a~%" command))
    (system command)))

(define (display-number->user display-number)
  "Find out the user by its display number.  Return user name or #f if the
user is not found."
  (let ((port (open-input-pipe
               (string-append
                "who"
                " | "
                (format #f "grep '(:~a)'" display-number)
                " | "
                "sed -re 's/^([^ ]+) +.* \\(:[0-9]+\\)/\\1/g'"
                " | "
                "head -1"))))
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
