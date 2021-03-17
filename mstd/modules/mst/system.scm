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
  #:export (notify-send
	    display-number->user
            graphics-available?
	    proc-get-pids
	    proc-environ
	    mount
	    set-system-debug!))


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

(define (mount device user)
  "Mount a DEVICE for a USER by means of udisksctl command."
  (let ((command (string-append
                  (format #f "sudo --user='~a' -- " user)
                  "udisksctl mount --no-user-interaction --block-device "
                  device)))
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
      (if (not (eof-object? result))
          result
          #f))))

(define (graphics-available?)
  (let* ((port   (open-input-pipe "who | grep -E '* :[0-9]+ *'"))
         (result (read-line port)))
    (not (eof-object? result))))


(define (proc-get-pids)
  (map string->number
       (scandir "/proc" (lambda (entry)
			  (string-match "[0-9]+" entry)))))

(define (proc-environ pid)
  "Get the environment of a process with the PID."
  (let ((port (open-input-file (format #f "/proc/~a/environ" pid))))
    (map (lambda (env)
	   (string-split env #\=))
	 (string-split (string-drop-right (read-line port) 1) #\nul))))
