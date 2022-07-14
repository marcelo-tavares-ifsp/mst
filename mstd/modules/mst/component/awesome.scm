;;; awesome.scm -- Awesome TWM procedures.

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

;; This module contains Awesome TWM procedures.


;;; Code:

(define-module (mst component awesome)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (awesome-pid
            awesome-wait
            awesome-proc-environ))

(define (awesome-pid)
  "Get the Awesome TWM PID."
  (let ((port (open-input-pipe "pidof awesome")))
    (and port
         (let ((pid (read-line port)))
           (close port)
           pid))))

(define (awesome-wait)
  (let ((pid (awesome-pid)))
    (when (eof-object? pid)
      (sleep 1)
      (awesome-wait))))

(define (pid->proc-environ-path pid)
  (format #f "/proc/~a/environ" pid))

(define (proc-environ->assoc-list environ-string)
  (map (lambda (entry)
         ;; (format (current-error-port)
         ;;         "entry: ~a~%"
         ;;         entry)
         (let ((lst (string-split entry #\=)))
           (cons (car lst) (cadr lst))))
       (filter (lambda (str)
                 (> (string-length str) 0))
               (string-split environ-string #\nul))))

(define (awesome-proc-environ)
  (let ((pid (awesome-pid)))
    (and pid
         (let ((port (open-input-file (pid->proc-environ-path pid))))
           (and port
                (let ((lst (proc-environ->assoc-list (read-line port))))
                  (close port)
                  lst))))))

;;; awesome.scm ends here.
