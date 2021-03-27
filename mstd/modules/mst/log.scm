;;; mstd -- MST daemon logging facilities.

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

;; This module contains procedures for logging.


;;; Code:

(define-module (mst log)
  #:use-module (ice-9 rdelim)
  #:export (log
            log-error
            log-warning
            log-info
            log-use-stderr!))

(define %logger "/usr/bin/logger")
(define %tag    "mstd")

(define *use-stderr?* #f)

(define (log-use-stderr! value)
  (set! *use-stderr?* value))

(define (log level fmt . args)
  (let* ((message (apply format #f fmt args))
         (command (format #f "~a ~a --priority=daemon.~a --tag='~a' '~a'"
                          %logger
                          (if *use-stderr?*
                              "--stderr"
                              "")
                          level
                          %tag
                          message))
         (result (system command)))
    (unless (zero? result)
      (error "Could not log a message"))))

(define (log-error fmt . args)
  (apply log "err" fmt args))

(define (log-warning fmt . args)
  (apply log "warning" fmt args))

(define (log-info fmt . args)
  (apply log "info" fmt args))

    
