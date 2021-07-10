;;; docker.scm -- MST docker procedures.

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

;; This module contains the procedures for working with Docker.


;;; Code:

(define-module (mst docker)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (docker-container-running?))

(define (docker-container-running? id)
  "Check if a Docker container with the given ID is running."
  (let ((command (format #f "docker container inspect -f '{{.State.Running}}' ~a"
                         id)))
    (let ((port (open-input-pipe command)))
      (catch #t
             (lambda ()
               (waitpid -1 WNOHANG))
             (lambda args
               #t))
      (if port
          (let ((result (read-line port)))
            (string=? result "true"))
          (begin
            (log-error "Could not run command: ~a" command)
            #f)))))

;;; docker.scm ends here.
