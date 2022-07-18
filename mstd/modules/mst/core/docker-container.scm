;;; docker-container.scm -- Guile API for Docker containers.

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

;; This module contains GNU Guile API for running and controlling Docker
;; containers.


;;; Code:


(define-module (mst core docker-container)
  #:use-module (oop goops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (mst core log)
  #:export (<docker-container>
            make-docker-container
            docker-container?
            docker-container-id
            docker-container-stop!
            docker-container-rm!
            docker-container-running?))


;; TODO: Set the actual path to the Docker binary
;; during the build.
(define %docker-binary "/usr/bin/docker")


(define-class <docker-container> ()
  ;; <string>
  (id
   #:init-value   #f
   #:init-keyword #:id
   #:getter       docker-container-id))


(define-method (%display (docker-container <docker-container>) (port <port>))
  (format port "#<docker-container ~a ~a>"
          (docker-container-id docker-container)
          (number->string (object-address docker-container) 16)))

(define-method (display (docker-container <docker-container>) (port <port>))
  (%display docker-container port))

(define-method (write (docker-container <docker-container>) (port <port>))
  (%display docker-container port))


(define-method (docker-container? x)
  "Check if X is a <docker-container> instance."
  (is-a? x <docker-container>))


(define-method (docker-container-stop! (docker-container <docker-container>))
  "Stop a DOCKER-CONTAINER."
  (system* %docker-binary "stop" (docker-container-id docker-container)))

(define-method (docker-container-rm! (docker-container <docker-container>))
  "Remove a DOCKER-CONTAINER."
  (system* %docker-binary
           "container"
           "rm"
           (docker-container-id docker-container)))

(define-method (docker-container-running? (docker-container <docker-container>))
  "Check if a DOCKER-CONTAINER is running."
  (let ((command (format #f "docker container inspect -f '{{.State.Running}}' ~a"
                         (docker-container-id docker-container))))
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


;; Run a COMMAND inside a Docker container using a Docker IMAGE with the
;; specified ARGUMENTS.
;;
;; Return the container ID on succes.
;;
;; Throw an error on failure.
(define* (make-docker-container image
                                command
                                #:key
                                (environ '())
                                (volumes '())
                                (devices '())
                                (remove-on-exit? #t)
                                (detach? #t)
                                (interactive? #t)
                                (tty? #t))

  (define (container-error docker-command)
    (log-error "Could not start a Docker container: ~a"
               docker-command)
    (error "Could not start a Docker container"
           docker-command))

  (let* ((docker-command `(,%docker-binary
                           "run"
                           ,@(if interactive?
                                 '("-i")
                                 '())
                           ,@(if tty?
                                 '("-t")
                                 '())
                           ,@(if detach?
                                 '("-d")
                                 '())
                           ,@(fold (lambda (dev prev)
                                     (append prev
                                             (list "-d" dev)))
                                   '()
                                   devices)
                           ,@(fold (lambda (env prev)
                                     (append prev
                                             (list "-e" env)))
                                   '()
                                   environ)
                           ,@(fold (lambda (v prev)
                                     (append prev
                                             (list "-v" v)))
                                   '()
                                   volumes)
                           ,image
                           ,@command))
         (pipe (apply open-pipe* OPEN_READ docker-command)))
    (unless pipe
      (container-error docker-command))
    (let ((id (read-line pipe)))
      (close-pipe pipe)
      (when (eof-object? id)
        (container-error docker-command))
      (log-info "Docker container started: ~a"
                id)
      (make <docker-container> #:id id)))) 

;;; docker-container.scm ends here.
