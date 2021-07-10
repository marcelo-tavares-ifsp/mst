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
  #:use-module (mst log)
  #:export (docker-container-running?
	    start-xephyr/docker
	    docker-stop
	    docker-container-rm

	    %make-command:xephyr/docker))


;;; Constants.

(define %docker-binary "/usr/bin/docker")
(define %xephyr-docker-image "gkaz/xephyr")
(define %xephyr-binary "/usr/bin/Xephyr")


;;;

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

(define (%make-command:xephyr/docker display-number resolution mouse keyboard)
  (let ((mouse-dev    (device-name->path mouse))
        (keyboard-dev (device-name->path keyboard)))

    (unless mouse-dev
      (log-error "Cannot find the specified mouse device: '~a'" mouse))

    (unless keyboard-dev
      (log-error "Cannot find the specified keyboard device: '~a'" keyboard))

    (if (and mouse-dev keyboard-dev)
        (string-join (list %docker-binary
                           "run"
                           "--rm"
                           "-it"
                           "-d"
                           "--device" mouse-dev
                           "--device" keyboard-dev
                           "-e" "DISPLAY=:0"
                           "-v" "/tmp/.X11-unix:/tmp/.X11-unix:rw"
                           %xephyr-docker-image
                           %xephyr-binary
                           "-softCursor"
                           "-ac"
                           "-br"
                           "-resizeable"
                           "-mouse" (format #f "evdev,5,device=~a" mouse-dev)
                           "-keybd" (format #f "evdev,,device=~a" keyboard-dev)
                           "-screen" (format #f "~a" resolution)
                           (format #f ":~a" display-number)))
        #f)))

(define (start-xephyr/docker display-number resolution mouse keyboard)
  (log-info "Starting Xephyr (~a) for display ~a; resolution: ~a; mouse: ~a; keyboard: ~a"
            %xephyr-docker-image
            display-number resolution mouse keyboard)
  (let ((command (%make-command:xephyr/docker display-number
                                              resolution
                                              mouse
                                              keyboard)))
    (if command

        (let ((port (open-input-pipe command)))
          (unless port
            (log-error "Could not start a Xephyr instance")
            (error "Could not start a Xephyr instance"))

          (let ((output (read-line port)))

            (when (eof-object? output)
              (log-error "Could not start a Xephyr instance")
              (error "Could not start a Xephyr instance"))

            (log-info "Xephyr is started.  Container ID: ~a" output)

            output))

        (begin
          (log-error "Could not make a Xephyr command")
          #f))))

(define (docker-stop id)
  "Stop a Docker container specified by an @var{id}."
  (system* %docker-binary "stop" id))

(define (docker-container-rm id)
  "Remove a Docker container specified by an @var{id}."
  (system* %docker-binary "container" "rm" id))

;;; docker.scm ends here.
