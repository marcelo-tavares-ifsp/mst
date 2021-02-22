(define-module (mst dm)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (add-seat
	    is-seat-used?
	    start-lightdm))

(define *debug?* #f)

(define (set-dm-debug! value)
  (set! *debug?* value))


(define (add-seat number)
  (system (format #f "/usr/bin/dm-tool add-local-x-seat ~a" number)))

(define (start-lightdm config-file)
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (system (format #f "/usr/sbin/lightdm --config ~a" config-file)))
     ((> pid 0)
      (when *debug?*
        (format #t "Lightdm started.  PID: ~a" pid))
      pid)
     (else
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
