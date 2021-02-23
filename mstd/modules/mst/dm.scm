(define-module (mst dm)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 ftw)
  #:export (add-seat
	    is-seat-used?
	    proc-get-pids
	    get-running-seats
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

(define (get-running-seats)
  "Get the number of running seats."
  (string->number
   (read-line
    (open-input-pipe "/usr/bin/dm-tool list-seats | grep -c 'Seat'"))))

(define (proc-get-pids)
  (scandir "/proc" (lambda (entry)
		     (string-match "[0-9]+" entry))))
    

(define (start-seats seat-number)
  (sleep 1))
  ;; (let loop ((idx 1))
  ;;   (if (not (is-seat-used? idx))

(define (main-loop seat-count)
  (start-lightdm "/etc/lightdm/lightdm-mst.conf")

  (system "xset -dpms")
  (system "xset s off")

  (let loop ((idx 1))
    (add-seat idx)
    (if (< idx seat-count)
	(loop (+ idx 1))))

  (while #t
	 (let ((running-seats-number (get-running-seats)))
	   (if (< running-seats-number seat-count)
	       (start-seats seat-count)))))
  
(define (dm-start seat-count)
  "Returns a new thread."
  (start-lightdm "/etc/lightdm/lightdm-mst.conf")
  (make-thread main-loop seat-count))
