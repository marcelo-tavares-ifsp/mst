(define-module (mst log)
  #:use-module (ice-9 rdelim)
  #:export (log
            log-error
            log-warning
            log-info))

(define %logger "/usr/bin/logger")
(define %tag    "mstd")

(define (log level fmt . args)
  (let* ((message (apply format #f fmt args))
         (command (format #f "~a --priority=daemon.~a --tag='~a' '~a'"
                          %logger
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

    
